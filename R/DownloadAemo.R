# R/DownloadAemo.R

#' @title  Download, extract, and convert AEMO CSVs → Parquet
#' @description
#'   1. Prompts the user to choose Archive vs Current.  
#'   2. Lists dataset folders and lets the user pick one.  
#'   3. Drills down until ZIPs are found.  
#'   4. Downloads missing ZIPs, extracts CSVs, combines each “ID table,” 
#'      and writes to Parquet under `data/parquet_tables/<TABLE_NAME>/<YYYYMMDD>.parquet`.  
#'   5. Cleans up raw CSVs.  
#' @return Invisibly `NULL`. Side‐effects: writes Parquet files under `data/parquet_tables/`.
#' @examples
#' \dontrun{
#'   AemoETL::main()
#' }
#' @export
main <- function() {
  # —————— Load required packages at runtime ——————
  library(rvest)
  library(stringr)
  library(httr)
  library(fs)
  library(purrr)
  library(tools)
  library(data.table)
  library(arrow)
  library(progressr)
  library(future)
  library(future.apply)
  library(here)
  library(Microsoft365R)
  
  #Progress bar & parallelism
  options(arrow.use_threads = TRUE)
  progressr::handlers("txtprogressbar")
  future::plan(multicore, workers = max(1, parallel::detectCores() - 1))
  
  # ── Prompt user: Archive or Current ──
  repeat {
    rep_type <- readline("Select report type: [1] Archive ; [2] Current: ")
    if (rep_type %in% c("1","2")) break
    message("❌ Invalid choice. Please enter 1 or 2.")
  }
  if (rep_type == "1") {
    base_archive <- "https://nemweb.com.au/Reports/Archive/"
    strip_prefix <- "^/Reports/Archive/"
  } else {
    base_archive <- "https://nemweb.com.au/Reports/Current/"
    strip_prefix <- "^/Reports/Current/"
  }
  
  root_page       <- rvest::read_html(base_archive)
  local_data_base <- here::here("data", "raw_data")
  parquet_output  <- here::here("data", "parquet_tables")
  download_timeout <- 600
  
  # ensure mapping folder exists
  mapping_dir <- here::here("mapping")
  fs::dir_create(mapping_dir)
  mapping_rds <- here::here("mapping","dataset_table_mapping.rds")
  
  if (fs::file_exists(mapping_rds)) {
    table_map <- readRDS(mapping_rds)
  } else {
    table_map <- list()
    saveRDS(table_map, mapping_rds)
  }
  
  # Function to update and save table map
  update_table_map <- function(dataset_name, new_table_names) {
    dataset_upper   <- toupper(dataset_name)
    current_tables  <- table_map[[dataset_upper]] %||% character(0)
    updated_tables  <- unique(c(current_tables, new_table_names))
    if (!setequal(current_tables, updated_tables)) {
      table_map[[dataset_upper]] <<- updated_tables
      saveRDS(table_map, mapping_rds)
      message("📌 Updated mapping for ", dataset_upper, ": ", paste(updated_tables, collapse = ", "))
    }
  }
  
  # Step 1: Choose dataset folder
  dataset_folders <- rvest::html_elements(root_page, "a") %>%
    rvest::html_attr("href") %>%
    stringr::str_subset("/$") %>%
    purrr::discard(~ .x %in% c("../")) %>%
    stringr::str_remove(strip_prefix)
  
  cat("\U0001F4E6 Available dataset folders from AEMO Archive:\n")
  for (i in seq_along(dataset_folders)) {
    cat(sprintf("[%02d] %s\n", i, dataset_folders[[i]]))
  }
  index        <- as.integer(readline("\U0001F522 Enter the number of the dataset folder to download from: "))
  dataset_path <- dataset_folders[index]
  current_url  <- paste0(base_archive, dataset_path)
  dataset_name <- gsub("/$", "", dataset_path)
  dataset_upper<- toupper(dataset_name)
  
  local_base  <- file.path(local_data_base, dataset_name)
  output_base <- parquet_output
  
  expected_tables <- table_map[[dataset_upper]] %||% character(0)
  expected_dirs   <- file.path(output_base, expected_tables)
  
  if (!fs::dir_exists(local_base))  fs::dir_create(local_base)
  if (!fs::dir_exists(output_base)) fs::dir_create(output_base)
  
  # Determine existing Parquet dates
  existing_parquet_dates <- unique(unlist(
    lapply(expected_dirs[fs::dir_exists(expected_dirs)], function(dir) {
      list.files(dir, pattern="\\.parquet$", full.names=TRUE) %>%
        basename() %>%
        stringr::str_extract("\\d{8}")
    })
  )) %>% na.omit() %>% unique()
  
  # Define helper to extract ID‐tables
  extract_id_tables <- function(file_path) {
    raw_lines <- readLines(file_path, warn = FALSE)
    header_indices <- grep("^I,", raw_lines, ignore.case = TRUE)
    if (length(header_indices) == 0) return(NULL)
    
    tables <- list(); name_count <- list()
    for (i in seq_along(header_indices)) {
      start_idx <- header_indices[i]
      end_idx   <- if (i < length(header_indices)) header_indices[i+1]-1 else length(raw_lines)
      chunk     <- raw_lines[start_idx:end_idx]
      
      header_row <- data.table::fread(text = raw_lines[start_idx], fill = TRUE, header = FALSE)
      raw_name   <- if (length(header_indices)==1) as.character(header_row[[2]]) else {
        c3 <- as.character(header_row[[3]]); if (!is.na(c3) && nzchar(c3)) c3 else as.character(header_row[[2]])
      }
      nm <- toupper(gsub("[^A-Za-z0-9_]", "_", raw_name))
      if (!nzchar(nm)) nm <- toupper(basename(dirname(file_path)))
      
      name_count[[nm]] <- (name_count[[nm]] %||% 0) + 1
      if (name_count[[nm]] > 1) nm <- paste0(nm, "_", name_count[[nm]])
      
      df <- data.table::fread(text = paste(chunk, collapse="\n"), fill = TRUE)
      tables[[nm]] <- if (!is.null(tables[[nm]])) rbind(tables[[nm]], df, fill=TRUE) else df
    }
    tables
  }
  
  # Recursive folder drill-down until .zip files are found
  navigate_until_zip <- function(url, level = 1) {
    page      <- rvest::read_html(url)
    links     <- rvest::html_elements(page, "a") %>% rvest::html_attr("href") %>% purrr::discard(is.na)
    zip_links <- links %>% stringr::str_subset("\\.zip$")
    folder_links <- links %>% stringr::str_subset("/$") %>% purrr::discard(~ .x %in% c("../"))
    
    if (length(zip_links) > 0) return(list(zip_files = zip_links, final_url = url))
    if (length(folder_links) == 0) stop("❌ No zip files or subfolders found.")
    cat("\n", strrep("—", level), "\U0001F4C1 Subfolders:\n", sep="")
    names <- basename(stringr::str_remove(folder_links, "/$"))
    for (i in seq_along(folder_links)) cat(sprintf("[%02d] %s\n", i, names[i]))
    sub_index <- as.integer(readline("\U0001F522 Select a subfolder: "))
    next_url  <- xml2::url_absolute(folder_links[sub_index], url)
    navigate_until_zip(next_url, level + 1)
  }
  
  # Find date codes to process
  result            <- navigate_until_zip(current_url)
  zip_files         <- result$zip_files
  final_url         <- result$final_url
  available_dates   <- unique(na.omit(stringr::str_extract(zip_files, "\\d{8}")))
  existing_csv_dirs <- list.dirs(local_base, full.names=TRUE, recursive=FALSE)
  existing_csv_dates<- existing_csv_dirs %>%
    purrr::keep(~ length(fs::dir_ls(.x, recurse=TRUE, regexp="(?i)\\.csv$"))>0) %>%
    basename() %>% stringr::str_extract("\\d{8}") %>% na.omit() %>% unique()
  
  processable_dates <- setdiff(available_dates, union(existing_parquet_dates, existing_csv_dates))
  reprocess_dates   <- setdiff(existing_csv_dates, existing_parquet_dates)
  
  if (length(processable_dates)==0 && length(reprocess_dates)==0) {
    message("✅ Up to date. Nothing to download."); return(invisible(NULL))
  }
  if (length(processable_dates)>0) cat("\n📅 Dates to download:", paste(processable_dates, collapse=", "), "\n")
  if (length(reprocess_dates)>0) cat("\n🔄 Dates to reprocess:", paste(reprocess_dates, collapse=", "), "\n")
  
  all_dates <- c(processable_dates, reprocess_dates)
  repeat {
    ans <- readline("📥 Enter date or Enter=all, 'b'=back, 'q'=quit: ")
    if (tolower(ans)=="q") return(invisible(NULL))
    if (tolower(ans)=="b") return(main())
    missing_dates <- if (ans=="") all_dates else if (ans %in% all_dates) ans else { message("❌ Try again."); next }
    break
  }
  
  # Download + extract + parquet conversion
  progressr::with_progress({
    p <- progressr::progressor(along = missing_dates)
    for (dt in missing_dates) {
      try({
        zmatches <- zip_files[stringr::str_detect(zip_files, dt)]
        if (length(zmatches)==0) { warning("No zip for ",dt); p(); next }
        fname       <- basename(zmatches[1])
        zip_path    <- file.path(tempdir(), fname)
        extract_dir <- file.path(local_base, tools::file_path_sans_ext(fname))
        
        if (!fs::dir_exists(extract_dir) ||
            length(list.files(extract_dir, "(?i)\\.csv$", recursive=TRUE))==0) {
          if (!fs::file_exists(zip_path)) {
            message("⬇️ Downloading ", fname)
            httr::GET(paste0(final_url, fname), httr::write_disk(zip_path, overwrite=TRUE), httr::timeout(download_timeout))
          }
          fs::dir_create(extract_dir)
          unzip(zip_path, exdir=extract_dir)
          inner <- list.files(extract_dir, "\\.zip$", full.names=TRUE)
          for (z in inner) { unzip(z, exdir=extract_dir); fs::file_delete(z) }
        } else {
          message("📁 Using existing CSVs for ", dt)
        }
        
        csvs <- list.files(extract_dir, "(?i)\\.csv$", full.names=TRUE, recursive=TRUE)
        tables <- unlist(lapply(csvs, extract_id_tables), recursive=FALSE)
        for (nm in names(tables)) {
          odir <- file.path(parquet_output, nm); fs::dir_create(odir)
          outp <- file.path(odir, paste0(dt, ".parquet"))
          arrow::write_parquet(tables[[nm]], outp)
          message("✅ Saved: ", outp)
        }
        fs::dir_delete(extract_dir)
      }, silent=TRUE)
      p()
    }
  })
  
  message("🎉 All downloads, extraction, and parquet conversion completed.")
}

#' @title  Simple “a %||% b” helper
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b
