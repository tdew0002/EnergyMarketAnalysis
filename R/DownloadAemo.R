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
# Combined Efficient Downloader + Extractor + Parquet Converter for AEMO Data

main <- function(){
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
  library(fs)
  library(here)
  #Progress bar
  options(arrow.use_threads = TRUE)
  handlers("txtprogressbar")
  
  
  plan(multicore, workers = max(1, parallel::detectCores() - 1))
  
  # — ask the user whether to use the Archive or the Current reports folder
  repeat {
    rep_type <- readline("Select report type: [1] Archive ; [2] Current: ")
    if (rep_type %in% c("1","2")) break
    message("❌ Invalid choice. Please enter 1 or 2.")
  }
  if (rep_type == "1") {
    base_archive <- "https://nemweb.com.au/Reports/Archive/"
    strip_prefix <- "^/Reports/Archive/"
  } else {
    base_archive <- "https://nemweb.com.au/Reports/CURRENT/"
    strip_prefix <- "^/Reports/CURRENT/"
  }
  
  root_page <- read_html(base_archive)
  
  
  local_data_base   <- here("data/raw_data")
  parquet_output    <- here("data/parquet_tables")
  download_timeout  <- 600
  
  
  
  # ensure mapping folder exists
  mapping_dir <- here("mapping")
  dir_create(mapping_dir)
  mapping_rds <- here("mapping","dataset_table_mapping.rds")
  
  if (file.exists(mapping_rds)) {
    table_map <- readRDS(mapping_rds)
  } else {
    table_map <- list()
    saveRDS(table_map, mapping_rds)
  }
  
  # Function to update and save table map
  update_table_map <- function(dataset_name, new_table_names) {
    dataset_upper <- toupper(dataset_name)
    current_tables <- table_map[[dataset_upper]] %||% character(0)
    updated_tables <- unique(c(current_tables, new_table_names))
    if (!setequal(current_tables, updated_tables)) {
      table_map[[dataset_upper]] <<- updated_tables
      saveRDS(table_map, mapping_rds)
      message("📌 Updated mapping for ", dataset_upper, ": ", paste(updated_tables, collapse = ", "))
    }
  }
  
  # Step 1: Choose dataset
  dataset_folders <- html_elements(root_page, "a") %>%
    html_attr("href") %>%
    str_subset("/$") %>%
    discard(~ .x %in% c("../")) %>%
    str_remove(strip_prefix)
  
  
  cat("\U0001F4E6 Available dataset folders from AEMO Archive:\n")
  for (i in seq_along(dataset_folders)) {
    cat(sprintf("[%02d] %s\n", i, dataset_folders[[i]]))
  }
  index <- as.integer(readline("\U0001F522 Enter the number of the dataset folder to download from: "))
  dataset_path <- dataset_folders[index]
  current_url <- paste0(base_archive, dataset_path)
  dataset_name <- gsub("/$", "", dataset_path)
  dataset_upper <- toupper(dataset_name)
  
  local_base <- file.path(local_data_base, dataset_name)
  output_base <- parquet_output
  
  # Use the table map to determine the correct table folders to check for parquet files
  expected_tables <- table_map[[dataset_upper]] %||% character(0)
  expected_dirs <- file.path(output_base, expected_tables)
  
  # Create necessary directories
  if (!dir_exists(local_base)) dir_create(local_base)
  if (!dir_exists(output_base)) dir_create(output_base)
  
  # Check for existing parquet files across expected table folders
  existing_parquet_dates <- unique(unlist(
    lapply(expected_dirs[file.exists(expected_dirs)], function(dir) {
      dir(dir, pattern = "\\.parquet$", full.names = TRUE) %>%
        basename() %>%
        str_extract("\\d{8}")
    })
  )) %>% na.omit() %>% unique()
  
  extract_id_tables <- function(file_path) {
    raw_lines      <- readLines(file_path, warn = FALSE)
    # find all the "I," header rows
    header_indices <- grep("^I,", raw_lines, ignore.case = TRUE)
    if (length(header_indices) == 0) return(NULL)
    
    # helper to fallback on the dataset folder name
    get_dataset_name <- function(path) {
      parts      <- str_split(normalizePath(path), .Platform$file.sep)[[1]]
      data_index <- which(parts == "data")
      if (length(data_index) > 0 && length(parts) > data_index)
        toupper(parts[data_index + 1])
      else
        "UNKNOWNTABLE"
    }
    fallback_name <- get_dataset_name(file_path)
    
    tables       <- list()
    total_tables <- length(header_indices)
    name_count   <- list()
    
    for (i in seq_along(header_indices)) {
      start_idx <- header_indices[i]
      end_idx   <- if (i < total_tables) header_indices[i + 1] - 1 else length(raw_lines)
      chunk     <- raw_lines[start_idx:end_idx]
      
      # read the header row to pick the name
      header_row <- fread(text = raw_lines[start_idx], fill = TRUE, header = FALSE)
      # choose name column:
      if (total_tables == 1) {
        raw_name <- as.character(header_row[[2]])
      } else {
        # try col 3 first, fallback to col 2
        c3 <- as.character(header_row[[3]])
        raw_name <- if (!is.na(c3) && nzchar(c3)) c3 else as.character(header_row[[2]])
      }
      
      # sanitize and fallback if needed
      nm <- toupper(gsub("[^A-Za-z0-9_]", "_", raw_name))
      if (!nzchar(nm)) nm <- fallback_name
      
      # de-duplicate: keep track of how many times we've seen 'nm'
      name_count[[nm]] <- (name_count[[nm]] %||% 0) + 1
      if (name_count[[nm]] > 1) nm <- paste0(nm, "_", name_count[[nm]])
      
      # finally parse that chunk and append to the right table
      df <- fread(text = paste(chunk, collapse = "\n"), fill = TRUE, showProgress = FALSE)
      if (!is.null(tables[[nm]])) {
        tables[[nm]] <- rbind(tables[[nm]], df, fill = TRUE)
      } else {
        tables[[nm]] <- df
      }
    }
    
    tables
  }
  
  
  navigate_until_zip <- function(url, level = 1) {
    page <- read_html(url)
    links <- html_elements(page, "a") %>% html_attr("href") %>% discard(is.na)
    zip_links <- links %>% str_subset("\\.zip$")
    folder_links <- links %>% str_subset("/$") %>% discard(~ .x %in% c("../"))
    
    if (length(zip_links) > 0) return(list(zip_files = zip_links, final_url = url))
    if (length(folder_links) == 0) stop("❌ No zip files or subfolders found.")
    cat("\n", strrep("—", level), "\U0001F4C1 Subfolders:\n", sep = "")
    folder_names <- basename(str_remove(folder_links, "/$"))
    for (i in seq_along(folder_links)) cat(sprintf("[%02d] %s\n", i, folder_names[i]))
    sub_index <- as.integer(readline("\U0001F522 Select a subfolder: "))
    next_url <- url_absolute(folder_links[sub_index], url)
    navigate_until_zip(next_url, level + 1)
  }
  
  result <- navigate_until_zip(current_url)
  zip_files <- result$zip_files
  final_url <- result$final_url
  
  available_dates <- str_extract(zip_files, "\\d{8}") %>% na.omit() %>% unique() %>% sort()
  
  existing_csv_dirs <- dir(local_base, full.names = TRUE, recursive = FALSE)
  existing_csv_dates <- existing_csv_dirs %>%
    keep(~ length(fs::dir_ls(.x, recurse = TRUE, regexp = "(?i)\\.csv$", type = "file")) > 0) %>%
    basename() %>%
    str_extract("\\d{8}") %>%
    na.omit() %>%
    unique()
  
  processable_dates <- setdiff(available_dates, union(existing_parquet_dates, existing_csv_dates))
  reprocess_dates <- setdiff(existing_csv_dates, existing_parquet_dates)
  
  if (length(processable_dates) == 0 && length(reprocess_dates) == 0) {
    message("✅ You are already up to date. Nothing to download.")
    return(invisible(NULL))
  }
  
  if (length(processable_dates) > 0) {
    cat("\n📅 Dates to be downloaded and processed:\n")
    print(processable_dates)
  }
  if (length(reprocess_dates) > 0) {
    cat("\n🔄 Dates with CSVs but missing parquet (to be reprocessed):\n")
    print(reprocess_dates)
  }
  
  all_dates <- c(processable_dates, reprocess_dates)
  repeat {
    user_input <- readline(
      "📥 Press Enter=all, YYYYMMDD, 'b'=change dataset, 'q'=quit: "
    )
    # Quit the entire function
    if (tolower(user_input) == "q") {
      message("👋 Goodbye.")
      return(invisible(NULL))
    }
    # Back to dataset selection
    if (tolower(user_input) == "b") {
      message("🔙 Returning to dataset menu.")
      return(main())
    }
    # All dates
    if (user_input == "") {
      missing_dates <- c(processable_dates, reprocess_dates)
      break
    }
    # Single-date
    if (user_input %in% c(processable_dates, reprocess_dates)) {
      missing_dates <- user_input
      break
    }
    message("❌ That date is not available. Please try again.")
  }
  
  
  with_progress({
    p <- progressor(along = missing_dates)
    for (date in missing_dates) {
      tryCatch({
        zip_match <- zip_files[str_detect(zip_files, date)]
        if (length(zip_match) == 0) {
          message("⚠️ No zip found for date: ", date)
          next
        }
        
        file_name <- basename(zip_match[1])
        full_url <- paste0(final_url, file_name)
        zip_path <- file.path(tempdir(), file_name)
        zip_base <- file_path_sans_ext(file_name)
        extract_dir <- file.path(local_base, zip_base)
        
        new_download <- FALSE
        
        if (!dir_exists(extract_dir) || length(dir(extract_dir, pattern = "(?i)\\.csv$", recursive = TRUE)) == 0) {
          if (!file_exists(zip_path)) {
            message("⬇️  Downloading: ", file_name)
            GET(full_url, write_disk(zip_path, overwrite = TRUE), timeout(600))
          }
          dir_create(extract_dir)
          unzip(zip_path, exdir = extract_dir)
          inner_zips <- dir(extract_dir, pattern = "\\.zip$", full.names = TRUE)
          for (z in inner_zips) {
            unzip(z, exdir = extract_dir)
            file_delete(z)
          }
          new_download <- TRUE
        } else {
          message("📁 Using existing CSVs for ", date)
        }
        
        csv_files <- dir(extract_dir, pattern = "(?i)\\.csv$", full.names = TRUE, recursive = TRUE)
        if (length(csv_files) == 0) {
          warning("⚠️ No CSVs found in: ", extract_dir)
          return(NULL)
        }
        
        
        # Discover table names by re-using our robust extract_id_tables()
        discovered_tables <- unique(unlist(
          lapply(csv_files, function(f) {
            tbls <- extract_id_tables(f)
            if (is.null(tbls)) NULL else names(tbls)
          })
        ))
        
        # Update mapping if brand-new download, or if we have no mapping yet
        if ( new_download ||
             is.null(table_map[[dataset_upper]]) ||
             length(table_map[[dataset_upper]])==0 ) {
          update_table_map(dataset_name, discovered_tables)
        }
        
        combined_tables <- list()
        for (csv_file in csv_files) {
          tables <- extract_id_tables(csv_file)
          if (is.null(tables)) next
          for (name in names(tables)) {
            if (!is.null(combined_tables[[name]])) {
              combined_tables[[name]] <- rbind(combined_tables[[name]], tables[[name]], fill = TRUE)
            } else {
              combined_tables[[name]] <- tables[[name]]
            }
          }
        }
        
        for (name in names(combined_tables)) {
          dir_create(file.path(output_base, name))
          parquet_path <- file.path(output_base, name, paste0(zip_base, ".parquet"))
          write_parquet(combined_tables[[name]], parquet_path)
          message("✅ Saved: ", parquet_path)
        }
        
        file_delete(csv_files)
        message("🧹 Deleted original CSVs for ", date)
      }, error = function(e) {
        warning("❌ Error while processing ", date, ": ", e$message)
      })
      p()
    }
  })
  
  message("🎉 All downloads, extraction, and parquet conversion completed.")
}

#' @title  Simple “a %||% b” helper
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b
