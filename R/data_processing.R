# R/data_processing.R

#' @title  Process a large Parquet file in chunks
#' @description
#'   Read `file` in slices of `chunk_size`, apply filter/select/timestamp
#'   from `rule`, and return a combined data.table.
#' @param file       Path to one Parquet file.
#' @param table_name AEMO table name (key in `filtering_rules`).
#' @param rule       A list with elements `$filter`, `$select`, `$timestamp`.
#' @param chunk_size Number of rows per slice (default: 1e6).
#' @return A data.table of filtered rows, or NULL if none survive.
#' @importFrom arrow read_parquet
#' @importFrom data.table as.data.table rbindlist
#' @export
process_file_in_chunks <- function(file, table_name, rule, chunk_size = 1e6) {
  tbl <- arrow::read_parquet(file, as_data_frame = FALSE)
  nrows <- tbl$num_rows
  chunks <- vector("list", length = ceiling(nrows / chunk_size))
  idx    <- 0
  
  for (start in seq(1, nrows, by = chunk_size)) {
    end    <- min(start + chunk_size - 1, nrows)
    df     <- tbl$Slice(offset = start - 1, length = end - start + 1)$to_data_frame()
    dt     <- data.table::as.data.table(df)
    
    # fix duplicate OPERATIONAL_DEMAND columns
    if (table_name == "OPERATIONAL_DEMAND") {
      dupes <- which(names(dt) == "OPERATIONAL_DEMAND")
      if (length(dupes) > 1) dt[, (dupes[-length(dupes)]) := NULL]
    }
    
    # apply filter
    dt_filt <- tryCatch(rule$filter(dt), error = \(e) NULL)
    if (is.null(dt_filt) || nrow(dt_filt) == 0) next
    
    # add region/fuel if applicable
    dt_filt <- add_regionid_if_applicable(dt_filt)
    
    # select columns
    sel_cols <- rule$select(dt_filt)
    if (length(sel_cols) == 0) next
    dt_sel <- dt_filt[, ..sel_cols]
    
    # add timestamp
    if (!is.null(rule$timestamp)) {
      tryCatch(rule$timestamp(dt_sel), error = \(e) NULL)
    }
    
    idx <- idx + 1
    chunks[[idx]] <- dt_sel
  }
  
  if (idx == 0) return(NULL)
  data.table::rbindlist(chunks[1:idx], use.names = TRUE)
}

#' @title  Process a single Parquet file
#' @description
#'   Read entire `file`, apply `rule`, and return filtered data.table.
#'   For very large tables it delegates to `process_file_in_chunks()`.
#' @param file       Path to one Parquet file.
#' @param table_name AEMO table name.
#' @param rule       A list with `$filter`, `$select`, `$timestamp`.
#' @return A data.table or NULL.
#' @importFrom arrow read_parquet
#' @importFrom data.table as.data.table
#' @export
files_processor <- function(file, table_name, rule) {
  if (table_name %in% c("BIDPEROFFER_D", "OUTAGEDETAIL")) {
    return(process_file_in_chunks(file, table_name, rule))
  }
  
  dt <- data.table::as.data.table(arrow::read_parquet(file))
  
  if (table_name == "OPERATIONAL_DEMAND") {
    dupes <- which(names(dt) == "OPERATIONAL_DEMAND")
    if (length(dupes) > 1) dt[, (dupes[-length(dupes)]) := NULL]
  }
  
  dt_filt <- rule$filter(dt)
  dt_filt <- add_regionid_if_applicable(dt_filt)
  
  sel_cols <- rule$select(dt_filt)
  if (length(sel_cols) == 0) return(NULL)
  
  dt_sel <- dt_filt[, ..sel_cols]
  if (!is.null(rule$timestamp)) rule$timestamp(dt_sel)
  dt_sel
}

#' @title  Run filtering over all new Parquet files
#' @description
#'   For each table in `filtering_rules`, find unprocessed files under `raw_dir`,
#'   apply either `process_file_in_chunks()` or `files_processor()`, and write out
#'   filtered Parquet under `out_dir`, tracking progress in “.processed_files.txt”.
#' @param raw_dir Directory of raw parquet tables (defaults to `data/parquet_tables`).
#' @param out_dir Directory for filtered parquet output (defaults to `data/filtered_parquet_tables`).
#' @return Invisibly NULL (side‐effects: writes filtered parquet files).
#' @importFrom fs dir_create dir_exists file_exists
#' @importFrom arrow write_parquet set_cpu_count
#' @importFrom data.table setDTthreads
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' @export
run_filtering <- function(
    raw_dir = here::here("data","parquet_tables"),
    out_dir = here::here("data","filtered_parquet_tables")
) {
  fs::dir_create(out_dir)
  arrow::set_cpu_count(parallel::detectCores())
  data.table::setDTthreads(0L)
  future::plan("multicore", workers = max(1L, parallel::detectCores() - 1L))
  
  for (table_name in names(filtering_rules)) {
    input_dir  <- file.path(raw_dir, table_name)
    output_dir <- file.path(out_dir, table_name)
    meta_file  <- file.path(output_dir, ".processed_files.txt")
    
    if (!fs::dir_exists(input_dir)) {
      message("⚠️ Skipping missing directory: ", table_name)
      next
    }
    fs::dir_create(output_dir)
    
    processed <- if (fs::file_exists(meta_file)) readLines(meta_file) else character()
    all_files <- dir(input_dir, pattern = "\\.parquet$", full.names = TRUE)
    to_proc   <- all_files[!basename(all_files) %in% processed]
    if (length(to_proc) == 0) {
      message("✅ ", table_name, ": up to date.")
      next
    }
    
    rule <- filtering_rules[[table_name]]
    message("🚀 Processing ", table_name, " (", length(to_proc), " new files)…")
    
    results <- future.apply::future_lapply(to_proc, function(f) {
      dt <- tryCatch(
        if (table_name %in% c("BIDPEROFFER_D","OUTAGEDETAIL")) {
          process_file_in_chunks(f, table_name, rule)
        } else {
          files_processor(f, table_name, rule)
        },
        error = \(e) NULL
      )
      if (!is.null(dt) && nrow(dt) > 0L) {
        out_fp <- file.path(output_dir, basename(f))
        arrow::write_parquet(dt, out_fp)
        message("✅ Saved: ", out_fp)
        return(basename(f))
      }
      NULL
    })
    
    new_processed <- unlist(results, use.names = FALSE)
    if (length(new_processed)) {
      writeLines(unique(c(processed, new_processed)), meta_file)
    }
  }
  
  invisible(NULL)
}
