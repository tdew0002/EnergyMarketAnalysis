safe_agg_by_region_and_time <- function(df, vars, region_filter = NULL) {
  df %>%
    collect() %>%
    { if (!is.null(region_filter) && length(region_filter) > 0) filter(., REGIONID %in% region_filter) else . } %>%
    mutate(timestamp_30m = floor_date(timestamp, "30 minutes")) %>%
    group_by(REGIONID, timestamp_30m) %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
}


safe_agg_by_duid_and_time <- function(df, vars, duid_filter = NULL) {
  has_duid <- "DUID" %in% names(df)
  
  # Filter on the lazy Arrow object before collecting
  if (!is.null(duid_filter) && has_duid) {
    df <- df %>% filter(DUID %in% duid_filter)
  }
  
  df <- collect(df)  # only collect after filtering
  
  # Determine fallback region
  if (!is.null(duid_filter) && has_duid) {
    duid_region <- df %>%
      filter(!is.na(REGIONID)) %>%
      distinct(DUID, REGIONID) %>%
      pull(REGIONID) %>%
      unique()
  } else {
    duid_region <- NULL
  }
  
  # Fallback to region filter if no DUID
  if (!has_duid && !is.null(duid_region) && "REGIONID" %in% names(df)) {
    df <- df %>% filter(REGIONID %in% duid_region)
  }
  
  df %>%
    mutate(timestamp_30m = lubridate::floor_date(timestamp, "30 minutes")) %>%
    {
      if (has_duid) {
        group_by(., DUID, REGIONID, timestamp_30m)
      } else if ("REGIONID" %in% names(.)) {
        group_by(., REGIONID, timestamp_30m)
      } else {
        group_by(., timestamp_30m)
      }
    } %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
}
