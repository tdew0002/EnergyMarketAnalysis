# R/bidstack.R

#' @title 5-Minute Bidstack Snapshot
#' @description
#'   For a single 5-minute timestamp, builds a cumulative supply (bidstack) curve by:
#'   1. Averaging each PRICEBAND over the day  
#'   2. Trimming 5-min BANDAVAIL by the highest intermittent forecast  
#'   3. Joining FuelType and computing cumulative volume  
#'   4. Marking total cleared volume with a dashed line  
#' @param bids_dir   Path to BIDDAYOFFER_D parquet folder  
#' @param per_dir    Path to BIDPEROFFER_D parquet folder  
#' @param interm_dir Path to INTERMITTENT_DS_PRED parquet folder  
#' @param units_dir  Path to UNIT_SOLUTION parquet folder  
#' @param region     Region code (e.g. "VIC1" or "all")  
#' @param ts         Timestamp string "YYYY-MM-DD HH:MM" in Australia/Brisbane tz  
#' @param price_cap  Maximum price on the y-axis (default: 500)  
#' @return A **ggplot2** object  
#' @importFrom arrow open_dataset  
#' @importFrom dplyr filter select collect group_by summarise pull left_join mutate ungroup  
#' @importFrom tidyr pivot_longer  
#' @importFrom stringr str_extract  
#' @importFrom lubridate ymd_hm as_date as_datetime days  
#' @importFrom ggplot2 ggplot aes geom_rect geom_vline scale_x_continuous scale_y_continuous coord_cartesian labs theme_minimal theme  
#' @importFrom scales number_format dollar_format  
#' @export
create_bidstack_5min_snapshot <- function(
    bids_dir, per_dir, interm_dir, units_dir,
    region, ts, price_cap = 500
) {
  # 1) parse inputs
  dt        <- lubridate::ymd_hm(ts, tz = "Australia/Brisbane")
  day_start <- lubridate::as_date(dt)
  day0      <- lubridate::as_datetime(day_start)
  day1      <- day0 + lubridate::days(1)
  regs      <- if (tolower(region)=="all") NULL else strsplit(region, ",\\s*")[[1]]
  
  # 2) PRICEBANDs: average each band over the day
  daily_price_tbl <- arrow::open_dataset(bids_dir) %>%
    dplyr::filter(timestamp >= day0, timestamp < day1) %>%
    { if (!is.null(regs)) dplyr::filter(., REGIONID %in% regs) else . } %>%
    dplyr::select(REGIONID, DUID, dplyr::starts_with("PRICEBAND")) %>%
    dplyr::collect() %>%
    dplyr::group_by(REGIONID, DUID) %>%
    dplyr::summarise(across(dplyr::starts_with("PRICEBAND"), ~ mean(.x, na.rm=TRUE)), .groups="drop") %>%
    tidyr::pivot_longer(dplyr::starts_with("PRICEBAND"), names_to="Band", values_to="Price") %>%
    dplyr::mutate(Band = as.integer(stringr::str_extract(Band, "\\d+")))
  
  # 3) INTERMITTENT forecast: pick highest priority at timestamp
  interm_tbl <- arrow::open_dataset(interm_dir) %>%
    dplyr::filter(timestamp == dt) %>%
    dplyr::collect() %>%
    dplyr::group_by(DUID, timestamp) %>%
    dplyr::filter(
      FORECAST_PRIORITY == max(FORECAST_PRIORITY),
      OFFERDATETIME    == max(OFFERDATETIME)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(DUID, FORECAST_POE50, timestamp)
  
  # 4) 5-min BANDAVAILs, trimmed by FORECAST_POE50
  avail_tbl <- arrow::open_dataset(per_dir) %>%
    dplyr::filter(timestamp == dt) %>%
    { if (!is.null(regs)) dplyr::filter(., REGIONID %in% regs) else . } %>%
    dplyr::select(REGIONID, DUID, timestamp, dplyr::starts_with("BANDAVAIL"), MAXAVAIL) %>%
    dplyr::collect() %>%
    tidyr::pivot_longer(dplyr::starts_with("BANDAVAIL"), names_to="Band", values_to="MW") %>%
    dplyr::mutate(Band = as.integer(stringr::str_extract(Band, "\\d+"))) %>%
    dplyr::left_join(interm_tbl, by = c("DUID", "timestamp")) %>%
    dplyr::mutate(
      effective_maxavail = ifelse(
        !is.na(FORECAST_POE50),
        pmin(FORECAST_POE50, MAXAVAIL),
        MAXAVAIL
      )
    ) %>%
    dplyr::group_by(REGIONID, DUID, timestamp) %>%
    dplyr::mutate(
      totalMW   = sum(MW, na.rm = TRUE),
      over      = totalMW - effective_maxavail,
      maxBandNZ = ifelse(any(MW > 0), max(Band[MW > 0]), NA_integer_)
    ) %>%
    dplyr::mutate(
      MW = ifelse(over > 0 & Band == maxBandNZ, pmax(MW - over, 0), MW)
    ) %>%
    dplyr::ungroup()
  
  # 5) fuel lookup
  fuel_tbl <- arrow::open_dataset(units_dir) %>%
    dplyr::select(DUID, FuelType) %>%
    dplyr::distinct() %>%
    dplyr::collect()
  
  # 6) build cumulative curve
  df <- daily_price_tbl %>%
    dplyr::inner_join(avail_tbl, by = c("REGIONID","DUID","Band")) %>%
    dplyr::left_join(fuel_tbl, by = "DUID") %>%
    dplyr::filter(!is.na(Price), !is.na(MW)) %>%
    dplyr::group_by(FuelType, Price, Band) %>%
    dplyr::summarise(MW = mean(MW, na.rm=TRUE), .groups="drop") %>%
    dplyr::arrange(Price) %>%
    dplyr::mutate(CumVol = cumsum(MW))
  
  # 7) total cleared
  total_cleared <- arrow::open_dataset(units_dir) %>%
    dplyr::filter(timestamp == dt) %>%
    { if (!is.null(regs)) dplyr::filter(., REGIONID %in% regs) else . } %>%
    dplyr::summarise(total_cleared = sum(TOTALCLEARED, na.rm=TRUE)) %>%
    dplyr::collect() %>%
    dplyr::pull(total_cleared)
  
  # 8) plot
  ggplot2::ggplot(df, ggplot2::aes(fill = FuelType)) +
    ggplot2::geom_rect(ggplot2::aes(
      xmin = CumVol - MW,
      xmax = CumVol,
      ymin = 0,
      ymax = Price
    )) +
    ggplot2::geom_vline(
      xintercept = total_cleared,
      linetype   = "dashed",
      size       = 1
    ) +
    ggplot2::scale_x_continuous(
      "Cumulative Offer Volume (MW)",
      labels = scales::number_format(scale = 1e-3, suffix = "K")
    ) +
    ggplot2::scale_y_continuous(
      "Offer Price ($/MWh)",
      labels = scales::dollar_format()
    ) +
    ggplot2::coord_cartesian(ylim = c(0, price_cap)) +
    ggplot2::labs(
      title    = paste0("Bidstack at ", ts, " (", region, ")"),
      subtitle = paste0("Total cleared: ", round(total_cleared), " MW"),
      fill     = "Fuel Type"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(panel.grid.minor = element_blank())
}


# R/bidstack.R

#' @title Daily Bidstack Animation
#' @description
#'   Creates an animated supply curve over every 5-minute interval of a day,
#'   annotated with marginal price and cleared volume.
#' @param bids_dir     Path to BIDDAYOFFER_D parquet folder  
#' @param per_dir      Path to BIDPEROFFER_D parquet folder  
#' @param interm_dir   Path to INTERMITTENT_DS_PRED parquet folder  
#' @param units_dir    Path to UNIT_SOLUTION parquet folder  
#' @param rrp_dir      Path to DREGION parquet folder  
#' @param region       Region code (e.g. "VIC1" or "all")  
#' @param date         Date string "YYYY-MM-DD" in Australia/Brisbane tz  
#' @param price_cap    Maximum price on the y-axis (default: 500)  
#' @return A **gganimate** object  
#' @importFrom arrow open_dataset  
#' @importFrom dplyr filter select collect group_by summarise slice_head ungroup left_join  
#' @importFrom tidyr pivot_longer  
#' @importFrom stringr str_extract  
#' @importFrom lubridate ymd as_datetime days  
#' @importFrom ggplot2 ggplot aes geom_rect geom_vline geom_text scale_x_continuous scale_y_continuous coord_cartesian labs theme_minimal theme  
#' @importFrom scales number_format dollar_format  
#' @importFrom gganimate transition_time ease_aes  
#' @export
create_bidstack_day_animation <- function(
    bids_dir, per_dir, interm_dir, units_dir,
    rrp_dir, region, date, price_cap = 500
) {
  library(dplyr); library(tidyr); library(stringr)
  library(lubridate); library(ggplot2); library(gganimate)
  library(arrow); library(scales)
  
  # 0) set up time window & region filter
  day0  <- ymd(date, tz = "Australia/Brisbane")
  start <- as_datetime(day0)
  end   <- start + days(1)
  regs  <- if (tolower(region)=="all") NULL else strsplit(region, ",\\s*")[[1]]
  
  # 1) one-day price bands (average)
  daily_price <- open_dataset(bids_dir) %>%
    filter(timestamp >= start, timestamp < end) %>%
    { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
    select(REGIONID, DUID, starts_with("PRICEBAND")) %>%
    collect() %>%
    group_by(REGIONID, DUID) %>%
    summarise(across(starts_with("PRICEBAND"), ~ mean(.x, na.rm=TRUE)), .groups="drop") %>%
    pivot_longer(starts_with("PRICEBAND"), names_to="Band", values_to="Price") %>%
    mutate(Band = as.integer(str_extract(Band, "\\d+")))
  
  # 2) best‐of intermittent forecasts
  interm_tbl <- open_dataset(interm_dir) %>%
    filter(timestamp >= start, timestamp < end) %>%
    collect() %>%
    group_by(DUID, timestamp) %>%
    filter(
      FORECAST_PRIORITY == max(FORECAST_PRIORITY, na.rm=TRUE),
      OFFERDATETIME    == max(OFFERDATETIME,    na.rm=TRUE)
    ) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    select(DUID, FORECAST_POE50, timestamp)
  
  # 3) all 5-min availabilities, trimmed
  avail_all <- open_dataset(per_dir) %>%
    filter(timestamp >= start, timestamp < end) %>%
    { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
    select(REGIONID, DUID, timestamp, starts_with("BANDAVAIL"), MAXAVAIL) %>%
    collect() %>%
    pivot_longer(starts_with("BANDAVAIL"), names_to="Band", values_to="MW") %>%
    mutate(Band = as.integer(str_extract(Band, "\\d+"))) %>%
    left_join(interm_tbl, by=c("DUID","timestamp")) %>%
    mutate(
      effective_maxavail = ifelse(!is.na(FORECAST_POE50),
                                  pmin(FORECAST_POE50, MAXAVAIL),
                                  MAXAVAIL)
    ) %>%
    group_by(REGIONID, DUID, timestamp) %>%
    mutate(
      totalMW   = sum(MW, na.rm=TRUE),
      over      = totalMW - effective_maxavail,
      maxBandNZ = ifelse(any(MW>0), max(Band[MW>0]), NA_integer_),
      MW        = ifelse(over>0 & Band==maxBandNZ, pmax(MW-over,0), MW)
    ) %>%
    select(REGIONID, DUID, timestamp, Band, MW) %>%
    ungroup()
  
  # 4) fuel lookup
  fuel_df <- open_dataset(units_dir) %>%
    select(DUID, FuelType) %>%
    distinct() %>%
    collect()
  
  # 5) build the “bidstack” at each timestamp
  df_all <- daily_price %>%
    inner_join(avail_all, by=c("REGIONID","DUID","Band")) %>%
    left_join(fuel_df, by="DUID") %>%
    filter(!is.na(Price), !is.na(MW)) %>%
    group_by(timestamp, FuelType, Price, Band) %>%
    summarise(MW = mean(MW, na.rm=TRUE), .groups="drop") %>%
    arrange(timestamp, Price) %>%
    group_by(timestamp) %>%
    mutate(CumVol = cumsum(MW)) %>%
    ungroup()
  
  # 6) total cleared per timestamp
  demand_ts <- open_dataset(units_dir) %>%
    filter(timestamp >= start, timestamp < end) %>%
    { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
    select(timestamp, TOTALCLEARED) %>%
    collect() %>%
    group_by(timestamp) %>%
    summarise(demand = sum(TOTALCLEARED, na.rm=TRUE), .groups="drop")
  
  # 7) join demand back into main data
  df_anim <- df_all %>%
    left_join(demand_ts, by="timestamp")
  
  # 8) build a one‐row‐per‐frame label‐df
  label_df <- df_anim %>%
    group_by(timestamp) %>%
    summarise(
      demand     = first(demand),
      marg_price = Price[ which(CumVol >= demand)[1] ],
      .groups    = "drop"
    ) %>%
    mutate(
      x         = demand,
      label_both = paste0("$", round(marg_price,1), "/MWh / ", round(demand), " MW"),
      price_cap = price_cap
    )
  
  # 9) now plot + animate
  ggplot(df_anim, aes(fill = FuelType)) +
    geom_rect(aes(
      xmin = CumVol - MW,
      xmax = CumVol,
      ymin = 0,
      ymax = Price
    )) +
    # dyed‐in‐the‐data demand line
    geom_vline(aes(xintercept = demand), linetype = "dashed", size = 1) +
    # one‐off text label per frame
    geom_text(
      data = label_df,
      aes(x = x, y = price_cap, label = label_both),
      inherit.aes = FALSE,
      hjust = 1.1, vjust = -0.5, size = 3
    ) +
    scale_x_continuous(
      "Cumulative Offer Volume (MW)",
      labels = number_format(scale = 1e-3, suffix = "K")
    ) +
    scale_y_continuous(
      "Offer Price ($/MWh)",
      labels = dollar_format()
    ) +
    coord_cartesian(ylim = c(0, price_cap)) +
    labs(
      title    = paste0("Bidstack on ", date, " (", region, ")"),
      subtitle = 'Time: {frame_time}',
      fill     = "Fuel Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank()) +
    transition_time(timestamp) +
    ease_aes('linear')
}

