#Animate supply curve
library(gganimate)

plot_supply_curve_animation <- function(bidday_df, bidper_df, duid_filter) {
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidyr)
  
  # Preprocessing
  bidday <- bidday_df %>% filter(DUID == duid_filter)
  bidper <- bidper_df %>% filter(DUID == duid_filter)
  
  price_cols <- grep("PRICEBAND", names(bidday), value = TRUE)
  avail_cols <- grep("BANDAVAIL", names(bidper), value = TRUE)
  
  daily_price <- bidday %>%
    select(timestamp, all_of(price_cols)) %>%
    pivot_longer(-timestamp, names_to = "Band", values_to = "Price") %>%
    mutate(Band = as.integer(str_extract(Band, "\\d+")))
  
  avail_data <- bidper %>%
    select(timestamp, all_of(avail_cols)) %>%
    pivot_longer(-timestamp, names_to = "Band", values_to = "MW") %>%
    mutate(Band = as.integer(str_extract(Band, "\\d+")))
  
  joined <- inner_join(daily_price, avail_data, by = c("timestamp", "Band")) %>%
    group_by(timestamp) %>%
    arrange(Price) %>%
    mutate(CumMW = cumsum(MW)) %>%
    ungroup()
  
  ggplot(joined, aes(x = CumMW, y = Price, group = timestamp)) +
    geom_step(color = "#0077b6") +
    labs(title = "Supply Curve — {frame_time}", x = "Cumulative MW", y = "Price ($/MWh)") +
    transition_time(timestamp) +
    ease_aes('linear') +
    theme_minimal()
}

#Band Availability Heatmap (Time vs Band)
plot_band_avail_heatmap <- function(bidper_df, duid_filter) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  bid_filtered <- bidper_df %>%
    filter(DUID == duid_filter)
  
  avail_cols <- grep("BANDAVAIL", names(bid_filtered), value = TRUE)
  
  bid_long <- bid_filtered %>%
    select(timestamp, all_of(avail_cols)) %>%
    pivot_longer(-timestamp, names_to = "Band", values_to = "MW") %>%
    mutate(Band = as.integer(str_extract(Band, "\\d+")))
  
  ggplot(bid_long, aes(x = timestamp, y = factor(Band), fill = MW)) +
    geom_tile() +
    labs(title = paste("Availability Heatmap for", duid_filter), x = "Time", y = "Band") +
    scale_fill_viridis_c() +
    theme_minimal()
}

#Overlay Plot: RRP vs Band Price vs Band Avail
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(gganimate)
library(lubridate)

plot_bid_vs_rrp_overlay_animated <- function(bidday_df, bidper_df, price_df, duid_filter, target_month = NULL) {
  
  # Automatically detect REGIONID from BIDPEROFFER table
  duid_region <- bidper_df %>%
    filter(DUID == duid_filter) %>%
    pull(REGIONID) %>%
    unique() %>%
    na.omit()
  
  if (length(duid_region) == 0) stop("❌ Could not detect REGIONID for the selected DUID.")
  
  region_filter <- duid_region[1]
  
  # Parse and round timestamps
  bid_price <- bidday_df %>%
    filter(DUID == duid_filter) %>%
    select(timestamp, starts_with("PRICEBAND")) %>%
    pivot_longer(-timestamp, names_to = "Band", values_to = "Price") %>%
    mutate(
      Band = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")
    )
  
  bid_avail <- bidper_df %>%
    filter(DUID == duid_filter) %>%
    select(timestamp, starts_with("BANDAVAIL")) %>%
    pivot_longer(-timestamp, names_to = "Band", values_to = "MW") %>%
    mutate(
      Band = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")
    )
  
  # Merge availability and price
  merged_bid <- inner_join(bid_price, bid_avail, by = c("timestamp", "Band"))
  
  # Filter RRP from correct region
  rrp_filtered <- price_df %>%
    filter(REGIONID == region_filter) %>%
    select(timestamp, RRP) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), "5 minutes"))
  
  merged_all <- left_join(merged_bid, rrp_filtered, by = "timestamp") %>%
    filter(!is.na(MW), !is.na(Price), !is.na(RRP))
  
  # ⏳ Optional: Filter by month if specified
  if (!is.null(target_month)) {
    merged_all <- merged_all %>%
      filter(format(timestamp, "%Y-%m") == target_month)
  }
  
  # 🔄 Animated plot
  ggplot(merged_all, aes(x = timestamp)) +
    geom_line(aes(y = MW, color = "MW"), alpha = 0.6) +
    geom_line(aes(y = Price, color = "Price"), alpha = 0.6) +
    geom_line(aes(y = RRP, color = "RRP"), linewidth = 1) +
    facet_wrap(~ Band, scales = "free_y") +
    labs(
      title = paste("Animated Bid vs RRP —", duid_filter, "| Region:", region_filter, "| {frame_along}"),
      x = "Time", y = "Value", color = "Legend"
    ) +
    theme_minimal() +
    transition_reveal(timestamp) +
    ease_aes("linear")
}

#Seasonal Decomposition
library(dplyr)
library(tidyr)
library(tsibble)
library(feasts)
library(fabletools)
library(ggplot2)

# --- 1. Convert to tsibble + fill gaps + replace NA with 0 or interpolate
prepare_bid_tsibble <- function(bidper_df, duid_filter, impute_method = c("zero", "locf")) {
  impute_method <- match.arg(impute_method)
  
  bid_tsibble <- bidper_df %>%
    filter(DUID == duid_filter) %>%
    pivot_longer(cols = starts_with("BANDAVAIL"), names_to = "Band", values_to = "MW") %>%
    mutate(
      Band = as.integer(stringr::str_extract(Band, "\\d+")),
      timestamp = as.POSIXct(timestamp)
    ) %>%
    select(timestamp, Band, MW) %>%
    as_tsibble(index = timestamp, key = Band) %>%
    fill_gaps(.full = TRUE)
  
  # Impute missing values
  if (impute_method == "zero") {
    bid_tsibble <- bid_tsibble %>% mutate(MW = ifelse(is.na(MW), 0, MW))
  } else if (impute_method == "locf") {
    bid_tsibble <- bid_tsibble %>%
      group_by(Band) %>%
      tidyr::fill(MW, .direction = "downup") %>%
      ungroup()
  }
  
  return(bid_tsibble)
}

# --- 2. Plot seasonal decomposition per band
plot_seasonal_decomp_facet <- function(bid_tsibble) {
  bid_tsibble %>%
    model(stl = STL(MW ~ season(window = "periodic"))) %>%
    components() %>%
    ggplot(aes(x = timestamp)) +
    geom_line(aes(y = MW), color = "#ff6b6b") +
    geom_line(aes(y = trend), color = "#1d3557", linewidth = 1) +
    facet_wrap(~ Band, scales = "free_y", ncol = 2) +
    labs(
      title = "Seasonal Decomposition of Offered MW by Band",
      subtitle = "Red = raw values | Blue = smoothed trend",
      y = "MW", x = "Time"
    ) +
    theme_minimal()
}


### RPP BID RSIDUAL DEMAND
plot_bid_vs_rrp_overlay_with_residual_animated <- function(
    bidday_df, bidper_df, rrp_df, op_demand_df, rooftop_df,
    duid_filter = "LOYYB1",
    target_month = NULL,
    log_y = FALSE
) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gganimate)
  library(stringr)
  library(lubridate)
  
  # Filter month if needed
  if (!is.null(target_month)) {
    bidday_df   <- bidday_df   %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
    bidper_df   <- bidper_df   %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
    rrp_df      <- rrp_df      %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
    op_demand_df <- op_demand_df %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
    rooftop_df  <- rooftop_df  %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
  }
  
  # Identify region from BIDPEROFFER
  region <- bidper_df %>%
    filter(DUID == duid_filter) %>%
    distinct(REGIONID) %>%
    pull(REGIONID) %>%
    first()
  
  # Clean BIDDAYOFFER
  price_cols <- grep("PRICEBAND", names(bidday_df), value = TRUE)
  bid_price <- bidday_df %>%
    filter(DUID == duid_filter) %>%
    pivot_longer(cols = all_of(price_cols), names_to = "Band", values_to = "Price") %>%
    mutate(
      Band = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")
    )
  
  # Clean BIDPEROFFER
  avail_cols <- grep("BANDAVAIL", names(bidper_df), value = TRUE)
  bid_avail <- bidper_df %>%
    filter(DUID == duid_filter) %>%
    pivot_longer(cols = all_of(avail_cols), names_to = "Band", values_to = "MW") %>%
    mutate(
      Band = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")
    )
  
  merged_bid <- inner_join(bid_price, bid_avail, by = c("timestamp", "Band"))
  
  # RRP data (filtered by region)
  rrp_filtered <- rrp_df %>%
    filter(REGIONID == region) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")) %>%
    select(timestamp, RRP) %>%
    distinct()
  
  # Residual demand = OPERATIONAL - ROOFTOP (POWERMEAN)
  op_df <- op_demand_df %>%
    filter(REGIONID == region) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")) %>%
    select(timestamp, OPERATIONAL_DEMAND)
  
  rooftop_df <- rooftop_df %>%
    filter(REGIONID == region) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")) %>%
    select(timestamp, POWERMEAN)
  
  residual_df <- full_join(op_df, rooftop_df, by = "timestamp") %>%
    mutate(residual_demand = OPERATIONAL_DEMAND - POWERMEAN) %>%
    filter(!is.na(residual_demand)) %>%
    select(timestamp, residual_demand) %>%
    distinct()
  
  # Final merge
  merged_all <- merged_bid %>%
    left_join(rrp_filtered, by = "timestamp") %>%
    left_join(residual_df, by = "timestamp") %>%
    filter(!is.na(MW), !is.na(Price), !is.na(RRP), !is.na(residual_demand)) %>%
    distinct(timestamp, Band, .keep_all = TRUE)
  
  # Plot
  p <- ggplot(merged_all, aes(x = timestamp)) +
    geom_line(aes(y = MW, color = "MW"), alpha = 0.6) +
    geom_line(aes(y = Price, color = "Price"), alpha = 0.6) +
    geom_line(aes(y = RRP, color = "RRP"), size = 1) +
    geom_line(aes(y = residual_demand, color = "Residual Demand"), linetype = "dashed", size = 1) +
    facet_wrap(~ Band, scales = "free_y") +
    labs(
      title = paste("Bid Dynamics for", duid_filter, "| Region:", region, "— {frame_along}"),
      y = "Value", x = "Time", color = "Legend"
    ) +
    theme_minimal() +
    transition_reveal(along = timestamp) +
    ease_aes("linear")
  
  if (log_y) {
    p <- p + scale_y_continuous(trans = "log1p")
  }
  
  return(p)
}



