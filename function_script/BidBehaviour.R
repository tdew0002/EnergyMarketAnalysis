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
    labs(title = paste("Supply Curve for", duid_filter, "— {frame_time}"), x = "Cumulative MW", y = "Price ($/MWh)") +
    transition_time(timestamp) +
    ease_aes('linear') +
    theme_minimal()+
    scale_y_log10()
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

prepare_single_variable_tsibble <- function(df, value_pattern, timestamp_col = "timestamp", 
                                            key_col = NULL, filter_val = NULL, 
                                            impute_method = c("zero", "locf")) {
  impute_method <- match.arg(impute_method)
  
  df <- df %>% 
    mutate(timestamp = as.POSIXct(.data[[timestamp_col]]))
  
  # Optional filtering (e.g., by REGIONID or DUID)
  if (!is.null(key_col) && !is.null(filter_val)) {
    df <- df %>% filter(.data[[key_col]] == filter_val)
  }
  
  # Select & reshape
  tsib <- df %>%
    select(timestamp, matches(value_pattern)) %>%
    pivot_longer(cols = -timestamp, names_to = "Band", values_to = "MW") %>%
    mutate(
      Band = as.integer(stringr::str_extract(Band, "\\d+"))
    ) %>%
    as_tsibble(index = timestamp, key = Band) %>%
    fill_gaps(.full = TRUE)
  
  # Impute
  if (impute_method == "zero") {
    tsib <- tsib %>% mutate(MW = ifelse(is.na(MW), 0, MW))
  } else {
    tsib <- tsib %>% group_by(Band) %>% tidyr::fill(MW, .direction = "downup") %>% ungroup()
  }
  
  return(tsib)
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
      title = paste("Seasonal Decomposition of Offered MW by Band",duid_filter),
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
    bidday_df    <- bidday_df    %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
    bidper_df    <- bidper_df    %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
    rrp_df       <- rrp_df       %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
    op_demand_df <- op_demand_df %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
    rooftop_df   <- rooftop_df   %>% filter(format(as.Date(timestamp), "%Y-%m") == target_month)
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
      Band      = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")
    )
  
  # Clean BIDPEROFFER
  avail_cols <- grep("BANDAVAIL", names(bidper_df), value = TRUE)
  bid_avail <- bidper_df %>%
    filter(DUID == duid_filter) %>%
    pivot_longer(cols = all_of(avail_cols), names_to = "Band", values_to = "MW") %>%
    mutate(
      Band      = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")
    )
  
  merged_bid <- inner_join(bid_price, bid_avail, by = c("timestamp", "Band"))
  
  # RRP data (filtered by region)
  rrp_filtered <- rrp_df %>%
    filter(REGIONID == region) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")) %>%
    select(timestamp, RRP) %>%
    distinct()
  
  # Raw operational demand (new)
  op_df <- op_demand_df %>%
    filter(REGIONID == region) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")) %>%
    select(timestamp, OPERATIONAL_DEMAND) %>%
    distinct()
  
  # Residual demand = OPERATIONAL - ROOFTOP
  rooftop_vals <- rooftop_df %>%
    filter(REGIONID == region) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), "5 minutes")) %>%
    select(timestamp, POWERMEAN) %>%
    distinct()
  
  residual_df <- full_join(op_df, rooftop_vals, by = "timestamp") %>%
    mutate(residual_demand = OPERATIONAL_DEMAND - POWERMEAN) %>%
    # keep OPERATIONAL_DEMAND so we can plot it
    select(timestamp, OPERATIONAL_DEMAND, residual_demand) %>%
    distinct()
  
  # Final merge
  merged_all <- merged_bid %>%
    left_join(rrp_filtered, by = "timestamp") %>%
    left_join(residual_df,   by = "timestamp") %>%
    filter(!is.na(MW), !is.na(Price), !is.na(RRP), !is.na(residual_demand)) %>%
    distinct(timestamp, Band, .keep_all = TRUE)
  
  
  # Plot: added the operational demand line
  p <- ggplot(merged_all, aes(x = timestamp)) +
    geom_line(aes(y = MW,                  color = "MW"),               alpha       = 0.6) +
    geom_line(aes(y = Price,               color = "Price"),            alpha       = 0.6) +
    geom_line(aes(y = RRP,                 color = "RRP"),              linewidth   = 1) +
    geom_line(aes(y = OPERATIONAL_DEMAND,  color = "Operational Demand"), linetype  = "dotdash", linewidth = 1) +
    geom_line(aes(y = residual_demand,     color = "Residual Demand"),  linetype     = "dashed",   linewidth = 1) +
    facet_wrap(~ Band, scales = "free_y") +
    labs(
      title = paste("Bid & Demand Dynamics for", duid_filter,
                    "| Region:", region, "— {frame_along}"),
      y = "Value", x = "Time", color = "Legend"
    ) +
    theme_minimal() +
    transition_reveal(along = timestamp) +
    ease_aes("linear")
    #scale_y_log10()
    #scale_y_continuous(trans = "log1p")
  
  return(p)
}

# plot count of bandavail and band price
plot_duid_priceband_distribution_animated <- function(
    bidday_df, bidper_df, price_df,
    region_filter = "VIC1",
    price_cap = 10000,
    month_filter = NULL
) {
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(stringr)
  library(gganimate)
  library(lubridate)
  
  # Extract PRICEBAND columns from BIDDAYOFFER_D
  price_cols <- grep("PRICEBAND", names(bidday_df), value = TRUE)
  avail_cols <- grep("BANDAVAIL", names(bidper_df), value = TRUE)
  
  # Pivot bidday_df (prices)
  price_long <- bidday_df %>%
    filter(REGIONID == region_filter) %>%
    select(timestamp, DUID, all_of(price_cols)) %>%
    pivot_longer(cols = starts_with("PRICEBAND"),
                 names_to = "Band", values_to = "Price") %>%
    mutate(
      Band = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), "1 day")
    )
  
  # Pivot bidper_df (availabilities)
  avail_long <- bidper_df %>%
    filter(REGIONID == region_filter) %>%
    select(timestamp, DUID, all_of(avail_cols)) %>%
    pivot_longer(cols = starts_with("BANDAVAIL"),
                 names_to = "Band", values_to = "MW") %>%
    mutate(
      Band = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), "1 day")
    )
  
  # Join on timestamp, DUID, Band
  bid_long <- inner_join(price_long, avail_long, by = c("timestamp", "DUID", "Band"))
  
  if (!is.null(month_filter)) {
    bid_long <- bid_long %>%
      filter(format(timestamp, "%Y-%m") == month_filter)
  }
  
  bid_long <- bid_long %>%
    filter(Price <= price_cap)
  
  # Aggregate: Sum MW per day per Band + Price
  mw_df <- bid_long %>%
    group_by(timestamp, Band, Price) %>%
    summarise(TotalMW = sum(MW, na.rm = TRUE), .groups = "drop")
  
  # Daily RRP
  rrp_daily <- price_df %>%
    filter(REGIONID == region_filter) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), "1 day")) %>%
    group_by(timestamp) %>%
    summarise(RRP = mean(RRP, na.rm = TRUE), .groups = "drop")
  
  # Merge
  plot_df <- left_join(mw_df, rrp_daily, by = "timestamp")
  
  # Animated Plot
  p <- ggplot(plot_df, aes(x = Price, y = TotalMW, group = interaction(Band, timestamp, Price))) +
    geom_col(fill = "firebrick", alpha = 0.8, width = 25) +
    geom_vline(aes(xintercept = RRP), color = "blue", linetype = "dashed", linewidth = 1) +
    facet_wrap(~ Band, scales = "free_y") +
    labs(
      title = paste("DUID Band Availability —", region_filter, "| {frame_time}"),
      x = "Offered Price ($/MWh)", y = "Total MW (BANDAVAIL)"
    ) +
    theme_minimal() +
    transition_time(timestamp) +
    ease_aes("linear")
  
  p
}





#plot duid count vs bandavail
plot_bandavail_animation <- function(
    bidper_df, price_df, op_demand_df,
    region_filter = "VIC1", month_filter = NULL,
    bin_width = 50, fps = 5
) {
  library(dplyr)
  library(ggplot2)
  library(gganimate)
  library(stringr)
  library(tidyr)
  library(lubridate)
  
  # Preprocessing
  avail_cols <- grep("BANDAVAIL", names(bidper_df), value = TRUE)
  
  bid_long <- bidper_df %>%
    filter(REGIONID == region_filter) %>%
    select(timestamp, DUID, all_of(avail_cols)) %>%
    pivot_longer(cols = starts_with("BANDAVAIL"), names_to = "Band", values_to = "MW") %>%
    mutate(
      Band = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(as.POSIXct(timestamp), unit = "5 minutes")
    ) %>%
    filter(!is.na(MW))
  
  # Optional month filter
  if (!is.null(month_filter)) {
    bid_long <- bid_long %>%
      filter(format(timestamp, "%Y-%m") == month_filter)
  }
  
  # Histogram binning
  bid_long <- bid_long %>%
    mutate(MW_bin = cut(MW, breaks = seq(0, max(MW, na.rm = TRUE) + bin_width, by = bin_width), include.lowest = TRUE)) %>%
    group_by(timestamp, Band, MW_bin) %>%
    summarise(DUID_Count = n_distinct(DUID), .groups = "drop")
  
  # RRP line
  rrp_filtered <- price_df %>%
    filter(REGIONID == region_filter) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), unit = "5 minutes")) %>%
    group_by(timestamp) %>%
    summarise(RRP = mean(RRP, na.rm = TRUE), .groups = "drop")
  
  # Demand line
  demand_filtered <- op_demand_df %>%
    filter(REGIONID == region_filter) %>%
    mutate(timestamp = floor_date(as.POSIXct(timestamp), unit = "5 minutes")) %>%
    group_by(timestamp) %>%
    summarise(Demand = mean(OPERATIONAL_DEMAND, na.rm = TRUE), .groups = "drop")
  
  # Merge all
  merged <- bid_long %>%
    left_join(rrp_filtered, by = "timestamp") %>%
    left_join(demand_filtered, by = "timestamp") %>%
    filter(!is.na(MW_bin))
  
  # Plot
  p <- ggplot(merged, aes(x = MW_bin, y = DUID_Count)) +
    geom_col(fill = "#1d3557", width = 0.8) +
    geom_hline(aes(yintercept = RRP), color = "purple", linetype = "dashed", linewidth = 0.4) +
    geom_hline(aes(yintercept = Demand), color = "darkgreen", linetype = "dotted", linewidth = 0.4) +
    facet_wrap(~ Band, scales = "free_y") +
    labs(
      title = paste("Offered Availability Distribution —", region_filter, "| Time: {frame_time}"),
      subtitle = "Dashed = RRP, Dotted = Operational Demand",
      x = "Availability Bin (MW)",
      y = "DUID Count"
    ) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    transition_time(timestamp) +
    ease_aes("linear")
  
  animate(p, fps = fps, duration = 20, width = 1000, height = 600)
}
