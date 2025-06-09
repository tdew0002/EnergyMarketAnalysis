# R/seasonality.R

#' @title Daily Bid Metrics (Seasonality)
#' @description
#'   Compute and plot daily time‐of‐day seasonality for offer price, band availability,
#'   RRP, residual demand, renewable generation, and operational demand for a single region.
#' @param bids_dir Path to your BIDDAYOFFER_D Parquet folder
#' @param avail_dir Path to your BIDPEROFFER_D Parquet folder
#' @param rrp_dir   Path to your DREGION Parquet folder
#' @param operational_dir Path to your OPERATIONAL_DEMAND Parquet folder
#' @param unit_dir  Path to your UNIT_SOLUTION Parquet folder
#' @param region    A region code (e.g. `"VIC1"`)
#' @param start_date Start date as `"YYYY-MM-DD"`
#' @param end_date   End date as `"YYYY-MM-DD"`
#' @return A list with elements
#'   - `price_plot`  
#'   - `availability_plot`  
#'   - `rrp_plot`  
#'   - `residual_plot`  
#'   - `renewable_plot`  
#'   - `operational_plot`  
#'   each one a **ggplot** object
#' @importFrom arrow open_dataset
#' @importFrom dplyr filter select collect group_by summarise mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @importFrom lubridate ymd days seconds floor_date as_date
#' @importFrom hms as_hms
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_tile stat_summary labs theme_minimal theme scale_x_date scale_x_time scale_y_continuous coord_cartesian
#' @importFrom scales comma dollar_format label_time
#' @export
plot_daily_bid_metrics <- function(
    bids_dir,      # BIDDAYOFFER_D
    avail_dir,     # BIDPEROFFER_D
    rrp_dir,       # DREGION for RRP
    operational_dir,  # OPERATIONAL_DEMAND
    unit_dir,      # UNIT_SOLUTION
    region,        # e.g. "VIC1"
    start_date,    # "YYYY-MM-DD"
    end_date       # "YYYY-MM-DD"
) {
  # 1) build window
  dt0 <- ymd(start_date, tz="Australia/Brisbane")
  dt1 <- ymd(end_date,   tz="Australia/Brisbane") + days(1) - seconds(1)
  floor30 <- function(x) floor_date(x, "30 minutes")
  
  # 2a) price per day
  price_df <- open_dataset(bids_dir) %>%
    filter(REGIONID == region, timestamp >= dt0, timestamp <= dt1) %>%
    select(timestamp, starts_with("PRICEBAND")) %>%
    collect() %>%
    pivot_longer(starts_with("PRICEBAND"),
                 names_to="Band", values_to="Price") %>%
    mutate(ts = floor30(timestamp)) %>%
    group_by(date = as_date(ts)) %>%
    summarise(Price = mean(Price, na.rm=TRUE), .groups="drop")
  
  # 2b) availability spaghetti
  avail_df <- open_dataset(avail_dir) %>%
    filter(REGIONID == region, timestamp >= dt0, timestamp <= dt1) %>%
    select(timestamp, starts_with("BANDAVAIL")) %>%
    collect() %>%
    pivot_longer(starts_with("BANDAVAIL"),
                 names_to="Band", values_to="Avail_MW") %>%
    mutate(ts = timestamp) %>%
    group_by(date = as_date(ts), ts) %>%
    summarise(Avail_MW = sum(Avail_MW, na.rm=TRUE), .groups="drop")
  
  # 2c) RRP spaghetti
  rrp_df <- open_dataset(rrp_dir) %>%
    filter(REGIONID == region, timestamp >= dt0, timestamp <= dt1) %>%
    select(timestamp, RRP) %>%
    collect() %>%
    mutate(
      ts   = timestamp,
      date = as_date(ts)
    )
  # 2d) renewable generation
  ren_df <- open_dataset(unit_dir) %>%
    filter(REGIONID==region,
           timestamp>=dt0, timestamp<=dt1,
           FuelType %in% c("Solar - Solar","Wind - Wind")) %>%
    select(timestamp,TOTALCLEARED) %>%
    collect() %>%
    mutate(ts=floor30(timestamp), date=as_date(ts)) %>%
    group_by(date,ts) %>%
    summarise(Renewable_MW=sum(TOTALCLEARED,na.rm=TRUE), .groups="drop")
  
  # 2e) **operational demand** from OPERATIONAL_DEMAND
  op_df <- open_dataset(operational_dir) %>%
    filter(REGIONID==region,
           timestamp>=dt0, timestamp<=dt1) %>%
    select(timestamp, OPERATIONAL_DEMAND) %>%
    collect() %>%
    mutate(ts=floor30(timestamp), date=as_date(ts)) %>%
    group_by(date,ts) %>%
    summarise(Demand=mean(OPERATIONAL_DEMAND, na.rm=TRUE), .groups="drop")
  
  # 2f) residual = demand – renewables
  resid_df <- inner_join(op_df, ren_df, by=c("date","ts")) %>%
    mutate(
      Residual = Demand - Renewable_MW,
      text     = paste0(
        "Date: ", date,
        "<br>Time: ", format(ts, "%H:%M"),
        "<br>Demand: ", comma(Demand), " MW",
        "<br>Renewable: ", comma(Renewable_MW), " MW",
        "<br>Residual: ", comma(Residual), " MW"
      )
    )
  
  # 3a) price plot: daily line
  p_price <- ggplot(price_df, aes(x = date, y = Price)) +
    geom_line(color="steelblue", linewidth=1) +
    geom_point(color="steelblue", size=2) +
    scale_x_date("Date", date_breaks="1 day", date_labels="%b %d") +
    scale_y_continuous("Average Offer Price ($/MWh)", labels=dollar_format()) +
    labs(title = sprintf("Daily Offer Price — %s (%s to %s)",
                         region, start_date, end_date)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  # 3b) availability plot: spaghetti + avg
  p_avail <- ggplot(avail_df, aes(
    x     = as_hms(ts - floor_date(ts, "day")),
    y     = Avail_MW,
    group = date,
    text  = paste0(
      "Date: ", date,
      "<br>Time: ", format(ts, "%H:%M"),
      "<br>Avail: ", comma(Avail_MW), " MW"
    )
  )) +
    geom_line(color="grey80", linewidth=0.4) +
    stat_summary(
      aes(group=1),
      fun        = mean,
      geom       = "line",
      color      = "firebrick",
      linewidth  = 1.1
    ) +
    scale_x_time(
      "Time of day",
      breaks = scales::breaks_width("4 hours"),
      labels = scales::label_time("%H:%M")
    ) +
    scale_y_continuous("Availability (MW)", labels=comma) +
    theme_minimal() +
    labs(
      title    = sprintf("Daily Availability — %s (%s to %s)",
                         region, start_date, end_date),
      subtitle = "Grey = each day; Red = average"
    )
  
  # 3c) RRP plot: spaghetti + avg
  p_rrp <- ggplot(rrp_df, aes(
    x     = as_hms(ts - floor_date(ts, "day")),
    y     = RRP,
    group = date,
    text  = paste0(
      "Date: ", date,
      "<br>Time: ", format(ts, "%H:%M"),
      "<br>RRP: $", comma(RRP)
    )
  )) +
    geom_line(color="grey80", linewidth=0.4) +
    stat_summary(
      aes(group=1),
      fun        = mean,
      geom       = "line",
      color      = "forestgreen",
      linewidth  = 1.1
    ) +
    scale_x_time(
      "Time of day",
      breaks = scales::breaks_width("4 hours"),
      labels = scales::label_time("%H:%M")
    ) +
    scale_y_continuous("RRP ($/MWh)", labels=dollar_format(prefix="$")) +
    theme_minimal() +
    labs(
      title    = sprintf("Daily RRP — %s (%s to %s)",
                         region, start_date, end_date),
      subtitle = "Grey = each day; Green = average"
    )
  
  # 3d) plot residual demand
  p_residual <- ggplot(resid_df, aes(
    x     = as_hms(ts - floor_date(ts,"day")),
    y     = Residual,
    group = date,
    text  = text
  )) +
    geom_line(color="grey80", linewidth=0.4) +
    stat_summary(aes(group=1), fun=mean, geom="line",
                 color="darkorange", linewidth=1.1) +
    scale_x_time("Time of day",
                 breaks = scales::breaks_width("4 hours"),
                 labels = scales::label_time("%H:%M")) +
    scale_y_continuous("Residual Demand (MW)", labels=comma) +
    labs(
      title    = sprintf("Daily Residual Demand — %s (%s to %s)",
                         region, start_date, end_date),
      subtitle = "Grey = each day; Orange = average"
    ) +
    theme_minimal()
  
  # 3e) Renewable generation: spaghetti + average (gold line)
  p_renewable <- ggplot(ren_df, aes(
    x     = as_hms(ts - floor_date(ts, "day")),
    y     = Renewable_MW,
    group = date,
    text  = paste0(
      "Date: ", date,
      "<br>Time: ", format(ts, "%H:%M"),
      "<br>Renewable: ", comma(Renewable_MW), " MW"
    )
  )) +
    geom_line(color="grey80", linewidth=0.4) +
    stat_summary(
      aes(group = 1),
      fun       = mean,
      geom      = "line",
      color     = "gold",
      linewidth = 1.1
    ) +
    scale_x_time(
      "Time of day",
      breaks = scales::breaks_width("4 hours"),
      labels = scales::label_time("%H:%M")
    ) +
    scale_y_continuous("Renewable Generation (MW)", labels = comma) +
    labs(
      title    = sprintf("Daily Renewable Generation — %s (%s to %s)",
                         region, start_date, end_date),
      subtitle = "Grey = each day; Gold = average"
    ) +
    theme_minimal()
  
  # 3f) Operational demand: spaghetti + average (purple line)
  p_operational <- ggplot(op_df, aes(
    x     = as_hms(ts - floor_date(ts, "day")),
    y     = Demand,
    group = date,
    text  = paste0(
      "Date: ", date,
      "<br>Time: ", format(ts, "%H:%M"),
      "<br>Demand: ", comma(Demand), " MW"
    )
  )) +
    geom_line(color="grey80", linewidth=0.4) +
    stat_summary(
      aes(group = 1),
      fun       = mean,
      geom      = "line",
      color     = "purple",
      linewidth = 1.1
    ) +
    scale_x_time(
      "Time of day",
      breaks = scales::breaks_width("4 hours"),
      labels = scales::label_time("%H:%M")
    ) +
    scale_y_continuous("Operational Demand (MW)", labels = comma) +
    labs(
      title    = sprintf("Daily Operational Demand — %s (%s to %s)",
                         region, start_date, end_date),
      subtitle = "Grey = each day; Purple = average"
    ) +
    theme_minimal()
  
  
  # 4) return
  list(
    price_plot        = p_price,
    availability_plot = p_avail,
    rrp_plot          = p_rrp,
    residual_plot     = p_residual,
    renewable_plot    = p_renewable,
    operational_plot  = p_operational
  )
}
