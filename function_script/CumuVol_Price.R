library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(scales)

create_bidstack_animation <- function(
    bids_dir,    # e.g. "filtered_parquet_tables/BIDDAYOFFER_D"
    per_dir,     # e.g. "filtered_parquet_tables/BIDPEROFFER_D"
    units_dir,   # e.g. "filtered_parquet_tables/UNIT_SOLUTION"
    prices_dir,  # e.g. "filtered_parquet_tables/PUBLIC_PRICES"
    region       = "VIC1",
    month_filter = NULL,      # e.g. "2024-06"
    price_cap    = 500,       # zoom up to $500/MWh
    fps          = 10,
    width        = 800,
    height       = 500
) {
  # 1) date window
  if (is.null(month_filter))
    stop("Please supply month_filter, e.g. '2024-06'")
  start_day <- ymd(paste0(month_filter, "-01"), tz="Australia/Brisbane")
  dt0 <- as_datetime(start_day)
  dt1 <- dt0 + months(1) - seconds(1)
  
  regs <- if (tolower(region)=="all") NULL else strsplit(region, ",\\s*")[[1]]
  
  # 2) pull & pivot PRICEBANDs (no more filter on price!)
  price_df <- open_dataset(bids_dir) %>%
    filter(timestamp >= dt0, timestamp <= dt1) %>%
    { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
    select(REGIONID, DUID, timestamp, starts_with("PRICEBAND")) %>%
    collect() %>%
    pivot_longer(
      cols     = starts_with("PRICEBAND"),
      names_to = "Band",
      values_to= "Price"
    ) %>%
    mutate(
      Band      = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(timestamp, "30 minutes")
    ) %>%
    group_by(REGIONID, DUID, timestamp, Band) %>%
    summarise(Price = mean(Price, na.rm=TRUE), .groups="drop")
  
  # 3) pull & pivot BANDAVAILs
  avail_df <- open_dataset(per_dir) %>%
    filter(timestamp >= dt0, timestamp <= dt1) %>%
    { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
    select(REGIONID, DUID, timestamp, starts_with("BANDAVAIL")) %>%
    collect() %>%
    pivot_longer(
      cols      = starts_with("BANDAVAIL"),
      names_to  = "Band",
      values_to = "MW"
    ) %>%
    mutate(
      Band      = as.integer(str_extract(Band, "\\d+")),
      timestamp = floor_date(timestamp, "30 minutes")
    ) %>%
    group_by(REGIONID, DUID, timestamp, Band) %>%
    summarise(MW = sum(MW, na.rm=TRUE), .groups="drop")
  
  # 4) fuel lookup
  units_df <- open_dataset(units_dir) %>%
    select(DUID, FuelType) %>%
    distinct() %>%
    collect()
  
  # 5) demand line
  demand_df <- open_dataset(prices_dir) %>%
    filter(timestamp >= dt0, timestamp <= dt1) %>%
    { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
    select(REGIONID, timestamp, TOTALDEMAND) %>%
    collect() %>%
    mutate(
      timestamp = floor_date(timestamp, "30 minutes")
    ) %>%
    group_by(REGIONID, timestamp) %>%
    summarise(demand = mean(TOTALDEMAND, na.rm=TRUE), .groups="drop")
  
  # 6) join everything
  df <- price_df %>%
    inner_join(avail_df,   by = c("REGIONID","DUID","timestamp","Band")) %>%
    left_join(units_df,   by = "DUID") %>%
    left_join(demand_df,  by = c("REGIONID","timestamp")) %>%
    filter(!is.na(Price), !is.na(MW))
  
  if (nrow(df)==0) stop("No data for ", month_filter, " / ", region)
  
  # … steps 1–6 unchanged …
  
  # 7) build cumulative per FuelType
  df <- df %>%
    arrange(timestamp, FuelType, Price) %>%
    group_by(timestamp, FuelType) %>%
    mutate(CumVol = cumsum(MW)) %>%
    ungroup()
  
  # 8) plot + animate (swapped axes, zoom price)
  p <- ggplot(df, aes(x = CumVol, y = Price, fill = FuelType)) +
    geom_col(width = 1) +
    geom_vline(aes(xintercept = demand),
               linetype = "dashed", color = "black", size = 1) +
    scale_x_continuous("Cumulative Volume (MW)", labels = comma) +
    scale_y_continuous("Offer Price ($/MWh)", labels = dollar_format()) +
    coord_cartesian(ylim = c(0, price_cap)) +
    labs(
      title    = "NEM Bidstack — {frame_time}",
      subtitle = paste("Region:", region),
      fill     = "Fuel Type"
    ) +
    theme_minimal(base_size = 14) +
    transition_time(timestamp) +
    ease_aes("linear")
  
  animate(
    p,
    fps     = fps,
    nframes = length(unique(df$timestamp)),
    width   = width,
    height  = height,
    renderer = gifski_renderer(loop = TRUE)
  )
}

# function_script/bidstack_comparison.R
library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)    # for combining two ggplots

create_bidstack_comparison <- function(
    bids_dir,     # e.g. "filtered_parquet_tables/BIDDAYOFFER_D"
    per_dir,      # e.g. "filtered_parquet_tables/BIDPEROFFER_D"
    units_dir,    # e.g. "filtered_parquet_tables/UNIT_SOLUTION"
    prices_dir,   # e.g. "filtered_parquet_tables/PUBLIC_PRICES"
    region = "VIC1",
    month1,       # e.g. "2024-02"
    month2,       # e.g. "2024-03"
    price_cap = 500  # zoom in on price axis
) {
  make_one <- function(month) {
    # 1) date window for that month
    first_day <- ymd(paste0(month, "-01"), tz="Australia/Brisbane")
    dt0 <- as_datetime(first_day)
    dt1 <- dt0 + months(1) - seconds(1)
    
    regs <- if (tolower(region)=="all") NULL else strsplit(region, ",\\s*")[[1]]
    
    # 2) pull + pivot PRICEBANDs
    price_df <- open_dataset(bids_dir) %>%
      filter(timestamp >= dt0, timestamp <= dt1) %>%
      { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
      select(REGIONID, DUID, timestamp, starts_with("PRICEBAND")) %>%
      collect() %>%
      pivot_longer(
        starts_with("PRICEBAND"),
        names_to="Band", values_to="Price"
      ) %>%
      mutate(
        Band      = as.integer(str_extract(Band, "\\d+")),
        timestamp = floor_date(timestamp, "30 minutes")
      )
    
    # 3) pull + pivot BANDAVAILs
    avail_df <- open_dataset(per_dir) %>%
      filter(timestamp >= dt0, timestamp <= dt1) %>%
      { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
      select(REGIONID, DUID, timestamp, starts_with("BANDAVAIL")) %>%
      collect() %>%
      pivot_longer(
        starts_with("BANDAVAIL"),
        names_to="Band", values_to="MW"
      ) %>%
      mutate(
        Band      = as.integer(str_extract(Band, "\\d+")),
        timestamp = floor_date(timestamp, "30 minutes")
      )
    
    # 4) fuel lookup
    units_df <- open_dataset(units_dir) %>%
      select(DUID, FuelType) %>% distinct() %>% collect()
    
    # 5) monthly average demand
    demand_df <- open_dataset(prices_dir) %>%
      filter(timestamp >= dt0, timestamp <= dt1) %>%
      { if (!is.null(regs)) filter(., REGIONID %in% regs) else . } %>%
      select(REGIONID, timestamp, TOTALDEMAND) %>%
      collect() %>%
      mutate(timestamp = floor_date(timestamp, "30 minutes")) %>%
      group_by(REGIONID) %>%
      summarise(avg_demand = mean(TOTALDEMAND, na.rm=TRUE), .groups="drop")
    
    # 6) join, sum over entire month
    df <- price_df %>%
      inner_join(avail_df, by = c("REGIONID","DUID","timestamp","Band")) %>%
      left_join(units_df,   by = "DUID") %>%
      filter(!is.na(Price), !is.na(MW)) %>%
      group_by(FuelType, Price) %>%
      summarise(MW = sum(MW, na.rm=TRUE), .groups="drop") %>%
      arrange(Price) %>%
      mutate(CumVol = cumsum(MW))
    
    # fetch the one avg_demand for this region
    dd <- demand_df %>%
      filter(REGIONID %in% (regs %||% unique(df$REGIONID))) %>%
      pull(avg_demand) %>% mean()
    
    # 7) build the ggplot
    ggplot(df, aes(x = CumVol, y = Price, fill = FuelType)) +
      geom_col(width = 1) +
      geom_vline(xintercept = dd,
                 linetype = "dashed", colour="steelblue", size=1) +
      scale_x_continuous("Cumulative Offer Volume (MW)", labels=comma) +
      scale_y_continuous("Offer Price ($/MWh)",
                         labels=dollar_format(),
                         limits = c(0, price_cap)) +
      coord_cartesian(expand=FALSE) +
      labs(
        title = format(first_day, "%b %Y"),
        fill  = "Fuel Type"
      ) +
      theme_minimal(base_size=12) +
      theme(
        plot.title = element_text(face="bold", size=14),
        legend.position = "right"
      )
  }
  
  # make each
  p1 <- make_one(month1)
  p2 <- make_one(month2)
  
  # stitch them vertically & add an overall title
  combined <- (p1 / p2) +
    plot_annotation(
      title = sprintf("NEM Bidstack Comparison — %s vs %s",
                      format(ymd(paste0(month1,"-01")), "%b %Y"),
                      format(ymd(paste0(month2,"-01")), "%b %Y")),
      theme = theme(plot.title = element_text(size=16, face="bold", hjust=0.5))
    ) &
    theme(legend.position="bottom")
  combined
}