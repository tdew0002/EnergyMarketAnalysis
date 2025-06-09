## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
# bidday <- bidday_df   %>% filter(DUID == duid_filter)
# bidper <- bidper_df   %>% filter(DUID == duid_filter)

## -----------------------------------------------------------------------------
# daily_price <- bidday %>%
#   select(timestamp, PRICEBAND_1:PRICEBAND_10) %>%
#   pivot_longer(-timestamp, names_to="Band", values_to="Price") %>%
#   mutate(Band = as.integer(str_extract(Band, "\\d+")))
# 
# avail_data <- bidper %>%
#   select(timestamp, BANDAVAIL_1:BANDAVAIL_10) %>%
#   pivot_longer(-timestamp, names_to="Band", values_to="MW") %>%
#   mutate(Band = as.integer(str_extract(Band, "\\d+")))
# 

## -----------------------------------------------------------------------------
# joined <- inner_join(daily_price, avail_data, by = c("timestamp","Band")) %>%
#   group_by(timestamp) %>%
#   arrange(Price) %>%
#   mutate(CumMW = cumsum(MW)) %>%
#   ungroup()
# 

## -----------------------------------------------------------------------------
# ggplot(joined, aes(x = CumMW, y = Price, group = timestamp)) +
#   geom_step(color = "#0077b6") +
#   scale_y_log10() +
#   labs(
#     title = paste("Supply Curve for", duid_filter, "— {frame_time}"),
#     x = "Cumulative MW",
#     y = "Price ($/MWh)"
#   ) +
#   transition_time(timestamp) +
#   ease_aes('linear') +
#   theme_minimal()
# 

## -----------------------------------------------------------------------------
# bid_filtered <- bidper_df %>%
#   filter(DUID == duid_filter)
# 

## -----------------------------------------------------------------------------
# avail_cols <- grep("BANDAVAIL", names(bid_filtered), value = TRUE)
# 
# bid_long <- bid_filtered %>%
#   select(timestamp, all_of(avail_cols)) %>%
#   pivot_longer(-timestamp, names_to = "Band", values_to = "MW") %>%
#   mutate(Band = as.integer(str_extract(Band, "\\d+")))
# 

## -----------------------------------------------------------------------------
# ggplot(bid_long, aes(
#     x    = timestamp,
#     y    = factor(Band),
#     fill = MW
#   )) +
#   geom_tile() +
#   labs(
#     title = paste("Availability Heatmap for", duid_filter),
#     x     = "Time",
#     y     = "Price Band"
#   ) +
#   scale_fill_viridis_c(name = "MW Available") +
#   theme_minimal()
# 

## -----------------------------------------------------------------------------
# day0  <- ymd(date)
# start <- as_datetime(day0)
# end   <- start + days(1)
# regs  <- if (tolower(region)=="all") NULL else strsplit(region, ",\\s*")[[1]]
# 

## -----------------------------------------------------------------------------
# daily_price <- open_dataset(bids_dir) %>%
#   filter(timestamp >= start, timestamp < end) %>%
#   filter(REGIONID %in% regs) %>%
#   select(REGIONID, DUID, starts_with("PRICEBAND")) %>%
#   collect() %>%
#   group_by(REGIONID, DUID) %>%
#   summarise(across(starts_with("PRICEBAND"), mean, na.rm=TRUE)) %>%
#   pivot_longer(starts_with("PRICEBAND"), names_to="Band", values_to="Price") %>%
#   mutate(Band = as.integer(str_extract(Band, "\\d+")))
# 

## -----------------------------------------------------------------------------
# interm_tbl <- open_dataset(interm_dir) %>%
#   filter(timestamp >= start, timestamp < end) %>%
#   collect() %>%
#   group_by(DUID, timestamp) %>%
#   filter(
#     FORECAST_PRIORITY == max(FORECAST_PRIORITY),
#     OFFERDATETIME    == max(OFFERDATETIME)
#   ) %>%
#   slice_head(n=1) %>%
#   select(DUID, FORECAST_POE50, timestamp)
# 

## -----------------------------------------------------------------------------
# avail_all <- open_dataset(per_dir) %>%
#   filter(timestamp >= start, timestamp < end) %>%
#   filter(REGIONID %in% regs) %>%
#   select(REGIONID, DUID, timestamp, starts_with("BANDAVAIL"), MAXAVAIL) %>%
#   collect() %>%
#   pivot_longer(starts_with("BANDAVAIL"), names_to="Band", values_to="MW") %>%
#   mutate(Band = as.integer(str_extract(Band, "\\d+"))) %>%
#   left_join(interm_tbl, by=c("DUID","timestamp")) %>%
#   mutate(
#     effective_maxavail = if_else(!is.na(FORECAST_POE50),
#                                  pmin(FORECAST_POE50, MAXAVAIL),
#                                  MAXAVAIL)
#   ) %>%
#   group_by(REGIONID, DUID, timestamp) %>%
#   mutate(
#     totalMW   = sum(MW, na.rm=TRUE),
#     over      = totalMW - effective_maxavail,
#     maxBandNZ = max(Band[MW>0], na.rm=TRUE),
#     MW        = if_else(over>0 & Band==maxBandNZ,
#                         pmax(MW - over, 0),
#                         MW)
#   ) %>%
#   ungroup()
# 

## -----------------------------------------------------------------------------
# fuel_df <- open_dataset(units_dir) %>%
#   select(DUID, FuelType) %>%
#   distinct() %>%
#   collect()
# 

## -----------------------------------------------------------------------------
# df_all <- daily_price %>%
#   inner_join(avail_all, by=c("REGIONID","DUID","Band")) %>%
#   left_join(fuel_df,  by="DUID") %>%
#   filter(!is.na(Price), !is.na(MW)) %>%
#   group_by(timestamp, FuelType, Price, Band) %>%
#   summarise(MW = mean(MW, na.rm=TRUE)) %>%
#   arrange(timestamp, Price) %>%
#   group_by(timestamp) %>%
#   mutate(CumVol = cumsum(MW)) %>%
#   ungroup()
# 

## -----------------------------------------------------------------------------
# demand_ts <- open_dataset(units_dir) %>%
#   filter(timestamp >= start, timestamp < end) %>%
#   filter(REGIONID %in% regs) %>%
#   select(timestamp, TOTALCLEARED) %>%
#   collect() %>%
#   group_by(timestamp) %>%
#   summarise(demand = sum(TOTALCLEARED), .groups="drop")
# 

## -----------------------------------------------------------------------------
# ggplot(df_anim, aes(fill=FuelType)) +
#   geom_rect(aes(
#     xmin = CumVol - MW, xmax = CumVol,
#     ymin = 0,         ymax = Price
#   )) +
#   geom_vline(aes(xintercept=demand),
#              linetype="dashed", color="steelblue") +
#   geom_text(data=label_df,
#             aes(x=demand, y=price_cap, label=label_both),
#             hjust=1.1, vjust=-0.5, size=3) +
#   coord_cartesian(ylim=c(0,price_cap)) +
#   scale_x_continuous("Cumulative MW", labels=number_format(scale=1e-3,suffix="K")) +
#   scale_y_continuous("Price ($/MWh)", labels=dollar_format()) +
#   labs(
#     title    = paste0("Bidstack on ", date, " (", region, ")"),
#     subtitle = "Time: {frame_time}", fill="Fuel Type"
#   ) +
#   theme_minimal(base_size=14) +
#   transition_time(timestamp) +
#   ease_aes("linear")
# 

## -----------------------------------------------------------------------------
# dt0 <- ymd(start_date, tz="Australia/Brisbane")
# dt1 <- ymd(end_date,   tz="Australia/Brisbane") + days(1) - seconds(1)
# floor30 <- function(x) floor_date(x, "30 minutes")
# 

## -----------------------------------------------------------------------------
# ggplot(df, aes(
#   x     = as_hms(ts - floor_date(ts, "day")),
#   y     = value,
#   group = date
# )) +
#   geom_line(color="grey80", linewidth=0.4) +
#   stat_summary(aes(group=1),
#                fun       = mean,
#                geom      = "line",
#                linewidth = 1.1) +
#   scale_x_time(..., labels=label_time("%H:%M")) +
#   scale_y_continuous(...) +
#   labs(
#     title    = sprintf("Daily %s — %s (%s to %s)", metric_name, region, start_date, end_date),
#     subtitle = "Grey = each day; <color> = average"
#   ) +
#   theme_minimal()
# 

## -----------------------------------------------------------------------------
# 

## -----------------------------------------------------------------------------
# 

## -----------------------------------------------------------------------------
# 

## -----------------------------------------------------------------------------
# 

