plot_bids_paginated <- function(bidday_df, bidper_df, region = "VIC1", duid_filter = NULL, page = 1, page_size = 12) {
  
  # Step 1: Filter region & collect
  bidday_df_filtered <- bidday_df |>
    filter(REGIONID == region) |>
    collect()
  
  bidper_df_filtered <- bidper_df |>
    filter(REGIONID == region) |>
    collect()
  
  # Step 2: Latest bids per DUID
  latest_bidday <- bidday_df_filtered |>
    group_by(DUID) |>
    filter(timestamp == max(timestamp)) |>
    ungroup()
  
  latest_bidper <- bidper_df_filtered |>
    group_by(DUID) |>
    filter(timestamp == max(timestamp)) |>
    ungroup()
  
  # Step 3: Extract bands
  price_cols <- grep("PRICEBAND", names(latest_bidday), value = TRUE)
  avail_cols <- grep("BANDAVAIL", names(latest_bidper), value = TRUE)
  
  price_long <- latest_bidday |>
    select(DUID, all_of(price_cols)) |>
    pivot_longer(-DUID, names_to = "Band", values_to = "Price") |>
    mutate(Band = as.integer(str_extract(Band, "\\d+")))
  
  avail_long <- latest_bidper |>
    select(DUID, all_of(avail_cols)) |>
    pivot_longer(-DUID, names_to = "Band", values_to = "MW") |>
    mutate(Band = as.integer(str_extract(Band, "\\d+")))
  
  band_df <- left_join(price_long, avail_long, by = c("DUID", "Band"))
  
  # Step 4: Filter by DUIDs (optional)
  if (!is.null(duid_filter)) {
    band_df <- band_df |> filter(DUID %in% duid_filter)
  }
  
  # Step 5: Pagination
  all_duids <- sort(unique(band_df$DUID))
  start_idx <- ((page - 1) * page_size + 1)
  end_idx <- min(start_idx + page_size - 1, length(all_duids))
  selected_duids <- all_duids[start_idx:end_idx]
  
  band_df <- band_df |> filter(DUID %in% selected_duids)
  
  # Supply curve
  supply_df <- band_df |>
    arrange(DUID, Price) |>
    group_by(DUID) |>
    mutate(CumMW = cumsum(MW)) |>
    ungroup()
  
  # Plot 1 - Price
  p1 <- ggplot(band_df, aes(x = Band, y = Price, fill = DUID)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ DUID, scales = "free_y") +
    labs(title = paste("Offered Price - Page", page), x = "Band", y = "Price") +
    theme_minimal()
  
  # Plot 2 - Quantity
  p2 <- ggplot(band_df, aes(x = Band, y = MW, fill = DUID)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ DUID, scales = "free_y") +
    labs(title = "Offered MW", x = "Band", y = "MW") +
    theme_minimal()
  
  # Plot 3 - Supply Curve
  p3 <- ggplot(supply_df, aes(x = CumMW, y = Price, group = DUID)) +
    geom_step(aes(color = DUID), size = 1) +
    facet_wrap(~ DUID, scales = "free") +
    labs(title = "Supply Curve", x = "Cumulative MW", y = "Price") +
    theme_minimal()
  
  # Return interactive plots
  list(
    ggplotly(p1),
    ggplotly(p2),
    ggplotly(p3)
  )
}
