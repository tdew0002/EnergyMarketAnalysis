# R/supply_plot.R

#' @title Animate Supply Curve
#' @description
#'   Create an animated supply curve (cumulative MW vs Price) over time for a single DUID.
#' @param bidday_df A data.frame or tibble of BIDDAYOFFER_D
#' @param bidper_df A data.frame or tibble of BIDPEROFFER_D
#' @param duid_filter Single DUID string
#' @return A **gganimate** object
#' @importFrom dplyr filter select mutate group_by arrange ungroup inner_join
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @importFrom ggplot2 ggplot aes geom_step labs theme_minimal scale_y_log10
#' @importFrom gganimate transition_time ease_aes
#' @export
plot_supply_curve_animation <- function(bidday_df, bidper_df, duid_filter) {
  bidday <- bidday_df %>% dplyr::filter(DUID == duid_filter)
  bidper <- bidper_df %>% dplyr::filter(DUID == duid_filter)
  
  price_cols <- grep("PRICEBAND", names(bidday), value = TRUE)
  avail_cols <- grep("BANDAVAIL", names(bidper), value = TRUE)
  
  daily_price <- bidday %>%
    dplyr::select(timestamp, all_of(price_cols)) %>%
    tidyr::pivot_longer(-timestamp, names_to="Band", values_to="Price") %>%
    dplyr::mutate(Band = as.integer(stringr::str_extract(Band, "\\d+")))
  
  avail_data <- bidper %>%
    dplyr::select(timestamp, all_of(avail_cols)) %>%
    tidyr::pivot_longer(-timestamp, names_to="Band", values_to="MW") %>%
    dplyr::mutate(Band = as.integer(stringr::str_extract(Band, "\\d+")))
  
  joined <- dplyr::inner_join(daily_price, avail_data, by = c("timestamp","Band")) %>%
    dplyr::group_by(timestamp) %>%
    dplyr::arrange(Price) %>%
    dplyr::mutate(CumMW = cumsum(MW)) %>%
    dplyr::ungroup()
  
  ggplot2::ggplot(joined, ggplot2::aes(x = CumMW, y = Price, group = timestamp)) +
    ggplot2::geom_step(color = "#0077b6") +
    ggplot2::labs(
      title = paste("Supply Curve for", duid_filter, "— {frame_time}"),
      x     = "Cumulative MW",
      y     = "Price ($/MWh)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_log10() +
    gganimate::transition_time(timestamp) +
    gganimate::ease_aes("linear")
}
