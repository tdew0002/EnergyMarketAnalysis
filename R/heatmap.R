# R/heatmap.R

#' @title Band Availability Heatmap
#' @description
#'   Plot a heatmap of 5-minute band availability (BANDAVAIL*) over time for one DUID.
#' @param bidper_df A data.frame or tibble of BIDPEROFFER_D (must include `timestamp` and `BANDAVAIL*` columns)
#' @param duid_filter A single DUID string to filter on
#' @return A **ggplot2** object (heatmap)
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_tile labs theme_minimal
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @importFrom ggplot2 scale_fill_viridis_c
#' @export
plot_band_avail_heatmap <- function(bidper_df, duid_filter) {
  bid_filtered <- bidper_df %>% 
    dplyr::filter(DUID == duid_filter)
  
  avail_cols <- grep("BANDAVAIL", names(bid_filtered), value = TRUE)
  
  bid_long <- bid_filtered %>%
    tidyr::pivot_longer(
      cols     = all_of(avail_cols),
      names_to = "Band",
      values_to= "MW"
    ) %>%
    dplyr::mutate(Band = as.integer(stringr::str_extract(Band, "\\d+")))
  
  ggplot2::ggplot(bid_long, ggplot2::aes(x = timestamp, y = factor(Band), fill = MW)) +
    ggplot2::geom_tile() +
    ggplot2::labs(
      title = paste("Availability Heatmap for", duid_filter),
      x     = "Time",
      y     = "Band"
    ) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme_minimal()
}
