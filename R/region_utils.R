# R/region_utils.R

#' @title  Load and clean DUID → REGIONID + FuelType map
#' @description
#'   Reads `mapping/duid_region_map.csv` (via here::here()), trims whitespace,
#'   coerces to character, deduplicates on DUID, and sets a data.table key.
#'   The cleaned map is stored in the object `duid_region_map`.
#' @return A data.table keyed on DUID.
#' @import data.table
#' @importFrom here here
#' @export
duid_region_map <- {
  # 1) read the CSV
  dt <- data.table::fread(
    here::here("mapping", "duid_region_map.csv"),
    strip.white = TRUE
  )
  
  # 2) use base-R assignments to avoid := at load time
  dt$DUID     <- trimws(as.character(dt$DUID))
  dt$REGIONID <- trimws(as.character(dt$REGIONID))
  dt$FuelType <- trimws(as.character(dt$FuelType))
  
  # 3) deduplicate
  dt <- dt[!duplicated(dt$DUID), ]
  
  # 4) set key
  data.table::setkey(dt, "DUID")
  
  dt
}

#' @title  Add REGIONID and FuelType to a data.table (by DUID)
#' @description
#'   If `dt` has a “DUID” column, looks up `duid_region_map` and, by reference,
#'   adds or updates `REGIONID` and `FuelType`. Otherwise returns `dt` unchanged.
#' @param dt A data.table that may contain a column “DUID.”
#' @return The same data.table `dt`, with REGIONID/FuelType filled in if applicable.
#' @import data.table
#' @export
add_regionid_if_applicable <- function(dt) {
  if (!"DUID" %in% names(dt)) {
    return(dt)
  }
  # ensure we have a data.table and trimmed DUIDs
  data.table::setDT(dt)
  dt[, DUID := trimws(as.character(DUID))]
  
  # now do the join by reference
  dt[
    duid_region_map,
    `:=`(
      REGIONID = i.REGIONID,
      FuelType = i.FuelType
    ),
    on = "DUID"
  ]
  
  dt
}
