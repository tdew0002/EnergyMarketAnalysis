# region_utils.R
library(data.table)

# Load and duplicate (keep first row per DUID since REGIONID is the same)
duid_region_map <- fread("duid_region_map.csv")
duid_region_map <- duid_region_map[!duplicated(DUID)]


# Safely merge REGIONID into any dataset that has DUID
add_regionid_if_applicable <- function(dt) {
  if ("DUID" %in% names(dt)) {
    dt <- merge(dt, duid_region_map, by = "DUID", all.x = TRUE)
  }
  return(dt)
}

