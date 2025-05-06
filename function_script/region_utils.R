# region_utils.R
library(data.table)

# 1) Read the map and clean up the header if necessary
duid_region_map <- fread("duid_region_map.csv", strip.white=TRUE)
# If it still has a space, uncomment:
# setnames(duid_region_map, "Fuel Type", "FuelType")

# 2) Trim & coerce DUID, REGIONID, FuelType
duid_region_map[, DUID     := trimws(as.character(DUID))]
duid_region_map[, REGIONID := trimws(as.character(REGIONID))]
duid_region_map[, FuelType := trimws(as.character(FuelType))]

# 3) Deduplicate and set key for super-fast joins
duid_region_map <- duid_region_map[!duplicated(DUID)]
setkey(duid_region_map, DUID)

# 4) The in-place merge function
add_regionid_if_applicable <- function(dt) {
  if (!"DUID" %in% names(dt)) return(dt)
  # Ensure your incoming DUID column is character & clean
  setDT(dt)[, DUID := trimws(as.character(DUID))]
  # Update-join by reference: pulls REGIONID & FuelType into dt
  dt[duid_region_map, 
     `:=`(REGIONID = i.REGIONID, 
          FuelType = i.FuelType), 
     on = "DUID"]
  return(dt)
}



