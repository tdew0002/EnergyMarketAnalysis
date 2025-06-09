# R/filtering_rules.R

#' @title  AEMO table‐specific filter/select/timestamp logic
#' @description
#'   A named list (`filtering_rules`) where each element corresponds to one AEMO table
#'   (e.g. “BIDDAYOFFER_D”, “DREGION”, etc.).  
#'   For each table name, it specifies three functions:  
#'   1. `$filter(dt)`: subset rows.  
#'   2. `$select(dt)`: return vector of column names to keep.  
#'   3. `$timestamp(dt)`: add or modify a POSIXct `timestamp` column.  
#' @format A named list of sub‐lists (filter, select, timestamp).
#' @import data.table
#' @importFrom lubridate minutes
#' @export
filtering_rules <- list(
  
  # 1. BIDDAYOFFER_D
  BIDDAYOFFER_D = list(
    filter = function(dt) dt[BIDTYPE == "ENERGY" & DIRECTION == "GEN"],
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",
        "SETTLEMENTDATE",
        grep("PRICEBAND", names(dt), value = TRUE)
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        SETTLEMENTDATE,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  ),
  
  # 2. BIDPEROFFER_D
  BIDPEROFFER_D = list(
    filter = function(dt) dt[BIDTYPE == "ENERGY" & DIRECTION == "GEN"],
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",
        "SETTLEMENTDATE",
        "PERIODID",
        "DIRECTION",
        "MAXAVAIL",
        "PASAAVAILIBILITY",
        "ENERGYLIMIT",
        grep("BANDAVAIL", names(dt), value = TRUE)
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        SETTLEMENTDATE,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      ) + lubridate::minutes((PERIODID - 1) * 5)]
    }
  ),
  
  # 3. DEMAND
  DEMAND = list(
    filter = function(dt) dt[SCADA_TYPE == "LOCL"],
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",
        "RUN_DATETIME",
        "SCADA_VALUE"
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        RUN_DATETIME,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  ),
  
  # 4. INTERMITTENT_DS_RUN
  INTERMITTENT_DS_RUN = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",
        "RUN_DATETIME",
        "ORIGIN"
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        RUN_DATETIME,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  ),
  
  # 5. INTERMITTENT_DS_PRED
  INTERMITTENT_DS_PRED = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",
        "INTERVAL_DATETIME",
        "ORIGIN",
        "FORECAST_MEAN",
        "FORECAST_POE50",
        "FORECAST_PRIORITY",
        "OFFERDATETIME"
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        INTERVAL_DATETIME,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  ),
  
  # 6. INTERMITTENT_FORECAST_TRK
  INTERMITTENT_FORECAST_TRK = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",
        "SETTLEMENTDATE",
        "ORIGIN",
        "FORECAST_PRIORITY"
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        SETTLEMENTDATE,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  ),
  
  # 7. OPERATIONAL_DEMAND  
  OPERATIONAL_DEMAND = list(
    filter = function(dt) dt,
    select = function(dt) {
      demand_cols  <- grep("^OPERATIONAL_DEMAND$", names(dt), value = TRUE)
      numeric_cols <- demand_cols[sapply(demand_cols, function(x) is.numeric(dt[[x]]))]
      base_cols    <- c("REGIONID", "INTERVAL_DATETIME")
      intersect(c(base_cols, numeric_cols), names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        INTERVAL_DATETIME,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  ),
  
  # 8. ROOFTOP       
  ROOFTOP = list(
    filter = function(dt) dt,
    select = function(dt) c("REGIONID", "INTERVAL_DATETIME", "POWERMEAN", "POWERPOE50"),
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        INTERVAL_DATETIME,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  ),
  
  # 9. UNIT_SOLUTION
  UNIT_SOLUTION = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",
        "DISPATCHINTERVAL",
        "AVAILABILITY",
        "TOTALCLEARED"
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := {
        date   <- substr(DISPATCHINTERVAL, 1, 8)
        period <- as.numeric(substr(DISPATCHINTERVAL, 9, 11))
        as.POSIXct(date, format = "%Y%m%d", tz = "Australia/Brisbane") +
          lubridate::minutes((period - 1) * 5)
      }]
    }
  ),
  
  # 10. CONSTRAINT
  CONSTRAINT = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",
        "DISPATCHINTERVAL",
        "CONSTRAINTID",
        "RHS",
        "MARGINALVALUE",
        "VIOLATIONDEGREE",
        "LHS"
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := {
        date   <- substr(DISPATCHINTERVAL, 1, 8)
        period <- as.numeric(substr(DISPATCHINTERVAL, 9, 11))
        as.POSIXct(date, format = "%Y%m%d", tz = "Australia/Brisbane") +
          lubridate::minutes((period - 1) * 5)
      }]
    }
  ),
  
  # 11. OUTAGEDETAIL 
  OUTAGEDETAIL = list(
    filter = function(dt) dt,
    select = function(dt) c("SUBSTATIONID", "ACTUAL_STARTTIME", "ACTUAL_ENDTIME"),
    timestamp = function(dt) {
      dt[, ACTUAL_START := as.POSIXct(
        ACTUAL_STARTTIME,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
      dt[, ACTUAL_END := as.POSIXct(
        ACTUAL_ENDTIME,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  ),
  
  # 12. DREGION
  DREGION = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c(
        "SETTLEMENTDATE", "REGIONID", "RRP", "TOTALDEMAND",
        "DEMANDFORECAST", "DISPATCHABLEGENERATION", "DISPATCHABLELOAD",
        "NETINTERCHANGE", "AVAILABLEGENERATION", "INITIALSUPPLY"
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        SETTLEMENTDATE,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      )]
    }
  )
)
        