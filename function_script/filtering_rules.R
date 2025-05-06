# function_script/filtering_rules.R

library(data.table)
library(lubridate)

filtering_rules <- list(
  
  # 1. BIDDAYOFFER_D
  BIDDAYOFFER_D = list(
    filter = function(dt) dt[BIDTYPE == "ENERGY" & DIRECTION == "GEN"],
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",             # ← include FuelType
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
        "FuelType",             # ← include FuelType
        "SETTLEMENTDATE",
        "PERIODID",
        "DIRECTION",
        "MAXAVAIL",
        grep("BANDAVAIL", names(dt), value = TRUE)
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(
        SETTLEMENTDATE,
        format = "%Y/%m/%d %H:%M:%S",
        tz = "Australia/Brisbane"
      ) + minutes((PERIODID - 1) * 5)]
    }
  ),
  
  # 3. DEMAND
  DEMAND = list(
    filter = function(dt) dt[SCADA_TYPE == "LOCL"],
    select = function(dt) {
      cols <- c(
        "REGIONID",
        "DUID",
        "FuelType",             # ← include FuelType
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
        "FuelType",             # ← include FuelType
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
        "FuelType",             # ← include FuelType
        "INTERVAL_DATETIME",
        "ORIGIN",
        "FORECAST_MEAN",
        "FORECAST_POE50"
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
        "FuelType",             # ← include FuelType
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
  
  # 7. OPERATIONAL_DEMAND  (no DUID, so FuelType not added)
  OPERATIONAL_DEMAND = list(
    filter = function(dt) dt,
    select = function(dt) {
      demand_cols <- grep("^OPERATIONAL_DEMAND$", names(dt), value = TRUE)
      numeric_cols <- demand_cols[sapply(demand_cols, function(x) is.numeric(dt[[x]]))]
      base_cols <- c("REGIONID", "INTERVAL_DATETIME")
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
  
  # 8. ROOFTOP        (no DUID, so FuelType not added)
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
        "FuelType",             # ← include FuelType
        "DISPATCHINTERVAL",
        "AVAILABILITY",
        "TOTALCLEARED"
      )
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := {
        date <- substr(DISPATCHINTERVAL, 1, 8)
        period <- as.numeric(substr(DISPATCHINTERVAL, 9, 11))
        as.POSIXct(date, format = "%Y%m%d", tz = "Australia/Brisbane") +
          minutes((period - 1) * 5)
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
        "FuelType",             # ← include FuelType
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
        date <- substr(DISPATCHINTERVAL, 1, 8)
        period <- as.numeric(substr(DISPATCHINTERVAL, 9, 11))
        as.POSIXct(date, format = "%Y%m%d", tz = "Australia/Brisbane") +
          minutes((period - 1) * 5)
      }]
    }
  ),
  
  # 11. OUTAGEDETAIL  (no DUID, so FuelType not added)
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
  
  # 12. PUBLIC_PRICES  (no DUID, so FuelType not added)
  PUBLIC_PRICES = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c(
        "SETTLEMENTDATE",
        "REGIONID",
        "RRP",
        "TOTALDEMAND",
        "DEMANDFORECAST",
        "DISPATCHABLEGENERATION",
        "DISPATCHABLELOAD",
        "NETINTERCHANGE",
        "AVAILABLEGENERATION",
        "INITIALSUPPLY"
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