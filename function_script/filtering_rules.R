# filtering_rules.R

filtering_rules <- list(
  
  BIDDAYOFFER_D = list(
    filter = function(dt) dt[BIDTYPE == "ENERGY" & DIRECTION == "GEN"],
    select = function(dt) {
      cols <- c("REGIONID", "DUID", "SETTLEMENTDATE", grep("PRICEBAND", names(dt), value = TRUE))
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  ),
  
  BIDPEROFFER_D = list(
    filter = function(dt) dt[BIDTYPE == "ENERGY" & DIRECTION == "GEN"],
    select = function(dt) {
      cols <- c("REGIONID", "DUID", "SETTLEMENTDATE", "PERIODID", "DIRECTION", "MAXAVAIL", grep("BANDAVAIL", names(dt), value = TRUE))
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane") +
           minutes((PERIODID - 1) * 5)]
    }
  ),
  
  DEMAND = list(
    filter = function(dt) dt[SCADA_TYPE == "LOCL"],
    select = function(dt) {
      cols <- c("REGIONID", "RUN_DATETIME", "DUID", "SCADA_VALUE")
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(RUN_DATETIME, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  ),
  
  INTERMITTENT_DS_RUN = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c("REGIONID", "RUN_DATETIME", "DUID", "ORIGIN")
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(RUN_DATETIME, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  ),
  
  INTERMITTENT_DS_PRED = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c("REGIONID", "DUID", "INTERVAL_DATETIME", "ORIGIN", "FORECAST_MEAN", "FORECAST_POE50")
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(INTERVAL_DATETIME, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  ),
  
  INTERMITTENT_FORECAST_TRK = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c("REGIONID", "SETTLEMENTDATE", "DUID", "ORIGIN", "FORECAST_PRIORITY")
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  ),
  
  OPERATIONAL_DEMAND = list(
    filter = function(dt) dt,#dt[REGIONID == "VIC1"],
    select = function(dt) {
      # exact match for the column named "OPERATIONAL_DEMAND":
      demand_cols <- grep("^OPERATIONAL_DEMAND$", names(dt), value = TRUE)
      
      # among those columns, pick only the numeric ones
      numeric_cols <- demand_cols[sapply(demand_cols, function(x) {
        is.numeric(dt[[x]])
      })]
      
      # if we find no numeric col named exactly "OPERATIONAL_DEMAND", warn:
      if (length(numeric_cols) == 0) {
        warning("No numeric OPERATIONAL_DEMAND column found!")
        numeric_cols <- character(0)
      }
      
      # always keep these
      base_cols <- c("REGIONID", "INTERVAL_DATETIME")
      keep_cols <- intersect(c(base_cols, numeric_cols), names(dt))
      keep_cols
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(INTERVAL_DATETIME, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  ),
  ROOFTOP = list(
    filter = function(dt) dt,#dt[REGIONID == "VIC1"],
    select = function(dt) c("REGIONID", "INTERVAL_DATETIME", "POWERMEAN", "POWERPOE50"),
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(INTERVAL_DATETIME, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  ),
  UNIT_SOLUTION = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c("REGIONID", "DUID", "DISPATCHINTERVAL", "AVAILABILITY", "TOTALCLEARED")
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := {
        date <- substr(DISPATCHINTERVAL, 1, 8)
        period <- as.numeric(substr(DISPATCHINTERVAL, 9, 11))
        as.POSIXct(date, format = "%Y%m%d", tz = "Australia/Brisbane") + minutes((period - 1) * 5)
      }]
    }
  ),
  
  CONSTRAINT = list(
    filter = function(dt) dt,
    select = function(dt) {
      cols <- c("REGIONID", "CONSTRAINTID", "DISPATCHINTERVAL", "DUID", "RHS", "MARGINALVALUE", "VIOLATIONDEGREE", "LHS")
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := {
        date <- substr(DISPATCHINTERVAL, 1, 8)
        period <- as.numeric(substr(DISPATCHINTERVAL, 9, 11))
        as.POSIXct(date, format = "%Y%m%d", tz = "Australia/Brisbane") + minutes((period - 1) * 5)
      }]
    }
  ),
  
  OUTAGEDETAIL = list(
    filter = function(dt) dt,
    select = function(dt) c("SUBSTATIONID", "ACTUAL_STARTTIME", "ACTUAL_ENDTIME"),
    timestamp = function(dt) {
      dt[, ACTUAL_START := as.POSIXct(ACTUAL_STARTTIME, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
      dt[, ACTUAL_END := as.POSIXct(ACTUAL_ENDTIME, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  ),
  
  PUBLIC_PRICES = list(
    filter = function(dt) dt,#dt[REGIONID == "VIC1"],
    select = function(dt) {
      cols <- c("SETTLEMENTDATE", "REGIONID", "RRP", "TOTALDEMAND", "DEMANDFORECAST",
                "DISPATCHABLEGENERATION", "DISPATCHABLELOAD", "NETINTERCHANGE",
                "AVAILABLEGENERATION", "INITIALSUPPLY")
      intersect(cols, names(dt))
    },
    timestamp = function(dt) {
      dt[, timestamp := as.POSIXct(SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane")]
    }
  )
)
