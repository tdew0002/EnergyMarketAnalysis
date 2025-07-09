# inst/app/app.R

## ---- Auto-install & load required packages ---------------------------------
required_pkgs <- c(
  "shiny", "shinydashboard",          # UI framework
  "arrow",                            # Parquet / Apache Arrow
  "dplyr", "tidyr", "lubridate",      # data wrangling
  "ggplot2", "gganimate", "scales",   # base + animated plotting
  "plotly",                           # interactive plots
  "here",                             # project-root paths
  "progressr", "future",              # (optional) progress + parallel
  "gifski",                           # GIF encoder used by gganimate
  "AemoETL"                           # your own package
)

# identify what’s missing
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      FUN.VALUE = logical(1), quietly = TRUE)]

# install any that aren’t present
if (length(missing_pkgs)) {
  message("Installing missing packages: ",
          paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

# now safely attach everything (suppress startup messages to keep the log tidy)
invisible(lapply(required_pkgs, function(pkg)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))))
## ---------------------------------------------------------------------------

# define data directories
data_dir        <- here("data", "filtered_parquet_tables")
bids_dir        <- file.path(data_dir, "BIDDAYOFFER_D")
per_dir         <- file.path(data_dir, "BIDPEROFFER_D")
operational_dir <- file.path(data_dir, "OPERATIONAL_DEMAND")
rrp_dir         <- file.path(data_dir, "DREGION")
unit_dir        <- file.path(data_dir, "UNIT_SOLUTION")
interm_dir      <- file.path(data_dir, "INTERMITTENT_DS_PRED")

# open datasets once
datasets <- list(
  BIDDAYOFFER_D        = open_dataset(bids_dir),
  BIDPEROFFER_D        = open_dataset(per_dir),
  OPERATIONAL_DEMAND   = open_dataset(operational_dir),
  DREGION              = open_dataset(rrp_dir),
  UNIT_SOLUTION        = open_dataset(unit_dir),
  INTERMITTENT_DS_PRED = open_dataset(interm_dir)
)

# derive available date range
date_bounds <- datasets$BIDDAYOFFER_D %>%
  summarise(
    min_date = floor_date(min(timestamp), unit = "day"),
    max_date = floor_date(max(timestamp), unit = "day")
  ) %>%
  collect()

ui <- dashboardPage(
  dashboardHeader(title = "NEM Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Supply Curve", tabName = "supply",    icon = icon("chart-line")),
      menuItem("Heatmap",      tabName = "heatmap",    icon = icon("th")),
      menuItem("Bidstack",     tabName = "bidstack",   icon = icon("layer-group")),
      menuItem("Seasonality",  tabName = "seasonality",icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tags$head(
    tags$style(HTML("
      .shiny-plot-output img,
      .shiny-image-output img {
        display: block;
        max-width: 100% !important;
        height: auto    !important;
      }
    "))
  ),
    tabItems(
      
      # 1) Supply Curve
      tabItem("supply",
              fluidRow(
                box(width = 4, title = "Controls",
                    selectInput("supply_duid", "DUID(s):", choices = NULL, multiple = TRUE),
                    selectInput("supply_month", "Month (YYYY-MM):",
                                choices  = seq(date_bounds$min_date, date_bounds$max_date, by = "month") %>% format("%Y-%m"),
                                selected = format(Sys.Date(), "%Y-%m")
                    ),
                    actionButton("go_supply", "Start", icon = icon("play"))
                ),
                box(width = 8,
                    tabBox(width = 12,
                           tabPanel("Animated", imageOutput("supplyAnim", height = "500px"))
                    )
                )
              )
      ),
      
      # 2) Heatmap
      tabItem("heatmap",
              fluidRow(
                box(width = 4, title = "Controls",
                    selectInput("heatmap_duid", "DUID:", choices = NULL),
                    actionButton("go_heatmap", "Start", icon = icon("play"))
                ),
                box(width = 8,
                    plotOutput("heatmapPlot", height = "500px")
                )
              )
      ),
      
      # 3) Bidstack
      tabItem("bidstack",
              # Row 1: full-width controls
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE, title = "Bidstack Controls",
                  selectInput("bidstack_region", "Region:", choices = NULL),
                  dateInput("bidstack_date",   "Date (for animation):",
                            value = date_bounds$min_date,
                            min   = date_bounds$min_date,
                            max   = date_bounds$max_date
                  ),
                  sliderInput("bidstack_ts", "Timestamp (5-min interval):",
                              min        = date_bounds$min_date,
                              max        = date_bounds$max_date + days(1) - minutes(5),
                              value      = date_bounds$min_date,
                              step       = 300,
                              timeFormat = "%Y-%m-%d %H:%M"
                  ),
                  actionButton("go_bidstack_static",   "Refresh Static",   icon = icon("play")),
                  actionButton("go_bidstack_anim",     "Refresh Animated", icon = icon("play"))
                )
              ),
              
              # Row 2: two half-width boxes, side by side
              fluidRow(
                column(
                  width = 6,
                  box(
                    width = NULL, status = "info", solidHeader = TRUE, title = "Static Bidstack",
                    plotOutput("bidstackStatic", height = "600px", width = "100%")
                  )
                ),
                column(
                  width = 6,
                  box(
                    width = NULL, status = "warning", solidHeader = TRUE, title = "Animated Bidstack",
                    # give it plenty of height so it doesn't squish
                    imageOutput("bidstackAnim", height = "600px", width = "100%")
                  )
                )
              )
      ),
      
      # 4) Seasonality
      tabItem("seasonality",
              fluidRow(
                box(width = 4, title = "Controls",
                    selectInput("season_region", "Region:", choices = NULL),
                    dateRangeInput("season_daterange", "Date range:",
                                   start = date_bounds$min_date,
                                   end   = date_bounds$max_date,
                                   min   = date_bounds$min_date,
                                   max   = date_bounds$max_date
                    ),
                    actionButton("go_season", "Start", icon = icon("play"))
                ),
                column(width = 8,
                    tabBox(width = 12,
                           tabPanel("Price",            plotlyOutput("seasonPrice")),
                           tabPanel("Availability",     plotlyOutput("seasonAvail")),
                           tabPanel("RRP",              plotlyOutput("seasonRRP")),
                           tabPanel("Residual",         plotlyOutput("seasonResidual")),
                           tabPanel("Operational Demand", plotlyOutput("seasonOperational")),
                           tabPanel("Renewable",         plotlyOutput("seasonRenewable"))
                    )
                )
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  # populate selectors
  observe({
    duids   <- datasets$BIDPEROFFER_D %>% distinct(DUID) %>% collect() %>% pull(DUID) %>% sort()
    regions <- datasets$DREGION         %>% distinct(REGIONID) %>% collect() %>% pull(REGIONID) %>% sort()
    updateSelectInput(session, "supply_duid",    choices = duids,    selected = duids[1])
    updateSelectInput(session, "heatmap_duid",   choices = duids,    selected = duids[1])
    updateSelectInput(session, "bidstack_region",choices = regions, selected = regions[1])
    updateSelectInput(session, "season_region",  choices = regions, selected = regions[1])
  })
  
  # re-adjust slider when date changes
  observeEvent(input$bidstack_date, {
    start_dt <- as_datetime(input$bidstack_date, tz="Australia/Brisbane")
    end_dt   <- start_dt + days(1) - minutes(5)
    updateSliderInput(session, "bidstack_ts",
                      min   = start_dt,
                      max   = end_dt,
                      value = start_dt
    )
  })
  
  # 1) Supply
  supply_anim <- eventReactive(input$go_supply, {
    req(input$supply_duid, input$supply_month)
    start_dt <- ymd(paste0(input$supply_month, "-01"), tz="Australia/Brisbane")
    end_dt   <- start_dt + months(1) - seconds(1)
    df1 <- datasets$BIDDAYOFFER_D %>%
      filter(timestamp >= start_dt, timestamp <= end_dt) %>%
      collect()
    df2 <- datasets$BIDPEROFFER_D %>%
      filter(timestamp >= start_dt, timestamp <= end_dt) %>%
      collect()
    plot_supply_curve_animation(df1, df2, input$supply_duid)
  })
  output$supplyAnim <- renderImage({
    anim <- supply_anim()
    outfile <- tempfile(fileext = ".gif")
    n_ts <- length(unique(anim$data$timestamp))
    gganimate::anim_save(
      filename = outfile, 
      animation = anim,
      nframes = n_ts, 
      fps = 5)
    list(src = outfile, contentType = "image/gif", width = "100%")
  }, deleteFile = TRUE)
  
  # 2) Heatmap
  heatmap_plot <- eventReactive(input$go_heatmap, {
    df <- datasets$BIDPEROFFER_D %>% collect()
    plot_band_avail_heatmap(df, input$heatmap_duid)
  })
  output$heatmapPlot <- renderPlot({ heatmap_plot() })
  
  # 3) Bidstack static
  bidstack_static <- eventReactive(input$go_bidstack_static, {
    ts_str <- format(with_tz(input$bidstack_ts), "%Y-%m-%d %H:%M")
    create_bidstack_5min_snapshot(
      bids_dir   = bids_dir,
      per_dir    = per_dir,
      interm_dir = interm_dir,
      units_dir  = unit_dir,
      region     = input$bidstack_region,
      ts         = ts_str,
      price_cap  = 500
    )
  })
  output$bidstackStatic <- renderPlot({ bidstack_static() })
  
  # 4) Bidstack animated
  bidstack_anim <- eventReactive(input$go_bidstack_anim, {
    create_bidstack_day_animation(
      bids_dir   = bids_dir,
      per_dir    = per_dir,
      interm_dir = interm_dir,
      units_dir  = unit_dir,
      rrp_dir    = rrp_dir,
      region     = input$bidstack_region,
      date       = as.character(input$bidstack_date),
      price_cap  = 500
    )
  })
  output$bidstackAnim <- renderImage({
    anim    <- bidstack_anim()
    outfile <- tempfile(fileext = ".gif")
    gganimate::animate(anim,
                       renderer = gganimate::gifski_renderer(outfile),
                       fps      = 5
    )
    list(src = outfile, contentType = "image/gif", width = "100%")
  }, deleteFile = TRUE)
  
  # 5) Seasonality
  season_plots <- eventReactive(input$go_season, {
    plot_daily_bid_metrics(
      bids_dir        = bids_dir,
      avail_dir       = per_dir,
      rrp_dir         = rrp_dir,
      operational_dir = operational_dir,
      unit_dir        = unit_dir,
      interm_dir      = interm_dir,
      region          = input$season_region,
      start_date      = as.character(input$season_daterange[1]),
      end_date        = as.character(input$season_daterange[2])
    )
  })
  output$seasonPrice       <- renderPlotly({ ggplotly(season_plots()$price_plot       , tooltip="y") })
  output$seasonAvail       <- renderPlotly({ ggplotly(season_plots()$availability_plot, tooltip="text") })
  output$seasonRRP         <- renderPlotly({ ggplotly(season_plots()$rrp_plot         , tooltip="text") })
  output$seasonResidual    <- renderPlotly({ ggplotly(season_plots()$residual_plot    , tooltip="text") })
  output$seasonOperational <- renderPlotly({ ggplotly(season_plots()$operational_plot , tooltip="text") })
  output$seasonRenewable   <- renderPlotly({ ggplotly(season_plots()$renewable_plot   , tooltip="text") })
}

shinyApp(ui, server)
