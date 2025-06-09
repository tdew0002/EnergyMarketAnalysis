---
editor_options: 
  markdown: 
    wrap: 72
---

<!-- README.md ------------------------------------------------------------- -->

# AemoETL

**AemoETL** is a lightweight R package that lets you

-   **Download → Extract → Transform** raw data from the Australian
    Energy Market Operator (AEMO)
-   Store clean tables in fast **Parquet** format
-   Explore historic bidding behaviour with an included **Shiny
    dashboard** (`run_app()`)
-   Re-use a set of tidy **helper functions** in your own analyses\
    (bid-stack snapshots, heat-maps, seasonality plots, & more)

------------------------------------------------------------------------

## Installation

```{r}
# stable release (when on CRAN)
install.packages("AemoETL")

# or the development version
remotes::install_github("tdew0002/EnergyMarketAnalysis")
```

```{r}
library(AemoETL)

# 1. Interactively pick the dataset(s) to download …
main()

# 2. Apply the built-in filtering rules …
run_filtering()

# 3. Dive into the data
run_app()          # launches the Shiny dashboard

```

## Read the documentation & examples

You can open any vignette straight from R:

```{r}
 browseVignettes("AemoETL")
```

Or click the pre-rendered HTML files in the repo:

| Resource                       | Where to open             |
|--------------------------------|---------------------------|
| **Introduction & Quick-start** | `doc/introduction.html`   |
| **Shiny dashboard tour**       | `doc/shiny_overview.html` |
| **Technical report**           | `doc/report.html`         |
| **Final presentation**         | `doc/slides.html`         |

## Using the Shiny dashboard

```{r}
library(AemoETL)
run_app()
```

The app ships inside the package (inst/app/) and opens in your default
browser. It contains four panels:

-   Supply curve animation (select DUID & month)

-   Availability heat-map (DUID × time × price band)

-   Bid-stack snapshot and day-long animation (region & date/ts)

-   Seasonality spaghetti plots (price, availability, RRP, demand …)

A quick “how-to” is in the Shiny Dashboard Tour vignette above.

## Shortcut — download filtered data - RECOMEMDED

If you’d prefer to skip the raw ETL and jump straight into analysis, a
pre-filtered Parquet snapshot is available on Google Drive:

<https://drive.google.com/uc?id=><file-id>&export=download

Unzip it into the project root so you have
data/filtered_parquet_tables/…, then launch the dashboard or your own
queries immediately.

## Licence

This package is released under the MIT Licence (see\
[`LICENSE`](LICENSE)). AEMO data is © AEMO Ltd and distributed under\
its open-data terms; please cite appropriately when you publish results.
