# **Instructions**

# How to run the study package:

1.  In RStudio, open the `EUPAS1000000823.Rproj` file
2.  Navigate to `extras/CodeToRun.R`
3.  Install and activate renv:
    -   `install.packages("renv")`
    -   `install.packages("CirceR")`
    -   `renv::activate()`
    -   `renv::restore()`
4.  Edit the connection details in `CodeToRun.R`:

-   Set the following variables:

```         
      dbName <- "" # Options: "CDW Bordeaux", "IQVIA DA Germany", "CPRD GOLD"
      refresh_date <- # IMPORTANT:  Please specify the latest database refresh date (YYYY-MM-DD)
      cdmSchema <- ""
      writeSchema <- ""
      tablePrefix <- "p20_"  # Use a short table prefix
      minCellCount <- 5      # Don't edit
```

-   Fill in connection details using DBI::dbConnect()

5.  Execute the study package:

-   Run Analysis: `source(here::here("R/RunAnalysis.R"))`

# Review results:

## Launch the StudyResultsExplorer Shiny app:

```         
# To view the shiny app run the following code
source("R/support/utils.R")
resultsFolder <- here::here("results")
launchResultsExplorer(dataFolder = resultsFolder, useCachedData = FALSE)

```
