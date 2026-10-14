# Launching StudyResultsExplorer

### 1. Open the study project

Open **`EUPAS1000000823.Rproj`** in RStudio (repository root).

### 2. Restore the project library

```r
renv::restore()
```

### 3. Run the study (produces results)

Configure and run via **`extras/CodeToRun.R`**. By default, exports land under:

```text
results/
└── <dbName>/          # e.g. CDW Bordeaux, IQVIA DA Germany, CPRD GOLD
    ├── cdm_snapshot_*.csv
    ├── attrition_*.csv
    ├── prev_*.csv
    ├── dus_obj3_*.csv
    ├── char*.csv
    └── log/
```

Flat CSVs under each `results/<dbName>/` folder are expected (no nested Attrition/Characterisation folders required).

### 4. Launch the Shiny app

From the **study repository root**:

```r
source("R/support/utils.R")
launchResultsExplorer(
  dataFolder = here::here("results"),
  useCachedData = FALSE
)
```

`dataFolder` must be the parent folder that contains one subdirectory per database name.  
Set `useCachedData = FALSE` after a new study run so `data/data.rds` is rebuilt.

**Note:** Do not commit `data/data.rds` or any copied result CSVs under this app folder.

For launching the Shiny app from the `inst/shiny/StudyResultsExplorer` project, you may place per-database CSV folders under `inst/shiny/StudyResultsExplorer/data/raw/<dbName>/` (the empty `data/raw/` folder is kept in git via `.gitkeep`; result files there stay gitignored). 
