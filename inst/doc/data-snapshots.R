## ----setup, include=FALSE-----------------------------------------------------
library(DSIR)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----live-workflow, eval=FALSE------------------------------------------------
# old_data <- gho_data("WHS4_100", spatial_type = "country", area = "PHL",
#                      year_from = 2020, backend = "xmart")
# data_snapshot(old_data, file = "snapshots/gho_old.rds")
# 
# # Later, repeat exactly the same selection.
# new_data <- gho_data("WHS4_100", spatial_type = "country", area = "PHL",
#                      year_from = 2020, backend = "xmart")
# data_snapshot(new_data, file = "snapshots/gho_new.rds")
# 
# result <- compare_snapshots("snapshots/gho_old.rds", "snapshots/gho_new.rds")
# result$summary
# result$modified

## ----synthetic-old------------------------------------------------------------
old_data <- data.frame(
  IndicatorCode = "EXAMPLE", SpatialDimType = "COUNTRY",
  SpatialDim = "PHL", TimeDimType = "YEAR",
  TimeDim = c(2020L, 2020L, 2021L, 2021L),
  Dim1Type = "SEX", Dim1 = rep(c("SEX_MLE", "SEX_FMLE"), 2),
  Dim2Type = "AGEGROUP", Dim2 = "AGEGROUP_YEARS15-49",
  Dim3Type = NA_character_, Dim3 = NA_character_,
  NumericValue = c(10, 12, 20, 22),
  Comments = NA_character_
)
scope <- list(spatial_type = "country", area = "PHL",
              year_from = 2020, year_to = NULL)
old_path <- tempfile(fileext = ".rds")
new_path <- tempfile(fileext = ".rds")
data_snapshot(old_data, old_path, backend = "legacy", scope = scope)

## ----synthetic-new------------------------------------------------------------
# Keep three old observations; omit the female observation from 2021.
new_data <- old_data[1:3, ]
new_data$NumericValue[1] <- 11
new_data$Comments[3] <- "Updated explanatory note"

# Add an observation in a later year.
new_row <- old_data[1, ]
new_row$TimeDim <- 2022L
new_row$NumericValue <- 30
new_data <- rbind(new_data, new_row)
data_snapshot(new_data, new_path, backend = "legacy", scope = scope)

result <- compare_snapshots(old_path, new_path)
result$summary[, c("n_old", "n_new", "n_added", "n_removed", "n_modified",
                  "n_unchanged", "n_value_modified", "n_metadata_only")]
result$modified[, c("TimeDim", "Dim1", "old_value", "new_value",
                   "absolute_change", "relative_change", "change_type")]

## ----read-snapshot------------------------------------------------------------
saved <- readRDS(old_path)
saved$schema_version
saved$key
identical(saved$data, old_data)

## ----selected-fields----------------------------------------------------------
compare_snapshots(old_path, new_path, fields = "NumericValue",
                  tolerance = 1e-8)$summary$n_modified

