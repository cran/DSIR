## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>", 
  eval     = TRUE
)

## ----setup--------------------------------------------------------------------
library(DSIR)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
who_countries

## -----------------------------------------------------------------------------
wpro_cty
length(wpro_cty)   # 28 Member States in WPR (since May 2025)

## -----------------------------------------------------------------------------
who_countries |>
  filter(is_pic) |>
  select(iso3, name_short)

## -----------------------------------------------------------------------------
iso3_to_region(c("PHL", "FRA", "ZAF", "USA", "XYZ"))
# "WPR" "EUR" "AFR" "AMR" NA

## -----------------------------------------------------------------------------
iso3_to_m49(c("PHL", "FRA", "JPN"))
# "608" "250" "392"

# Case-insensitive; non-Member areas return NA
iso3_to_m49(c("phl", "PRI"))
# "608" NA

## -----------------------------------------------------------------------------
# Does WHO have life-expectancy data for France?
gho_has_data("WHOSIS_000001", area = "FRA")
# TRUE

# Bulk-screen several indicators at once
inds <- c("WHOSIS_000001", "NCDMORT3070", "MDG_0000000026")
vapply(inds, gho_has_data, logical(1), area = "PHL")

## -----------------------------------------------------------------------------
gho_count("WHOSIS_000001", area = wpro_cty)

## -----------------------------------------------------------------------------
gho_coverage("WHOSIS_000001", area = c("FRA", "DEU", "JPN"))
#>   location year_min year_max n_obs
#> 1 DEU          2000     2021    66
#> 2 FRA          2000     2021    66
#> 3 JPN          2000     2021    66

## -----------------------------------------------------------------------------
gho_indicators("UHC") |> head()

## -----------------------------------------------------------------------------
uhc <- gho_data(
  indicator    = "UHC_INDEX_REPORTED",
  spatial_type = "country",
  area         = wpro_cty,
  year_from    = 2015
)

uhc |> glimpse()

## -----------------------------------------------------------------------------
uhc_clean <- gho_clean(uhc)
uhc_clean

## -----------------------------------------------------------------------------
# Unweighted geometric mean
geomean(c(0.6, 0.8, 0.95))
#> 0.7720589

# With optional weights — useful when tracers have different 
# methodological importance
geomean(c(0.6, 0.8, 0.95), w = c(2, 1, 1))

## ----fig.width = 7, fig.height = 4--------------------------------------------
uhc_clean |>
  filter(iso3 %in% c("AUS", "CHN", "PHL", "FJI")) |>
  left_join(who_countries, by = "iso3") |>
  ggplot(aes(x = year, y = value_num, group = iso3, color = name_short)) +
  geom_line(linewidth = .8) +
  geom_point(size = 1.8) +
  theme_dsi() +
  labs(
    title    = "UHC Service Coverage Index, selected WPR Member States",
    subtitle = "2015 onwards",
    x = NULL, y = "SCI", color = NULL
  )

## ----fig.width = 7, fig.height = 4--------------------------------------------
uhc_clean |>
  filter(year == max(year)) |>
  left_join(who_countries, by = "iso3") |>
  arrange(desc(value_num)) |>
  head(10) |>
  ggplot(aes(reorder(name_short, value_num), value_num)) +
  geom_col(fill = "#0093D5") +
  coord_flip() +
  scale_y_dsi_col() +
  theme_dsi(grid = "x") +
  labs(
    title    = "UHC Service Coverage Index, top 10 WPR Member States",
    subtitle = "Latest available year",
    x = NULL, y = "SCI"
  )

## ----fig.width = 8, fig.height = 5--------------------------------------------
uhc_clean |>
  left_join(who_countries, by = "iso3") |>
  filter(is_pic) |>
  ggplot(aes(x = year, y = value_num)) +
  geom_line(color = "#0093D5", linewidth = 0.8) +
  geom_point(color = "#0093D5", size = 1.5) +
  facet_wrap(~ name_short, ncol = 4) +
  theme_dsi_facet() +
  labs(
    title    = "UHC Service Coverage Index, Pacific Island Countries",
    subtitle = "Each panel shows one country's trajectory",
    x = NULL, y = "SCI"
  )

## ----fig.width = 8, fig.height = 5--------------------------------------------
uhc_clean |>
  left_join(who_countries, by = "iso3") |>
  filter(is_pic) |>
  ggplot(aes(x = year, y = value_num)) +
  geom_line(color = "#0093D5", linewidth = 0.8) +
  facet_wrap(~ name_short, ncol = 4) +
  theme_dsi_facet(strip_fill = "#E5F4FB") +
  labs(title = "UHC SCI, PIC — with custom strip colour",
       x = NULL, y = "SCI")

## -----------------------------------------------------------------------------
library(flextable)
dsi_flextable_defaults(font_family = "Geogria")

uhc_clean |>
  filter(year == max(year)) |>
  left_join(who_countries, by = "iso3") |>
  select(name_short, value_num) |>
  arrange(desc(value_num)) |>
  flextable() |>
  set_table_properties("autofit", width = .6) %>%
  set_caption("UHC SCI in WPR, latest year")

## -----------------------------------------------------------------------------
# All indicators that mention both mortality and cancer
sdg_indicators("mortality cancer")

# Same as above, but with explicit terms (allows whitespace inside a term)
sdg_indicators(c("maternal", "mortality"))

## -----------------------------------------------------------------------------
# ISO3 — regional vector passed straight through
sdg <- sdg_data(
  indicator = "3.4.1",
  area      = wpro_cty
)
sdg |> glimpse()

# M49 also works (e.g. when copy-pasting codes from sdg_areas())
sdg_data("3.4.1", area = c("608", "250"))

## -----------------------------------------------------------------------------
sdg_clean(sdg)

## -----------------------------------------------------------------------------
# Two indicators on the same topic from different APIs:
#   GHO NCDMORT3070 (probability of premature NCD mortality)
#   SDG 3.4.1       (mortality rate from NCDs)
gho_ncd <- gho_data("NCDMORT3070", area = wpro_cty) |> gho_clean()
sdg_ncd <- sdg_data("3.4.1",        area = wpro_cty) |> sdg_clean()
bind_indicators(gho_ncd, sdg_ncd) |> glimpse()

## -----------------------------------------------------------------------------
sdg_coverage("3.b.1", area = c("156", "608"))
#>   location series      year_min year_max n_obs
#> 1 156      SH_ACS_DTP3     2000     2023    24
#> 2 156      SH_ACS_HPV      2018     2023     6
#> 3 156      SH_ACS_MCV2     2000     2023    24
#> 4 156      SH_ACS_PCV3     2017     2023     7
#> 5 608      SH_ACS_DTP3     2000     2023    24
#> 6 608      SH_ACS_HPV      2017     2023     7
#> 7 608      SH_ACS_MCV2     2000     2023    24
#> 8 608      SH_ACS_PCV3     2014     2023    10

