## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 6.5,
  fig.height = 4,
  fig.align  = "center"
)

## ----setup, message = FALSE---------------------------------------------------
library(DSIR)
library(dplyr)
library(ggplot2)

## ----eval = FALSE-------------------------------------------------------------
# ncd <- gho_data("NCDMORT3070", spatial_type = "country",
#                 area = c("PHL", "VNM", "KHM", "MNG", "FJI", "LAO")) |>
#   gho_clean()

## ----simulate-----------------------------------------------------------------
set.seed(416)

countries <- who_countries |>
  filter(iso3 %in% c("PHL", "VNM", "KHM", "MNG", "FJI", "LAO")) |>
  select(iso3, location_name = name_short)

grid <- expand.grid(
  iso3 = countries$iso3,
  year = 2000:2023,
  dim1 = c("SEX_BTSX", "SEX_MLE", "SEX_FMLE"),
  stringsAsFactors = FALSE
) |>
  as_tibble()

base_rate <- c(PHL = 30, VNM = 26, KHM = 28, MNG = 34, FJI = 31, LAO = 29)
decline   <- c(PHL = 0.010, VNM = 0.016, KHM = 0.012,
               MNG = 0.006, FJI = -0.002, LAO = 0.011)
sex_shift <- c(SEX_BTSX = 1, SEX_MLE = 1.25, SEX_FMLE = 0.8)

ncd <- grid |>
  left_join(countries, by = "iso3") |>
  mutate(
    value_num = base_rate[iso3] * (1 - decline[iso3])^(year - 2000) *
      sex_shift[dim1] * exp(rnorm(n(), sd = 0.01)),
    low       = value_num * 0.93,
    high      = value_num * 1.07,
    source    = "gho",
    id        = "NCDMORT3070",
    indicator = "Probability of premature death from NCDs (%)",
    location  = iso3,
    year      = as.integer(year),
    value     = as.character(round(value_num, 1)),
    series    = NA_character_,
    dim2      = NA_character_,
    dim3      = NA_character_
  ) |>
  select(source, id, indicator, location, iso3, location_name, year,
         value, value_num, low, high, series, dim1, dim2, dim3)

ncd

## ----ribbon, fig.height = 4.5-------------------------------------------------
ncd |>
  filter(dim1 == "SEX_BTSX") |>
  ggplot(aes(year, value_num)) +
  geom_ribbon(aes(ymin = low, ymax = high), fill = "#0093D5", alpha = 0.2) +
  geom_line(color = "#0093D5", linewidth = 0.7) +
  facet_wrap(~ location_name) +
  labs(
    title    = "Premature NCD mortality is declining in most countries",
    subtitle = "Probability of premature death from NCDs, both sexes, 2000-2023",
    x = NULL, y = "%",
    caption  = "Simulated data for illustration."
  ) +
  theme_dsi_facet()

## ----dim-breakdown, fig.height = 4.5------------------------------------------
ncd |>
  filter(dim1 %in% c("SEX_MLE", "SEX_FMLE")) |>
  ggplot(aes(year, value_num, color = dim1)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ location_name) +
  scale_color_viridis_d(end = 0.6,
                        labels = c(SEX_FMLE = "Female", SEX_MLE = "Male")) +
  labs(
    title    = "Men die prematurely from NCDs more often than women",
    x = NULL, y = "%", color = NULL,
    caption  = "Simulated data for illustration."
  ) +
  theme_dsi_facet()

## ----forest-------------------------------------------------------------------
latest <- ncd |>
  filter(dim1 == "SEX_BTSX") |>
  slice_max(year, by = iso3)

ggplot(latest, aes(value_num, reorder(location_name, value_num))) +
  geom_pointrange(aes(xmin = low, xmax = high),
                  color = "#0093D5", linewidth = 0.7) +
  labs(
    title   = "Premature NCD mortality, 2023",
    x = "%", y = NULL,
    caption = "Point estimate and 95% interval. Simulated data."
  ) +
  theme_dsi()

## ----dumbbell-----------------------------------------------------------------
start <- ncd |>
  filter(dim1 == "SEX_BTSX", year == 2000) |>
  select(location_name, start = value_num)
end <- ncd |>
  filter(dim1 == "SEX_BTSX", year == 2023) |>
  select(location_name, end = value_num)

dumbbell <- start |>
  left_join(end, by = "location_name") |>
  mutate(
    direction     = ifelse(end < start, "Decline", "Increase"),
    location_name = reorder(location_name, start)
  )

ggplot(dumbbell) +
  geom_segment(aes(x = start, xend = end,
                   y = location_name, yend = location_name,
                   color = direction),
               linewidth = 1.5) +
  geom_point(aes(start, location_name), size = 3, color = "grey65") +
  geom_point(aes(end, location_name, color = direction), size = 3) +
  scale_color_manual(values = c(Decline = "#0072B2", Increase = "#D55E00")) +
  labs(
    title   = "Change in premature NCD mortality, 2000 to 2023",
    x = "%", y = NULL, color = NULL,
    caption = "Grey dot = 2000, colored dot = 2023. Simulated data."
  ) +
  theme_dsi()

## ----aarr---------------------------------------------------------------------
progress <- ncd |>
  filter(dim1 == "SEX_BTSX") |>
  summarise(aarr = aarr(year, value_num), .by = c(iso3, location_name))

progress |>
  mutate(aarr_pct = round(100 * aarr, 1)) |>
  arrange(desc(aarr))

## ----projection---------------------------------------------------------------
latest |>
  left_join(progress, by = c("iso3", "location_name")) |>
  mutate(projected_2030 = value_num * (1 - aarr)^(2030 - year)) |>
  select(location_name, value_2023 = value_num, aarr, projected_2030) |>
  arrange(projected_2030)

