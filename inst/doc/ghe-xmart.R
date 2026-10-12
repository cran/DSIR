## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')
library(DSIR)

## ----eval=FALSE---------------------------------------------------------------
# gho_indicators('mortality', backend = 'xmart')
# gho_dimensions('NCDMORT3070', 'DIM_SEX', backend = 'xmart')
# ncd <- gho_data('NCDMORT3070', area = 'PHL', year_from = 2020,
#                 year_to = 2021, dimensions = list(DIM_SEX = 'TOTAL'),
#                 backend = 'xmart')
# gho_clean(ncd, keep_dimensions = TRUE, keep_metadata = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# financial <- gho_data('FINANCIALHARDSHIP_PROPORTIONOFPOP', area = 'PHL',
#                        backend = 'legacy')
# gho_clean(financial, keep_dimensions = TRUE, keep_metadata = TRUE)
# 
# # Optional session default; explicit arguments still take precedence.
# options(DSIR.who_backend = 'xmart')
# gho_indicators('mortality', backend = 'legacy')

## ----eval=FALSE---------------------------------------------------------------
# ghe_dimensions('year')
# ghe_dimensions('area')
# ghe_dimensions('sex')
# ghe_dimensions('age')
# ghe_dimensions('measure')
# ghe_causes('diabetes')
# ghe_causes('stroke')

## ----eval=FALSE---------------------------------------------------------------
# phl_deaths <- ghe_data('PHL', year = 2023, age = 'TOTAL', sex = 'TOTAL',
#                        cause = 0, measure = 'deaths')
# phl_rates <- ghe_data('PHL', year = 2023, age = 'Y40T44',
#                       sex = c('FEMALE', 'MALE'), cause = 0,
#                       measure = 'death_rate')
# ghe_coverage('PHL', age = 'TOTAL', sex = 'TOTAL', cause = 0)
# ghe_clean(phl_rates, keep_dimensions = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# phl <- snapshot(
#   ghe_data('PHL', year = 2023, age = 'TOTAL', sex = 'TOTAL', cause = 0,
#             measure = 'deaths'),
#   'data/phl_ghe_2023.rds'
# )
# attr(phl, 'who_provenance')

## -----------------------------------------------------------------------------
who_std_pop
count <- seq_len(nrow(who_std_pop))
population <- rep(10000, nrow(who_std_pop))
age_standardize(count, population, who_std_pop$weight)

