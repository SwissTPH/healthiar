## ----prepare, include=FALSE---------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(healthiar)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(knitr)
library(readxl)
library(remotes)

## Avoid using pacman here, as it causes error in installation if it's not installed already
# library(pacman)
# pacman::p_load(healthiar, tibble, dplyr, purrr, tidyr, stringr, knitr, readxl)

options(knitr.kable.max_rows = 10)
set.seed(1)


## ----install package, include=TRUE, echo=TRUE, eval=FALSE---------------------
# remotes::install_github(
#   repo = "best-cost/best-cost_WPs",
#   subdir = "/r_package/healthiar",
#   ref = "HEAD",
#   build_vignettes = TRUE)

## ----basic template, include=TRUE, echo=TRUE, eval=FALSE----------------------
# ## Pathway ID: copy here the pathway ID
# 
# ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
# # data <- ...
# 
# ## healthiar FUNCTION CALL
# results <- healthiar::attribute_health(
#   erf_shape = ,
#   rr_central = ,
#   rr_increment = ,
#   exp_central = ,
#   prop_pop_exp = ,
#   cutoff_central = ,
#   bhd_central = ,
#   geo_id_disaggregated = ,
#   geo_id_aggregated =
# )
# 
# ## RESULT(S) COMPARISON ASSESSMENT:
# ## List here the results of the comparison assessment you selected
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----extended template, include=TRUE, echo=TRUE, eval=FALSE-------------------
# testthat::test_that("results correct <pathway_id>", {
# 
# ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
# # data <- ...
# 
# testthat::expect_equal(
#   ## healthiar FUNCTION CALL
#   object =
#     healthiar::attribute_health(
#       erf_shape = ,
#       rr_central = ,
#       rr_increment = ,
#       exp_central = ,
#       prop_pop_exp = ,
#       cutoff_central = ,
#       bhd_central = ,
#       geo_id_disaggregated = ,
#       geo_id_aggregated =
#     )$health_main$impact_rounded,
#   ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#   expected =
#     c( )
# )
# })
# 
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----add test name basic, include=TRUE, echo=TRUE, eval=FALSE-----------------
# ## Pathway ID:
# ## |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|

## ----add test name extended, include=TRUE, echo=TRUE, eval=FALSE--------------
# testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|", { ... })

## ----add test details, include=TRUE, echo=TRUE, eval=FALSE--------------------
# ## ASSESSOR:
# ## Axel Luyten, Swiss TPH
# ## ASSESSMENT DETAILS:
# ## attribute DALYs from ischemic heart disease to road noise exposure
# ## INPUT DATA DETAILS:
# ## noise exposure data from NIPH; cutoff based on exposure data; baseline DALYs from GBD; relative risk from WHO (2003)
# 

## ----test with hard-coded inputs, include=TRUE, echo=TRUE, eval=FALSE---------
# ## healthiar FUNCTION CALL
# results <- healthiar::attribute_health(
#   erf_shape = "log_linear",
#   rr_central = 1.08,
#   rr_increment = 10,
#   exp_central = c(53.0, 57.5, 62.5, 67.5, 72.5, 77.5),
#   pop_exp = c(4268785, 387500, 286000, 191800, 72200, 7700), # OPTION A
#   cutoff_central = 53,
#   bhd_central = 85362.08,
#   geo_id_disaggregated = NULL,
#   geo_id_aggregated = NULL
# )
# 
# ## healthiar FUNCTION CALL
# results <- healthiar::attribute_health(
#   erf_shape = "log_linear",
#   rr_central = 1.08,
#   rr_increment = 10,
#   exp_central = c(53.0, 57.5, 62.5, 67.5, 72.5, 77.5),
#   prop_pop_exp = c( # OPTION B
#     0.818718312, 0.074319355, 0.054852478,
#     0.036785683, 0.013847374, 0.001476797
#     ),
#   cutoff_central = 53,
#   bhd_central = 85362.08,
#   geo_id_disaggregated = NULL,
#   geo_id_aggregated = NULL
# )

## ----inspect excel file, eval=FALSE, include=FALSE, eval=FALSE----------------
# # system.file("extdata", package = "healthiar") |> list.files()
# # test <- readxl::read_xlsx(path = system.file("extdata", "example_road_noise_niph.xlsx", package = "healthiar"), sheet = "Relative_risk_IHD_WHO_2003a")

## ----test with loaded inputs, include=TRUE, echo=TRUE, eval=FALSE-------------
# ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
# data_raw  <-  readxl::read_xlsx(
#   path = system.file("extdata", "example_road_noise_niph.xlsx", package = "healthiar"),
#   sheet = "Relative_risk_IHD_WHO_2003a")
# data  <- data_raw |>
#   dplyr::filter(!is.na(data_raw$exposure_mean))
# 
# ## healthiar FUNCTION CALL
# results <- healthiar::attribute_health(
#   erf_shape = "log_linear",
#   rr_central = 1.08,
#   rr_increment = 10,
#   exp_central = data$exposure_mean,
#   prop_pop_exp = data$prop_exposed,
#   cutoff_central = min(data$exposure_mean),
#   bhd_central = data$gbd_daly[1],
#   geo_id_disaggregated = NULL,
#   geo_id_aggregated = NULL
# )

## ----add results basic--------------------------------------------------------
## RESULT(S) COMPARISON ASSESSMENT:
## 1151 attributable DALYs

## ----add results extended, include=TRUE, echo=TRUE, eval=FALSE----------------
# # [...]
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object = ,
#     ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     expected =
#       data_raw |>
#       dplyr::filter(exposure_category %in% "Total exposed")|>
#       dplyr::select(daly)|>
#       dplyr::pull() |>
#       round()
# )
# # [...]

## ----compare results manually, include=TRUE, echo=TRUE, eval=TRUE-------------
## healthiar FUNCTION CALL
results <- healthiar::attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.08,
  rr_increment = 10,
  exp_central = c(53.0, 57.5, 62.5, 67.5, 72.5, 77.5),
  prop_pop_exp = c(0.818718312, 0.074319355, 0.054852478, 
                   0.036785683, 0.013847374, 0.001476797),
  cutoff_central = 53,
  bhd_central = 85362.08
)

results$health_main$impact_rounded |> 
  print()

## RESULT(S) COMPARISON ASSESSMENT:
## 1151 attributable DALYs

## ----compare results automatically, include=TRUE, echo=TRUE, eval=TRUE--------
testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|", {
  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        exp_central = c(53.0, 57.5, 62.5, 67.5, 72.5, 77.5),
        prop_pop_exp = c(0.818718312, 0.074319355, 0.054852478, 
                         0.036785683, 0.013847374, 0.001476797),
        cutoff_central = 53,
        bhd_central = 85362.08,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear"
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(1151)
  )
})

## ----filled out template basic, include=TRUE, echo=TRUE, eval=TRUE------------
## Pathway ID: |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|

## healthiar FUNCTION CALL
healthiar::attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.08,
  rr_increment = 10,
  exp_central = c(53.0, 57.5, 62.5, 67.5, 72.5, 77.5),
  prop_pop_exp = c(0.818718312, 0.074319355, 0.054852478, 
                   0.036785683, 0.013847374, 0.001476797),
  cutoff_central = 53,
  bhd_central = 85362.08
)$health_main$impact_rounded |> 
  print()

## RESULT(S) COMPARISON ASSESSMENT:
## 1151 attributable DALYs
## ASSESSOR: 
## Axel Luyten, Swiss TPH
## ASSESSMENT DETAILS: 
## attribute DALYs from ischemic heart disease to road noise exposure
## INPUT DATA DETAILS: 
## noise exposure data from NIPH; cutoff based on exposure data; baseline DALYs from GBD; relative risk from WHO (2003)

## ----filled out template extended, include=TRUE, echo=TRUE, eval=TRUE---------
testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data_raw  <-  readxl::read_xlsx(
    path = system.file("extdata", "example_road_noise_niph.xlsx", package = "healthiar"),
    sheet = "Relative_risk_IHD_WHO_2003a")
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))
  
  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        erf_shape = "log_linear",
        rr_central = 1.08,
        rr_increment = 10,
        exp_central = data$exposure_mean,
        prop_pop_exp = data$prop_exposed,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1]
        )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(daly)|>
      dplyr::pull() |>
      round()
  )
})

## ASSESSOR: 
## Axel Luyten, Swiss TPH
## ASSESSMENT DETAILS: 
## attribute DALYs from ischemic heart disease to road noise exposure
## INPUT DATA DETAILS: 
## noise exposure data from NIPH; cutoff based on exposure data; baseline DALYs from GBD; relative risk from WHO (2003)

## ----uncertainty example part 1, include=TRUE, echo=TRUE, eval=FALSE----------
# ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
# # data <- ...
# 
# ## First create an assessment using attribute_health() or compare()
# results_interim <- healthiar::attribute_health(
#   erf_shape = "log_linear",
#   rr_central = 1.08,
#   rr_lower = 1.08 - 0.02, # FAKE
#   rr_upper = 1.08 + 0.02,
#   rr_increment = 10,
#   exp_central = c(53.0, 57.5, 62.5, 67.5, 72.5, 77.5),
#   pop_exp = c(4268785, 387500, 286000, 191800, 72200, 7700), # OPTION A
#   cutoff_central = 5,
#   bhd_central = 85362.08
# )
# 
# ## healthiar::summarize_uncertainty() FUNCTION CALL
# results <- healthiar::summarize_uncertainty(
#   output_attribute = results_interim,
#   n_sim = 100 # Please take this value, to reduce time needed for all tests
# )

## ----uncertainty example part 2 extract results, eval=TRUE--------------------
results$uncertainty_main$impact_rounded
## 26676 (central impact estimates), 22183 (lower), 31407 (upper)

## ----uncertainty example part 3 fill out template, include=TRUE, echo=TRUE, eval=FALSE----
# 
# testthat::test_that("results correct [pathway_id]", {
# 
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   # data <- ...
# 
#   ## First create an assessment using attribute_health() or compare()
#   result_interim <- healthiar::attribute_health(
#     erf_shape = "log_linear",
#     rr_central = 1.08,
#     rr_lower = 1.08 - 0.02, # FAKE
#     rr_upper = 1.08 + 0.02,
#     rr_increment = 10,
#     exp_central = c(53.0, 57.5, 62.5, 67.5, 72.5, 77.5),
#     pop_exp = c(4268785, 387500, 286000, 191800, 72200, 7700), # OPTION A
#     cutoff_central = 5,
#     bhd_central = 85362.08)
# 
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object = healthiar::summarize_uncertainty(
#       output_attribute = results_interim,
#       n_sim = 100 # Please take this value, to reduce time needed for all tests
#     )$uncertainty_main$impact_rounded,
# 
#     ## RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     expected =
#       c(26676, 22183, 31407)
#     )
# })
# 
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----test uncertainty rr iteration, eval=FALSE, include=FALSE-----------------
# testthat::test_that("results correct [pathway_id]", {
# 
#   result_interim_iteration <-
#     healthiar::attribute_health(
#       exp_central = list(8, 7.5),
#       exp_lower = list(7, 6.2),
#       exp_upper = list(9, 8.1),
#       cutoff_central = 5,
#       cutoff_lower = 4,
#       cutoff_upper = 6,
#       bhd_central = list(1E5, 1E5),
#       bhd_lower = list(5E4, 5E4),
#       bhd_upper = list(2E5, 2E5),
#       rr_central = 1.118,
#       rr_lower = 1.060,
#       rr_upper = 1.179,
#       rr_increment = 10,
#       erf_shape = "log_linear",
#       geo_id_disaggregated = c("a", "b")
#     )
# 
#   testthat::expect_equal(
#     object =
#       healthiar::summarize_uncertainty(
#         output_attribute = result_interim_iteration,
#         n_sim = 100,
#         seed = 123
#       )$uncertainty_main$impact_rounded,
# 
#     expected = # Results on 2025-06-04; no comparison study
#       c(2591, 745, 7400, 2599, 537, 6566)
#   )
# })
# 
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template rr complete basic, include=TRUE, echo=TRUE, eval=FALSE----------
# ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
# # data <- ...
# 
# ## healthiar FUNCTION CALL
# healthiar::attribute_health(
#   approach_risk = "relative_risk",
#   erf_shape = ,
#   rr_central = ,
#   rr_lower = NULL,
#   rr_upper = NULL,
#   rr_increment = ,
#   exp_central = ,
#   exp_lower = NULL,
#   exp_upper = NULL,
#   cutoff_central = ,
#   cutoff_lower = NULL,
#   cutoff_upper = NULL,
#   bhd_central = ,
#   bhd_lower = NULL,
#   bhd_upper = NULL,
#   geo_id_disaggregated = NULL,
#   geo_id_aggregated = NULL
# )
# 
# ## RESULT(S) COMPARISON ASSESSMENT:
# ## List here the results of the comparison assessment you selected
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template rr complete extended, include=TRUE, echo=TRUE, eval=FALSE-------
# testthat::test_that("results correct [pathway_id]", {
# 
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   # data <- ...
# 
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object =
#       healthiar::attribute_health(
#         approach_risk = "relative_risk",
#         erf_shape = ,
#         rr_central = ,
#         rr_lower = NULL,
#         rr_upper = NULL,
#         rr_increment = ,
#         exp_central = ,
#         exp_lower = NULL,
#         exp_upper = NULL,
#         cutoff_central = ,
#         cutoff_lower = NULL,
#         cutoff_upper = NULL,
#         bhd_central = ,
#         bhd_lower = NULL,
#         bhd_upper = NULL,
#         geo_id_disaggregated = NULL,
#         geo_id_aggregated = NULL
#       )$health_main$impact_rounded,
#     ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     expected =
#       c( )
#     )
# })
# 
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template ar complete basic, include=TRUE, echo=TRUE, eval=FALSE----------
# ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
# # data <- ...
# 
# healthiar::attribute_health(
#   approach_risk = "absolute_risk",
#   erf_eq_central = ,
#   erf_eq_lower = NULL,
#   erf_eq_upper = NULL,
#   exp_central = ,
#   exp_lower = NULL,
#   exp_upper = NULL,
#   pop_exp = ,
#   geo_id_disaggregated = NULL,
#   geo_id_aggregated = NULL
# )
# 
# ## RESULT(S) COMPARISON ASSESSMENT:
# ## List here the results of the comparison assessment you selected
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template ar complete extended, include=TRUE, echo=TRUE, eval=FALSE-------
# testthat::test_that("results correct [pathway_id]", {
# 
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   # data <- ...
# 
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object =
#       healthiar::attribute_health(
#         approach_risk = "absolute_risk",
#         erf_eq_central = ,
#         erf_eq_lower = NULL,
#         erf_eq_upper = NULL,
#         exp_central = ,
#         exp_lower = NULL,
#         exp_upper = NULL,
#         pop_exp = ,
#         geo_id_disaggregated = NULL,
#         geo_id_aggregated = NULL
#       )$health_main$impact_rounded,
#     ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     expected =
#       c( )
#     )
# })
# 
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template monetization complete basic, include=TRUE, echo=TRUE, eval=FALSE----
# ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
# # data <- ...
# 
# ## healthiar FUNCTION CALL
# healthiar::monetize(
#   output_attribute = NULL, # Only specify one of either "output_attribute" or "impact" argument
#   impact = , # Only specify one of either "output_attribute" or "impact" argument
#   valuation = ,
#   cost = ,
#   discount_rate = NULL,
#   discount_shape = NULL,
#   discount_years = 1,
#   inflation = NULL
# )
# 
# ## RESULT(S) COMPARISON ASSESSMENT:
# ## List here the results of the comparison assessment you selected
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template monetization complete extended, include=TRUE, echo=TRUE, eval=FALSE----
# testthat::test_that("results correct [pathway_id]", {
# 
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   # data <- ...
# 
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object =
#       healthiar::monetize(
#         output_attribute = NULL, # Only specify one of either "output_attribute" or "impact" argument
#         impact = , # Only specify one of either "output_attribute" or "impact" argument
#         valuation = ,
#         cost = ,
#         discount_rate = NULL,
#         discount_shape = NULL,
#         discount_years = 1,
#         inflation = NULL
#         )$monetization_main$monetized_impact,
#     ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     expected =
#       c( )
#     )
# })
# 
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template cba complete basic, include=TRUE, echo=TRUE, eval=FALSE---------
# 
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   # data <- ...
# 
#     ## healthiar FUNCTION CALL
#     object =
#       healthiar::cba(
#         output_attribute = NULL,
#         positive_impact = ,
#         valuation = ,
#         cost = ,
#         discount_shape = ,
#         discount_rate_benefit = ,
#         discount_rate_cost = ,
#         discount_years_benefit = 1,
#         discount_years_cost = 1)
# 
# ## RESULT(S) COMPARISON ASSESSMENT:
# ## List here the results of the comparison assessment you selected
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template cba complete extended, include=TRUE, echo=TRUE, eval=FALSE------
# testthat::test_that("results correct [pathway_id]", {
# 
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   # data <- ...
# 
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object =
#       healthiar::cba(
#         output_attribute = NULL,
#         positive_impact = ,
#         valuation = ,
#         cost = ,
#         discount_shape = ,
#         discount_rate_benefit = ,
#         discount_rate_cost = ,
#         discount_years_benefit = 1,
#         discount_years_cost = 1)$cba_main$net_benefit_rounded,
#     ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     expected =
#       c( )
#     )
# })
# 
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template social analysis basic, include=TRUE, echo=TRUE, eval=FALSE------
# ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
# # data <- ...
# 
# ## First run an iteration (see iteration template for full argument list)
# result_interim <- healthiar::attribute_health(
#   exp_central = as.list( ),
#   bhd_central = as.list( ),
#   cutoff_central = ,
#   rr_central = ,
#   erf_shape = ,
#   rr_increment = ,
#   geo_id_disaggregated =
# )
# 
# ## healthiar FUNCTION CALL
# healthiar::socialize(
#   output_attribute = result_interim,
#   geo_id_disaggregated = , # geo IDs of the preparatory iteration call above and this function call must match!
#   social_indicator = ,
#   n_quantile = , # Specify number of quantiles, e.g. 10
#   approach = "quantile" # default (and currently only) approach
# )
# 
# ## RESULT(S) COMPARISON ASSESSMENT:
# ## List here the results of the comparison assessment you selected
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

## ----template social analysis extended, include=TRUE, echo=TRUE, eval=FALSE----
# 
# testthat::test_that("results correct [pathway_id]", {
# 
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   # data <- ...
# 
#   ## First run an iteration (see iteration template for full argument list)
#   result_interim <- healthiar::attribute_health(
#     exp_central = as.list( ),
#     bhd_central = as.list( ),
#     cutoff_central = ,
#     rr_central = ,
#     erf_shape = ,
#     rr_increment = ,
#     geo_id_disaggregated =
#   )
# 
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object =
#         healthiar::socialize(
#           output_attribute = result_interim,
#           geo_id_disaggregated = , # geo IDs of the preparatory iteration call above and this function call must match!
#           social_indicator = ,
#           n_quantile = , # Specify number of quantiles, e.g. 10
#           approach = "quantile" # default (and currently only) approach
#         ) |>
#       purrr::pluck("social_main") |>
#       dplyr::filter(
#         difference_type == "absolute" &
#           difference_compared_with == "overall")  |>
#       dplyr::select(difference_value) |>
#       base::unlist() |>
#       base::as.numeric(),
# 
#     ## RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     expected =
#       c( )
#     )
# })
# 
# ## ASSESSOR:
# ## Add here your name and your institute abbreviation
# ## ASSESSMENT DETAILS:
# ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
# ## INPUT DATA DETAILS:
# ## Add here input data details: data sources, measured vs. modelled, ...

