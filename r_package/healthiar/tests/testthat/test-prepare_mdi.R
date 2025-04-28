## COMMENTED OUT SO THAT FUNCTION OUTPUT DOESN'T SHOW UP IN TESTING OVERVIEW
# testthat::test_that("results correct", {
#
#   data <- base::readRDS(testthat::test_path("data", "exdat_prepare_mdi.rds"))
#
#   testthat::expect_equal(
#     object =
#       healthiar::prepare_mdi(
#         geo_id_disaggregated = exdat_prepare_mdi$id,
#         edu = exdat_prepare_mdi$edu,
#         unemployed = exdat_prepare_mdi$unemployed,
#         single_parent = exdat_prepare_mdi$single_parent,
#         pop_change = exdat_prepare_mdi$pop_change,
#         no_heating = exdat_prepare_mdi$no_heating,
#         n_quantile = 10)$MDI[1:50] |> round(digits = 7), # Extract the MDI of the first 50 geo units
#     expect = c(0.2117721, 0.4319924, 0.1847750, 0.3787937, 0.3121354, 0.2565185, 0.2245822,
#                0.2140148, 0.2656597, 0.3566141, 0.1746261, 0.2108900, 0.2221277, 0.2226641,
#                0.2478800, 0.2513628, 0.3595167, 0.2321278, 0.2879502, 0.2962355, 0.1866685,
#                0.3168898, 0.2928375, 0.2658067, 0.2646959, 0.2306425, 0.2273271, 0.1892787,
#                0.3941645, 0.2268015, 0.2973753, 0.1853536, 0.2850227, 0.2642978, 0.2867885,
#                0.2775215, 0.3538885, 0.2835787, 0.2554690, 0.2423936, 0.3548941, 0.2564889,
#                0.3041343, 0.1991622, 0.2355921, 0.2671123, 0.2708377, 0.2348108, 0.2902528,
#                0.3112479) # Results on 2025-04-15; Sciensano results
#   )
# })
