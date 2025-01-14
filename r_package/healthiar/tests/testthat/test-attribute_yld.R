test_that("results correct ar yld with uncertainties in dw and duration", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  niph_noise_ha_input <-
    niph_noise_ha_excel |>
    dplyr::filter(!is.na(niph_noise_ha_excel$exposure_mean))

  expect_equal(
    object = healthiar::attribute_yld(
      approach_risk = "absolute_risk",
      exp_central = niph_noise_ha_input$exposure_mean,
      population = sum(niph_noise_ha_input$population_exposed_total),
      prop_pop_exp = niph_noise_ha_input$population_exposed_total/sum(niph_noise_ha_input$population_exposed_total),
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
      duration_central = 1, duration_lower = 0.1, duration_upper = 10,
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")) |>
      helper_extract_main_results(),
    expected = niph_noise_ha_excel |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(number)|>
      dplyr::pull() |>
      round() / 2 # With dw_central = 0.5 & duration_central = 1 the expected results are half of those we would obtain without dw & duration arguments
  )
})
