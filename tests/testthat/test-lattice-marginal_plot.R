test_that("marginal_plot produces correct legend labels", {
  enviro <- lattice::environmental

  enviro <- transform(
    enviro,
    temperature = (temperature - 32) / 1.8,
    smell = cut(
      ozone,
      breaks = c(0, 30, 50, Inf),
      labels = c("ok", "hmmm", "yuck"),
      ordered = TRUE
    ),
    is.windy = factor(
      wind > 10,
      levels = c(TRUE, FALSE),
      labels = c("windy", "calm")
    ),
    wind = wind * 0.44704
  ) |> Label(
    ozone    = "Ozon [ppm]",
    radiation = "Radiation",
    is.windy  = "is Windy",
    wind      = "Wind speed [m/s]",
    smell     = "Smell"
  )

  p1 <- marginal_plot(
    enviro,
    ozone, radiation, is.windy, wind, smell,
    by = ~ temperature,
    strip.names = c("Ozon [ppm]", "Radiation", "is Windy", "Wind speed [m/s]", "Smell"),
    main = "Distribution",
    cut.breaks = c(-Inf, 20, 25, 30, Inf),
    cut.labels = c("bis 20°C", "20°C bis 25°C", "25°C bis 30°C", "über 30°C"),
    auto.key   = list(lines = TRUE, title = "Temperatur")
  )

  expect_s3_class(p1, "trellis")
  expect_equal(
    p1$legend$right$args$text,
    c("bis 20°C", "20°C bis 25°C", "25°C bis 30°C", "über 30°C")
  )
})
