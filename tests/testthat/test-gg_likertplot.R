#library(testthat)
library(ggplot2)




test_data <- structure(
  list(cut = structure(
c(1L, 2L, 3L, 4L, 5L, 1L, 2L,
3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L,
4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L,
5L), levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"
), class = "factor"),
clarity = structure(c(1L, 1L, 1L, 1L, 1L,
2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 5L,
5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 8L, 8L,
8L, 8L, 8L), levels = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2",
"VVS1", "IF"), class = "factor"), Freq = c(210L, 96L, 84L, 205L,
146L, 466L, 1081L, 2100L, 2949L, 2598L, 408L, 1560L, 3240L, 3575L,
4282L, 261L, 978L, 2591L, 3357L, 5071L, 170L, 648L, 1775L, 1989L,
3589L, 69L, 286L, 1235L, 870L, 2606L, 17L, 186L, 789L, 616L,
2047L, 9L, 71L, 268L, 230L, 1212L),
.grouping = c("A", "A", "A",
"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
"A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B",
"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B")),
class = "data.frame", row.names = c(NA, -40L))

#
# p<- gg_likertplot(
#   test_data,
#   x = clarity,
#   fill = cut,
#   weight = Freq,
#   include.percent = TRUE,
#   include.order = "l",
#   include.reference = 2.5,
#   facet_formula = .grouping ~ .)

#
test_that("gg_likertplot basic tests", {
  # Test: Funktion läuft
  p <- gg_likertplot(
    test_data,
    x = clarity,
    fill = cut,
    weight = Freq,
    include.percent = TRUE,
    include.order = "l",
    include.reference = 2.5,
    facet_formula = .grouping ~ .
  )

  # Test: Gibt ggplot zurück
  expect_s3_class(p, "ggplot")

 # expect_equal(
 #   names(p),
  #  c("data", "layers", "scales", "guides", "mapping", "theme", "coordinates",
  #    "facet", "plot_env", "layout", "labels")
 # )

  expect_equal(  names(p$data),
                 c("cut", "clarity", "Freq", ".grouping", "mean_weight")
  )



  # layer <- p$layers[[1]]
  # expect_equal(
  #   names(layer),
  #   c(
  #     "mapping",
  #     "geom_params",
  #     "show.legend",
  #     "constructor",
  #     "stat_params",
  #     "stat",
  #     "inherit.aes",
  #     "geom",
  #     "position",
  #     "super",
  #     "data",
  #     "aes_params"
  #   )
  # )

})

test_that("gg_stacked basic tests", {
 DF <- tibble::tibble(
  id = 1:6,
  .grouping = rep("Domain A", 6),
  Sex = rep(c("female","male"), 3),
  Item = factor(rep(c("Fruit", "Vegetables", "Milk"), each = 2)),
  levels = rep(c(TRUE, FALSE), 3),
  Freq = c(20,10, 15,25, 30,12)
)

p <- gg_stacked(DF)
# Test: Gibt ggplot zurück
expect_s3_class(p, "ggplot")

# expect_equal(
#   names(p),
#   c("data", "layers", "scales", "guides", "mapping", "theme", "coordinates",
#     "facet", "plot_env", "layout", "labels")
# )




})




