

library(testthat)
library(car)  # Für Prestige Dataset

test_that("Tbll_effect works correctly", {
  # Setup
  model <- lm(prestige ~ type * education + income + women, data = Prestige)

  # Test 1: Alle Effekte ohne zusätzliche Parameter
  expect_silent(result1 <- Tbll_effect(model))
  expect_type(result1, "list")
  expect_true(length(result1) > 0)
  expect_true(all(sapply(result1, is.data.frame)))

  # Test 2: Mit Fallzahlen
  result2 <- Tbll_effect(model, include.n = TRUE, digits = 0)
  expect_true("n" %in% names(result2[[1]]))
  expect_true(is.numeric(result2[[1]]$n) | is.integer(result2[[1]]$n))

  # Test 3: Mit Konfidenzintervallen
  result3 <- Tbll_effect(model, include.ci = TRUE, digits = 2)
  expect_true("CI" %in% names(result3[[1]]))
  expect_true(is.character(result3[[1]]$CI))

  # Test 4: Mit Standardfehlern
  result4 <- Tbll_effect(model, include.se = TRUE, digits = 2)
  expect_true("SE" %in% names(result4[[1]]))
  expect_true(is.character(result4[[1]]$SE))

  # Test 5: Spezifischer Term als String
  result5 <- Tbll_effect(model, term = "type * education", digits = 0)
  expect_equal(ncol(result5), 4)
  expect_true(all(c("type", "education") %in% names(result5)))

  # Test 6: Spezifischer Term als Formula
  result6 <- Tbll_effect(model, term = ~ type * education, digits = 0)
  expect_equal(ncol(result6), 4)
  expect_true(all(c("type", "education") %in% names(result6)))

  # Test 7: Mehrere Terms per Formula
  result7 <- Tbll_effect(model, term = ~ type * education + women, digits = 0)
  expect_equal(length(result7), 2)
  expect_true("women" %in% names(result7[[2]]))

  # Test 8: Mehrere Terms als Character-Vektor
  result8 <- Tbll_effect(model, term = c("type", "education"), digits = 0)
  expect_equal(length(result8), 2)
  expect_true("type" %in% names(result8[[1]]))
  expect_true("education" %in% names(result8[[2]]))

  # Test 9: Digits-Parameter funktioniert
  result9_0 <- Tbll_effect(model, term = "income", digits = 0)
  result9_2 <- Tbll_effect(model, term = "income", digits = 2)


})



test_that("Tbll_effect output structure", {
  model <- lm(prestige ~ type * education + income + women, data = Prestige)

  result <- Tbll_effect(model, term = "income", include.n = TRUE,
                        include.ci = TRUE, include.se = TRUE, digits = 2)

  # Test 1: Ergebnis ist eine Liste
  expect_type(result, "list")

  # Test 2: Jedes Listenelement ist ein Data Frame
  #expect_true(all(sapply(result, is.data.frame)))

  # Test 3: Enthält alle angeforderten Spalten

  expect_true("Avarage" %in% names(result))
  expect_true("CI" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("SE" %in% names(result))


})



test_that("Tbll_effect returns expected values", {
  model <- lm(prestige ~ type * education + income + women, data = Prestige)

  result <- Tbll_effect(model, term = "income", include.n = TRUE, digits = 0)
  expect_equal(result,
    structure(
    list(
      income = c(2000, 8000, 10000, 20000, 30000),
      n = c(35L, 48L, 11L, 2L, 2L),
      Avarage = c("43", "50", "52", "62", "73"),
      CI = c("[39,  47]", "[46,  53]", "[48,  55]", "[55,  70]", "[60,  86]")
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c("1", "2", "3", "4", "5"),
    caption = "Regression Models Response:  prestige ",
    note = "",
    labels = NA
  )
)





})

# # Performance Test für große Modelle
# test_that("Tbll_effect performance", {
#   # Einfaches Modell sollte schnell sein
#   model <- lm(prestige ~ type + education, data = Prestige)
#
#   expect_silent(
#     system.time(Tbll_effect(model), gcFirst = TRUE)["elapsed"] < 2
#   )
# })
