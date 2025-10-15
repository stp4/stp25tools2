library(testthat)
library(tibble)

# Testdaten
DF <- structure(list(
  q2.almdd = structure(c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                       label = "Alm Dudler"),
  q2.minrl = structure(c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
                       label = "Mineral"),
  q2.cola = structure(c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
                      label = "Cola"),
  q2.bier = structure(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
                      label = "Bier"),
  q2.wein = structure(c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                      label = "Wein")),
  row.names = c(NA, 15L),
  class = "data.frame")

DF$sex <- factor(c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2), 1:2, c("f", "m"))



test_that("Summarise_multi works with wide format", {
  result <- DF |> Summarise_multi(~ q2.almdd + q2.minrl + q2.cola + q2.bier + q2.wein, type.wide = TRUE)
  result$Item<- as.character( result$Item)
  # Erwartete Werte
  expected <- tibble::tribble(
    ~Item,        ~"TRUE", ~"FALSE",
    "Alm Dudler", 13,    2,
    "Mineral",    8,     7,
    "Cola",       10,    5,
    "Bier",       4,     11,
    "Wein",       2,     13
  )

  # Überprüfe ob die Werte übereinstimmen
  expect_equal(result, as.data.frame(expected), ignore_attr = TRUE)

})

test_that("Summarise_multi works with long format", {
  result <- DF |> Summarise_multi(~ q2.almdd + q2.minrl + q2.cola + q2.bier + q2.wein, type.wide = FALSE)

  # Überprüfe die Struktur
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("Item", "levels", "Freq"))
  expect_equal(nrow(result), 10)
})

test_that("Summarise_multi works with grouping and by variable", {
  result <- DF |>
    Summarise_multi("Soda (gas)", q2.almdd, q2.minrl, q2.cola, "Alkohol", q2.bier, q2.wein, by = ~sex)

  # Überprüfe die Struktur
  expect_s3_class(result, "tbl_df")
  expect_named(result, c(".grouping", "sex", "Item", "levels", "Freq"))
  expect_equal(nrow(result), 20)

  # Überprüfe spezifische Werte
  expect_equal(result$Freq[result$Item == "Alm Dudler" & result$sex == "f" & result$levels == "TRUE"], 5)
  expect_equal(result$Freq[result$Item == "Wein" & result$sex == "m" & result$levels == "TRUE"], 0)
})

test_that("Tbll_multi works with variable input", {
  result <- DF |>
    Tbll_multi("Soda (gas)", q2.almdd, q2.minrl, q2.cola, "Alkohol", q2.bier, q2.wein, by = ~sex)

  # Überprüfe die Struktur
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(ncol(result) >= 3)
})

test_that("Tbll_multi works with formula input", {
  result <- DF |> Tbll_multi("Soda (gas)" + q2.almdd + q2.minrl + q2.cola +
                               "Alkohol" + q2.bier + q2.wein ~ sex)

  # Überprüfe die Struktur
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(ncol(result) >= 3)
})

test_that("tbll_likert attribute is correctly set", {
  result <- DF |>
    Tbll_multi("Soda (gas)", q2.almdd, q2.minrl, q2.cola, "Alkohol", q2.bier, q2.wein, by = ~sex)

  # Überprüfe das Attribut
  attr_result <- attr(result, "tbll_likert")
  expect_s3_class(attr_result, "data.frame")
  expect_named(attr_result, c(".grouping", "sex", "Item", "TRUE", "FALSE"))
  expect_equal(nrow(attr_result), 10)
})

test_that("Both Tbll_multi methods produce same tbll_likert attribute", {
  result1 <- DF |>
    Tbll_multi("Soda (gas)", q2.almdd, q2.minrl, q2.cola, "Alkohol", q2.bier, q2.wein, by = ~sex)

  result2 <- DF |>
    Tbll_multi("Soda (gas)" + q2.almdd + q2.minrl + q2.cola +
                 "Alkohol" + q2.bier + q2.wein ~ sex)

  # Überprüfe ob die Attribute identisch sind
  attr1 <- attr(result1, "tbll_likert")
  attr2 <- attr(result2, "tbll_likert")

  expect_equal(attr1, attr2, ignore_attr = TRUE)

})

# test_that("Error handling works correctly", {
#   # Test mit nicht existierenden Variablen
#   expect_error(DF |> aggregat_likert(~ non_existent_var))
#
#   # Test mit ungültiger Formel
#   expect_error(DF |> aggregat_likert(invalid ~ formula))
# })

# Zusätzliche Tests für Randfälle
test_that("Edge cases are handled", {
  # Test mit leerem Dataframe
  empty_df <- DF[0, ]
  expect_error(empty_df |> aggregat_likert(~ q2.almdd))

  # Test mit nur einer Variable
  result <- DF |> Summarise_multi(~ q2.almdd, type.wide = TRUE)
  expect_equal(nrow(result), 1)
  expect_named(result, c("Item", "TRUE", "FALSE"))
})
