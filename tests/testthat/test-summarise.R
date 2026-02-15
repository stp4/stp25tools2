library(testthat)
library(tibble)

# Testdaten erstellen
df <- data.frame(
  month = rep(1:3, 2),
  student = rep(c("Amy", "Bob"), each = 3),
  A = c(9, 7.1, 6.2, 8, 6.2, 9.4),
  B = c(6, 7.3, 8, 5, 6.9, 7)
)

# Hilfsfunktion für Vergleich von Data Frames (ignoriert Zeilennamen und kleine numerische Unterschiede)
expect_equal_df <- function(object, expected, tolerance = 0.1) {
  expect_equal(dim(object), dim(expected))
  expect_equal(names(object), names(expected))
  
  for (col in names(object)) {
    if (is.numeric(object[[col]])) {
      expect_equal(object[[col]], expected[[col]], tolerance = tolerance)
    } else {
      expect_equal(object[[col]], expected[[col]])
    }
  }
}

test_that("Summarise basic functionality works", {
  # Test 1: Formelsyntax mit median
  result1 <- Summarise(A + B ~ student, df, fun = median)
  expected1 <- data.frame(
    student = c("Amy", "Amy", "Bob", "Bob"),
    variable = factor(c("A", "B", "A", "B")),
    value = c(7.1, 7.3, 8.0, 6.9)
  )
  expect_equal_df(result1, expected1)
  
  # Test 2: Variablennamen-Syntax mit median
  result2 <- Summarise(df, A, B, by = ~ student,fun = median)
  expect_equal_df(result2, expected1)
})

test_that("Summarise works with multiple statistics", {
  # Test 3: Formelsyntax mit mean und sd
  result3 <- Summarise(A + B ~ student, df,
                       fun = function(x) render_f(
                         c(mean = mean(x), sd = sd(x)),
                         digits = 2
                       ))
  expected3 <- data.frame(
    student = c("Amy", "Amy", "Bob", "Bob"),
    variable = factor(c("A", "B", "A", "B")),
    mean = c("7.43", "7.10", "7.87", "6.30"),
    sd = c("1.43", "1.01", "1.60", "1.13")
  )
  expect_equal_df(result3, expected3, tolerance = 0.01)
  
  # Test 4: Variablennamen-Syntax mit mean und sd
  result4 <- Summarise(df, A, B, by = ~ student, 
                       fun = function(x) render_f(
                         c(mean = mean(x), sd = sd(x)),
                         digits = 2
                       ))
  expect_equal_df(result4, expected3, tolerance = 0.01)
})

test_that("Summarise works with totals and wide format", {
  # Test 5: Mit Total und Wide-Format
  result5 <- Summarise(
    A + B ~ student,
    df,
    fun = function(x) render_f(c(mean = mean(x), sd = sd(x)), digits = 2),
    include.total = TRUE,
    key = "Note",
    formula = student ~ Note
  )
  
  # Erwartete Ausgabe als tibble
  expected5 <- tibble::tibble(
    student = c("Amy", "Bob", "Total"),
    mean_A = c("7.43", "7.87", "7.65"),
    sd_A = c("1.43", "1.60", "1.38"),
    mean_B = c("7.10", "6.30", "6.70"),
    sd_B = c("1.01", "1.13", "1.05")
  )
  
  expect_s3_class(result5, "tbl_df")
  expect_equal(nrow(result5), 3)
  expect_equal(ncol(result5), 5)
  expect_equal(result5$student, expected5$student)
})

test_that("Summarise works with margins = TRUE", {
  # Test 6: Mit margins = TRUE
  result6 <- Summarise(
    A + B ~ student,
    df,
    fun = function(x) render_f(c(mean = mean(x), sd = sd(x)), digits = 2),
    include.total = TRUE,
    key = "Note",
    formula = student ~ Note,
    margin = TRUE
  )
  
  expect_s3_class(result6, "tbl_df")
  expect_equal(nrow(result6), 3)
  expect_equal(ncol(result6), 5)
})

test_that("Summarise works with include.total as formula", {
  # Test 7: Mit include.total als Formel
  result7 <- Summarise(
    A + B ~ student,
    df,
    fun = function(x) render_f(c(mean = mean(x), sd = sd(x)), digits = 2),
    include.total = value ~ 1,
    key = "Note"
  )
  
  expect_s3_class(result7, "data.frame")
  # Überprüfe, dass Total-Zeilen vorhanden sind
  expect_true("Total" %in% result7$student)
})

test_that("Summarise handles edge cases", {
  # Test 8: Leere Eingabe sollte Fehler werfen
  expect_error(Summarise())
  
  # Test 9: Ungültige Formel sollte Fehler werfen
  expect_error(Summarise(invalid ~ formula, df))
  
  # Test 10: Mit Standard-Funktion (count non-missing)
  result10 <- Summarise(A ~ student, df)
  expect_s3_class(result10, "data.frame")
  expect_true("value" %in% names(result10))
})

test_that("Summarise preserves variable labels when include.label = TRUE", {
  # Test 11: Mit Labels
  result11 <- Summarise(A + B ~ student, df, include.label = TRUE)
  expect_s3_class(result11, "data.frame")
})

test_that("Summarise works with different na.action settings", {
  # Test 12: Mit verschiedenen na.action Einstellungen
  df_na <- df
  df_na$A[1] <- NA

  result12_na_pass <- Summarise(A ~ student, df_na, fun=median, na.action = na.pass)
  result12_na_omit <- Summarise(A ~ student, df_na, fun=median, na.action = na.omit)

  expect_s3_class(result12_na_pass, "data.frame")
  expect_s3_class(result12_na_omit, "data.frame")
  expect_equal(result12_na_pass$value, c(NA,  8))
  expect_equal(result12_na_omit$value, c(6.65, 8.00))
  
  sum1 <- Summarise(A ~ student ,df_na, fun=median,  na.action = na.omit)$value  
  agg1 <-   aggregate(A ~ student, df_na, FUN=median,  na.action = na.omit)$A
  expect_equal(sum1, agg1)
})