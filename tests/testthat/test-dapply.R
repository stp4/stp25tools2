test_that("Dapply works", {


  df1 <- tibble::tibble(
    month = rep(1:3, 2),
    student = rep(c("Amy", "Bob"), each = 3),
    A = c(9, 7, 6, 8, 6, 9),
    B = c(6, 7, 8, 5, 6, 7),
    C = c(1.6, 2.7, 3.8, 4.5, 5.6, 6.7)
  ) |>
    Label(
      month = "Monat",
      student = "Schüler",
      A = "Deutsch",
      B = "Mathe"
    )



  rslt <- tibble::tribble(
    ~ month, ~ student, ~ A, ~ B, ~ C,
    1L,    "Amy",    900,    600 ,    1.6,
    2L,    "Amy",   700 ,   700,    2.7,
    3L,    "Amy" ,    600 ,    800 ,    3.8,
    1L,    "Bob",    800 ,    500 ,    4.5,
    2L,    "Bob",    600,   600,    5.6,
    3L,    "Bob",    900,    700 ,    6.7
  ) |>
    Label(
      month = "Monat",
      student = "Schüler",
      A = "Deutsch",
      B = "Mathe"
    )


  # dplyr::mutate(df1, dplyr::across(c("A", "B"), .fns= function(x) x * 100 ))
  testthat::expect_equal(Dapply(~ A + B, df1, ~ .x * 100), rslt)
  testthat::expect_equal(Dapply(~ A + B, df1,
    fun = function(x)  x * 100),
    rslt)


  testthat::expect_equal(df1 |> Dapply(
    ~ A + B,
    fun = function(x)
      x * 100
  ), rslt)


  testthat::expect_equal(df1 |> Dapply(
    A, B,
    fun = function(x)
      x * 100
  ), rslt)


})
