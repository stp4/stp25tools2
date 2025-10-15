test_that("same result as pivot_longer", {

  df <- data.frame(
    month = rep(month.abb[1:3], 2),
    student = rep(c("Amy", "Bob"), each = 3),
    A = c(9, 7, 6, 8, 6, 9),
    B = c(6, 7, 8, 5, 6, 7),
    C = c(1, 3, 6, 3, 4, 7)
  )

  # df |> Long(3, 4, 5, by =  ~ month)
  #
  #   Long(A + B  ~ month, df)
  #
  # df |> Long(A, B, by =  ~ month)



  testthat::expect_equal(
    Long(A + B  ~ month, df),
    Long(df, A + B  ~ month))

  # testthat::expect_equal(
  #   Long(df, A + B  ~ month),
  #   stp25tools::Long(df, A + B  ~ month))
  #
  # testthat::expect_equal(
  #   df |> Long(. ~ month+student, key = "klass", value = "grade"),
  #   df |> stp25tools::Long(. ~ month+student, key = "klass", value = "grade")
  # )

  testthat::expect_equal(
    df |> Long(. ~ month+student, key = "klass", value = "grade"),

    tibble::tribble(
      ~month, ~student, ~klass, ~grade,
      "Jan","Amy","A", 9,
      "Jan","Amy","B", 6,
      "Jan","Amy","C", 1,
      "Feb","Amy","A", 7,
      "Feb","Amy","B", 7,
      "Feb","Amy","C", 3,
      "Mar","Amy","A", 6,
      "Mar","Amy","B", 8,
      "Mar","Amy","C", 6,
      "Jan","Bob","A", 8,
      "Jan","Bob","B", 5,
      "Jan","Bob","C", 3,
      "Feb","Bob","A", 6,
      "Feb","Bob","B", 6,
      "Feb","Bob","C", 4,
      "Mar","Bob","A", 9,
      "Mar","Bob","B", 7,
      "Mar","Bob","C", 7) |>
      dplyr::mutate(klass =factor(klass))
  )



  df |>
    tidyr::pivot_longer(cols = c("A", "B", "C"),
                        names_to = "grade", values_to = "count") |>
    dplyr::mutate(grade =factor(grade)) -> orginal_t1
  df |>
    Long( A+B+C ~., key = "grade", value = "count") -> new_t1

  # df |> stp25tools::Long(A, B, C,
  # by = ~month+student, key = "grade", value = "count") -> old_t1
  df |>  Long(A, B, C,
              by = ~month+student,
              key = "grade", value = "count") -> new_t2
  df |>  Long(A, B, C,
              key = "grade", value = "count") -> new_t3

  testthat::expect_equal(orginal_t1, new_t1)
  # testthat::expect_equal(new_t1, old_t1)
  testthat::expect_equal(new_t1, new_t2)
  testthat::expect_equal(new_t1[3:4], new_t3)

})
