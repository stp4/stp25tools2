test_that("same results as pivot_wider", {



    df <- data.frame(
      month = rep(month.abb[1:3], 2),
      student = rep(c("Amy", "Bob"), each = 3),
      sex = rep(c("f", "m"), each = 3),
      A = c(9, 7, 6, 8, 6, 9),
      B = c(6, 7, 8, 5, 6, 7),
      C = c(1, 3, 6, 3, 4, 7)
    )

    # das Orginal
    df |>
      tidyr::pivot_wider(names_from = student, values_from = c(A, B, C)) ->  Orginal
    df |> Wide(student, A, B, C, names_vary = "fastest") ->  Kopie
  #  df |> stp25tools::Wide(student, A, B, C) -> Old

    testthat::expect_equal(Orginal, Kopie)
   # testthat::expect_equal(Kopie, Old)

  #  df |>Wide(month ~ student, A , B, C) ->  Kopie2
 #   df |> stp25tools::Wide(month ~ student, A , B, C) -> Old2
  #  testthat::expect_equal(Kopie2, Old2)


    df_long <-
      tibble::tribble(
        ~month, ~student, ~project, ~grade,
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
        "Mar","Bob","C", 7)

    testthat::expect_equal(
      df_long |>  tidyr::pivot_wider(names_from = project, values_from = grade),
      df_long |>  Wide(project, grade, names_vary = "fastest")
    )
    #df_long |>  Wide(~project, grade)


    testthat::expect_equal(
      df_long |>  Wide(student + project  ~ month, grade, names_vary = "fastest"),

      df_long |>
        tidyr::pivot_wider(
          names_from = month,  # Spaltennamen kommen aus der month-Spalte
          values_from = grade,  # Werte kommen aus der grade-Spalte
          id_cols = c(student, project)  # Diese Spalten bleiben als Identifikatoren
        ))







})
