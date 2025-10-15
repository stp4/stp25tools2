test_that("alles works", {




    testthat::expect_warning(
      DF <- data.frame(
        x = 1:6,
        y = 1:6,
        z = 1:6,
        BMI = c(1, 2, 3, 1, 2, 3),
        WHtR = gl(2, 3, labels = c("Amy", "Bob")),
        WHtR_1 = c(9, 7, 6, 8, 6, 9),
        bildprof = c(6, 7, 8, 5, 6, 7)
      ) |>
        Label(
          BMI = "Body-Mass-Index",
          WHtR =  "Waist-Height-Ratio",
          WHtR_1 = "Waist-Height-Ratio",
          dummy = "Dummy",
          dummy2 = "Dummy"
        )
    )


    testthat::expect_equal(
      get_label(DF),
      c(x ="x" , y =  "y", z = "z", BMI = "Body-Mass-Index" ,
        WHtR ="Waist-Height-Ratio", WHtR_1 ="Waist-Height-Ratio",
        bildprof = "bildprof")
    )



    DF <- set_label(DF, c(bildprof = "Bildungsprofil"))

    testthat::expect_equal(
      get_label(DF),
      c(x ="x" , y =  "y", z = "z", BMI = "Body-Mass-Index" ,
        WHtR ="Waist-Height-Ratio", WHtR_1 ="Waist-Height-Ratio",
        bildprof = "Bildungsprofil")
    )

    DF <- delet_label(DF)
    testthat::expect_equal(
      get_label(DF),
      c(x ="x" , y =  "y", z = "z", BMI = "BMI" ,
        WHtR ="WHtR", WHtR_1 ="WHtR_1",
        bildprof = "bildprof")
    )

})
