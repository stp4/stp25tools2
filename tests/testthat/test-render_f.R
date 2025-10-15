test_that("alles zusammen  works", {
  # Vector
  x <- c(1, 0, 2.45896, 2548.256)
  y <- c(0.1, 0.01 , 0.001, NA)
  z <- c(1L, 2L, 3L, 0)
  Item <- letters[1:4]
  # erratet die digits

  expect_equal(render_f(x), c("1.00", "0.00", "2.46", "2548.26"))
  expect_equal(render_f(y), c("0.1000", "0.0100", "0.0010", ""))
  expect_equal(render_f(z), c("1", "2", "3", "0"))
  expect_equal(render_f(Item), c("a", "b", "c", "d"))



  # jedes Element mit unterschiedlichen digits runden

  expect_equal(render_f(x, digits = c(1, 2, 3, 4)),
               c("1.0", "0.00", "2.459", "2548.2560"))
  expect_equal(render_f(x, digits = c(1, 2)), c("1.0", "0.00", "2.46", "2548.26"))



  # Data.Frame
  dat <- data.frame(Item = Item, x = x, y = y)
  # erratet die digits
  # wenn digits ein vector dann bei data.frame Zeilenweise runden



  expect_equal(render_f(dat), as.data.frame(
    tibble::tribble(
      ~ Item,      ~ x,      ~ y,
      "a",      "1.00",      "0.1000",
      "b",      "0.00",      "0.0100",
      "c",      "2.46",      "0.0010",
      "d" ,      "2548.26",      ""
    )
  ))

  expect_equal(render_f(dat, digits = c(1, 2, 3, 4)), as.data.frame(
    tibble::tribble(
      ~ Item,      ~ x,      ~ y,
      "a",      "1.0",      "0.1",
      "b",      "0.00",      "0.01",
      "c",      "2.459",      "0.001",
      "d" ,      "2548.2560",      ""
    )
  ))



  expect_equal(render_f(dat, digits = c(1, 2)), as.data.frame(
    tibble::tribble(
      ~ Item,      ~ x,      ~ y,
      "a",      "1.0",      "0.1",
      "b",      "0.00",      "0.01",
      "c",      "2.46",      "0.00",
      "d" ,      "2548.26",      ""
    )
  ))

  expect_equal(render_f(dat, digits = 2), as.data.frame(
    tibble::tribble(
      ~ Item,      ~ x,      ~ y,
      "a",      "1.00",      "0.10",
      "b",      "0.00",      "0.01",
      "c",      "2.46",      "0.00",
      "d" ,      "2548.26",      ""
    )
  ))

  render_f(dat, digits = 2)




  # wenn digits eine liste dann spaltenweise Runden ( die vectoren Zeilenweise)
  expect_equal(
    render_f(
      dat,
      digits = list(c(1, 2), c(3)),
      drop0leading = list(FALSE, TRUE)
    ) ,
    as.data.frame(
      tibble::tribble(
        ~ Item,~ x,~ y,
        "a",        "1.0",        ".100",
        "b",        "0.00",        ".010",
        "c",        "2.46",        ".001",
        "d" ,        "2548.26",        ""
      )
    )
  )


  datt <- tibble::as_tibble(dat)

  expect_equal(render_f(datt), tibble::as_tibble(render_f(dat)))



  expect_equal(render_f(datt, digits = c(1, 2, 3, 4)), tibble::as_tibble(render_f(dat, digits = c(1, 2, 3, 4))))
  expect_equal(render_f(datt, digits = c(1, 2)), tibble::as_tibble(render_f(dat, digits = c(1, 2))))

  expect_equal(render_f(datt, digits = 2), tibble::as_tibble(render_f(dat, digits =  2)))
  expect_equal(render_f(
    datt,
    digits = list(c(1, 2), c(3)),
    drop0leading = list(FALSE, TRUE)
  ) ,
  tibble::as_tibble(render_f(
    dat,
    digits = list(c(1, 2), c(3)),
    drop0leading = list(FALSE, TRUE)
  )))


  # Matrix
  # wie dataframe
  expect_equal(render_f(cbind(y), digits = 0:3), cbind(y = c("0", "0.0", "0.00", "")))

  expect_equal(render_f(cbind(y, x)), cbind(
    y = c("0.1000", "0.0100", "0.0010" , "") ,
    x = c("1.00" , "0.00" , "2.46" , "2548.26")
  ))



  # List
  mx <- list(
    x = x,
    y = y,
    z = c(
      2.16199999,
      -0.38411,
      0.19811,
      0.675222,-1.647222,
      1.13622,
      1.510223,
      -0.770321,
      0.387123,
      -0.559123
    )
  )
  expect_equal(render_f(mx), list(
    x = c("1.00", "0.00", "2.46", "2548.26"),
    y = c("0.1000", "0.0100", "0.0010", ""),
    z = c(
      "2.162",
      "-0.384" ,
      "0.198",
      "0.675",
      "-1.647",
      "1.136",
      "1.510",
      "-0.770",
      "0.387",
      "-0.559"
    )
  ))

  expect_equal(render_f(mx, digits = 1:2)$y, c("0.1" , "0.01", "0.00", ""))
  expect_equal(render_f(mx, digits = list(2, 0, 1))$y, c("0" , "0", "0", ""))





})




test_that("Signifikante digits  works", {
  expect_equal(render_sigf(c(123456, 23.2546, 1), 4), c("123500", "23.25", "1.000"))

})
