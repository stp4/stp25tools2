test_that("Rbind2 combines simple data.frames", {
  df1 <- data.frame(a = 1, b = 2)
  df2 <- data.frame(a = 2, c = 3, d = 5)

  out <- Rbind2(df1, df2)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("which", "a", "b", "c", "d") %in% names(out)))
  expect_equal(nrow(out), 2)
  expect_equal(as.character(out$which), c("df1", "df2"))
})

test_that("Rbind2 keeps all columns and fills missing with NA", {
  df1 <- data.frame(CustomerId = 1:2, Product = "Oven")
  df2 <- data.frame(CustomerId = 3, Product = "Television", State = "CA")

  out <- Rbind2(df1, df2)
  expect_equal(nrow(out), 3)
  expect_true("State" %in% names(out))
  expect_true(any(is.na(out$State)))
})

test_that("Rbind2 without .id works", {
  df1 <- data.frame(x = 1)
  df2 <- data.frame(x = 2)

  out <- Rbind2(df1, df2, .id = NULL)
  expect_false("which" %in% names(out))
  expect_equal(nrow(out), 2)
})
