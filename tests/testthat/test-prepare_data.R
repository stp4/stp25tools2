



df <- data.frame(
  u = 1:10,
  w =  c(1.24,3.2,6.2,2.2,3.47,4.2,5.2,6.2,3.2,2.2),
  x = c(5.2,1.4,3.2,6.41,2.52,3.2,4, 7.27, 3.2,2.2),
  y =  c(6.2,2.2,3.62,8.2,4.2,5.2,6.14,7.32,3,2.2),
  z = gl(2, 5, labels = c("Control", "Treat"))
)
r1 <-  prepare_data(x[mean, 3, ttest] + y[1, aov, median] ~ z, df)

test_that("names to formula works", {



testthat::expect_equal(names(r1$data), c("x", "y", "z"))
testthat::expect_equal(r1$formula, x + y ~ z, ignore_attr = TRUE)
testthat::expect_equal(r1$formula_in, x[mean, 3, ttest] + y[1, aov, median] ~ z, ignore_attr = TRUE)
})


test_that("measure and digits works", {
testthat::expect_equal(r1$measure.vars, c("x", "y"))
testthat::expect_equal(r1$measure, c(x = "mean", y = "median"))
testthat::expect_equal(r1$digits, c(x = 3, y = 1))


})


test_that("Labels works", {
testthat::expect_equal(r1$row_name, c(x = "x", y = "y"))
testthat::expect_equal(r1$group.vars, c("z"))
})



test_that("formula works", {



testthat::expect_equal(
  prepare_data(~ u + w + x + y + z, df)$formula,
  prepare_data(df, ~ .)$formula)

testthat::expect_equal(
  prepare_data(df, ~ u + w + x + y + z)$formula,
  prepare_data(df, u, w, x, y, z)$formula)

})






dat<-
  data.frame(
    sex = 1:2,
    m1 = 1:2,
    m2 = 1:2,
    m3 = 1:2,
    m4 = 1:2,
    m5 = 1:2,
    m6 = 1:2,
    geschl = 1:2
  )


test_that("warning works", {
  expect_warning(prepare_data( ~ m1 + m1 + m2 + m3 + m4, dat))
})

test_that("formula vs names works", {
  expect_equal(
    prepare_data(~ m1 + m2 + m3 + m4, dat),
    prepare_data(dat,m1,m2,m3,m4),
    ignore_attr = TRUE

  )
})

test_that("number vs names works", {
  expect_equal(
    prepare_data(dat, 1,2,6),
    prepare_data(dat,sex,m1,m5),
    ignore_attr = TRUE

  )
})


test_that("formula vs names with digits and test", {
  expect_equal(
    prepare_data(m1[4, median,aov]+m2+ m3+ m4[5] ~ geschl,dat),
    prepare_data(dat, m1[4, median, aov], m2, m3, m4[5], by =  ~ geschl),
    ignore_attr = TRUE
  )
})







test_that("calculate", {
  x<-prepare_data(~ log(m1) + m2 + m3 + m4, dat)

  expect_equal(
    x$data[[1]],
    log(dat$m1),
    ignore_attr = TRUE

  )
})



test_that("helper which_measure", {
  expect_equal(which_measure("", "numeric" , "BMI"), c(BMI = "numeric"))
})

