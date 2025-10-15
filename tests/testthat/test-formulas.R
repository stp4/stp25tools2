test_that("create_formulas works", {



    testthat::expect_equal(
      create_formulas(
        measure_vars = c("log(m1)", "m2", "m3", "m4"),
        measure_vars_clean = c("log(m1)", "m2", "m3", "m4"),
        by = ~ sex
      ),
      list(
        formula_in = log(m1) + m2 + m3 + m4  ~ sex,
        formula = m1 + m2 + m3 + m4 ~ sex,
        formula_trans = log(m1) + m2 + m3 + m4  ~ sex,
        by = ~ sex,
        group.vars = "sex",condition.vars =NULL,
        measure_vars_clean = c("m1", "m2", "m3", "m4"),
        measure_vars = c("log(m1)", "m2", "m3", "m4"),
        all_vars =     c("m1", "m2", "m3", "m4", "sex")
      ) ,
      ignore_attr = TRUE
    )


    testthat::expect_equal(
      create_formulas(
        measure_vars = c("log(m1)", "m2", "m3", "m4"),
        measure_vars_clean = c("log(m1)", "m2", "m3", "m4"),
        by = NULL
      ),

      list(
        formula_in = ~ log(m1) + m2 + m3 + m4,
        formula =     ~ m1 + m2 + m3 + m4,
        formula_trans =     ~ log(m1) + m2 + m3 + m4,
        by = NULL,

        group.vars = NULL,  condition.vars =NULL,
        measure_vars_clean = c("m1", "m2", "m3", "m4"),
        measure_vars = c("log(m1)", "m2", "m3", "m4"),
        all_vars =     c("m1", "m2", "m3", "m4")
      ) ,
      ignore_attr = TRUE
    )






})




test_that("expand_dot_formula works", {
  testthat::expect_equal(expand_dot_formula(
    ~ . , letters[1:7]),
    ~ a + b + c + d + e + f + g)
  testthat::expect_equal(expand_dot_formula(a + b + c ~ . , letters[1:7]), a + b + c ~ d + e + f + g)
  testthat::expect_equal(expand_dot_formula(. ~  a + b + c , letters[1:7]), d + e + f + g ~ a + b + c)




  nms <- c(
    "sex",
    "age",
    "bmi",
    "smoking",
    "cholesterol",
    "homocystein",
    "glucose",
    "tbg",
    "harnsaeure",
    "visits" ,
    "id",
    "clinic",
    "any_visit"
  )


  expect_equal(
    expand_dot_formula(
      sex + age + bmi + smoking + cholesterol + homocystein + glucose + tbg + harnsaeure + visits  ~ .,
      nms
    ),
    sex + age + bmi + smoking + cholesterol + homocystein + glucose + tbg + harnsaeure + visits ~ id + clinic + any_visit
  )


  expect_equal(
    expand_dot_formula(~ ., nms),
    ~ sex + age + bmi + smoking + cholesterol + homocystein + glucose +  tbg + harnsaeure + visits + id + clinic + any_visit
  )




  })

test_that("LHS RHS works", {
  expect_equal(all.vars(LHS(a + b ~ c + d)), c("a", "b"))
  expect_equal(all.vars(LHS( ~ c + d)), c("c", "d"))

  expect_equal(all.vars(RHS(a + b ~ c + d)), c("c", "d"))

  expect_equal(RHS( ~ c + d), NULL)

  expect_equal(all.vars(RHS(. ~ c + d)), c("c", "d"))







})



test_that("extracts_lhs dapply works", {
expect_equal(extracts_lhs(~ . , letters[1:7]),
letters[1:7])

expect_equal(extracts_lhs(. ~  a + b + c , letters[1:7]),
c("d", "e", "f", "g"))

  expect_equal(extracts_lhs(a + b + c ~ . , letters[1:7]),
c( "a", "b", "c"))

   expect_equal( extracts_lhs(a + b + c ~ d , letters[1:7]),
c( "a", "b", "c"))

   expect_equal( extracts_lhs(~ a + b, letters[1:7]),
c("a", "b"))

})

