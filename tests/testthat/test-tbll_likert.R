



set.seed(42)
n<- 2*3*9*3
levels = c(
  "Strongly disagree",
  "Disagree",
  "Neither agree nor disagree",
  "Agree",
  "Strongly agree")




DF_lik <- tibble::tibble(
  q1 = sample(levels, n, replace = TRUE, prob = c(3,2, 1, 4,5)),
  q2 = sample(levels, n, replace = TRUE, prob = c(3,1, 1, 1,1)),
  q3 = sample(levels, n, replace = TRUE, prob = c(7,5, 1, 0,0)),
  q4 = sample(levels, n, replace = TRUE, prob = c(0,0, 0, 1,5)),
  q5 = sample(levels, n, replace = TRUE, prob = c(1,4, 1, 1,5)),
  q6 = sample(levels, n, replace = TRUE, prob = c(2,3, 0, 2,3)),
  q7 = sample(levels, n, replace = TRUE, prob = c(3,2, 0, 1,7)),
  q8 = sample(levels, n, replace = TRUE, prob = c(1,2, 1, 4,1)),
  q9 = sample(levels, n, replace = TRUE, prob = c(6,0, 0, 0,6))
) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ factor(.x, levels = levels))) |>
  dplyr::mutate(Sex = factor(sample(c("male", "female"), n, replace = TRUE)),
         Age = factor(sample(c("18-30", "30-50", ">50"), n, replace = TRUE)))
DF_lik$q8[sample.int(n)[seq_len(ceiling (0.02*n))] ] <- NA
DF_lik$q9[sample.int(n)[seq_len(ceiling (0.06*n))] ] <- NA
DF_lik <-  DF_lik[c(10,11,1:9)]



set_opt(percent = list(null_percent_sign = "."))



test_that("tbl_likert works", {

  r1 <-  DF_lik |>
    Summarise_likert(q1, q2, q3, q4, by = ~ Sex) |>
    Tbll_likert()

  r2 <- DF_lik |>
    Tbll_likert(q1, q2, q3, q4, by = ~ Sex)

  r3 <- DF_lik |>
    Tbll_likert(q1 + q2 + q3 + q4 ~ Sex)


  r4 <- Tbll_likert(q1 + q2 + q3 + q4 ~ Sex, DF_lik)

  expect_equal(r1, r2)

  expect_equal(r1, r3)


})




test_that("tbl_likert include", {

DF_lik |>
  Tbll_likert(
    "FC.2", q1, q2,
    "FC.3", q3,q4,q5,
    "FC.4", q6,q7,q8,q9,
    by = ~ Sex,
    include.count = FALSE,
    include.reference = 2.5,
    reference.labels = c("disagree", "agree"),
    include.total =TRUE,
    include.n=TRUE,
    include.na =TRUE,
    include.mean = TRUE) -> r5

  expect_equal(
  names(r5),
  c(".grouping", "Sex", "Item", "disagree(1:2)", "agree(3:5)",
    "Missing", "n", "Average")
  )




  expect_equal(r5$Missing ,
               c(".", ".", ".", ".", ".", ".", ".", "3%", "4%", ".", ".", ".",
               ".", ".", ".", ".", "1%", "8%", ".", ".", ".", ".", ".", ".",
               ".", "2%", "6%"))



expect_equal(r5[[4]] ,
             c("39%", "64%", "90%", ".", "40%", "51%", "44%", "29%", "45%",
            "37%", "67%", "95%", ".", "38%", "45%", "33%", "29%", "40%",
            "38%", "65%", "92%", ".", "40%", "48%", "39%", "29%", "43%"))


expect_equal(r5[[1]],
             structure(c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 2L, 2L,
                       2L, 3L, 3L, 3L, 3L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
                       levels = c("FC.2", "FC.3", "FC.4"),
                       class = "factor")
)

})
