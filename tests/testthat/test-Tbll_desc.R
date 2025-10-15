n <- 100
set.seed(1)
DF <- data.frame(
  sex = gl(2, n / 2, labels = c("male", "female")),
  group = gl(2, n / 2, labels = c("Control", "Treat"))[sample.int(n)],
  age = runif(n, min = 18, max = 73),
  bmi = runif(n, min = 18, max = 30),
  glucose = runif(n, min = 60, max = 110),
  harnstoff = runif(n, min = 2, max = 8)
) |> Label(
  sex =  "Geschlecht",
  group = "Behandlung",
  age =  "Alter",
  bmi =  "BMI",
  glucose =  "Glucose",
  harnstoff = "Harnstoff"
)


test_that("include.value works", {
 res3 <-  Tbll_desc(
    warpbreaks,
    "H1",
    breaks,
    tension,
    by = ~ wool,
    include.value =
      c(breaks = "ES = 26",
        tension = "OR = .0256")
  )

  rslt1 <- Tbll_desc(
    warpbreaks,
    "H1",
    breaks,
    tension,
    by = ~ wool,
    include.value = data.frame(
      ES = 1:2,
      OR = 3:4,
      row.names = c("breaks", "tension")
    )
  )


  rslt2 <- tibble::tribble( ~Item,~A,~B,~ES,~OR,
                            "(N) ","27","27",NA,    NA,
                            "H1 ","","",NA,NA,
                            "breaks (mean)", "31.04 (15.85)", "25.26 (9.30)",1,3,
                            "tension ","","",2,4,
                            "    L","33% (9)","33% (9)",NA,    NA,
                            "    M","33% (9)","33% (9)",NA,    NA,
                            "    H","33% (9)","33% (9)",NA  ,  NA)


  expect_equal(rslt1, rslt2, ignore_attr = TRUE)

  expect_equal(res3$value, c("", "", "ES = 26", "OR = .0256", "", "", ""))

})



test_that("group and options and include.total", {

set_opt(
    mean = list( style=4,  plusmin_str= "  ±  "),
    median = list(digits = 0, style=1),
    percent = list(style=2, null_percent_sign =  ' . ')
  )

  rslt1 <-
    DF |>
    Tbll_desc( sex ,
               group ,
               age[median,0],
               bmi[median, 2],
               "Labor",
               glucose[0],
               harnstoff[2],
               by= ~group,
               include.total = TRUE)

  rslt2 <-
    tibble::tribble(
      ~Item,~Total,~Control,~Treat,
      "(N) ","100","50","50",
      "Geschlecht ","","","",
      "    male","50 (50%)","24 (48%)","26 (52%)",
      "    female","50 (50%)","26 (52%)","24 (48%)",
      "Behandlung ","","","",
      "    Control","50 (50%)","50 (100%)","0 ( . )",
      "    Treat","50 (50%)","0 ( . )","50 (100%)",
      "Alter (median)","46 (34, 58)","43 (34, 51)","52 (34, 60)",
      "BMI (median)","22.80 (20.16, 26.48)", "23.87 (20.70, 27.35)", "21.89 (20.05, 25.18)",
      "Labor ","","","",
      "Glucose (mean)","86  ±  15","84  ±  15","89  ±  13",
      "Harnstoff (mean)", "4.93  ±  1.76","5.00  ±  1.65","4.86  ±  1.87",

    )

  expect_equal(rslt1, rslt2, ignore_attr = TRUE)

})


test_that("NA works", {

DF$bmi[2:5] <- NA
DF$sex[10:14] <- NA

rslt11 <-
  DF |>
  Tbll_desc( sex ,
             group ,
             age[median,0],
             bmi[median, 2],
             "Labor",
             glucose[0],
             harnstoff[2],
             by= ~group,
             include.total = TRUE)

set_opt(percent = list( useNA = "ifany"))

rslt21 <-
  DF |>
  Tbll_desc( sex ,
             group ,
             age[median,0],
             bmi[median, 2],
             "Labor",
             glucose[0],
             harnstoff[2],
             by= ~group,
             include.total = TRUE)
set_opt( percent = list( useNA = "always"))

rslt31 <-
  DF |>
  Tbll_desc( sex ,
             group ,
             age[median,0],
             bmi[median, 2],
             "Labor",
             glucose[0],
             harnstoff[2],
             by= ~group,
             include.total = TRUE)


expect_equal(rslt11[[2]],
c("95", "", "", "100", "", "", "100", "96", "", "100", "100")
)
expect_equal(rslt21[[2]],
c("100", "", "", "", "100", "", "", "100", "96", "", "100", "100")
)
expect_equal(rslt31[[2]],
c("100", "", "", "", "100", "", "", "", "100", "96", "", "100",  "100")
)

})

test_that("include.test works", {

#init_stp25_options()

x <- (1:30)/30+ c(1.36, -0.1, 0.39, -0.05, -1.38,
                  -0.41, -0.39, -0.06, 1.1, 0.76,
                  -0.16, -0.25, 0.7, 0.56, -0.69,
                  -0.71, 0.36, 0.77, -0.11, 0.88,
                  0.4, -0.61, 0.34, -1.13, 1.43,
                  1.98, -0.37, -1.04, 0.57, -0.14)
y<- cut(x, 2, c("low", "up"))
DF <- data.frame(
  g2 = factor(rep(1:2, each   = 15), 1:2, c("A", "B")),
  g3 = factor(rep(1:3, each   = 10), 1:3, c("A", "B", "C")),
  a = x,  b = x,  c = x,  d = x,  e = x,  f = x,  g = x,  h = x,
  i = y,  j = y,  k = y,  l = y,  m = y,  n = y,  o = y,  p = x,
  q  = x
)

#
# rslt1 <-
#   DF |>
#   prepare_data(
#     a[contest],
#     b[wilcox],
#     c[utest],
#     d[htest],
#     e[kruskal],
#     f[ttest],
#     g[anova],
#     h[aov],
#     i[cattest],
#     j[fisher],
#     k[chisq],
#     l[ordtest],
#     m[binomial],
#     n[shapiro],
#     o[kstest],
#     p[notest],
#     q[dummy],
#     by= ~ g2)
#
# rslt1$measure.test

rslt <-
DF |>
  Tbll_desc(
    a[contest],
    b[wilcox],
    #   c[utest],
    d[htest],
    #  e[kruskal],
    f[ttest],
    g[anova],
    #    h[aov],
    i[cattest, multi],
    j[fisher, multi],
    k[chisq, multi],
    l[ordtest, multi],
    m[, multi],
    # n[shapiro],
    # o[kstest],
    p[notest],
    q[dummy],
    by= ~ g2,
    include.test = TRUE, include.n = FALSE)

expect_equal(
rslt$Statistics,
 c(
  "F(1, 28)=2.70, p=.112",
  "U=74.00, p=.115",
  "H(1)=2.55, p=.110" ,
  "T(27)=-1.95, p=.062",
  "F(1, 28)=3.80, p=.061",
  "X2(1)=2.14, p=.143" ,
  "OR=2.89, p=.272" ,
  "X2(1)=2.14, p=.143",
  "ordtest" ,
  "X2(1)=2.14, p=.143" ,
  "" ,
  "F(1, 28)=2.70, p=.112"
)
)
})


test_that("Tbll_test works", {

  expect_equal(
    Tbll_test(breaks + tension ~ wool, data = warpbreaks)[[2]],
    c( "F(1, 52)=1.33, p=.253" ,"X2(2)=0.00, p=1.000"),
    ignore_attr = TRUE
  )

  expect_equal(
    Tbll_test(breaks[aov] + tension ~ wool, data = warpbreaks)[[2]],
    c("F(1, 52)=2.67, p=.108", "X2(2)=0.00, p=1.000") ,
    ignore_attr = TRUE
  )

})
