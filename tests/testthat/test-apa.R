test_that("basic APA  works", {



  require(stp25tools2)

  dat <- data.frame(
    g = gl(2, 8, labels = c("Control", "Treat")),
    t = factor(c("t1", "t1", "t1", "t1", "t1",
                 "t2", "t2", "t2", "t2", "t2",
                 "t3", "t3", "t3", "t3", "t3", "t3") ),
    x = 1:16,
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 2, 3, 4, 5, 5, 8, 9, 9),
    z = c(1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0)
  )

  expect_equal(lm(x ~ g, dat) |> APA(),
       "R2=.75, ad.R2=.74, F(1, 14)=42.67, p<.001")
  expect_equal(wilcox.test(x ~ g, dat) |> APA(),
       "W=0.00, p<.001")

  expect_equal(t.test(x ~ g, dat) |> APA(),
       "T(14)=-6.53, p<.001")

  expect_equal(glm(z ~ g, dat, family= binomial()) |> APA(),
       "LogLik=-8.31, X2(1)=4.56, p=.033")




  expect_equal(aov(z ~ t, dat) |> APA(),
       "R2=.32, ad.R2=.22, F(2, 13)=3.12, p=.078")

  expect_equal(kruskal.test(z ~ t, dat) |> APA(),
       "W=4.87, p=.088")

  expect_equal(aov(z ~ t, dat) |> APA(),
       "R2=.32, ad.R2=.22, F(2, 13)=3.12, p=.078")





})
