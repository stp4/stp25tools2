# tests/testthat/test-tbll_anova.R

#context("Tbll_anova Funktion Tests")
 


test_that("Tbll_anova funktioniert mit aov-Objekten", {
  # Testdaten vorbereiten
  df <- iris
  df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
  
  # Einfaches aov-Modell
  model1 <- aov(Sepal.Length ~ Sepal.Big, data = df)
  
  # Teste Basis-Funktionalität
  result <- Tbll_anova(model1)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("Parameter" %in% names(result))
  expect_true("F" %in% names(result))
  expect_true("p" %in% names(result))
})

test_that("Tbll_anova funktioniert mit lm-Objekten", {
  df <- iris
  df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
  
  model2 <- lm(Sepal.Length ~ Sepal.Big, data = df)
  
  result <- Tbll_anova(model2)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("Verschiedene Quadratsummen-Typen funktionieren", {
 
data("Moore", package="carData"  )
data(package = "car")
  op <- options(contrasts = c("contr.sum", "contr.poly"))
  mod2_aov <- aov(conformity ~ fcategory*partner.status, data=Moore)
  mod2_lm <- lm(conformity ~ fcategory*partner.status, 
                data=Moore,
                contrasts=list(fcategory=contr.sum, partner.status=contr.sum))
  
  options(contrasts = c("contr.treatment", "contr.poly"))
  mod1_aov <- aov(conformity ~ fcategory*partner.status, data=Moore) 
  mod1_lm <- lm(conformity ~ fcategory*partner.status, data=Moore)
  
  expect_error(Tbll_anova(mod1_lm))
  
  # Tbll_anova(mod1_lm, type = 3) -> x1
  # x2<- structure(list(Parameter = c("fcategory", "partner.status", "fcategory:partner.status",
  #                                   "Residuals"), Sum_Squares = c("11.61", "212.21", "175.49", "817.76"
  #                                   ), df = c("2", "1", "2", "39"), F = c("0.28", "10.12", "4.18",
  #                                                                         ""), Eta2_partial = c("0.01", "0.21", "0.18", ""), p = c(".760",
  #                                                                                                                                  ".003", ".023", "")), class = c("tbl_df", "tbl", "data.frame"
  #                                                                                                                                  ), row.names = c(NA, 4L), caption = "Anova (Type 2 tests), Response: conformity", note = "", labels = NA)
  # 
  # expect_equal(x1,x2)
  
  
  
  expect_error(Tbll_anova(mod1_aov) ) 
  # x2<- structure(list(Parameter = c("fcategory", "partner.status", "fcategory:partner.status", 
  #                                   "Residuals"), Sum_Squares = c("11.61", "212.21", "175.49", "817.76"
  #                                   ), df = c("2", "1", "2", "39"), F = c("0.28", "10.12", "4.18", 
  #                                                                         ""), Eta2_partial = c("0.01", "0.21", "0.18", ""), p = c(".760", 
  #                                                                                                                                  ".003", ".023", "")), class = c("tbl_df", "tbl", "data.frame"
  #                                                                                                                                  ), row.names = c(NA, 4L), caption = "Anova (Type 2 tests), Response: conformity", note = "", labels = NA)
  # 
  # expect_equal(x1, x2)
  
  
  Tbll_anova(mod2_lm, type=3)  -> x1
  x2<- structure(list(Parameter = c("fcategory", "partner.status", "fcategory:partner.status", 
                                    "Residuals"), Sum_Squares = c("36.02", "239.56", "175.49", "817.76"
                                    ), df = c("2", "1", "2", "39"), F = c("0.86", "11.42", "4.18", 
                                                                          ""), Eta2_partial = c("0.04", "0.23", "0.18", ""), p = c(".431", 
                                                                                                                                   ".002", ".023", "")), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                   ), row.names = c(NA, 4L), caption = "Anova (Type 3 tests), Response: conformity", note = "", labels = NA)
  
  expect_equal(x1,x2)
  
  
  Tbll_anova(mod2_aov, type=3)  -> x1
  x2<- structure(list(Parameter = c("fcategory", "partner.status", "fcategory:partner.status", 
                                    "Residuals"), Sum_Squares = c("36.02", "239.56", "175.49", "817.76"
                                    ), df = c("2", "1", "2", "39"), F = c("0.86", "11.42", "4.18", 
                                                                          ""), Eta2_partial = c("0.04", "0.23", "0.18", ""), p = c(".431", 
                                                                                                                                   ".002", ".023", "")), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                   ), row.names = c(NA, 4L), caption = "Anova (Type 3 tests), Response: conformity", note = "", labels = NA)
  
  
  
  expect_equal(x1,x2)
  
  options(op)
})



test_that("Mehrere Modelle werden korrekt verarbeitet", {
  df <- iris
  df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
  
  mod1 <- aov(Sepal.Length ~ Sepal.Big, data = df)
  mod2 <- aov(Sepal.Length ~ Species, data = df)
  
  # Teste mit benannten Modellen
  result_multiple <- Tbll_anova(
    "Sepal.Big" = mod1, 
    "Species" = mod2
  )
  
  expect_s3_class(result_multiple, "data.frame")
  expect_true(nrow(result_multiple) > nrow(Tbll_anova(mod1)))
})
