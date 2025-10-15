#' Tbll_test
#'
#' @param ... formula und daten
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' Tbll_test(breaks + tension ~ wool , data = warpbreaks)
#'
#'
Tbll_test <- function(...) {
  X <- prepare_data(...)
  x <- X$measure.vars
  by = X$group.vars[1]
  rslt <- NULL
  for (i in x) {
    rslt <- rbind(rslt,
                  cbind(
                    Source = paste(i, "by", by),
                    auto_test(
                      x =  X$data[[i]],
                      by = X$data[[by]],
                      measure.test = X$measure.test[[i]]
                    )
                  ))
  }
  rownames(rslt) <-  X$measure.vars
  prepare_output( rslt,
                  caption = "Sig. Test",
                  N = nrow(X$data))

}


#' @rdname Tbll_test
#'
#' @description auto_test(): Interne Funktion fuer Sig-Test in  Tbll_desc.  include.custom = auto_test
#' @param x  vector , formula x~gruppe
#' @param data data.frame
#' @param by  vector (group)
#'
#' @param measure not used
#' @param measure.test test statistics
#' @param test_name c("contest", "wilcox","utest",
#' "htest", "kruskal", "ttest","aov", "anova")
#'
#' @return formatierter String, matrix with one row
#' @export
#' @examples
#'
#'  dat <- data.frame(
#'    m1 = c(1, 2, 1, 3, 1, 4, 1, 1,
#'        3, 5, 3, 1, 3, 6, 3, 1),
#'    geschl = gl(2, 8, labels = c("m", "f"))
#'  )
#'  dat$m2 <- cut(dat$m1, 2)
#'
#'
#'  dat |> Tbll_desc(
#'   m1[median, 1],
#'   m2,
#'   by = ~ geschl,
#'   include.custom = auto_test,
#'   # include.test = TRUE,
#'   include.total = TRUE
#'  )
auto_test <- function(x= 1:16,
                      by=gl(2, 8, labels = c("m", "f")),
                      measure=NULL,
                      measure.test="contest"
                        ) {


  cattest <-  get_opt("test_fun_cattest")
  contest <-  get_opt("test_fun_contest")

  dat <-  na.omit(data.frame(x = x, by = by))
  rslt <- NULL

  if (measure.test == "notest") {rslt <-  ""}
  else if (measure.test == "contest") {
    if (inherits(x, "factor")) {dat$x <- as.numeric(dat$x)}
    rslt <- conTest(x ~ by, dat)}
  else if (measure.test == "cattest") {rslt <- catTest(~ x + by, dat)}
  else if (measure.test %in% contest) {
    if (inherits(x, "factor")) {dat$x <-as.numeric(dat$x) }
    rslt <- conTest(x ~ by, dat, measure.test) }
  else if (measure.test %in% cattest) {rslt <- catTest(~ x + by, dat, measure.test) }

return(rslt)
  # workaround der aber nicht fehlerfrei ist
  # wenn bei den  stp-options mark.sig = TRUE dann get das nicht
  # print(rslt)
  # rslt <-   strsplit(rslt, ', p')[[1]]
  # cbind("Test Statistics" = rslt[1],
  #         "p Value" = gsub("=", "", rslt[2]))

}



#' @noRd
size_data_tabel <- function(x, data) {
  g <- all.vars(x)
  data <- na.omit(data[g])
  g <- g[length(g)]
  table(data[[g]])
}




#' @rdname Tbll_test
#' @description conTest(): spearmanTest2, WilkoxTest2,
#' KruskalTest2,TTest2, Aov2
#' @examples
#'
#' \donttest{
#'
#' dat <- data.frame(
#' m1 = c(1, 2, 1, 3, 1, 4, 1, 1,
#'        3, 5, 3, 1, 3, 6, 3, 1),
#' geschl = gl(2, 8, labels = c("m", "f"))
#' )
#' dat$m2 <- cut(dat$m1, 2)
#' #'
#' stp25tools2:::conTest(m1  ~ geschl, dat)
#' stp25tools2:::catTest(m1  ~ geschl, dat)
#' #stp25tools2:::ordTest(m1  ~ geschl, dat) Fehler???
#' stp25tools2:::spearmanTest2(m1  ~ geschl, dat)
#' stp25tools2:::TTest2(m1  ~ geschl, dat)
#' stp25tools2:::Aov2(m1  ~ geschl, dat)
#' stp25tools2:::KruskalTest2(m1  ~ geschl, dat)
#' stp25tools2:::WilkoxTest2(m1  ~ geschl, dat)
#' stp25tools2:::chisqTest2(m1  ~ geschl, dat)
#' stp25tools2:::chisqTest2( ~ m1 + geschl, dat)
#' stp25tools2:::fisherTest2( ~ m1 + geschl, dat)
#' stp25tools2:::fisherTest2( ~ m2 + geschl, dat)
#'
#' # Wilcoxon.Test       "F(1, 14)=3.20, p=.095"
#' # V2                  "Error wrong formula!"
#' # Logistic.Regression "LR(1)=3.14, p=.077"
#' # Wilcoxon.Test.1     "F(1, 14)=3.20, p=.095"
#' # T.Tests             "T(12)=-1.87, p=.086"
#' # ANOVA               "F(1, 14)=3.49, p=.083"
#' # Kruskal.Wallis.Test "H(1)=2.79, p=.095"
#' # Wilcoxon.Test.2     "U=17.00, p=.106"
#' # V9                  "Error wrong formula!"
#' # Pearson.Chi.squared "X2(5)=7.09, p=.214"
#' # V11                 "wrong dim for fisher-test"
#' # Fisher.Exact.Test   "OR=2.21, p=1.000"
#'
#' }
conTest = function(x,
                   data,
                   test_name = TRUE) {
  g <- size_data_tabel(x, data)

  if (all(g > 4)) {
    if (is.logical(test_name)) {
      spearmanTest2(x, data)
    } else{
      if (test_name == "wilcox")
        WilkoxTest2(x, data)
      else if (test_name == "utest")
        WilkoxTest2(x, data)
      else if (test_name == "htest")
        KruskalTest2(x, data)
      else if (test_name == "kruskal")
        KruskalTest2(x, data)
      else if (test_name == "ttest")
        TTest2(x, data)
      else if (test_name == "aov")
        Aov2(x, data)
      else if (test_name == "anova")
        Aov2(x, data)
      else
        test_name
    }
  }
  else{
    if(get_opt("Fstat","small.sampel"))
      paste0("sample to small (", paste(g, collapse = "/"), ")")
    else NA_character_
  }

}

#' @rdname Tbll_test
#' @param include.test Tbll_desc kann entweder logical oder den Test uebergeben
#' @description catTest(): chisqTest2, fisherTest2, gml_binomial
catTest = function(x,
                   data,
                   include.test = "chisq") {
  g <- size_data_tabel(x, data)
  #cat("\n include.test: ", include.test,"\n")
  if (all(g > 4)) {
    if (include.test == "chisq")
      chisqTest2(x, data)
    else if (include.test == "fisher")
      fisherTest2(x, data)
    else if (include.test == "binomial")
      gml_binomial(x, data)
    else{
      include.test
    }
  }
  else{
    if(get_opt("Fstat","small.sampel"))
      paste0("sample to small (", paste(g, collapse = "/"), ")")
    else NA_character_
  }

}

#' @rdname Tbll_test
#' @description  ordTest(). Logistic Regression Model  >Hier gibt es noch einen Fehler!!!
ordTest = function(x, data) {
  g <- size_data_tabel(x, data)
  if (all(g > 4)) {
    f <- rms::lrm(x, data)$stats
    # list(P = f["P"], stat = f["Model L.R."], df = f["d.f."],
    #      testname = "Proportional odds likelihood ratio",
    #      statname = "Chi-square", plotmathstat = "chi[df]^2")
    res<-gsub("X2", "LR", rndr_X(f["Model L.R."], f["d.f."], NULL, f["P"]))
    names(res)<- "Logistic Regression"
    res
  }
  else{
    if(get_opt("Fstat","small.sampel"))
      paste0("sample to small (", paste(g, collapse = "/"), ")")
    else NA_character_
  }
}


#' @rdname Tbll_test
#' @description  spearmanTest2(): Uses midranks in case of ties, as described by Hollander and Wolfe.
#' P-values for Spearman, Wilcoxon, or Kruskal-Wallis tests are
#' approximated by using the t or F distributions.
spearmanTest2 <- function(x, data) {
  st <- Hmisc::spearman2(x, data)
  if (is.na(st[3]))
    return("Error")

  res <- rndr_F(st[2], st[3], st[4], st[5])
  if (st[3] == 1)
    names(res) <- "Wilcoxon-Test"
  else
    names(res) <- "Kruskal-Wallis-Test"

  res
}


#' @rdname Tbll_test
#' @description WilkoxTest2(): wilcox.test
WilkoxTest2 <- function(x, data) {
  suppressWarnings(res <-
                     stats::wilcox.test(x, data,
                                        alternative =  "two.sided",
                                        exact=FALSE))
  res <- rndr_U(res$statistic, res$p.value)
  names(res) <- "Wilcoxon-Test"
  res
}


#' @rdname Tbll_test
#' @description KruskalTest2(): kruskal.test
KruskalTest2 <- function(x, data) {
  res <- stats::kruskal.test(x, data)
  res <-
    rndr_H(res$statistic, res$parameter, res$p.value)
  names(res) <- "Kruskal-Wallis-Test"
  res
}


#' @rdname Tbll_test
#' @description Aov2():  car::Anova(lm(...))
Aov2 <- function(x, data) {
  res <- stats::lm(x, data)
  res <- car::Anova(res, type = 3)
  res <-
    rndr_F(res[2, 3], res[2, 2], res[3, 2], res[2, 4])
  names(res) <- "ANOVA"
  res
}


#' @rdname Tbll_test
#' @description TTest2(): t.test
TTest2 <- function(x, data) {
  res <- stats::t.test(x, data, alternative =  "two.sided")
  res <-
    rndr_T(res$statistic, res$parameter, res$p.value)
  names(res) <- "T-Test"
  res
}


#' @rdname Tbll_test
#' @description chisqTest2(): chisq.test

chisqTest2 <- function(x, data) {
  # print(stats::xtabs(x, data, drop.unused.levels = TRUE))

  M <- stats::xtabs(x, data, drop.unused.levels = TRUE)
   # if( length(M)==4 & sum(M == 0 ) == 2) return("n.a.")

  res <- suppressWarnings(stats::chisq.test(M, correct = FALSE))

  if (!grepl("Pearson", res$method))
    return("n.a.")
  res <-
    rndr_X(res$statistic, res$parameter, NULL, res$p.value)
  names(res) <- "Pearson Chi-squared"
  res
}


# das geht nicht wegen lmertest
# gml_binomial <- function(x, data) {
#   xt <- as.data.frame(stats::xtabs(x, data))
#   fm <- formula(paste("Freq ~ ", paste(all.vars(x), collapse = "*")))
#   APA(glm(fm, xt, family = poisson()))
#
# }


#' @rdname Tbll_test
#' @description gml_binomial(): glm
gml_binomial <- function(x, data) {
  fm <- as.formula(paste(all.vars(x), collapse = "~"))
 # fm0 <- as.formula(paste(all.vars(x)[1],  "~ 1"))
  fit_1 <- glm(fm, data, family = binomial())
 # fit0 <- glm(fm0, data, family = binomial())

  # # https: /  / api.rpubs.com / tomanderson_34 / lrt
  #
  # A <- logLik(fit1)
  # M0 <- logLik(fit0)
  # teststat <- -2 * (as.numeric(M0) - as.numeric(A))
  #
  # df <- attr(A, "df") - 1
  # p <- pchisq(teststat, df = df, lower.tail = FALSE)
  # rslt <- paste(
  #   "log Lik ",
  #   paste(format(as.numeric(A), digits = 2), collapse = ", "),
  #   " (df=",
  #   format(df),
  #   "), ",
  #   rndr_P(p),
  #   sep = ""
  # )

  rslt <- APA(fit_1)

  names(rslt) <- "LRT-Test"
  rslt
}






#' @rdname Tbll_test
#' @description fisherTest2(): fisher.test
fisherTest2 <- function(x, data) {
  xt <- stats::xtabs(x, data)
  if (all(dim(xt) == c(2, 2))) {
    res <- stats::fisher.test(xt)
    res <- rndr_fischer(res$estimate, res$p.value)
    names(res) <- "Fisher Exact Test"
    res

  } else
    "wrong dim for fisher-test"
}

