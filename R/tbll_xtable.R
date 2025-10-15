#' Kreuztabellen
#'
#'
#'
#' @param x Objekt  glm, xtab, formula
#' @param ...  alles weitere
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' Plank <- data.frame(
#'   therapy = c("H+A", "H+A+P", "H+A", "H+A+P", "H+A", "H+A+P", "H+A", "H+A+P"),
#'   mace = c("yes", "yes", "no", "no", "yes", "yes", "no", "no"),
#'   infarction = c("NSTEMI", "NSTEMI", "NSTEMI", "NSTEMI", "STEMI", "STEMI", "STEMI", "STEMI"),
#'   freq = c(7, 1, 140, 54, 63, 14, 221, 126)
#' )
#' Plank$mace_bin <- ifelse(Plank$mace == "yes", 1, 0)
#'
#'
#' xtabs(freq ~ therapy + infarction + mace, Plank)
#'
#' # OR
#' glm_logistic <- glm(mace_bin ~ therapy * infarction,
#'                     data=Plank, weights = freq,
#'                     family = binomial())
#' # RR
#' # glm_poisson <- glm(freq ~ therapy * mace + infarction * mace,family = poisson(),data = Plank)
#'
#'
#' set_opt(percent=list(digits=1))
#' Tbll_xtabs(freq ~ therapy + infarction + mace,
#'            Plank,
#'            margin= 1:2,
#'            add.margins = 3)
#'
#' # andere Methode die Prozent zu berechnen -> glm-logistic
#' Tbll_effect(glm_logistic, digits = 3, include.ci = FALSE)
#'
#'
#'
#'
#' data(infert, package = "datasets")
#' infert$case  <- factor(infert$case ,1:0, c("case", "control") )
#' infert$spontaneous <- factor(infert$spontaneous)
#' infert$induced2    <- factor(infert$induced==0)
#'
#' tab_1 <- xtabs( ~  case, infert)
#' tab_2x2 <- xtabs( ~ induced2 + case, infert)
#' tab_3x2 <- xtabs( ~ induced + case, infert)
#' tab_3x3 <- xtabs( ~ induced + education, infert)
#' tab_3x3x2 <- xtabs( ~ induced + education + case, infert)
#'
#' Tbll(summary(tab_3x3x2))
#'
#' Tbll_xtabs(tab_1, include.test=TRUE)
#' Tbll_xtabs(tab_2x2, include.test=TRUE)
#' Tbll_xtabs(tab_3x2, include.test=TRUE)
#' Tbll_xtabs(tab_3x3, include.test=TRUE)
#' Tbll_xtabs(tab_3x3x2, include.test=TRUE)
#'
#'
#' Klassifikation(tab_2x2)
#'
#' tab <- matrix(c(94, 40, 39, 40), ncol = 2, byrow = TRUE)
#' tbll_extract(caret::confusionMatrix(tab))
#' tbll_extract(epiR::epi.tests(tab) )
#'
#' Klassifikation(as.table(tab))
#'
#' }
#'
Tbll_xtabs <-   function(x, ...) {
  UseMethod("Tbll_xtabs")
}

#' @rdname Tbll_xtabs
#' @export
Tbll_xtabs.default <- function(x, ...) {
  cat("Keine Methode fuer ", class(x), " vorhanden.")
}

#' @rdname Tbll_xtabs
#' @export
Tbll_xtabs.NULL <- function() {
  Info_Statistic(
    c(
      "include.chisq",
      "include.fisher",
      "include.correlation",
      "include.diagnostic"
    ),
    c("vcd", "stats", "vcd", "caret"),
    c(
      "assocstats",
      "fisher.test",
      "assocstats",
      "confusionMatrix"
    ),
    paste(methods("Tbll_xtabs"), collapse = ", ")
  )
}

#' @rdname Tbll_xtabs
#' @export
Tbll_xtabs.glm <- function(x,
                          thresh = 0.5,
                          ...) {

 Klassifikation(x, thresh)$xtab

}

#' @rdname Tbll_xtabs
#' @export
#' @param data formula und data.frame geht an xtabs
#' @param ... include.test usw
Tbll_xtabs.formula <-
  function(x,
           data = NULL,
           addNA = FALSE,
           exclude = if (!addNA) c(NA, NaN),
           drop.unused.levels = FALSE,
           margin = NULL,
           add.margins = NULL,
           N_data = nrow(data),
           ...) {

    x_tab <- stats::xtabs(
      x, data,
      addNA = addNA,
      exclude = exclude,
      drop.unused.levels = drop.unused.levels
    )
    N_data <- nrow(data)
    N_table <- sum(x_tab)

    dnn <- dimnames(x_tab)
    if(length(dnn) == length(all.vars(x)))
      names(dnn) <- stp25tools:::get_label2(data[all.vars(x)] )
    dimnames(x_tab) <- dnn

    if (is.character(margin))
      margin <- which(all.vars(x) %in% margin)
    if (is.character(add.margins))
      add.margins <- which(all.vars(x) %in% add.margins)

    Tbll_xtabs.xtabs(x_tab,
                     margin = margin,
                     add.margins = add.margins,
                     N_data = N_data,
                     ...)
  }


#' @rdname Tbll_xtabs
#' @export
Tbll_xtabs.data.frame <-
  function(x,
           ...,
           include.count = TRUE,
           include.percent = TRUE,
           include.prop.chisq = FALSE,
           include.chisq = FALSE,
           include.fisher = FALSE,
           include.test = any(c(include.fisher, include.chisq, include.prop.chisq)),
           include.correlation = FALSE,
           include.diagnostic = FALSE,
           margin = NULL,
           add.margins = NA,
           digits = get_opt("prozent", "digits"),
           prevalence = NULL,
           addNA = FALSE,
           exclude = if (!addNA) c(NA, NaN),
           drop.unused.levels = FALSE) {

    X <- stp25tools::prepare_data2(x, ...)
if(!is.null( X$group.vars ))
  stop("group.vars sind fuer xtabs nicht definert!")

    Tbll_xtabs.formula(
      as.formula(paste(
        "~", paste(X$measure.vars, collapse = "+"))),
      X$data,
      include.count = include.count,
      include.percent = include.percent,
      include.prop.chisq = include.prop.chisq,
      include.chisq = include.chisq,
      include.fisher = include.fisher,
      include.test = include.test,
      include.correlation = include.correlation,
      include.diagnostic = include.diagnostic,
      prevalence = prevalence,
      margin = margin,
      add.margins = add.margins,
      digits = digits,
      addNA = addNA,
      exclude = exclude,
      drop.unused.levels = drop.unused.levels,
      N_data = nrow(data)
    )
  }

#' @rdname Tbll_xtabs
#' @export
Tbll_xtabs.table <- function(...) Tbll_xtabs.xtabs(...)


#' @param margin,add.margins Prozent und Total add.margins gibt an welche Spalten  geht am addmargins()
#' @param include.count Anzahl
#' @param include.percent  Prozent
#' @param include.prop.chisq,include.chisq,include.fisher,include.test  Sig. Test
#' @param include.correlation Korrelation
#' @param include.diagnostic,prevalence  Diagnostic
#' @param addNA,exclude,drop.unused.levels an xtabs()'
#' @param lvs,N_data  internal was fuer eine Tabelle kommt
#' @param digits Nachkommastellen
#' @rdname Tbll_xtabs
#' @export
#'
#' @return list("xtab","fisher_test","diagnostic.test")
Tbll_xtabs.xtabs  <- function(x,
                              include.count = TRUE,
                              include.percent = TRUE,
                              include.prop.chisq = FALSE,
                              include.chisq = FALSE,
                              include.fisher = FALSE,
                              include.test = any(c(include.fisher, include.chisq, include.prop.chisq)),
                              include.correlation = FALSE,
                              include.diagnostic = FALSE,
                              margin = NULL,
                              add.margins = NULL,
                              digits = get_opt("prozent", "digits"),
                              prevalence = NULL,
                              N_data = sum(x),
                              ...) {

  res <- list()
  dim_x <- dimension(x)
  # get position of margin
  var_nms <- names(dimnames(x))
  if (is.character(margin))
    margin <- which(var_nms %in% margin)
  if (is.character(add.margins))
    add.margins <- which(var_nms %in% add.margins)

  res$xtab <- prepare_output(
    format_xtab(
      x,
      margin = margin,
      # mrgn$prop,
      add.margins =  add.margins,
      #mrgn$add,
      include.count,
      include.percent,
      digits = digits,
      dim_x = dim_x
    ),
    caption = "Haeufigkeitstabellen",
    N =  N_data
  )

  if (include.test) {
    include.chisq.sumary <- FALSE
    if (!any(include.fisher, include.chisq, include.prop.chisq)) {
      dm <- dim(x)
      ldm <-  length(dm)
      if (ldm == 1)
        include.prop.chisq <- TRUE
      else if (ldm == 2 &
               prod((dm - 1)) == 1)
        include.fisher <- TRUE
      else if (ldm == 2)
        include.chisq <- TRUE
      else
        include.chisq.sumary <- TRUE
    }
    if (include.prop.chisq) {
      cat(
        "\nFunktion  Proportion noch nicht fertig. Daher bitte APA(binom.test(tab_1)) verwenden.\n"
      )
      res$prop.chisq <- NULL
    }
    else if (include.fisher & dim_x == 1) {
      fisher_test <- fisher.test(x)
      res$fisher_test <- prepare_output(
        data.frame(
          OR  = render_f(fisher_test$estimate),
          CI  = rndr_CI(matrix(fisher_test$conf.int, ncol = 2)),
          p   = rndr_P(fisher_test$p.value),
          stringsAsFactors = FALSE
        ),
        caption = "Fisher's Exact Test",
        N =  N_data
      )
    }
    else if (include.chisq & dim_x == 2) {
      chisq_tests <-  vcd::assocstats(x)
      res$chisq_tests <- prepare_output(
        data.frame(
          Test = rownames(chisq_tests$chisq_tests),
          Chi2 = render_f(chisq_tests$chisq_tests[, 1], 2),
          df   = render_f(chisq_tests$chisq_tests[, 2], 0),
          p    = rndr_P(chisq_tests$chisq_tests[, 3]),
          stringsAsFactors = FALSE
        ),
        caption = "Chi-Squared Test",
        N =  N_data
      )
    }
    else if (include.chisq.sumary) {
      # hier gibt es noch eine spezifikation
      res$chisq_tests <- Tbll.summary.table(summary(x))
    }
    else {
      res$chisq_tests <- Tbll.summary.table(summary(x))
    }
  }

  if (include.correlation) {
    corr_test <-  vcd::assocstats(x)
    res$corr_test <- prepare_output(data.frame(
      Test = c("Phi-Coefficient",
               "Contingency Coefficient",
               "Cramer's V"),
      r = render_f(
        c(corr_test$phi,
          corr_test$contingency,
          corr_test$cramer),
        3
      ),
      stringsAsFactors = FALSE
    ),
    caption = "Correlation Test",
    N =  N_data)
  }

  if (include.diagnostic) {
    if(dim_x == 1)
    res$diagnostic.test <-
      prepare_output(
        Klassifikation(x,
                       prevalence = prevalence)$statistic,
                     caption = "Diagnostic")
    else {
      warning("\nDie Diagnostic gibt es nur bei 2x2-Tabellen (wir haben hier die Dimensions von ", dim_x, ").\n")
      }
  }

  res
}

# Helpers -----------------------------------------------------------------

#' @rdname Tbll_xtabs
Tbll.summary.table <- function(x, ...) {
  prepare_output(data.frame(
    Chisq =    render_f(x$statistic, 2),
    df = x$parameter,
    p =  rndr_P(x$p.value, FALSE)
  ),
  caption = "Pearson's Chi-squared Test for Count Data")
}

#' main function for xtabs
#' @noRd
format_xtab <- function(x,
                        margin = NULL,
                        add.margins = NULL,
                        include.count = TRUE,
                        include.percent = TRUE,
                        digits =  get_opt("prozent", "digits"),
                        dim_x = dimension(x),
                        style = get_opt("prozent", "style"))  {
  style <-
    if (include.count & include.percent) style
    else if (include.count &  !include.percent) 4
    else if (!include.count &  include.percent) 3
    else 1


  if (dim_x > 0) {
    if (!is.null(add.margins)) {
    #   cat("\n format_xtab \n")
    #   cat("  add.margins:  ")
    #   print(add.margins)
    # #  print(addmargins(x, add.margins))
    #   cat("\n-----------------\n")
      cnt <- ftable(addmargins(x, add.margins))

      prop_table <- prop.table(x, margin)
      prop_table[which(is.na(prop_table))] <- 0
      prc <-
        ftable(addmargins(prop_table * 100,
                          add.margins))
    } else{
      cnt <- ftable(x)
      prop_table <- prop.table(x, margin)
      prop_table[which(is.na(prop_table))] <- 0
      prc <-  ftable(prop_table * 100)
    }

    return(rndr_percent(prc, cnt, digits = digits, style = style))

  }
  else{
    # 1 x Tabelle
    prc <- prop.table(x) * 100
    rslt <-
      stp25tools::fix_to_df(rndr_percent(prc, x, digits = digits, style = style))
    names(rslt)[1:2] <- c(names(dimnames(x)), "m")

    return (rslt)
  }
}


dimension <- function(x) {
  dm <- dim(x)
  ldm <-  length(dm)
  if (ldm == 1)  0
  else if (ldm == 2 & prod((dm - 1)) == 1)  1
  else if (ldm == 2)  2
  else ldm
}


# Klassifikation ----------------------------------------------------------


#' @rdname Tbll_xtabs
#' @description Classification Table  classification_table
#'  Richtige und falsche Klassifikationen
#'  Bei  2x2 Tabellen der Kappa Test
#'
#' Sensitivity = A/(A+C)
#'
#' Specificity = D/(B+D)
#'
#' Prevalence = (A+C)/(A+B+C+D)
#'
#' PPV = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
#'
#' NPV = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))
#'
#' Detection Rate = A/(A+B+C+D)
#'
#' Detection Prevalence = (A+B)/(A+B+C+D)
#'
#' Balanced Accuracy = (sensitivity+specificity)/2
#'
#' Precision = A/(A+B)
#'
#' Recall = A/(A+C)
#'
#' F1 = (1+beta^2)*precision*recall/((beta^2 * precision)+recall)
#'
#'
#' Klassifikation fuer Binominal-GLM
#'
#' @return A data.frame Objekt.
#' @export
Klassifikation <- function(x, ...) {
  UseMethod("Klassifikation")
}

#' @param thresh Klassifikation auf Basis der Vorhersage Schwelle bei P=0.5
#' @rdname Tbll_xtabs
#' @description Klassifikation.glm
#' @export
Klassifikation.glm <-
  function(x,
           thresh = 0.5,
           ...) {
    response <- all.vars(formula(formula(x)))[1]
    data <- x$model
    predictor <- fitted(x) # vorhergesagte Wahrscheinlichkeit
    data$Response <- data[, response]


    mylevels <-
      if (is.factor(data$Response)) levels(data$Response)
      else 0:1

    data$Predictor <- cut(predictor,
                          breaks = c(-Inf, thresh, Inf),
                          labels = mylevels)
#print(car::some(data))

    # Kontingenztafel: tatsaechliche vs. vorhergesagte Kategorie
    cTab <- stats::xtabs( ~ Response + Predictor, data = data)

    if (length(cTab) == 4) {
      res <- Klassifikation.xtabs(cTab)

      res$response = data$Response
      res$predictor = predictor
    }
    else
      res <- list(
        xtab = cTab,
        statistic = NULL,
        response = response,
        predictor = predictor
      )
    res
  }

#' @rdname Tbll_xtabs
#' @export
Klassifikation.table <- function(...) Klassifikation.xtabs(...)

#' @rdname Tbll_xtabs
#' @description xtabs-Objekt
#' @export
Klassifikation.xtabs <-
  function(x,
           lvs = c("positiv", "negativ"),
           digits = 2,
           prevalence = NULL,
           ...) {
    if (!length(x) == 4)
      stop("Klassifikation: nur mit 2x2 Tabellen moeglich!")

    Positive_Class <-
      paste(attr(x, "dimnames")[[1]][1],
            attr(x, "dimnames")[[2]][1], sep = "/")


    attr(x, "dimnames")[[1]] <- lvs
    attr(x, "dimnames")[[2]] <- lvs

    x_asco <- caret::confusionMatrix(x, prevalence = prevalence)


    list(
      xtab = x ,
      statistic = tbll_extract.confusionMatrix(x_asco,
                                               digits = digits,
                                               Positive_Class = Positive_Class)
    )

}



