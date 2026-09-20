



# setup -------------------------------------------------------------------


# #if (!("stp25tools2" %in% .packages())) {
#   library(tidyverse)
#   library(stp25output2)
#   library(stp25tools2)
#   #library(arulesViz)
#   # library(ggraph)
#   library(ggplot2)
#   setwd("C:/Users/wpete/Dropbox/1_Projekte/943_Abdelrahman_Omar")
#   # source('R/miscFun.r', echo=F)
#   load("Processed data/AbdelrahmanOmar.Rdata")
#   
# }
# 

#' Kreuztabellen
#'
#'
#'
#' @param x Objekt  glm, xtab, formula
#' @param ...  alles weitere an prepare.data
#' @param margin,add.margins Prozent und Total
#' add.margins gibt an welche Spalten  geht am addmargins()
#' @param include.count Anzahl
#' @param include.percent  Prozent
#' @param include.prop.chisq,include.chisq,include.fisher,include.test  Sig. Test
#' @param include.correlation Korrelation
#' @param include.diagnostic,prevalence  Diagnostic
#' @param addNA,exclude,drop.unused.levels an xtabs()'
#' @param formula an Wide
#' @param lvs,N_data  internal was fuer eine Tabelle kommt
#' @param digits Nachkommastellen
#' @return list("xtab","fisher_test","diagnostic.test")

#' @export
#' @importFrom caret confusionMatrix
#' @importFrom stats xtabs
#' @importFrom vcd assocstats
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
#'
#' xtabs(freq ~ therapy + infarction + mace, Plank) |>
#'   Tbll_xtabs(digits = 0)
#' # # OR
#' # glm_logistic <- glm(mace_bin ~ therapy * infarction,
#' #                     data=Plank, weights = freq,
#' #                     family = binomial())
#' # # andere Methode die Prozent zu berechnen -> glm-logistic
#' #
#' # # RR
#' #  glm_poisson <- glm(freq ~ therapy * mace + infarction * mace,data = Plank,
#' #                     family = poisson())
#' #  Tbll_effect(glm_logistic, digits = 3, include.ci = FALSE)
#' #  Tbll_effect(glm_poisson, digits = 3, include.ci = FALSE)
#'
#' set_opt(percent=list(digits=0))
#'
#'
#' data(infert, package = "datasets")
#' infert2<- infert
#' infert2$case  <- factor(infert2$case ,1:0, c("case", "control") )
#' infert2$spontaneous <- factor(infert2$spontaneous)
#' infert2$induced2    <- factor(infert2$induced==0)
#'
#' tab_1 <- xtabs( ~  case, infert2)
#' tab_2x2 <- xtabs( ~ induced2 + case, infert2)
#' tab_3x2 <- xtabs( ~ induced + case, infert2)
#' tab_3x3 <- xtabs( ~ induced + education, infert2)
#' tab_3x3x2 <- xtabs( ~ induced + education + case, infert2)
#'
#'
#'
#' #Tbll_xtabs(tab_1)
#'
#' Tbll_xtabs(tab_2x2)
#' Tbll_xtabs(tab_3x2)
#' Tbll_xtabs(tab_3x3)
#' Tbll_xtabs(tab_3x3x2)
#'
#' Tbll_xtabs(  induced + education ~ case, infert2)
#' Tbll_xtabs(  induced + education ~ case, infert2, include.test = TRUE)
#'
#' Tbll_xtabs_diagnostic(tab_2x2)
#'
#' # tab <- matrix(c(94, 40, 39, 40), ncol = 2, byrow = TRUE)
#' # tbll_extract(caret::confusionMatrix(tab))
#' # #tbll_extract(epiR::epi.tests(tab) )
#'
#' Tbll_xtabs(  induced  ~ case, infert2 )
#' Tbll_xtabs(  induced  ~ case, infert2, margin= "case" )
#' Tbll_xtabs(  induced  ~ case, infert2, margin= "case", add.margins="induced")
#' }
#'
Tbll_xtabs <-   function(x, ...) {
  UseMethod("Tbll_xtabs")
}

#' @rdname Tbll_xtabs
#' @export
Tbll_xtabs.default <- function(...,
                               include.count = TRUE,
                               include.percent = TRUE,
                               include.prop.chisq = FALSE,
                               include.chisq = FALSE,
                               include.fisher = FALSE,
                               include.test = any(c(include.fisher, include.chisq, include.prop.chisq)),
                               include.correlation = FALSE,
                               include.diagnostic = FALSE,
                               formula = NULL,
                               margin = NULL,
                               add.margins = NULL,
                               digits = get_opt("percent", "digits"),
                               prevalence = NULL,
                               addNA = FALSE,
                               exclude = if (!addNA)
                                 c(NA, NaN),
                               drop.unused.levels = FALSE) {
 # cat("\n Tbll_xtabs.default \n")
  formula <- NULL
  X <-  prepare_data(...)
  var_nms <- names(X$data)
  
  # an xtabs
  formula  <- formula(paste("~", paste(var_nms, collapse = "+")))
  # zum Formatieren zu Long Wide
  if (length(X$formula_in) == 2)
    X$formula_in <- NULL
  
  
  
  if (is.character(margin))
    margin <- which(var_nms %in% margin)
  if (is.character(add.margins))
    add.margins <- which(var_nms %in% add.margins)
  
  tab_x <- tab_n <- xtabs(formula, X$data)
  tab_prc <- prop.table(tab_n, margin = margin)
  
  if (!is.null(add.margins)) {
    # if( is.logical(add.margins)) add.margins <- margin
    tab_n <- addmargins(tab_n, margin = add.margins)
    tab_prc <- addmargins(tab_prc, margin = add.margins)
  }
  
  
  
  rslt <- list(
    xtab = format_xtabs(
      tab_n,
      tab_prc,
      X$formula_in,
      include.count = include.count,
      include.percent = include.percent
    )
  )
  
  if (include.test) {
    rslt <-
      my_xtest(tab_x,
               rslt,
               include.fisher,
               include.chisq,
               include.prop.chisq)
  }
  
  if (include.correlation) {
    rslt <- my_corr(tab_x, rslt)
  }
  
  if (include.diagnostic) {
    if (dim_x == 1)
      rslt$diagnostic.test <-
        prepare_output(Tbll_xtabs_diagnostic(x, prevalence = prevalence)$statistic,
                       caption = "Diagnostic")
    else {
      warning(
        "\nDie Diagnostic gibt es nur bei 2x2-Tabellen (wir haben hier die Dimensions von ",
        dim_x,
        ").\n"
      )
    }
  }
  
  
  if (length(rslt) == 1)
    rslt[[1]]
  else
    rslt
}

#' @rdname Tbll_xtabs
#' @export
Tbll.summary.table <- function(x, ...) {
  prepare_output(data.frame(
    Chisq =    render_f(x$statistic, 2),
    df = x$parameter,
    p =  rndr_P(x$p.value, FALSE)
  ),
  caption = "Pearson's Chi-squared Test for Count Data")
}

#' @rdname Tbll_xtabs
#' @export
Tbll_xtabs.glm <- function(x, thresh = 0.5, ...) {
  Tbll_xtabs_diagnostic.glm(x, thresh)$xtab
  
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
Tbll_xtabs_diagnostic <- function(x, ...) {
  UseMethod("Tbll_xtabs_diagnostic")
}

#' @param thresh Klassifikation auf Basis der Vorhersage Schwelle bei P=0.5
#' @rdname Tbll_xtabs

#' @export
Tbll_xtabs_diagnostic.glm <-
  function(x, thresh = 0.5, ...) {
    response <- all.vars(formula(formula(x)))[1]
    data <- x$model
    predictor <- fitted(x) # vorhergesagte Wahrscheinlichkeit
    data$Response <- data[, response]
    
    
    mylevels <-
      if (is.factor(data$Response))
        levels(data$Response)
    else
      0:1
    
    data$Predictor <- cut(predictor,
                          breaks = c(-Inf, thresh, Inf),
                          labels = mylevels)
    
    
    # Kontingenztafel: tatsaechliche vs. vorhergesagte Kategorie
    cTab <- stats::xtabs(~ Response + Predictor, data = data)
    
    if (length(cTab) == 4) {
      res <- Tbll_xtabs_diagnostic.xtabs(cTab)
      
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
Tbll_xtabs_diagnostic.table <- function(...)
  Tbll_xtabs_diagnostic.xtabs(...)

#' @rdname Tbll_xtabs
#' @description xtabs-Objekt
#' @export
Tbll_xtabs_diagnostic.xtabs <-
  function(x,
           lvs = c("positiv", "negativ"),
           digits = 2,
           prevalence = NULL,
           ...) {
    if (!length(x) == 4)
      stop("Klassifikation: nur mit 2x2 Tabellen moeglich!")
    
    Positive_Class <-
      paste(attr(x, "dimnames")[[1]][1], attr(x, "dimnames")[[2]][1], sep = "/")
    
    
    attr(x, "dimnames")[[1]] <- lvs
    attr(x, "dimnames")[[2]] <- lvs
    
    x_asco <-
      caret::confusionMatrix(x, prevalence = prevalence)
    
    list(
      xtab = x ,
      statistic = extract_confusionMatrix
      (x_asco, 
        digits = digits, 
        Positive_Class = Positive_Class)
    )
    
  }

# stp25stat2
extract_confusionMatrix <- 
function(x,
         digits = 2,
         Positive_Class= x$positiv,
         ...) {
  prepare_output(
    data.frame(
      Statistic =
        c(
          "Accuracy",
          "95% CI",
          "No Information Rate",
          "P-Value [Acc > NIR]",
          "Kappa",
          "Mcnemar's Test P-Value",
          "Sensitivity",
          "Specificity",
          "Pos Pred Value" ,
          "Neg Pred Value",
          "Precision",
          "Recall",
          "F1",
          "Prevalence",
          "Detection Rate",
          "Detection Prevalence" ,
          "Balanced Accuracy",
          "Positive Class"
        ),
      estimate =
        c(
          render_f(x$overall["Accuracy"], digits),
          rndr_CI(x$overall[c("AccuracyLower", "AccuracyUpper")]),
          render_f(x$overall["AccuracyNull"], digits),
          rndr_P(x$overall["AccuracyPValue"]),
          render_f(x$overall["Kappa"], digits),
          rndr_P(x$overall["McnemarPValue"]),
          render_f(x$byClass, digits),
          Positive_Class
        ),
      stringsAsFactors = FALSE
    ),
    caption="Associated Statistics"
  )
}




format_xtabs <- function(tab_n,
                         tab_prc,
                         formula_in,
                         include.count = TRUE,
                         include.percent = TRUE,
                         digits =  get_opt("percent", "digits"),
                         # dim_x = dimension(tab_n),
                         style = get_opt("percent", "style")) {
  length_dim <- length(dim(tab_n))
  
  # as.vector(ftable) sortiert anderst als as.vector(tab_n)
  # daher gab es einen Fehler!
  if (length_dim > 1) {
    ftable_n <- ftable(tab_n)
    ftable_prc <- ftable(tab_prc)
    row_vars <- attr(ftable_n, "row.vars")
    col_vars <- attr(ftable_n, "col.vars")
    # Dimensionen
    n_rows <- nrow(ftable_n)
    n_cols <- ncol(ftable_n)
    #print(col_vars)
    if (any(is.na(ftable_prc)))
      ftable_prc[is.na(ftable_prc)] <- 0
  }
  else{
    ftable_n <- tab_n
    ftable_prc <- tab_prc
    row_vars <- names(tab_n)
    col_vars <-  "M"
    n_rows <-  length(tab_n)
    n_cols <- 1
    formula_in <- NULL
    
  }
  
  # Formatierte Matrix
  if (include.count & include.percent) {
    rslt <- matrix(
      rndr_percent(
        as.vector(ftable_prc) * 100,
        as.vector(ftable_n),
        digits = digits,
        style = style
      ),
      nrow = n_rows,
      ncol = n_cols
    )
  }
  else if (include.count) {
    rslt <- matrix(as.vector(ftable_n),
                   nrow = n_rows, ncol = n_cols)
  }
  else if (include.percent) {
    rslt <- matrix(
      rndr_percent(
        as.vector(ftable_prc) * 100,
        as.vector(ftable_n),
        digits = digits,
        style = 3
      ),
      nrow = n_rows,
      ncol = n_cols
    )
  }
  else{
    return(NULL)}
  
  if (is.null(formula_in)) {
    colnames(rslt) <-
      paste(names(col_vars)[1L], col_vars[[1]], sep = "_")
    if (is.list(row_vars)) {
      if (length(row_vars) == 1) {
        rslt <- cbind(nms1 = row_vars[[1]], rslt)
        colnames(rslt)[1] <- names(row_vars)
      }
      else if (length(row_vars) == 2) {
        # Workaraund wegen der Lerzeichen
        n_leere <- length(row_vars[[2]]) - 1
        nms1 <- rep("", length(row_vars[[1]]) * (n_leere + 1))
        indices <- seq(1, length(nms1), by = n_leere + 1)
        nms1[indices] <- row_vars[[1]]
        nms2 <- rep(row_vars[[2]], length(row_vars[[1]]))
        
        rslt <- cbind(nms1, nms2, rslt)
        colnames(rslt)[1:2] <- names(row_vars)
        
      }
      else{
        #  print(ftable_n)
        j_levels <- 1
        for (i in rev(seq_along(row_vars))) {
          if (j_levels > 1)
            row_vars[[i]] <-
              rep(row_vars[[i]], each = j_levels)
          rslt <- cbind(data.frame(row_vars[i]), rslt)
          j_levels <- j_levels * length(row_vars[[i]])
        }
      }
      
    } else{
      mmm <- data.frame(attr(tab_n, "dimnames")[1])
      rslt <- cbind(mmm, rslt)
    }
    
    
  }
  else{
   colnames(rslt) <-col_vars[[1]]
    # paste(names(col_vars)[1L],, sep = "_")
  #  c_names<- 
   # rslt <- as.data.frame(rslt) ]
    j_levels <- 1
    for (i in rev(seq_along(row_vars))) {
      if (j_levels > 1)
        row_vars[[i]] <-
          rep(row_vars[[i]], each = j_levels)
      rslt <- cbind(data.frame(row_vars[i]),
                    rslt)
      j_levels <-  length(row_vars[[i]])
    }
    
cat("\nformula:")
l_formula <- all.vars(formula_in)
l_formula <- paste(".~", paste(l_formula[-length(l_formula)], collapse =
                                 "+"))
 rslt <-
   Long(formula(l_formula), rslt, key = names(col_vars)[1L])
 #  |> Wide(key = !!formula_in)
    
  }
  
  
  prepare_output(rslt, caption = "Haeufigkeitstabellen", N = sum(tab_n))
}




my_xtest <- function(x,
                     rslt,
                     include.fisher,
                     include.chisq,
                     include.prop.chisq) {
  dimension <- function(x) {
    dm <- dim(x)
    ldm <-  length(dm)
    if (ldm == 1)
      0
    else if (ldm == 2 & prod((dm - 1)) == 1)
      1
    else if (ldm == 2)
      2
    else
      ldm
  }
  
  dim_x <- dimension(x)
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
    rslt$prop.chisq <- NULL
  }
  else if (include.fisher & dim_x == 1) {
    fisher_test <- fisher.test(x)
    rslt$fisher_test <- prepare_output(
      data.frame(
        OR  = render_f(fisher_test$estimate),
        CI  = rndr_CI(matrix(fisher_test$conf.int, ncol = 2)),
        p   = rndr_P(fisher_test$p.value),
        stringsAsFactors = FALSE
      ),
      caption = "Fisher's Exact Test",
      N =  sum(x)
    )
  }
  else if (include.chisq & dim_x == 2) {
    chisq_tests <-  vcd::assocstats(x)
    rslt$chisq_tests <- prepare_output(
      data.frame(
        Test = rownames(chisq_tests$chisq_tests),
        Chi2 = render_f(chisq_tests$chisq_tests[, 1], 2),
        df   = render_f(chisq_tests$chisq_tests[, 2], 0),
        p    = rndr_P(chisq_tests$chisq_tests[, 3]),
        stringsAsFactors = FALSE
      ),
      caption = "Chi-Squared Test",
      N =  sum(x)
    )
  }
  else if (include.chisq.sumary) {
    # hier gibt es noch eine spezifikation
    rslt$chisq_tests <- Tbll.summary.table(summary(x))
  }
  else {
    rslt$chisq_tests <- Tbll.summary.table(summary(x))
  }
  
  
  rslt
}

my_corr <- function(x, rslt) {
  corr_test <-
    vcd::assocstats(x)
  
  rslt$corr_test <-
    prepare_output(
      data.frame(
        Test = c("Phi-Coefficient", "Contingency Coefficient", "Cramer's V"),
        r = render_f(
          c(corr_test$phi, corr_test$contingency, corr_test$cramer),
          3
        ),
        stringsAsFactors = FALSE
      ),
      caption = "Correlation Test",
      N =  sum(x)
    )
  
  rslt
}



# Tbll_xtabs(~  time + sptgroups + Age + sex, DF_long, include.percent = FALSE)
# 
# 
# Tbll_xtabs(
#   eaely.smoke + time+ parental.smoke  ~ sex ,
#   DF_long,
#   # add.margins = 2,
#   include.percent = FALSE
# ) -> x1
#  data.frame(x1)
#  
#  Wide(x1, eaely.smoke + time+  parental.smoke ~ sex )
# xtabs(~ ~ eaely.smoke+ time + parental.smoke + sex, DF_long) |> ftable()
# 
#  
# if (0) {
#   # 1
#   Tbll_xtabs(~ sptgroups, DF_long)
#   Tbll_xtabs( ~ sptgroups +  Age, DF_long)
#   Tbll_xtabs( ~ sptgroups + Age + sex, DF_long)
#   Tbll_xtabs( ~ sptgroups + Age + sex + eaely.smoke, DF_long)
#   DF_long |> Tbll_xtabs(sptgroups, Age)
#   Tbll_xtabs(sptgroups ~ Age, DF_long)
#   
#   
#   t1 <- Tbll_xtabs(~ sptgroups, DF_long, include.percent = FALSE)
#   t2 <- Tbll_xtabs( ~ sptgroups +  Age, DF_long, include.percent = FALSE)
#   t3 <- Tbll_xtabs( ~ sptgroups + Age + sex, DF_long, include.percent = FALSE)
#   t4 <- Tbll_xtabs( ~ sptgroups + Age + sex + eaely.smoke, DF_long, include.percent = FALSE)
#   
#   x1 <- xtabs(~ sptgroups, DF_long)
#   x2 <- xtabs( ~ sptgroups +  Age, DF_long)
#   x3 <- xtabs( ~ sptgroups + Age + sex, DF_long)
#   x4 <- xtabs( ~ sptgroups + Age + sex + eaely.smoke, DF_long)
#   
#   list(as.vector(x1), t1[[2]])
#   
#   list(as.numeric(unlist(t2[-1])), as.vector(x2))
#   
#   
#   list(as.numeric(unlist(t3[-c(1:2)])), as.vector(ftable(x3)))
#   
#   
#   
#   list(as.numeric(unlist(t4[-c(1:3)])), as.vector(ftable(x4)))
#   
#   
#   
#   
#   # x<- ftable(xtabs(~sptgroups + Age + sex + eaely.smoke,  DF_long   ))
#   # x
#   # r <- format(x, quote = TRUE, digits = 1)
#   
#   
#   
#   
#   
#   Tbll_xtabs(time + Age ~ sptgroups ,
#              DF_long,
#              add.margins = 3,
#              margin = c(1, 2))
#   Tbll_xtabs( ~ time + sptgroups +  Age, DF_long, include.percent = FALSE)
#   
#   
#   
#   Tbll_xtabs(
#     time + sptgroups ~  Age,
#     DF_long,
#     add.margins = 2,
#     include.percent = FALSE,
#     margin = c(1, 3)
#   )
#   
#   Tbll_xtabs(time + sptgroups ~  Age,
#              DF_long,
#              add.margins = 2,
#              margin = c(1, 3))
#   
#   
#   DF_long |>
#     Tbll_xtabs(time + Age ~ sptgroups,
#                add.margins = 3,
#                margin = c(1, 2))
#   
#   DF_long |>
#     Tbll_xtabs(time,
#                Age,
#                sptgroups,
#                add.margins = 3,
#                margin = c(1, 2))
#   
#   
#   
#   Tbll_xtabs(~ time + Age + sptgroups ,
#              DF_long,
#              add.margins = 3,
#              margin = c(1, 2)) -> x1
#   
#   
#   Tbll_xtabs(time + Age ~ sptgroups ,
#              DF_long,
#              add.margins = 3,
#              margin = c(1, 2)) -> x
#   
#   
#   
#   Tbll_xtabs(Age ~ sptgroups,
#              DF_long,
#              include.count = FALSE,
#              include.test = TRUE)
# }