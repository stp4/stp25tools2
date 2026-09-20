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
#'   Tbll_xtabs_ol(digits = 0)
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
#' #Tbll_xtabs_ol(tab_1)
#' 
#' Tbll_xtabs_ol(tab_2x2)
#' Tbll_xtabs_ol(tab_3x2)
#' Tbll_xtabs_ol(tab_3x3)
#' Tbll_xtabs_ol(tab_3x3x2)
#' 
#' Tbll_xtabs_ol(  induced + education ~ case, infert2)
#' Tbll_xtabs_ol(  induced + education ~ case, infert2, include.test = TRUE)
#' 
#' Tbll_xtabs_ol_diagnostic(tab_2x2)
#' 
#' # tab <- matrix(c(94, 40, 39, 40), ncol = 2, byrow = TRUE)
#' # tbll_extract(caret::confusionMatrix(tab))
#' # #tbll_extract(epiR::epi.tests(tab) )
#' 
#' Tbll_xtabs_ol(  induced  ~ case, infert2 )
#' Tbll_xtabs_ol(  induced  ~ case, infert2, margin= "case" )
#' Tbll_xtabs_ol(  induced  ~ case, infert2, margin= "case", add.margins="induced")
#' }
#'
Tbll_xtabs_ol <-   function(x, ...) {
  UseMethod("Tbll_xtabs_ol")
}

#' @rdname Tbll_xtabs_ol
#' @export
Tbll_xtabs_ol.default <-
  function(...,
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
    X <- prepare_data(...)
    x_formula <- formula(paste("~", 
                               paste(all.vars(X$formula), collapse = "+")))
    rslt <- stats::xtabs(
      x_formula,
      X$data,
      addNA = addNA,
      exclude = exclude,
      drop.unused.levels = drop.unused.levels
    ) |>
      Tbll_xtabs_ol.xtabs(
        include.count = include.count,
        include.percent = include.percent,
        # include.prop.chisq = include.prop.chisq,
        # include.chisq = include.chisq,
        # include.fisher = include.fisher,
        include.test = include.test,
        include.correlation = include.correlation,
        include.diagnostic = include.diagnostic,
        margin = margin,
        add.margins = add.margins,
        digits = digits
        #  prevalence = prevalence
        
      )
    
    # An Wide 
    if(is.null(formula)){
        vars_f <- all.vars(X$formula)
           if (length(vars_f) > 1) 
             formula <- as.formula(paste(
               paste(vars_f[1:(length(vars_f)-1)], collapse = " + "),
               "~", vars_f[length(vars_f)]))
           else formula <- NULL
    }
    if(!is.null(formula))
      rslt$xtab <- Wide(data = rslt$xtab, key = !!formula)

      
    
    if (length(rslt) == 1)
      rslt[[1]]
    else
      rslt
  } 


#' @rdname Tbll_xtabs_ol
#' @export
Tbll_xtabs_ol.glm <- function(x,
                           thresh = 0.5,
                           ...) {
  
  Tbll_xtabs_ol_diagnostic.glm(x, thresh)$xtab
  
}

#' @rdname Tbll_xtabs_ol
#' @export
Tbll_xtabs_ol.table <- function(...) Tbll_xtabs_ol.xtabs(...)



#' @rdname Tbll_xtabs_ol
#' @export
Tbll_xtabs_ol.xtabs  <- function(x,
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
                              digits = get_opt("percent", "digits"),
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
          Tbll_xtabs_ol_diagnostic(x,
                         prevalence = prevalence)$statistic,
          caption = "Diagnostic")
    else {
      warning("\nDie Diagnostic gibt es nur bei 2x2-Tabellen (wir haben hier die Dimensions von ", dim_x, ").\n")
    }
  }
  
  res
}


# # @rdname Tbll_xtabs_ol
# # @export
# Tbll.summary.table <- function(x, ...) {
#   prepare_output(data.frame(
#     Chisq =    render_f(x$statistic, 2),
#     df = x$parameter,
#     p =  rndr_P(x$p.value, FALSE)
#   ),
#   caption = "Pearson's Chi-squared Test for Count Data")
# }
# Helpers -----------------------------------------------------------------



#' main function for xtabs
#' @noRd
format_xtab <- function(x,
                        margin = NULL,
                        add.margins = NULL,
                        include.count = TRUE,
                        include.percent = TRUE,
                        digits =  get_opt("percent", "digits"),
                        dim_x = dimension(x),
                        style = get_opt("percent", "style"))  {
  
  rslt <- prc <- cnt <- NULL
  
  style <-
    if (include.count & include.percent) style
  else if (include.count &  !include.percent) 4
  else if (!include.count &  include.percent) 3
  else 1
  
  if (dim_x > 0) {  
    if (!is.null(add.margins)) {
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
  }
  else{
    cnt <- x
    prc <- prop.table(x) * 100
  }
  
  rslt <- as.data.frame(cnt)
  rslt$Freq <-  rndr_percent(as.vector(prc),
                             as.vector(cnt),
                             digits = digits,
                             style = style)
  
  rslt
}


dimension <- function(x) {
  dm <- dim(x)
  ldm <-  length(dm)
  if (ldm == 1)  0
  else if (ldm == 2 & prod((dm - 1)) == 1)  1
  else if (ldm == 2)  2
  else ldm
}


