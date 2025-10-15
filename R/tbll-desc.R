#' Generate Descriptive Statistics Tables
#'
#' A flexible function for creating descriptive statistics tables with support for
#' grouping variables, multiple output formats, and custom statistical tests.
#'
#' @param ... A formula specifying variables to analyze and optional grouping variables.
#'            Format: `var1 + var2 ~ group_var` or `var1[measure,digits] + ...`
#' @param include.label Logical. Include variable labels in output? Default is TRUE.
#' @param include.n Logical. Include counts of valid observations? Default is TRUE.
#' @param include.nr Logical or numeric. Include counts only in first row? Default is FALSE.
#' @param include.total Logical. Include column for combined groups? Default is FALSE.
#' @param include.test Logical or character. Include statistical tests? Can specify test type.
#'                    Options: "wilcoxon", "t.test", "anova", etc. Default is FALSE.
#' @param include.normality.tests Logical. Include normality tests? Default is FALSE.
#' @param include.multiresponse Logical. Handle multiple response variables? Default is FALSE.
#' @param include.custom Function. Custom analysis function to apply to each variable.
#' @param include.value Vector or data.frame. Additional values to include in output.
#' @param include.measure Character. Override default measures for all variables.
#' @param digits Numeric. Default decimal places for output.
#' @param use.level Numeric. Level for hierarchical variables (default 1).
#' @param use.duplicated Logical. Allow duplicate variables? Default is FALSE.
#' @param include.prepare.data daten aus prepare_data
#' @importFrom stats IQR aggregate as.formula binom.test binomial glm lm logLik median nobs pchisq quantile reorder sd
#' @importFrom utils head read.table str type.convert
#'
#' @return A data.frame containing the formatted results, with attributes for
#'         caption and notes.
#'
#' @details
#' The function supports several pre-defined measures:
#' - "mean": Arithmetic mean with SD
#' - "median": Median with IQR/range
#' - "factor": Frequency counts with percentages
#' - "multi": Multiple response frequencies
#'
#' Custom measures can be specified using the syntax `var[fun]` where `fun` is
#' a function that takes a vector and returns a formatted result.
#'
#' @export
#' @examples
#' ## ----include = FALSE----------------------------------------------------------
#'  #knitr::opts_chunk$set(
#'  # collapse = TRUE,
#'  # comment = "#>"
#'  #)
#'
#' ## ----setup, include = FALSE---------------------------------------------------
#' # library(stp25tools2)
#'
#' #' ## ----include = FALSE----------------------------------------------------------
#' set.seed(1)
#' n <- 100
#' DF <- data.frame(
#'   sex = gl(2, n/2, labels = c("male", "female")),
#'   group = gl(2, n/2, labels = c("Control", "Treat"))[sample.int(n)],
#'   age = round(runif(n, min = 18, max = 73)),
#'   bmi = round(runif(n, min = 18, max = 30),1),
#'   bmi.t1 = rnorm(n),
#'   bmi.t2 = rnorm(n),
#'   glucose = round(runif(n, min = 60, max = 110),1),
#'   harnstoff = round(runif(n, min = 2, max = 8),2),
#'   crp = c(rep(TRUE, 10), rep(FALSE, n-10))[sample.int(n)]
#' ) |> transform(
#'   bmi.t1 = round(bmi + bmi.t1,2),
#'   bmi.t2 = round(bmi + bmi.t2 + as.numeric(sex) -
#'                    ifelse(bmi > 28, as.numeric(group) * 2, 0) + age/20,1)
#' ) |>
#'   Label(
#'     sex= "Geschlecht",
#'     group = "Behandlung",
#'     age= "Alter",
#'     bmi = "BMI (t0)",
#'     bmi.t1 = "BMI (t1)",
#'     bmi.t2 = "BMI (t2)",
#'     glucose= "Glucose",
#'     harnstoff = "Harnstoff",
#'     crp = "CRP <10mg/l"
#'   )
#'
#' # library(tinyplot)
#' #
#' #
#' # dat<- DF |> Long( bmi + bmi.t1 + bmi.t2 ~ sex + group)
#' # tinyplot(value ~ group|variable, dat, facet  = ~ sex,  type = "boxplot")
#'
#' #'
#'
#' ## -----------------------------------------------------------------------------
#' set_opt(
#'   mean = list(style=4, plusmin_str=" ± "),
#'   median = list(digits=0, style=1),
#'   percent = list(style=2, null_percent_sign=" . ")
#' )
#'
#' ## -----------------------------------------------------------------------------
#' rslt1 <- DF |>
#'   Tbll_desc(
#'     sex,
#'     group,
#'     age[median,0],
#'     bmi[median,2],
#'     "Labor",
#'     glucose[0],
#'     harnstoff[2],
#'     by = ~group,
#'     include.total = TRUE
#'   )
#'
#' ## -----------------------------------------------------------------------------
#' n_mean_sd <- function(x, n, digits, ...) {
#'   data.frame(
#'     lev = c("   n", "   mean", "   sd"),
#'     n = c(as.character(n), "", ""),
#'     m = c(
#'       length(x),
#'       render_f(mean(x, na.rm = TRUE), digits),
#'       render_f(sd(x, na.rm = TRUE), digits)
#'     )
#'   )
#' }
#'
#' set_opt(
#'   measure_fun = list(fun="n_mean_sd"),
#'   measure = list(fun="factor")
#' )
#'
#' ## -----------------------------------------------------------------------------
#' DF |>
#'   Tbll_desc(
#'     sex,
#'     age[median,0],
#'     bmi[median,2],
#'     "Labor",
#'     glucose[0],
#'     harnstoff[fun],
#'     crp[multi],
#'     by = ~group
#'   )
#'
#' ## -----------------------------------------------------------------------------
#' Tbll_desc(
#'   warpbreaks,
#'   "H1",
#'   breaks,
#'   tension,
#'   by = ~ wool,
#'   include.value = data.frame(
#'     ES = 1:2,
#'     OR = 3:4,
#'     row.names = c("breaks", "tension")
#'   )
#' )
#'
#' ## -----------------------------------------------------------------------------
#' DF |>
#' Tbll_desc(
#'   glucose[0],
#'   harnstoff[2],
#'   by = ~ group,
#'   include.total = TRUE,
#'   include.test = TRUE,
#'   include.custom = function(x, by, ...) {
#'     #dat <- data.frame(x = x, by = by)
#'     d <- effectsize::cohens_d(x ~ by)
#'     round(d, 2)[1]
#'   }
#' )
#'
#'
Tbll_desc <-
  function (...,
            include.label = TRUE,
            include.n = TRUE,
            include.nr = FALSE,
            include.total = FALSE,
            include.test = FALSE,
            include.normality.tests = FALSE,
            include.multiresponse = FALSE,
            include.custom = NULL,
            include.value = NULL,
            include.measure = NULL,
            digits = NULL,
            use.level = 1, # multiresponse,
            use.duplicated = FALSE,
            include.prepare.data = FALSE
  ) {
    rslt_all <- NULL
    tbl_rstl <- NULL
    caption <- "Summary"

    X <- prepare_data(...)

    if(use.duplicated){
     # erlaube doppelte parameter
      in_vars <- strsplit(as.character(X$formula)[2L], " \\+ ")[[1L]]
      X$measure.vars <- in_vars
      X$measure.class <- X$measure.class[in_vars]
      X$digits <- X$digits[in_vars]
      X$measure <- X$measure[in_vars]
      X$row_name <- X$row_name[in_vars]
      X$measure.test <- X$measure.test[in_vars]
    }

    if (!is.null(include.measure)) {
      h__h <- X$measure == "character"
      if(any(h__h)) X$measure[which(h__h)] <- "character"
      X$measure[which(!h__h)] <- include.measure[1]
    }

    if (is.character(include.test)) {
      include.test <- gsub("[^[:alpha:]]", "", tolower(include.test))
      which_test <-
        match.arg(include.test,
                  c(contest, cattest, notest, ordtest, disttest, cortest))
      X$measure.test <- rep(which_test, length(X$measure.test))
      if (which_test %in% disttest) {
        include.test <- FALSE
        include.normality.tests <- TRUE
      } else{
        include.test <- TRUE
      }
    }


    length_measure_vars <- length(X$measure.vars)
    note <- ""

    any_missing_measure <-
      sapply(X$data[X$measure.vars],
             function(x) length(na.omit(x)))

    if (!include.label)
      X$row_name <- X$measure.vars

    if (include.n & sum(any_missing_measure[X$measure!="header"] - X$N) == 0) {
      # keine fehlenden dann nur erste Zeile mit N
      include.n <- FALSE
      include.nr <- TRUE
    }

     if (include.multiresponse){
      # wegen Formel und weil hier auch Zahlen kommen
      if(!is.null(digits)) X$digits <- rep(0, length(X$digits))
      X$measure <- rep("multi", length(X$measure))
    }

    # Start der Auswertung
    # 1. Mittelweret mit purrr::pmap
    if (is.null(X$group.vars)) {
      include.total <- FALSE
      rslt_all <-
        list_rbind(purrr::pmap(
          list(
            x = X$data[X$measure.vars],
            digits = X$digits,
            measure = X$measure,
            row_name = X$row_name,
            use.level = use.level
          ),
          prct_or_mean
        ))
    }

    # 2. oder include.total  mit purrr::pmap
    if (include.total) {
          rslt_all <-
        list_rbind(purrr::pmap(
          list(
            x = X$data[X$measure.vars],
            digits = X$digits,
            measure = X$measure,
            row_name = X$row_name,
            use.level = use.level
          ),
          prct_or_mean
        ))


      names(rslt_all)[3:ncol(rslt_all)] <-
        paste0("Total_", names(rslt_all)[3:ncol(rslt_all)])
    }

    # 3. Gruppenvergleich mit split() und for()
    if (!is.null(X$group.vars)) {
      if (length(X$group.vars) > 1) {
        X$data$group <- interaction2(X$data[X$group.vars])
        caption <- paste(X$group.vars, collapse = ", ")
        X$group.vars <- "group"
      } else {
        caption <- X$group.vars
      }

      data <- split(X$data[X$measure.vars], X$data[[X$group.vars]])
      if(any(sapply(data, lengths) == 0 )) {
        cat("\n\nMoegliche Fehlerquelle:\nIn der Gruppen-variable gibt es Leere Factoren!\n\n")
        print(table( X$data[[X$group.vars]], useNA ="ifany"))
      }

      for (i in names(data)) {
        tbl_part_i <-
          list_rbind(purrr::pmap(
            list(
              x = data[[i]],
              digits = X$digits,
              measure = X$measure,
              row_name = X$row_name,
              use.level = use.level
            ),
            prct_or_mean
          ))

        if (is.null(tbl_rstl))
          tbl_rstl <- tbl_part_i[1:2] # Linke Seite der Tabelle

        names(tbl_part_i) <- paste0(i, "_", names(tbl_part_i))
        tbl_rstl <- cbind(tbl_rstl, tbl_part_i[-c(1:2)])
      }

      if (include.total)
        rslt_all <- cbind(rslt_all, tbl_rstl[-c(1:2)])
      else
        rslt_all <- tbl_rstl
    }

    # 4. Anzahl entweder als Spalte oder als Singel-Zeile
    if (include.nr) {

      n.out <- c("(N)", rep("", ncol(rslt_all) - 1))
      names(n.out) <- names(rslt_all)

      if (is.null(X$group.vars)) {
        n.out[names(rslt_all) == "m" ] <- X$N
      }
      else {
        tsum <- table(X$data[[X$group.vars]])
        if (include.total) {
          n.out[stringr::str_ends(names(rslt_all), "_m")] <-
            c(as.character(sum(tsum)),
              as.character(tsum))
        }
        else{
          n.out[stringr::str_ends(names(rslt_all), "_m")] <-
            as.character(tsum)
        }
      }
      rslt_all <- rbind(n.out, rslt_all)
    }

    if (!include.n) {
      length.out <- if (is.null(X$group.vars)) 1
                    else nlevels(X$data[[X$group.vars]]) + include.total
      rslt_all <-
        rslt_all[-(seq(
          from = 3,
          by = 2,
          length.out = length.out
        ))]
      names(rslt_all) <- gsub("_m$", "", names(rslt_all))
    }

    # workaround um eindeutige row-names zu bekommen
    # die werden zum mergen gebraucht
    rownames(rslt_all) <- gsub("\\.first_factor", "", rownames(rslt_all))

    #  Eigene Funktion fun(x, by, measure, measure.test)
    #  return vector oder matrix
    #  die länge ist gleich wie bei measure oder die anzahl an factoren
    if (!is.null(include.custom)) {
      rslt_custom <- NULL
      # schleife statt purrr::pmap weil es einfacher lesbar ist
      for (i in seq_len(length_measure_vars)) {
        if (is.null(X$group.vars))
          tmp <- do.call(
            include.custom, # include.custom ist eine function
            list(
              X$data[[X$measure.vars[i]]],
              measure = X$measure[i],
              measure.test = X$measure.test[i]
            )
          )
        else
          tmp <- do.call(
            include.custom,
            list(
              X$data[[X$measure.vars[i]]],
              X$data[[X$group.vars[1]]],
              measure = X$measure[i],
              measure.test = X$measure.test[i]
            )
          )
        # tmp kann ein vector der laenge 1 oder eine matrix sein
        if (is.vector(tmp)) {

          if (X$measure[i] != "factor") {
            rslt_custom <- append(rslt_custom, tmp)
          }
          else  if (X$measure[i] == "factor" & length(tmp) == 1) {
            rslt_custom <- append(rslt_custom, tmp)
            rslt_custom <-
              append(rslt_custom, rep("", nlevels(X$data[[X$measure.vars[i]]])))
          } else if (X$measure[i] == "factor" &
                     length(tmp) == nlevels(X$data[[X$measure.vars[i]]])) {
            rslt_custom <- append(rslt_custom, c("", tmp))
          } else{
            stop("In rslt_custom stimmen die Laenge der Rueckgabe nicht!")
          }
        }
        else{
          if (X$measure[i] != "factor") {
            rslt_custom <- rbind(rslt_custom, tmp)
          }
          else if (X$measure[i] == "factor" & nrow(tmp) == 1) {
            rslt_custom <- rbind(rslt_custom, tmp)
            rslt_custom <- rbind(rslt_custom,
                                 matrix(
                                   "",
                                   ncol = ncol(rslt_custom),
                                   nrow = nlevels(X$data[[X$measure.vars[i]]])
                                 ))
          }
          else if (X$measure[i] == "factor" &
                   nrow(tmp) == nlevels(X$data[[X$measure.vars[i]]])) {
            rslt_custom <- rbind(rslt_custom,
                                 matrix("",
                                        ncol = ncol(rslt_custom),
                                        nrow = 1))
            rslt_custom <- rbind(rslt_custom, tmp)
          } else{
            stop("In rslt_custom stimmen die Laenge der Rueckgabe nicht!")
          }
        }
      }
      # Ausgabe ist jetzt eine vector
      if (is.vector(rslt_custom)) {
        if (include.nr)
          rslt_custom <- append(rslt_custom, "", after = 0)
        rslt_all$custom <- rslt_custom
      }
      else {
        # Ausgabe ist jetzt eine matrix
        if (include.nr)
          rslt_custom <- rbind(rep("", ncol(rslt_custom)), rslt_custom)
        rslt_all <- cbind(rslt_all, rslt_custom)
      }
    }


    # Signifikanz Test
    if (include.test) {
      cattest <- get_opt("test_fun_cattest")
      contest <- get_opt("test_fun_contest")
      note <- paste(note, ". Test Statistic:", sep = "")
      rslt_test <- NULL

      for (i in seq_len(length_measure_vars)) {
        temp <- NULL
        fm_chi <-
          formula(paste("~", X$measure.vars[i], "+", X$group.vars[1]))
        fm_aov <-
          formula(paste(X$measure.vars[i], "~", X$group.vars[1]))

        if (X$measure.test[i] == "notest") {
          rslt_test <- append(rslt_test, "")
        }
        else if (X$measure.test[i] == "contest") {
          if (X$measure.class[i] == "factor") {
            temp <- X$data[[X$measure.vars[i]]]
            X$data[[X$measure.vars[i]]] <-
              as.numeric(X$data[[X$measure.vars[i]]])
          }

          rslt_test <-
            append(rslt_test, conTest(fm_aov, X$data))



        }
        else if (X$measure.test[i] == "cattest") {
          rslt_test <- append(rslt_test, catTest(fm_chi, X$data))
        }
        else if (X$measure.test[i] %in% contest) {
          if (X$measure.class[i] == "factor") {
            temp <- X$data[[X$measure.vars[i]]]
            X$data[[X$measure.vars[i]]] <-
              as.numeric(X$data[[X$measure.vars[i]]])
          }
          rslt_test <-
            append(rslt_test, conTest(fm_aov, X$data, X$measure.test[i]))
        }
        else if (X$measure.test[i] %in% cattest) {
          rslt_test <-
            append(rslt_test, catTest(fm_chi, X$data, X$measure.test[i]))
        }

        if (!is.null(temp))
          X$data[[X$measure.vars[i]]] <- temp


       # if (X$measure[i] == "factor")
       #   rslt_test <- append(rslt_test, rep("", nlevels(X$data[[X$measure.vars[i]]])))

#
#         cat("\n measure")
#         print(X$measure[i])
#         print(get_opt("measure", X$measure[i]))
#         cat("\n\n")

        if(X$measure[i] %in% c("factor", "freq", "ratio") ) {
          # hier muessen noch Zeilen hinzugefuegt werden
          n_space <-  nlevels(factor(X$data[[X$measure.vars[i]]]))
          rslt_test <-
            append(rslt_test, rep_len("", n_space))

        }


         }

      if (include.nr)
        rslt_test <- append(rslt_test, "", after = 0)

      note <- paste(note, " ",
                    paste(unique(names(rslt_test)[nzchar(names(rslt_test))]),
                          collapse = ", "), ".", sep = "")

    #  print( rslt_test )

      rslt_all$statistics <- rslt_test
    }

    if (include.normality.tests) {
      rslt_disttest <- NULL
      for (i in seq_len(length_measure_vars)) {
        if (X$measure[i] %in% c("numeric", "mean", "median")) {
          ix <- na.omit(as.numeric(X$data[[X$measure.vars[i]]]))
          if (X$measure.test[i] == "kstest"){
            r <- APA(stats::ks.test(ix, "pnorm", mean(ix), sd(ix)))
            note <- c(note, "Kolmogorov-Smirnov Tests")
            }
          else{
            r <- APA(stats::shapiro.test(ix))
            note <- c(note, "Shapiro-Wilk Normality Test")
            }
        } else if (X$measure[i] == "factor")
          r <- rep("", nlevels(X$data[[X$measure.vars[i]]]))
        else
          r <- ""
        rslt_disttest <- append(rslt_disttest, r)
      }
      if (include.nr)
        rslt_disttest <- append(rslt_disttest, "", after = 0)

      note <- paste( unique(note), collapse = " ")
      rslt_all$normality.tests <- rslt_disttest
    }

    # Rechts extra Spalten einfuegen
    if (!is.null(include.value)) {
     # cat("\nin include.value\n")
      if (is.vector(include.value) & !is.null(names(include.value))) {
        rslt_value <- rep("", nrow(rslt_all))
        for (i in names(include.value)) {
          pos <- which(rownames(rslt_all) == i)
          rslt_value[pos] <- include.value[i]
        }
        rslt_all$value <- rslt_value
      }
      else if (is.vector(include.value)) {
        stop("Fehler in include.value, jetzt sind zwingend namen bei 'vector' erforderlich!")
      }
      else if (is.data.frame(include.value)) {
        lng_rslt_all <- nrow(rslt_all)
        if (any(rownames(rslt_all) %in% rownames(include.value))){

          rslt_all <-
            Combine(
              rslt_all,
              include.value,
              by = 0,
              #all = TRUE,
              sort = FALSE,
              suffixes = c("", ".1"),
              include.label = FALSE
            )[-1]
          }
        else{
          stop("Fehler in include.value,
               jetzt sind zwingend rownames bei 'data.frames' erforderlich!")
        }
        if (lng_rslt_all != nrow(rslt_all)){
          warning("Fehler in include.value, eventuell stimmen die rownames nicht")
        }
      }
      else if (is.matrix(include.value)) {
        if (any(rownames(rslt_all) %in% rownames(include.value)))
          rslt_all <-
            Combine(rslt_all,
                               data.frame(include.value),
                               by = 0,
                             #  all = TRUE,
                               sort = FALSE,
                               suffixes = c("", ".1"),
                               include.label = FALSE
                   )[-1]
        else{
          stop("Fehler in include.value, jetzt sind zwingend rownames bei 'matrix' erforderlich!")
        }
      }
      else{
        stop("Fehler in include.value, mit dem Daten-Type ",
             class(include.value),
             " kann ich nichts anfangen!")
      }
    }

    rslt_all[[1]] <- paste(rslt_all[[1]], rslt_all[[2]])
    rslt_all <- names_option(rslt_all[-2])

    if(include.prepare.data)
      attr(rslt_all, "prepare_data") <- X


    prepare_output(
      rslt_all,
      caption = caption,
      note = note,
      N = X$N)

  }







# Die Berechnung ----------------------------------------------------------


#' Die Berechnung
#'
#' @noRd
prct_or_mean <- function(x,
                         digits,
                         measure,
                         row_name,
                         sep = paste(symbol_nbsp(), symbol_nbsp()),
                         useNA = get_opt("percent", "useNA"),
                         max_factor_length = 35,
                         use.level = 1) {
  rslt <- NULL
  measure_fun <- get_opt("measure_fun", measure)
 # if( all(is.na(x)) ) measure_fun <- "emty_tbll"

  # bei mean beauche ich keine NA
  if (useNA == "no" |
      measure_fun %in% c("mean_tbll", "median_tbll")) {
    x <- na.omit(x)
    n <- length(x)
  }
  else {
    n <- length(x)
  }

  # Prüfen ob Funktion existiert
  if (!exists(measure_fun, mode = "function")) {
    return(paste("Funktion", measure_fun, "nicht gefunden"))
  }

  res <- do.call(measure_fun,
                    list(x,
                         digits = digits,
                         n = n,
                         useNA = useNA,
                         max_factor_length = max_factor_length,
                         use.level = use.level))

  if (nrow(res)>1) {
    x0 <- data.frame(
      Item = row_name,
      lev = get_opt("percent", "include_name"),
      n = res$n[1] ,
      m = "",
      stringsAsFactors = FALSE,
      row.names="first_factor"
    )
    res$n <- ""
    x1 <- cbind(Item = sep, res)
    rslt <- rbind(x0, x1)
  } else {
    rslt <- cbind(Item = c(row_name, rep("", nrow(res) - 1)), res)
  }

  rslt
}


# Leer --------------------------------------------------------------------


#' Leerer Data.Frame
#'
#' @noRd
emty_tbll <-
  function(...) {
    data.frame(
      lev = "",
      n = "",
      m = "",
      stringsAsFactors = FALSE
    )
  }


# median ------------------------------------------------------------------



#' @noRd
median_tbll <- function(x,
                        digits = 2,
                        n = length(x),
                        style = get_opt("median","style"),
                        include.level = get_opt("median","include_name"),
                        ...) {
  if (is.null(include.level))
    include.level <- ""

  data.frame(
    lev = include.level,
    n = as.character(n),
    m = calc_median(x, digits, n, style),
    stringsAsFactors = FALSE
  )

}


#' @noRd
calc_median <-
  function(x,
           digits = get_opt("median", "digits"),
           n = length(x),
           style = get_opt("median", "style")) {

    if(length(x) == 0) return("NA")

    median_quantil <- function()
      rndr_median_quant(quantile(x, na.rm = TRUE),
                        digits = digits)

    median_iqr <- function()
      rndr_median_iqr(median(x),
                      ifelse(n > 2, IQR(x), NA),
                      digits = digits)

    median_range <- function()
      rndr_median_range(quantile(x, na.rm = TRUE),
                        digits = digits)

    median_iqr_range <- function()
      rndr_median_iqr_range(
        median(x, na.rm = TRUE),
        IQR(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        digits = digits
      )

    if (all(is.na(x)))
      return(NaN)

    if (!is.numeric(x)) {
      x <- make_numeric(x)
    }

    if (is.null(style))
      median_quantil()
    else if (style == "IQR" | style=="4" | style == "IRQ")
      median_iqr()
    else if (style == 1)
      median_quantil()
    else if (style == 2 | style == "long")
      median_iqr_range()
    else if (style == 3)
      median_range()
    else
      rndr_median_quant()

  }


# mean --------------------------------------------------------------------


#' @noRd
mean_tbll <- function(x,
                      digits = get_opt("mean", "digits"),
                      n = length(x),
                      style = get_opt("mean","style"),
                      include.level = get_opt("mean", "include_name"),
                      ...) {

  if (is.null(include.level))
    include.level <- ""

  data.frame(
    lev = include.level,
    n = as.character(n),
    m = calc_mean(x, digits, n, style),
    stringsAsFactors = FALSE
  )
}


#' @noRd
make_numeric <- function(x) {
    if (is.numeric(x))  return(x)
    else if (is.factor(x)) {
      if (nlevels(x) == 2)
        ifelse(x == levels(x)[1], 1, 0)
      else
        as.numeric(x)
    }
    else {
      warning("Die Werte sind eventuel falsch class=", class(x), "!")
      x
    }
  }


#' @noRd
calc_mean <- function(x,
                       digits = get_opt("mean", "digits") ,
                       n = length(x),
                       style = get_opt("mean", "style")) {

  if(length(x) == 0) return("NA")

  if (!is.numeric(x)) {
    x <- make_numeric(x)
  }
  # calc and format
  if (is.null(style)) {
    rndr_mean(mean(x, na.rm = TRUE),
              ifelse(n > 2, sd(x, na.rm = TRUE), NA),
              digits = digits)
  }
  else if (style == "1") {
    rndr_mean(mean(x, na.rm = TRUE),
              ifelse(n > 2, sd (x, na.rm = TRUE), NA),
              digits = digits)
  }
  else if (style == "2" | style == "long") {
    rndr_mean_range(
      mean(x, na.rm = TRUE),
      ifelse(n > 2, sd (x, na.rm = TRUE), NA),
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE),
      digits = digits
    )
  }
  else {
    rndr_mean(mean(x),
              ifelse(n > 2, sd(x), NA),
              digits = digits)
  }
}


# percent -----------------------------------------------------------------


#' @param n intern
#' @param useNA an table c("no", "ifany", "always")
#' @param max_factor_length lange  kuerzen
#' @noRd
prct_tbll <-
  function(x,
           digits = get_opt("percent", "digits"),
           n = length(x),
           useNA = "no",
           max_factor_length = 25,
           style = get_opt("percent", "style"),
           ...)  {
    if(length(x) == 0) return("NA")

    tbl <- calc_percent(
      x,
      digits = digits,
      useNA = useNA,
      max_factor_length = max_factor_length,
      style = style
    )

    data.frame(
      lev = names(tbl),
      n = c(n, rep("", length(tbl) - 1)),
      m = tbl,
      stringsAsFactors = FALSE
    )
  }


#' @param use.level welcher level wir gezaelt
#' @param include.level mit (yes) in labels?
#' @noRd
multi_tbll <- function(x,
                       digits = get_opt("percent", "digits"),
                       n = length(x),
                       use.level = 1,
                       include.level = get_opt("percent", "include_level_multi"),
                       style = get_opt("percent", "style"),
                       ...) {

  # wenn ein h__1__h kommt
  if (is.character(x)) return(emty_tbll())

  if (is.logical(x)) {
    res <- prct_tbll(x, digits = digits, n = n)
    res$lev <- "true"
  } else if (is.factor(x)) {
    res <- prct_tbll(ifelse(x == levels(x)[use.level], TRUE, FALSE),
                     digits = digits,
                     n = n)
    res$lev <- levels(x)[use.level]
  } else if (is.numeric(x)) {
    res <- prct_tbll(ifelse(x == use.level, TRUE, FALSE),
                     digits = digits,
                     n = n)
    res$lev <- use.level
  }
  else{
    stop(
      "multi_tbll(): Hier kommt eie unbekannter Vector class = ",
      class(x)
    )
  }

  if (!include.level)
    res$lev <- ""

  res[1, ]
}


#' @noRd
calc_percent <- function(x,
                         digits = get_opt("percent", "digits") ,
                         useNA = "no",
                         max_factor_length = 25,
                         style = get_opt("percent", "style"),
                         ...) {

  if(length(x) == 0) return("NA")
  # Table Creation
  if (is.factor(x)) {
    tbl <- table(x, useNA = useNA)

    if (length(tbl) > max_factor_length) {
      x <- factor(x,
                  levels(x)[1:max_factor_length],
                  exclude = NULL)
      x <- addNA(x)  #- addNA modifies a factor by turning NA into an extra level
      tbl <- table(x)
    }
  }
  else if (is.logical(x)) {
    x <- factor(x, c(TRUE, FALSE), c("true", "false"))
    tbl <- table(x, useNA = useNA)
  }
  else {
    xt <- factor(x)
    if (nlevels(xt) > max_factor_length){
      stop("Hier kommen viele unterschiedliche Zahlen!
           class = ", class(xt), " nlevels = ", nlevels(xt))}
    else
      tbl <- table(xt, useNA = useNA)
  }

  rslt <-
    rndr_percent(
      x = as.vector(prop.table(tbl)) * 100,
      n = as.vector(tbl),
      digits = digits,
      style = style
    )

  names(rslt) <- names(tbl)

  if(any(is.na(names(rslt))))
    names(rslt)[is.na(names(rslt))] <- "n.a."

  rslt
}

#' @noRd
ratio_tbll <- function(x, n = length(x), sep = ":") {
  rslt <- table(x)
  data.frame(
    lev = paste0("(", paste(names(rslt), collapse = sep), ")"),
    n = n,
    m = paste(rslt, collapse = sep),
    stringsAsFactors = FALSE
  )
}


# helper ------------------------------------------------------------------


#' @noRd
names_option <- function(rslt_all) {
  names(rslt_all)[1] <- get_opt("table", "stubhead")
  if (names(rslt_all)[2] == "m")
    names(rslt_all)[2] <- get_opt("table", "measure.name.m")
  else  if (names(rslt_all)[2] == "Total")
    names(rslt_all)[2] <- get_opt("table", "measure.name.total")

  if (names(rslt_all)[ncol(rslt_all)] == "statistics")
    names(rslt_all)[ncol(rslt_all)] <-
      get_opt("table", "measure.name.statistics")

  rslt_all
}


#' @description Kopie von interaction()
#'  die Labels werden anderst sortiert.
#'
#'  i lauft in umgekehrter richtung und past ist auch umgedreht
#'  ansonsten identich mit interaction
#'
#' @param ...	the factors for
#' @param sep	string to construct the new level labels by joining the constituent ones.
#'
#' @examples
#'  \donttest{
#' interaction2(
#' gl(2, 8, labels = c("Z", "X")),
#' gl(2, 8, labels = c( "A","B")),
#' gl(2, 8, labels = c( "a","b"))
#' )
#' }
#' @noRd
interaction2 <- function(...,
                         sep = "_") {
  args <- list(...)
  narg <- length(args)
  if (narg < 1L)
    stop("No factors specified")
  if (narg == 1L && is.list(args[[1L]])) {
    args <- args[[1L]]
    narg <- length(args)
  }

  for (i in 1L:narg) {
    f <- as.factor(args[[i]])[, drop = FALSE]
    l <- levels(f)
    if1 <- as.integer(f) - 1L

    if (i == 1) {
      ans <- if1
      lvs <- l
    }
    else {
      ans <- ans * length(l) + if1
      lvs <- paste(rep(lvs, each = length(l)),
                   rep(l, length(lvs)),
                   sep = sep)

      if (anyDuplicated(lvs)) {
        ulvs <- unique(lvs)
        while ((i <- anyDuplicated(flv <- match(lvs,  ulvs)))) {
          lvs <- lvs[-i]
          ans[ans + 1L == i] <- match(flv[i], flv[1:(i - 1)]) - 1L
          ans[ans + 1L > i] <- ans[ans + 1L > i] - 1L
        }
        lvs <- ulvs
      }
    }
  }
  structure(as.integer(ans + 1L),
            levels = lvs,
            class = "factor")
}

#' liste als DF
#'
#' @noRd
list_rbind <- function(l)
  as.data.frame(do.call(rbind, (l)))
