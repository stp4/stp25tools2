#' Summarise data with flexible aggregation and optional totals
#'
#' @description
#' An extended version of [stats::aggregate()] that works seamlessly with wide or long
#' data formats via [Long()]. It supports formula or variable name syntax, optional
#' label handling, and the addition of total/margins rows.
#'
#' @param ... Variables or a formula passed to [Long()] to reshape data into long format
#'   before summarisation.
#' @param fun Function to apply to each group (default: counts non-missing values via
#'   `function(x) length(na.omit(x))`).
#' @param key Name of the column to hold variable names after reshaping (default: `"variable"`).
#' @param value Name of the column to hold measured values after reshaping (default: `"value"`).
#' @param na.action Function for handling missing values during aggregation
#'   (default: [na.pass]).
#' @param formula Optional formula for reshaping the output to wide format.
#' @param include.total Logical; if `TRUE`, add totals across specified grouping variables.
#' @param margins Logical or formula; if `TRUE`, compute totals by `key`, or if a formula is supplied,
#'   use it to specify how margins are calculated.
#' @param margins_name Character string for the label used in total rows (default: `"Total"`).
#' @param include.label Logical; if `TRUE`, preserve variable labels in reshaping (passed to [Long()]).
#' @param  names_vary "slowest" "fastest" pivot_wider: When names_from identifies a column (or columns) with multiple unique values
#'
#' @return A [tibble::tibble] containing the summarised data, optionally with totals and/or wide reshaping.
#'
#' @details
#' The function:
#' 1. Reshapes data to long format with [Long()].
#' 2. Aggregates values by the specified formula or inferred grouping variables.
#' 3. Optionally computes and appends totals (margins).
#' 4. Optionally reshapes the result back to wide format via [tidyr::pivot_wider()].
#'
#' If `fun` returns a matrix (e.g., multiple statistics), each statistic will be expanded into its own column.
#'
#' @note Documentation created with assistance from ChatGPT.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7.1, 6.2, 8, 6.2, 9.4),
#'   B = c(6, 7.3, 8, 5, 6.9, 7)
#' )
#'
#' Summarise(A + B ~ student, df,
#'           fun = function(x) render_f(
#'             c(mean = mean(x), sd = sd(x)),
#'             digits = 2
#'           ))
#'
#' # Example output:
#'
#' # student variable mean  sd
#' # <chr>   <fct>    <chr> <chr>
#' # 1 Amy     A        7.43  1.43
#' # 2 Amy     B        7.10  1.01
#' # 3 Bob     A        7.87  1.60
#' # 4 Bob     B        6.30  1.13
#'
#'
#'  Summarise(
#'   A + B ~ student,
#'   df,
#'   fun = function(x)
#'     render_f(c(mean = mean(x), sd = sd(x)), digits = 2),
#'   include.total = TRUE,
#'   key = "Note" ,
#'   formula = student ~ Note
#' )
#'
#'
#' Summarise(
#'   A + B ~ student,
#'   df,
#'   fun = function(x)
#'     render_f(c(mean = mean(x), sd = sd(x)), digits = 2),
#'   include.total = value ~ 1 ,
#'   key = "Note" ,
#'   # formula = student ~ Note
#' )
Summarise <- function(...,
                      fun = function(x) length(na.omit(x)),
                      key = "variable",
                      value = "value",
                      na.action = na.pass,
                      formula = NULL,
                      include.total = FALSE,
                      margins = include.total,
                      margins_name = "Total",
                      include.label = TRUE,
                      names_vary = "slowest") {

  # Hilfsfunktion: Aggregate mit Standardparametern
  run_aggregate <- function(f, data) {
    aggregate(f, data, FUN = fun, na.action = na.action)
  }

  # Hilfsfunktion: Letzte Spalte auftrennen, falls Matrix
  split_matrix_column <- function(df) {
    last_col <- df[[ncol(df)]]
    if (is.matrix(last_col)) {
      cbind(df[-ncol(df)], last_col)
    } else {
      names(df)[ncol(df)] <<- value
      df
    }
  }

  # Long-Format erzeugen
  long_data <- Long(..., key = key, value = value, use.label = include.label)

  # Standardformel erstellen
  default_formula <- formula(
    paste(value, "~", paste(names(long_data)[-ncol(long_data)], collapse = "+"))
  )

  # Hauptaggregation
  summary_data <- run_aggregate(default_formula, long_data)
  summary_data <- split_matrix_column(summary_data)
  summary_data <- summary_data[order(summary_data[[1]]), ]

  # Margins (Total-Zeilen) hinzufugen
  if (isTRUE(margins) || inherits(margins, "formula")) {

    if (!inherits(margins, "formula")) {
      margins <- if (ncol(long_data) == 2) {
        formula(paste(value, "~1"))
      } else {
        formula(paste(value, "~", key))
      }
    }

    margins_data <- run_aggregate(margins, long_data)
    margins_data <- split_matrix_column(margins_data)


    # Position & Namen fur Total
    pos_total <- seq_len(nrow(margins_data)) + nrow(summary_data)
    summary_data <- dplyr::bind_rows(summary_data, margins_data)

    for (col_idx in seq_len(which(names(summary_data) == key))) {
      if (all(is.na(summary_data[pos_total, col_idx]))) {
        if (is.factor(summary_data[[col_idx]])) {
          summary_data[[col_idx]] <- factor(summary_data[[col_idx]],
                                            c(margins_name, levels(summary_data[[col_idx]])))
        }
        summary_data[pos_total, col_idx] <- margins_name
      }
    }
  }

  # Wide-Format falls Formel vorhanden
  if (!is.null(formula)) {
    #lhs <- LHS(formula)
    rhs <- RHS(formula)

    if (length(setdiff(all.vars(formula), names(summary_data))) > 0) {
      stop("Hallo! Die variablen mussen schon existieren!",
           paste(setdiff(
             all.vars(formula), names(summary_data)
           ), collapse = ", "))
    }

    values_from <-
      setdiff(names(summary_data), all.vars(formula))

    print(list( names_from =  (rhs),
                values_from =   (values_from),

                names_vary = names_vary ))

    summary_data <- tidyr::pivot_wider(summary_data,
                                       names_from = tidyselect::all_of(rhs),
                                       values_from =  tidyselect::all_of(values_from),                                       ,
                                       names_vary = names_vary)
  }

  # Fehlende Spaltennamen ersetzen
  if (any(is.na(names(summary_data)))) {
    names(summary_data)[is.na(names(summary_data))] <- "NA"
  }

  summary_data
}


#' @rdname Summarise
#'
#' @description
#' Summarizes Likert scale data into a structured format suitable for plotting.
#' Groups items and calculates frequency distributions.
#'
#'
#' @return A \code{likert_summary} object (tibble) with grouped Likert data
#'
#' @export
#'
Summarise_likert <- function(...){
  result  <- aggregat_likert(...)
  class(result) <- c("stp_likert", class(result))
  return(result)
}




#' @export
#' @rdname Summarise
Summarise_likert_long <- function(...){
  aggregat_likert(..., type.wide = FALSE)

}


#' @export
#' @rdname Summarise
Summarise_likert_wide <- function(...){
  aggregat_likert(..., type.wide = TRUE)

}

#' @rdname Summarise
#' @export
Summarise_multi <-
  function(...,
           type.wide = FALSE,
           use.level = 1,
           reverse.levels = FALSE,
           reorder.levels = NA) {

    result <-
      aggregat_likert(
      ...,
      type.wide = type.wide,
      use.level = use.level,
      reverse.levels = reverse.levels
    )
    class(result) <- c("stp_multi", class(result))
    return(result)
  }



# Aggregate Funktion ------------------------------------------------------


#'
#' @param grouping vector.
#' @param include.total logical.
#' @param include.order logical.
#' @param decreasing logical.
#' @param reverse.levels,exclude.levels,reorder.levels levels manupulieren
#' @param type.wide welches Format
#' @param use.level multiresponse
#' @param prepare.data  prepare.data-Objekt
#'
#' @rdname Summarise
aggregat_likert <-
  function(...,
           grouping = NULL,
           include.total = FALSE,
           include.order = FALSE,
           decreasing = FALSE,
           reverse.levels = FALSE,
           exclude.levels = NULL,
           reorder.levels = NA,
           type.wide = TRUE,
           use.level = NULL,
           prepare.data = NULL
  ) {
    if(is.null(prepare.data)){
      prepare.data <- prepare_data(...)
    }
    #cat("\n aggregat_likert\n")
    headings <- grepl("h__(.+)__h", prepare.data$measure.vars)
    pos_heading <- which(headings)
    pos_measure <- which(!headings)

    # bastel aus sub_haeding das groupings
    if (length(pos_heading) >= 1L) {
      for (i in seq_along(pos_heading)) {
        pos_h <- pos_heading[i]
        pos_start <- pos_h + 1
        pos_end <- pos_heading[i + 1] - 1
        if (is.na(pos_end))
          pos_end <- pos_measure[length(pos_measure)]
        nms <- as.vector(prepare.data$row_name[pos_h])

        grouping[[nms]] <- prepare.data$measure.vars[pos_start:pos_end]
      }
      prepare.data$measure.vars <-  prepare.data$measure.vars[pos_measure]
    }

    prepare.data$formula = to_formula(prepare.data$measure.vars,
                                      all.vars(prepare.data$by))
    prepare.data$grouping = grouping

    if (!is.null(exclude.levels)) {
      if (!is.na(reorder.levels))
        stop(" reorder.levels in kombination mit exclude.levels geht nicht!")
      reorder.levels <- exclude.levels * (-1)
    }

    tbll <- data_to_sum(
      prepare.data$formula,
      prepare.data$data,
      prepare.data$by,
      use.level = use.level,
      reverse.levels = reverse.levels,
      include.total = include.total,
      reorder.levels = reorder.levels
    )

    tbll$mean <- data_to_mean(
      prepare.data$formula,
      prepare.data$data,
      prepare.data$by,
      use.level = use.level,
      reverse.levels = reverse.levels,
      include.total = include.total,
      reorder.levels = reorder.levels
    )

    if (!is.null(prepare.data$grouping)) {
      tbll$grouping <- TRUE
      # Workaraund um die Namen zu extrahieren
      itm <- grp <-
        data_to_sum(
          prepare.data$formula,
          prepare.data$data,
          include.label = FALSE,
          use.level = use.level,
          reverse.levels = reverse.levels,
          include.total = include.total
        )$freq$Item

      # Schleife duch die unterschiedlich langen labels
      for (i in seq_along(prepare.data$grouping)) {
        new_lvl <- names(prepare.data$grouping)[i]
        for (k in prepare.data$grouping[[i]])
          levels(grp)[levels(grp) == k] <- new_lvl
      }

      tbll$freq <-
        dplyr::bind_cols(`.grouping` = grp, tbll$freq)

      # um konsistent mit Tbll_Likert zu bleiben
      tbll$lhs <- seq_len(length(tbll$lhs) + 1)

      # cat("\nin summary\n")
      if (length (all.names(tbll$formula[3])) == 1) {
        tbll$formula <- Item ~ . | .grouping
        # "hier blauch ich keine formula"
        tbll$facet_formula <-   ".grouping"
      }
      else{
        rhs_formula <- paste(all.vars(tbll$formula)[-c(1:2)], collapse = "+")
        tbll$formula <- formula(paste("Item~.|", rhs_formula, "+.grouping"))
        tbll$facet_formula <- formula(paste(".grouping ~", rhs_formula))
      }
    }
    else{
      tbll$grouping <- FALSE
      tbll$facet_formula <- NULL
    }

    dat_long  <- dat_wide <- tbll$freq
    tbll$names <- tbll$freq[tbll$lhs]
    tbll$freq  <- tbll$freq[-tbll$lhs]
    tbll$rhs   <- seq_len(ncol(tbll$freq)) + ncol(tbll$names)
    rhs_names <- paste0("LBL_", seq_along(tbll$rhs))
    names(dat_long)[tbll$rhs] <- rhs_names

    tbll$measure = names(tbll$freq)
    tbll$items =  "Item"
    tbll$groups =  setdiff(names(tbll$names), "Item")
    tbll$columns =  setdiff(tbll$groups, ".grouping")

    dat_long <-
      tidyr::pivot_longer(
        dat_long,
        cols = tidyselect::all_of(tbll$rhs),
        names_to = "levels",
        values_to = "Freq"
      )
    dat_long$levels <- factor(dat_long$levels, labels = tbll$levels)

    if (include.order) {
      warning("Nicht implemintiert")
    }

    if (type.wide) {
      attr(dat_wide, "tbll") <- tbll
      attr(dat_wide, "data_long") <- dat_long
      attr(dat_wide, "likert") <- "wide"
      return(dat_wide)
    }
    else {
      attr(dat_long, "tbll") <- tbll
      attr(dat_long, "data_wide") <- dat_wide
      attr(dat_long, "likert") <- "long"
      return(dat_long)
    }
  }



#' @noRd
data_to_sum  <- function(...,
                         include.total = FALSE,
                         use.level = NULL,
                         reverse.levels = FALSE,
                         reorder.levels = NA) {
  rslt <-
    Summarise(
      ...,
      fun = function(x) {
        if (is.logical(x)) {
          x <- factor(x, c(TRUE, FALSE))
        }
        else if (is.numeric(x)) {
          if (any(max(x, na.rm = TRUE) > 1))
            stop(
              "\nWenn Zahlen uebergeben werden duerfen die nur im Wertebereich von 0 bis 1 liegen!\n"
            )
          x <- factor(x, 1:0)
        }

        if (!is.null(use.level)) {
          x <- factor(x == levels(x)[use.level], c(TRUE, FALSE))
        }
        else if (reverse.levels) {
          x <- rev(x)
        }
        else if (!is.na(reorder.levels)) {
          if (is.numeric(reorder.levels))
            x <- factor(x, levels(x)[reorder.levels])
          else
            x <- factor(x, reorder.levels)
        }
        table(x, useNA = "no")
      },
      key = "Item",
      margins = include.total
      #value = "value",
      #na.action = na.pass,
      # formula = NULL,
      # margins_name = "Total",
      # include.label = TRUE
    )

  ncl <- ncol(rslt)
  col_names <- names(rslt)
  pos_col_names <- grep("Item", col_names)
  lhs <- c(1:pos_col_names)


  if (pos_col_names == 1) {
    fm <- Item ~ .
    facet_fm <- NULL
  }
  else {
    facet_fm <- paste(col_names[1:(pos_col_names - 1)], collapse = "+")
    fm <- formula(paste("Item ~ .|", facet_fm))
    facet_fm <- formula(paste("~", facet_fm))
  }

  list(
    freq = rslt,
    mean = NULL,
    nlevels = length(col_names[-lhs]),
    levels = col_names[-lhs],
    lhs = lhs,
    N = NULL,
    facet_formula = facet_fm,
    formula = fm
  )
}
#' @noRd
data_to_mean  <- function(...,
                          include.total = FALSE,
                          use.level = NULL,
                          reverse.levels = FALSE,
                          reorder.levels = NA) {
  Summarise(
    ...,
    fun = function(x) {
      if (is.logical(x)) {
        x <- factor(x, c(TRUE, FALSE))
      }
      else if (is.numeric(x)) {
        if (any(max(x, na.rm = TRUE) > 1))
          stop(
            "\nWenn Zahlen uebergeben werden duerfen die nur im Wertebereich von 0 bis 1 liegen!\n"
          )
        x <- factor(x, 1:0)
      }

      if (!is.null(use.level)) {
        x <- factor(x == levels(x)[use.level], c(TRUE, FALSE))
      }
      else if (reverse.levels) {
        x <- rev(x)
      } else if (!is.na(reorder.levels)) {
        if (is.numeric(reorder.levels))
          x <- factor(x, levels(x)[reorder.levels])
        else
          x <- factor(x, reorder.levels)
      }

      n <- length(x)
      x <-  na.omit(as.numeric(x))
      c(
        m = signif(mean(x, na.rm = TRUE),4),
        sd = signif(sd(x, na.rm = TRUE),4),
        n = length(x),
        missing = n - length(x)
      )
    },
    key = "Item",
    margins = include.total,
    #value = "value",
    #na.action = na.pass,
    # formula = NULL,
    # margins_name = "Total",
    include.label = TRUE
  )
}



#' Praevalenz
#'
#' Clopper Pearson method for confidence intervals
#'
#' @param x logical
#' @param by not used
#' @param ... not used
#' @export
#'
#' @return vector
#'
#' @examples
#'
#' DF<- data.frame(
#'   sex = gl(2, 50, labels = c("male", "female")),
#'   Adipositas = rbinom(n=100, 1, prob=.2),
#'   Bewegungsmangel =rbinom(n=100, 1, prob=.4),
#'   Nikotinabusus = rbinom(n=100, 1, prob=.3)
#' )
#'
#' DF |> Summarise(
#'   Adipositas,
#'   Bewegungsmangel,
#'   Nikotinabusus,
#'   #  by=~sex,
#'   key = "Risikofaktor",
#'   fun = Praevalenz,
#'   # formula= Risikofaktor ~ sex
#' )
#'
Praevalenz <- function(x, by, ...) {
  x <- na.omit(x)
  n <- length(x)
  a <- sum(x)
  rst <- stats::binom.test(a, n)
  Percent <- render_f(rst$estimate * 100, 0)
  CI.95 <-  paste( render_f(rst$conf.int * 100, digits = 1),collapse =", " )
  c('Anteil' = paste0(a, "/", n),
    'Praevalenz % [95%-CI]' = paste0(Percent,"% [", CI.95, "]"))
}



