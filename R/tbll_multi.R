#' Analyze multi-response items
#'
#' @description
#' Functions for analyzing multi-response items (Likert scales and multiple choice questions).
#'
#' @param ... Variables to analyze, can be specified as formula, variable names,
#'   or with headings for grouping
#' @param by Grouping variable(s) as formula
#' @param digits Number of digits for percentages
#' @param include.order Logical, whether to include ordering
#' @param exclude.last.order Logical, whether to exclude last order
#' @param use.level Which level to use for analysis (1 for TRUE, 2 for FALSE)
#' @param include.label Logical, whether to include variable labels
#' @param include.n Logical, whether to include sample size
#' @param include.nr Logical, whether to include number of responses
#' @param include.total Logical, whether to include total column
#' @param include.test Logical, whether to include statistical tests
#' @param include.normality.tests Logical, whether to include normality tests
#' @param include.custom Custom content to include
#' @param include.value Value to include
#' @return A tibble with analysis results
#'
#' @name Tbll_multi
#' @examples
#' DF <-
#'   structure(list(
#'     q2.almdd = structure(
#'     c(TRUE, FALSE, FALSE, TRUE,TRUE, TRUE, TRUE, TRUE,
#'     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#'                          label = "Alm Dudler"),
#'     q2.minrl = structure(
#'     c(TRUE, FALSE,FALSE, FALSE, TRUE, FALSE, FALSE,
#'     TRUE, TRUE, TRUE, FALSE, FALSE,TRUE, TRUE, TRUE),
#'                          label= "Mineral"),
#'     q2.cola = structure(
#'     c(TRUE,TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE,
#'     TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
#'                         label = "Cola"),
#'     q2.bier = structure(
#'     c(TRUE,FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
#'     FALSE, FALSE, FALSE,FALSE, TRUE, FALSE, FALSE, FALSE),
#'                         label = "Bier"),
#'     q2.wein = structure(
#'     c(TRUE,FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
#'     FALSE, FALSE, FALSE,FALSE, FALSE, FALSE, FALSE, FALSE),
#'                         label = "Wein")),
#'     row.names = c(NA,15L),
#'     class = "data.frame")
#'
#' DF$sex <- factor(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2),
#'                  1:2, c("f", "m"))
#'
#' # DF |> aggregat_likert(~ q2.almdd+q2.minrl+q2.cola+q2.bier+q2.wein,
#' #                       type.wide=TRUE)
#'
#' DF |> Summarise_multi(~ q2.almdd+q2.minrl+q2.cola+q2.bier+q2.wein,
#' type.wide=TRUE)
#'
#' DF |> Summarise_multi(~ q2.almdd+q2.minrl+q2.cola+q2.bier+q2.wein,
#' type.wide=FALSE)
#'
#' DF |>
#'   Summarise_multi("Soda (gas)", q2.almdd,
#'                   q2.minrl,q2.cola, "Alkohol", q2.bier,q2.wein, by=~sex)
#'
#' typ_name <-DF |>
#'   Tbll_multi("Soda (gas)", q2.almdd,
#'              q2.minrl,q2.cola, "Alkohol", q2.bier,q2.wein, by=~sex)
#'
#' type_formula <- DF |> Tbll_multi(
#' "Soda (gas)"+q2.almdd + q2.minrl + q2.cola +
#'  "Alkohol"+q2.bier +q2.wein ~sex)
#'
#' # type_formula
#' # typ_name
#'
#' attr(typ_name, "tbll_likert")
#'
#' attr(type_formula, "tbll_likert")
#' @rdname Tbll_multi
#' @export
Tbll_multi <- function(...,
                       by = NULL,
                       digits = 0,
                       include.order = TRUE,
                       exclude.last.order = FALSE,
                       use.level = 1,
                       include.label = TRUE,
                       include.n = TRUE,
                       include.nr = FALSE,
                       include.total = FALSE,
                       include.test = FALSE,
                       include.normality.tests = FALSE,
                       include.custom = NULL,
                       include.value = NULL) {
  rslt <-  Tbll_desc(
    ...,
    by = by,
    include.label = include.label,
    include.n = include.n,
    include.nr = include.nr,
    include.total = include.total,
    include.test = include.test,

    include.normality.tests = include.normality.tests,    include.multiresponse = TRUE,
    include.custom = include.custom,
    include.value = include.value,
    digits = digits,
    use.level = use.level,
    include.prepare.data = TRUE
  )

  attr(rslt, "tbll_likert") <-
    aggregat_likert(
                    include.total = include.total,
                    use.level = use.level,
                    prepare.data =attr(rslt, "prepare_data"),
                    reverse.levels = FALSE,
                    reorder.levels = NA
                    )
  attr(rslt, "prepare_data") <- NULL

  rslt
}

