#' Tbll_likert
#'
#' Create a summary table for Likert-type items.
#'
#' This function extracts and formats results from \code{Summarise_likert()}.
#' It provides flexible options to include mean values, counts,
#' percentages, missing data, and reference groupings.
#'
#' @param x A \code{data.frame} or a Likert object created by \code{Summarise_likert()}.
#' @param ... Additional arguments passed to \code{Summarise_likert()} or \code{prepare.data()}.
#'
#' @param include.reference Numeric. Defines how reference groups are calculated:
#'   * \code{2} → three groups (e.g. low, neutral, high)
#'   * \code{2.5} → two groups (e.g. disagree vs. agree)
#'   * \code{3} → three groups with the neutral category kept separate.
#'   Use together with \code{reference.labels}.
#' @param reference.labels Character vector of labels for the reference groups
#'   (default: \code{c("low", "neutral", "high")}).
#'
#' @param include.mean Logical. If \code{TRUE}, include mean values in the output.
#' @param include.n Logical. If \code{TRUE}, include the sample size per item.
#' @param include.na Logical. If \code{TRUE}, missing values are shown and included
#'   in the percentage calculation.
#' @param include.order Logical. If \code{TRUE}, items are sorted by their mean value.
#' @param decreasing Logical. If \code{TRUE} (default), sorting is in decreasing order.
#' @param include.percent Logical. If \code{TRUE}, percentages are included.
#' @param include.count Logical. If \code{TRUE}, counts are included.
#'
#' @param include.total Logical or string. If \code{TRUE} or a label (e.g. \code{"All"}),
#'   adds a row with total results across all groups.
#' @param reorder.levels Integer vector. Reorder factor levels using
#'   \code{factor(item, levels(item)[reorder.levels])}.
#' @param reverse.levels Logical. If \code{TRUE}, reverses the order of levels.
#' @param exclude.levels Integer. Position(s) of levels to exclude (e.g.
#'   \code{exclude.levels = 5} is equivalent to \code{reorder.levels = -5}).
#'
#' @return A tibble with grouped Likert summary statistics (percentages, counts,
#'   means, missing values, etc.).
#'
#' @export
#'
#' @examples
#' require(dplyr)
#' require(tibble)
#' set.seed(42)
#'
#' levels <- c(
#'   "Strongly disagree", "Disagree", "Neither agree nor disagree",
#'   "Agree", "Strongly agree")
#'
#' n <- 2*3*9*3
#' DF_lik <- tibble(
#'   q1 = sample(levels, n, replace = TRUE, prob = c(3,2, 1, 4,5)),
#'   q2 = sample(levels, n, replace = TRUE, prob = c(3,1, 1, 1,1)),
#'   q3 = sample(levels, n, replace = TRUE, prob = c(7,5, 1, 0,0)),
#'   q4 = sample(levels, n, replace = TRUE, prob = c(0,0, 0, 1,5)),
#'   q5 = sample(levels, n, replace = TRUE, prob = c(1,4, 1, 1,5)),
#'   q6 = sample(levels, n, replace = TRUE, prob = c(2,3, 0, 2,3)),
#'   q7 = sample(levels, n, replace = TRUE, prob = c(3,2, 0, 1,7)),
#'   q8 = sample(levels, n, replace = TRUE, prob = c(1,2, 1, 4,1)),
#'   q9 = sample(levels, n, replace = TRUE, prob = c(6,0, 0, 0,6))
#' ) |>
#'   mutate(across(everything(), ~ factor(.x, levels = levels))) |>
#'   mutate(Sex = factor(sample(c("male", "female"), n, replace = TRUE)),
#'          Age = factor(sample(c("18-30", "30-50", ">50"), n, replace = TRUE)))
#'
#' # Basic usage:
#' DF_lik |>
#'   Summarise_likert(q1, q2, q3, q4, by = ~ Sex) |>
#'   Tbll_likert()
#'
#' # With ordering
#' DF_lik |>
#'   Tbll_likert(q1, q2, q3, q4, by = ~ Sex, include.order = TRUE)
#'
#' # Using reference groups
#' DF_lik |>
#'   Tbll_likert(
#'     "FC.2", q1, q2,
#'     "FC.3", q3, q4, q5,
#'     "FC.4", q6, q7, q8, q9,
#'     by = ~ Sex,
#'     include.count = FALSE,
#'     include.reference = 2.5,
#'     reference.labels = c("disagree", "agree"),
#'     include.total = TRUE,
#'     include.n = TRUE,
#'     include.na = TRUE,
#'     include.mean = TRUE)

Tbll_likert  <-
  function(x,
           ...,
           include.reference = NULL,
           include.mean = TRUE,
           include.n = FALSE,
           include.na = FALSE,
           include.order = FALSE,
           include.percent = TRUE,
           include.count = TRUE,
           include.total = FALSE,
           exclude.levels = NULL,
           decreasing = TRUE,
           reference.labels = c("low", "neutral", "high"),
           reverse.levels = FALSE,
           reorder.levels = NA) {

    if (is.null(attr(x, "likert"))) {
      x <- Summarise_likert(
        x,...,
        reverse.levels = reverse.levels,
        reorder.levels = reorder.levels,
        include.total = include.total,
        exclude.levels = exclude.levels
      )
    }

    extract_likert(
      x,
      include.reference = include.reference,
      include.mean = include.mean,
      include.n = include.n,
      include.na = include.na,
      include.order = include.order,
      include.percent = include.percent,
      include.count = include.count,
      reference.labels = reference.labels,
      decreasing = decreasing
    )
  }


extract_likert <-
  function(x,
           include.reference = NULL,
           include.mean = TRUE,
           include.n = FALSE,
           include.na = FALSE,
           include.order = FALSE,
           include.percent = TRUE,
           include.count = TRUE,
           reference.labels = c("low", "neutral", "high"),
           decreasing = TRUE,
           ...) {
    rslt <-  attr(x, "tbll")
    note <- NULL

    if (!is.null(include.reference)) {
      if (length(reference.labels) == 2)
        reference.labels <- c(reference.labels[1], "neutral", reference.labels[2])

      if (is.character(include.reference))
        include.reference <- which(rslt$levels %in% include.reference)
      else if (!is.numeric(include.reference))
        include.reference <- median(seq_len(rslt$nlevels))

      if (ceiling(include.reference) == floor(include.reference)) {
        lowrange <- seq_len((include.reference - 1))
        neutral <- include.reference
        highrange <- (include.reference + 1):rslt$nlevels

        freq <- data.frame(
          lowrange =  sum_range(rslt$freq[, lowrange]),
          neutral =   rslt$freq[, neutral],
          highrange = sum_range(rslt$freq[, highrange]),
          stringsAsFactors = FALSE
        )
        if (is.null(note))
          note <-
          paste(
            "lowrange:",
            paste(rslt$levels[lowrange], "\n", collapse = "|"),
            "neutral:",
            paste(rslt$levels[neutral], "\n", collapse = "|"),
            "highrange:",
            paste(rslt$levels[highrange], collapse = "|")
          )

        names(freq) <-
          c(
            paste0(reference.labels[1], "(1:", include.reference - 1, ")"),
            paste0(reference.labels[2], "(", include.reference, ")"),
            paste0(
              reference.labels[3],
              "(",
              include.reference + 1,
              ":",
              rslt$nlevels,
              ")"
            )
          )

        rslt$freq <- freq

      } else{
        lowrange <- seq_len(floor(include.reference))
        highrange <- ceiling(include.reference):rslt$nlevels

        freq <- data.frame(
          lowrange =  sum_range(rslt$freq[, lowrange]),
          highrange = sum_range(rslt$freq[, highrange]),
          stringsAsFactors = FALSE
        )

        names(freq) <-
          c(
            paste0(reference.labels[1], "(1:", floor(include.reference), ")"),
            paste0(
              reference.labels[3],
              "(",
              ceiling(include.reference),
              ":",
              rslt$nlevels,
              ")"
            )
          )

        if (is.null(note))
          note <-
          paste(
            "lowrange:",
            paste(rslt$levels[lowrange], "\n", collapse = "|"),
            "highrange:",
            paste(rslt$levels[highrange], collapse = "|")
          )
        rslt$freq <- freq
      }
    }


    # Missing: hier werden die Prozent inclusive der NAs berechnet.
    if (include.na) {
      rslt$freq$Missing <- rslt$mean$missing
      rslt$mean$n <-  rowSums(rslt$freq)
    }
    nms_freq <- names(rslt$freq)
    if (include.percent & include.count) {
      rslt$freq <-
        data.frame(Map(rndr_percent, rslt$freq / rslt$mean$n * 100, rslt$freq))
      names(rslt$freq) <- nms_freq
    }
    else if (include.percent) {
      rslt$freq <-
        data.frame(Map(
          rndr_percent,
          rslt$freq / rslt$mean$n * 100,
          rslt$freq,
          style = 3
        ))
      names(rslt$freq) <- nms_freq
    }


    if (include.n)
      rslt$freq$n <- rslt$mean$n

    if (include.mean)
      rslt$freq$Average <- rndr_mean(rslt$mean$m, rslt$mean$sd)


    rslt$freq <- cbind(rslt$names, rslt$freq)

    if (include.order) {
      rslt$freq <-  rslt$freq[
        order(rslt$mean$m, decreasing = decreasing),
        ]
      if (any(rslt$freq[[1]] %in% "Total")) {
        pos_total <- which(rslt$freq[[1]] %in% "Total")
        rslt$freq <- rslt$freq[
          c(seq_len(nrow(rslt$freq))[-pos_total], pos_total),
          ]
      }
    }

    rslt$freq <-
      prepare_output(rslt$freq,
                     caption = "Likert",
                     note = note,
                     N = rslt$N)
    attr(rslt$freq, "tbll_likert") <- x
    rslt$freq

  }

#' @noRd
sum_range <- function(x)
  if (is.vector(x)) x else rowSums(x, na.rm = TRUE)




# my personal preference, when dealing with likert scales, is to complement the
# presentation of the detailed responses with the so-called Dominant Opinion Index
# (don't remember who first came up with the idea) :
#
# DOI = (% positive - % negative)
#         x (% positive + % negative) =
#                    (% positive - % negative) x (100% - % neutral)
# # the formula becomes slightly more complicated if the intensity of opinion
# (e.g., agree vs strongly agree) is taken into account and the percentages are
# weighted based on that
#
# the index ranges from -100 (strongly negative) to +100 (strongly positive),
# with 0 midpoint as neutral
#
# details are there for whoever needs them (usually shown as diverging bars
# with extra neutrals), but I'm focused on the DOI
#
# https://jakec007.github.io/2021-06-23-R-likert/
# https://blog.datawrapper.de/divergingbars/



#' Dominant Opinion Index (DOI) für Likert-Skalen
#'
#' @param positive Prozentualer Anteil positiver Antworten (z. B. Agree + Strongly agree).
#' @param negative Prozentualer Anteil negativer Antworten (z. B. Disagree + Strongly disagree).
#' @param neutral Prozentualer Anteil neutraler Antworten (Default = 100 - positive - negative).
#'
#' @return Numerischer Wert zwischen -100 und +100.
#'
#' @examples
#' stp25tools2:::doi_likert(positive = 40, negative = 30, neutral = 30)
#'
doi_likert <- function(positive, negative, neutral = NULL) {
  if (is.null(neutral)) {
    neutral <- 100 - positive - negative
  }
  (positive - negative) * (100 - neutral) / 100
}

# Likert |>
#   Tbll_likert(q1, q2, q3, q4, by = ~ Sex, include.order = TRUE)

