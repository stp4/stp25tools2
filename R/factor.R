# reorder2 <- function(x, X, ...,
#                         decreasing = TRUE,
#                         last = NULL,
#                         threshold = NULL,
#                         threshold.na.strings = "Other",
#                         reverse = FALSE) {
#
#   if (!is.factor(x)) stop("`x` muss ein factor sein.")
#   # Label erhalten (falls vorhanden)
#   lbl <- attr(x, "label", exact = TRUE)
#
#   x_new <- x
#
#   # --- Fall: kein X übergeben => Häufigkeitssortierung (+ optional Lumping) ---
#   if (missing(X)) {
#     x_new <- forcats::fct_infreq(x_new, ordered = TRUE)
#
#     if (!is.null(threshold)) {
#       # Erkenne "count-like" (Integer oder numerisch ganzzahlig >= 1)
#       # is_count_like <- is.integer(threshold) ||
#       #   (is.numeric(threshold) && threshold >= 1 && floor(threshold) == threshold)
#
#       if (threshold >= 1) {
#         min_count <- as.integer(threshold)
#         x_new <- forcats::fct_lump_min(x_new, min = min_count, other_level = threshold.na.strings)
#       } else {
#         prop <- as.numeric(threshold)
#         if (!(prop > 0 && prop < 1)) {
#           stop("Wenn `threshold` nicht als count interpretiert wird, muss es eine proportion sein: 0 < threshold < 1.")
#         }
#         x_new <- forcats::fct_lump_prop(x_new, prop = prop, other_level = threshold.na.strings)
#       }
#     }
#
#   } else {
#     # --- Fall: X übergeben ---
#
#
#     if (is.numeric(X)) {
#     if (length(x) != length(X)) stop("`X` muss die gleiche Länge wie `x` haben.")
#       # klassisches Reorder anhand einer numerischen Hilfsvariable
#       x_new <- forcats::fct_reorder(x_new, X, ..., .desc = decreasing)
#
#     } else if (is.character(X) || is.factor(X)) {
#       # Interpretiere X als gewünschte Reihenfolge der Levels (erste Vorkommen bestimmt Reihenfolge)
#       levs <- unique(as.character(X))
#       levs <- intersect(levs, levels(x_new))
#       missing_levs <- setdiff(levels(x_new), levs)
#       if (length(missing_levs) > 0) {
#         levels(x_new)[(levels(x_new) %in% missing_levs)] <- threshold.na.strings
#         levs <- c(levs, threshold.na.strings)
#       }
#       x_new <- forcats::fct_relevel(x_new, levs)
#
#
#
#     } else {
#       stop("`X` muss numerisch, factor oder character sein.")
#     }
#   }
#
#   # Reverse levels (falls gewünscht)
#   if (isTRUE(reverse)) x_new <- forcats::fct_rev(x_new)
#
#   # Bestimmte Levels ans Ende schieben
#   if (!is.null(last)) {
#     for (lvl in last) {
#       x_new <- forcats::fct_relevel(x_new, lvl, after = Inf)
#     }
#   }
#
#   # Label wieder anhängen (falls vorhanden)
#   if (!is.null(lbl)) attr(x_new, "label") <- lbl
#
#   x_new
# }
# # reorder2 <- function(x,
# #                      X,
# #                      ...,
# #                      decreasing = TRUE,
# #                      last = NULL,
# #                      threshold = NULL,
# #                      threshold.na.strings = "Other",
# #                      reverse = FALSE) {
# #   lbl <-  attr(x, "label")
# #
# #   if (!reverse) {
# #
# #     if (missing(X)) {
# #       if (is.null(threshold)) {
# #         x <-
# #           factor(x, levels(x)[order(table(x), decreasing = decreasing)])
# #       }
# #       else {
# #         xt <- table(x)
# #
# #         if (is.na(threshold.na.strings)) {
# #           x <-
# #             factor(x, names(sort(xt[xt > threshold], decreasing = decreasing)))
# #         }
# #         else{
# #           x.names <- names(sort(xt, decreasing = decreasing))
# #           lvl <-
# #             names(sort(xt[xt > threshold], decreasing = decreasing))
# #           lvl <-
# #             c(lvl , rep(threshold.na.strings, length(x.names) - length(lvl)))
# #
# #           x <-
# #             factor(x, x.names, lvl)
# #         }
# #       }
# #
# #       if (!is.null(last))
# #         for (ref in last)
# #           x <- relevel2(x, ref)
# #     }
# #     # klassiche reorder
# #     else{
# #       x <- stats::reorder(x, X, ...)
# #        if (!is.null(last))
# #        for (ref in last)
# #          x <- stats::relevel(x, ref)
# #     }
# #   } else {
# #     x <- factor(x, rev(levels(x)))
# #   }
# #
# #   attr(x, "label") <- lbl
# #   x
# # }
#
# # helper
# # letzter level am ende
# # copie von relevel ohne fehlerpruefung
# # relevel2 <- function(x,
# #                      last,
# #                      lev = levels(x))  {
# #   ref <- match(last, lev)
# #   nlev <- length(lev)
# #   factor(x, levels = lev[c(seq_along(lev)[-ref], ref)], exclude = NULL)
# # }
#
#
#
#
#
#
# #
# #
# #
# #
# # x <-
# #   c(
# #     rep(1, 21),rep(2, 120),rep(3, 28),rep(4, 4),
# #     rep(5, 56),rep(6, 2),rep(7, 92),
# #     rep(8, 42),rep(9, 74),rep(10, 20),NA
# #   )
# #
# # x <- factor(x, 1:10, letters[1:10])
# #
# #
# #  table(x)
# #  # a   b   c   d   e   f   g   h   i   j
# #  # 21 120  28   4  56   2  92  42  74  20
# #
# #  table(reorder2(x))
# #  table(reorder2(x))
# # # b   g   i   e   h   c   a   j   d   f
# # # 120  92  74  56  42  28  21  20   4   2
# #
# #  table(reorder2(x, threshold = 30))
# #  ## 2) Mit threshold
# #  table(reorder2(x, threshold = 30))
# # # b     g     i     e     h Other
# # # 120    92    74    56    42    75
# #
# #
# #   table(reorder2(x, last ="a"))
# #   table(reorder2(x, last ="a"))
# # #  b   g   i   e   h   c   j   d   f   a
# # #  120  92  74  56  42  28  20   4   2  21
# #
# #     table(reorder2(x, last = c("a", "b")))
# #     table(reorder2(x, last = c("a", "b")))
# # #  g   i   e   h   c   j   d   f   a   b
# # #  92  74  56  42  28  20   4   2  21 120
# #
# #
# #
#
# #
# #     x <- c(
# #       rep(1, 21),rep(2, 120),rep(3, 28),rep(4, 4),
# #       rep(5, 56),rep(6, 2),rep(7, 92),
# #       rep(8, 42),rep(9, 74),rep(10, 20),NA
# #     )
# #     x <- factor(x, 1:10, letters[1:10])
# #
# #     ## 1) Standard Häufigkeitssortierung
# #     table(reorder2(x))
# #     # b g i e h c a j d f
# #
# #     ## 2) Mit threshold
# #     table(reorder2(x, threshold = 30))
# #     # b g i e h Other
# #
# #     ## 3) Numerisches X: sortiere nach Mittelwert
# #     set.seed(1)
# #     X_num <- rnorm(length(x))
# #     table(reorder2(x, X_num))
# #     # Levels werden anhand mean(X_num | x) sortiert
# #
# #     ## 4) Character X: direkte Reihenfolge
# #     X_char <- c("i","h","b","a")
# #     table(reorder2(x, X_char))
# #     # Reihenfolge folgt X_char
# #
# #     levels(reorder2(x, X_char))
#

#' Reorder factor levels by frequency, auxiliary variable, or custom rules
#'
#' Wrapper um forcats::fct_infreq(), fct_reorder(), fct_lump_min(),
#' fct_lump_prop(), fct_relevel() und fct_collapse(), der automatisch den
#' passenden Weg wählt.
#'
#' @param x Ein Faktor.
#' @param decreasing Logical. Bei TRUE (default) wird absteigend sortiert.
#' @param last Character-Vektor von Levelnamen, die ans Ende verschoben werden.
#' @param threshold Numeric oder integer.
#'   - NULL: kein Lumping
#'   - integer oder "integer-like": benutze fct_lump_min(min = threshold)
#'   - proportion (0 < threshold < 1): benutze fct_lump_prop(prop = threshold)
#' @param threshold.na.strings Name der Sammelkategorie beim Lumping. Default "Other".
#' @param reverse Logical. TRUE → Levelreihenfolge umkehren.
#' @param ... Optional:
#'   - Ein numeric Vektor (gleich lang wie x) → fct_reorder()
#'   - Ein character/factor Vektor (gleich lang wie x) → fct_relevel(), fehlende Levels → "Other"
#'   - Ein oder mehrere benannte Character-Vektoren → fct_collapse()
#'
#' @return Faktor mit geänderter Levelreihenfolge
#' @export
#'
#' @examples
#' x <- factor(rep(1:5, c(10,20,5,2,8)), labels = letters[1:5])
#'
#' # Frequenzsortierung
#' reorder2(x)
#'
#' # Mit Lumping per Count
#' reorder2(x, threshold = 10)
#'
#' # Mit Lumping per Proportion
#' reorder2(x, threshold = 0.2)
#'
#' # Numerisches Hilfsargument
#' set.seed(1)
#' reorder2(x, rnorm(length(x)))
#'
#' # Character-Vektor als Reihenfolge
#' reorder2(x, c("b","a"))
#'
#' # Collapse mit benannten Character-Vektoren
#' reorder2(x,
#'   small = c("d","e"),
#'   big   = c("a","b","c")
#' )
reorder2 <- function(x,
                        ...,
                        decreasing = TRUE,
                        last = NULL,
                        threshold = NULL,
                        threshold.na.strings = "Other",
                        reverse = FALSE) {

  if (!is.factor(x)) stop("`x` muss ein factor sein.")
  lbl <- attr(x, "label", exact = TRUE)

  dots <- list(...)

  # --- Fall 1: kein ... übergeben -> Häufigkeit + evtl. Lumping ---
  if (length(dots) == 0) {

    x_new <- forcats::fct_infreq(x, ordered = TRUE)

    if (!is.null(threshold)) {
      is_count_like <- is.integer(threshold) ||
        (is.numeric(threshold) && threshold >= 1 && floor(threshold) == threshold)

      if (is_count_like) {
        x_new <- forcats::fct_lump_min(x_new, min = as.integer(threshold),
                                       other_level = threshold.na.strings)
      } else {
        prop <- as.numeric(threshold)
        if (!(prop > 0 && prop < 1)) {
          stop("Wenn `threshold` keine ganze Zahl ist, muss es eine proportion (0<prop<1) sein.")
        }
        x_new <- forcats::fct_lump_prop(x_new, prop = prop,
                                        other_level = threshold.na.strings)
      }
    }


  } else if (length(dots) == 1 && is.null(names(dots))) {
    # --- Fall 2: ein unbenannter Vektor ---
    X <- dots[[1]]


    if (is.numeric(X)) {
      if (length(X) != length(x)) stop("`X` muss gleiche Länge wie `x` haben.")
      x_new <- forcats::fct_reorder(x, X, .desc = decreasing)
    } else if (is.character(X) || is.factor(X)) {

       # Interpretiere X als gewünschte Reihenfolge der Levels (erste Vorkommen bestimmt Reihenfolge)
      levs <- unique(as.character(X))
      x_levs <- levels(x)
      levs <- intersect(levs, x_levs)
      missing_levs <- setdiff(x_levs, levs)
      if (length(missing_levs) > 0) {
        levels(x)[x_levs %in% missing_levs] <- threshold.na.strings
        levs <- c(levs, threshold.na.strings)
      }
      x_new <- forcats::fct_relevel(x, levs)
      # levs <- unique(as.character(X))
      # missing_levs <- setdiff(levels(x), levs)
      #
      # if (length(missing_levs) > 0) {
      #   x_tmp <- forcats::fct_collapse(x,
      #                                  !!threshold.na.strings := missing_levs)
      # } else {
      #   x_tmp <- x
      # }
      # levs_final <- c(levs, setdiff(levels(x_tmp), levs))
      # x_new <- forcats::fct_relevel(x_tmp, levs_final)
    } else {
      stop("Unbekannter Typ in `...`: muss numeric, character oder factor sein.")
    }

  } else if (!is.null(names(dots))) {
    # --- Fall 3: mehrere benannte Character-Vektoren -> fct_collapse ---
    collapse_list <- dots
    if (!all(vapply(collapse_list, is.character, logical(1)))) {
      stop("Bei benannten ... müssen die Werte character-Vektoren sein.")
    }
    x_new <- forcats::fct_collapse(x, !!!collapse_list)

  } else {
    stop("Ungültige Eingabe in `...`.")
  }


  # Reverse
  if (isTRUE(reverse)) x_new <- forcats::fct_rev(x_new)

  # Levels ans Ende verschieben
  if (!is.null(last)) {
    for (lvl in last) {
      x_new <- forcats::fct_relevel(x_new, lvl, after = Inf)
    }
  }

  if (!is.null(lbl)) attr(x_new, "label") <- lbl

  x_new
}


#
# x <- factor(rep(1:5, c(10,20,5,2,8)), labels = letters[1:5])
#
# # Frequenzsortierung
# reorder2(x)
#
# # Mit Lumping per Count
# reorder2(x, threshold = 10)
#
# # Mit Lumping per Proportion
# reorder2(x, threshold = 0.2)
#
# # Numerisches Hilfsargument
# set.seed(1)
# reorder2(x, rnorm(length(x)))
#
# # Character-Vektor als Reihenfolge
# reorder2(x, c("b","a"))
#
# # Collapse mit benannten Character-Vektoren
# reorder2(x,
#   small = c("d","e"),
#   big   = c("a","b","c")
# )
