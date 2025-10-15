#' Combine Multiple Data Frames with Custom Join Functions
#'
#' Diese Funktion kombiniert mehrere Data Frames durch aufeinanderfolgende
#' Joins basierend auf einer gemeinsamen Schlüsselvariable. Die Funktion
#' ist flexibel und erlaubt die Übergabe beliebiger Join-Funktionen. Optional
#' können Labels von den ursprünglichen Data Frames übernommen werden.
#'
#' @param ... Data Frames, die kombiniert werden sollen. Alle Data Frames
#'   müssen die in `by` spezifizierte Schlüsselvariable enthalten.
#' @param by Character Vektor mit den Namen der Variablen, die für den Join
#'   verwendet werden sollen, oder die Zahl `0` um Rownames als Join-Variable
#'   zu verwenden. Bei `by = 0` werden die Rownames aller Data Frames als
#'   Spalte "rownames" hinzugefügt und für den Join verwendet.
#' @param sort Logical. Falls `TRUE`, wird das Ergebnis nach der Join-Variable
#'   sortiert. Standard ist `FALSE`.
#' @param suffixes Character Vektor der Länge 2, der die Suffixe für
#'   gleichnamige Variablen bei Konflikten spezifiziert. Falls `NULL` (Standard),
#'   werden die dplyr Standard-Suffixe ".x" und ".y" verwendet.
#' @param include.label Logical. Falls `TRUE`, werden Labels von
#'   den ursprünglichen Data Frames übernommen und auf das kombinierte
#'   Ergebnis angewendet. Standard ist `FALSE`.
#' @param merge_fun Function. Die Join-Funktion, die verwendet werden soll.
#'   Standard ist `dplyr::full_join`. Andere Optionen sind `dplyr::left_join`,
#'   `dplyr::right_join`, `dplyr::inner_join`, oder beliebige custom Funktionen
#'   mit der gleichen Signatur.
#'
#' @return Ein Data Frame, der alle übergebenen Data Frames durch die
#'   spezifizierte Join-Funktion kombiniert. Die Join-Variable(n) werden an die
#'   erste Position verschoben. Falls `include.label = TRUE`, werden relevante
#'   Labels übernommen.
#'
#' @export
#'
#' @examples
#' # Beispieldaten erstellen
#' n <- 10
#' df1 <- data.frame(
#'   id = 1:n,
#'   origin = sample(c("A", "B", "C", "D", "E"), n, replace = TRUE),
#'   N = sample(seq(9, 27, 0.5), n, replace = TRUE),
#'   P = sample(seq(0.3, 4, 0.1), n, replace = TRUE)
#' )
#'
#' df2 <- data.frame(
#'   id = 1:n,
#'   foo = sample(c(TRUE, FALSE), n, replace = TRUE)
#' )
#'
#' df3 <- data.frame(
#'   id = 1:n,
#'   Q = sample(seq(1456, 1483, 100), n, replace = TRUE)
#' )
#'
#' # Standard Full Join
#' result_full <- Combine(df1, df2, df3, by = "id")
#'
#' # Verschiedene Join-Typen durch Funktionsübergabe
#' result_left <- Combine(df1, df2, df3, by = "id", merge_fun = dplyr::left_join)
#' result_inner <- Combine(df1, df2, df3, by = "id", merge_fun = dplyr::inner_join)
#' result_right <- Combine(df1, df2, df3, by = "id", merge_fun = dplyr::right_join)
#'
#' # Beispiel mit unvollständigen Daten
#' df_partial1 <- data.frame(id = 1:5, value1 = letters[1:5])
#' df_partial2 <- data.frame(id = 3:8, value2 = LETTERS[3:8])
#'
#' # Verschiedene Join-Strategien
#' left_result <- Combine(df_partial1, df_partial2, by = "id",
#'                       merge_fun = dplyr::left_join)
#' inner_result <- Combine(df_partial1, df_partial2, by = "id",
#'                        merge_fun = dplyr::inner_join)
#'
#' # Mit custom Suffixes und Sortierung
#' sorted_result <- Combine(df1, df2, by = "id",
#'                         merge_fun = dplyr::left_join,
#'                         suffixes = c("_first", "_second"),
#'                         sort = TRUE)
#'
#' # Beispiel mit Rownames als Join-Variable
#' mtcars_subset1 <- mtcars[1:5, 1:4]
#' mtcars_subset2 <- mtcars[1:5, 5:8]
#' mtcars_subset3 <- mtcars[1:5, 9:11]
#'
#' result_rownames <- Combine(mtcars_subset1, mtcars_subset2, mtcars_subset3,
#'                           by = 0, merge_fun = dplyr::full_join)
#'
#' # Custom Join-Funktion (Beispiel)
#' safe_left_join <- function(x, y, by, suffix = c(".x", ".y")) {
#'   tryCatch({
#'     dplyr::left_join(x, y, by = by, suffix = suffix)
#'   }, error = function(e) {
#'     warning("Join failed, returning first dataframe")
#'     return(x)
#'   })
#' }
#'
#' safe_result <- Combine(df1, df2, by = "id", merge_fun = safe_left_join)
#'
Combine <- function(...,
                    by,
                    sort = FALSE,
                    suffixes = NULL,
                    include.label = FALSE,
                    merge_fun = dplyr::full_join) {

  data_list <- list(...)

  # Spezialbehandlung für by = 0 (Rownames als Join-Variable)
  if (length(by) == 1 && by == 0) {
    # Rownames zu einer Spalte namens "rownames" konvertieren
    data_list <- lapply(data_list, function(df) {
      df$rownames <- rownames(df)
      return(df)
    })
    by <- "rownames"
  }

  # Suffixes verarbeiten
  suffix_args <- if (is.null(suffixes)) c(".x", ".y") else suffixes

  # Join mit der übergebenen Funktion
  data <- purrr::reduce(data_list,
                        merge_fun,
                        by = by,
                        suffix = suffix_args
  )

  # Join-Variable(n) an erste Position verschieben
  data <- dplyr::relocate(data, !!by)

  # Optionale Sortierung
  if (sort) {
    data <- dplyr::arrange(data, !!!rlang::syms(by))
  }

  # Label-Verarbeitung
  if (include.label) {
    lvl <- NULL
    for (i in seq_len(length(data_list))) {
      lv <- get_label(data_list[[i]])
      lvl <- c(lvl, lv[setdiff(names(lv), names(lvl))])
    }
    data <- set_label(data, lvl)
  }

  data
}



