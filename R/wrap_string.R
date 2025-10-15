#' Wrap and Truncate Strings or Labels
#'
#' `wrap_string()` formatiert Text, indem er überlange Zeichenketten in mehrere Zeilen umbricht
#' und optional die Anzahl der Zeichen oder Zeilen begrenzt.
#' Unterstützt Objekte vom Typ `character`, `factor` und `data.frame` (Labels werden ebenfalls umbrochen).
#'
#' @param x Ein Vektor, Faktor oder Data Frame, dessen Zeichenketten umbrochen werden sollen.
#' @param width `numeric`. Maximale Zeichenanzahl pro Zeile (Standard: 25 für Zeichenketten,
#'   20 für Faktoren und Data Frames).
#' @param max.lines `numeric` oder `NULL`. Falls gesetzt, werden die Zeichenketten nach der
#'   angegebenen Anzahl von Zeichen abgeschnitten und mit `max.lines.char` ergänzt.
#' @param max.lines.char `character`. Zeichenfolge, die bei abgeschnittenem Text angehängt wird
#'   (Standard: ` ... `).
#' @param sep `character`. Zeichen, das als Trenner für Zeilen im umgebrochenen Text verwendet wird (Standard: `\\n`).
#' @param pattern (nur `split_string()`) Regulärer Ausdruck oder fester String, an dem der Text gesplittet wird.
#' @param remove (nur `split_string()`) Regulärer Ausdruck, der aus den gesplitteten Strings entfernt werden soll.
#' @param pos (nur `split_string()`) Numerischer Index der Spalte, die nach dem Splitten extrahiert wird.
#' @param ... Weitere Argumente, die an die spezifischen Methoden weitergegeben werden.
#'
#' @details
#' **Methoden**:
#' - `wrap_string.character()` bricht Zeichenketten um und schneidet sie optional ab.
#' - `wrap_string.factor()` bricht die Levelnamen eines Faktors um.
#' - `wrap_string.data.frame()` bricht die Spalten-Labels eines Data Frames um.
#'
#' **Hilfsfunktionen**:
#' - `wrap_label()` formatiert nur die Spalten-Labels eines Data Frames.
#' - `split_string()` teilt Zeichenketten an einem bestimmten Muster und bereinigt sie.
#'
#' @return
#' Ein Objekt vom gleichen Typ wie `x`, bei dem der Text umgebrochen wurde.
#'
#' @export
#'
#' @examples
#' #'
#' strg <-   c(
#'   "[A     5] Bei wie vielen ist das der Fall?",
#'   " [B 1] Bei wie vielen ist das der Fall?",
#'   "[  B 1] Bei wie vielen ist das der Fall?"
#' )
#'
#' df <- data.frame(
#'   id = 1:3,
#'   group = factor(factor(strg)),
#'   einzel = 1:3,
#'   text = strg,
#'   factor = factor(strg)
#' )  |>
#'   Label(
#'     id = "Die       ID ist eine Identifikationsnummer.",
#'     group = "Wie viele Schüler*innen haben Sie insgesamt
#'     im Unterricht, Ergänzungsfächer mit eingeschlossen",
#'     einzel = "[im Einzelunterricht] Wie viele Schüler*innen
#'     haben Sie jeweils im Einzel und Gruppenunterricht?",
#'     text = "    Gibt es an Ihrer Musikschule Angebote,
#'     die sich speziell an Menschen mit Behinderungen richten
#'     und auf deren Bedürfnisse abgestimmt sind?",
#'     factor = "wegen Ablenken vom Spielen des Instrumentes
#'     oder des Kursinhaltes durch Sprechen"
#'   )
#'
#'  # Umbruch von Zeichenketten
#'  df$text <- wrap_string(df$text, 5, max.lines = 10)
#'
#'  # Faktor-Levels umbrechen
#'  df$factor <- wrap_string(df$factor, 3, max.lines = 5)
#'
#'  # Textteile extrahieren und bereinigen
#'  df$group <- split_string(df$group)
#'
#'  # Labels eines Data Frames umbrechen
#'  # wrap_label(df, 10, max.lines = 40)
#'  wrap_string(df, 10, max.lines = 40)
#'
#'
wrap_string <- function(x, ...) {
  UseMethod("wrap_string")
}


#' @rdname wrap_string
#' @export
wrap_string.character <- function(x,
                                  width = 25,
                                  max.lines = NULL,
                                  max.lines.char = " ...",
                                  sep =  "\n",
                                  ...) {
  if (is.null(x)) {
    return(x)
    warning("Da kommt nix!")
  }

  atr_label <- attr(x, "label")
  # removes whitespace
  x <- stringr::str_squish(x)
  if (!is.null(max.lines)) {
    x <- sapply(x, function(z) {
      stringr::str_trunc(z,
                         width =  max.lines,
                         side = "right",
                         ellipsis = max.lines.char)
    })
  }

  if (is.numeric(width)) {
    x <- stringi::stri_wrap(x, width = width, simplify = FALSE)
    x <- vapply(x, stringr::str_c, collapse = sep, character(1))
  }

  if (!is.null(atr_label))
    attr(x, "label") <- atr_label
  x
}


#' @rdname wrap_string
#' @export
wrap_string.factor  <- function(x,
                                width = 20,
                                max.lines = NULL,
                                max.lines.char = "...",
                                sep = "\n",
                                ...) {
  # atr_label <- attr(x, "label")
  lvl <- levels(x)
  lvl <- wrap_string(
    lvl,
    width = width,
    max.lines = max.lines,
    max.lines.char = max.lines.char,
    sep = sep
  )

  levels(x) <- lvl

  x
}

#' @rdname wrap_string
#' @export
wrap_string.data.frame <- function(x,
                                   width = 20,
                                   max.lines = NULL,
                                   max.lines.char = "...",
                                   sep = "\n",
                                   ...) {
  lbl <- wrap_label(
    x,
    width = width,
    max.lines = max.lines,
    max.lines.char = max.lines.char,
    sep = sep
  )
  set_label(x, lbl)
}

#' @rdname wrap_string
#' @export
wrap_label <- function(x,
                       width = 20,
                       max.lines = NULL,
                       max.lines.char = "...",
                       sep = "\n") {
  lbl <- lapply(x, attr, "label")
  lbl <- lapply(
    lbl,
    \(s) wrap_string(
      s,
      width = width,
      max.lines = max.lines,
      max.lines.char = max.lines.char,
      sep = sep
    )
  )

  unlabelled <- which(vapply(lbl, is.null, logical(1)))
  lbl[unlabelled] <- names(x)[unlabelled]
  unlist(lbl)
}

#' @rdname wrap_string
#' @export
split_string <- function(x,
                         pattern = "]",
                         remove = "\\[",
                         pos = 1) {
  atr_label <- attr(x, "label")
  x <- stringr::str_split(x , pattern = "]" , simplify = TRUE)[, pos]
  x <- stringr::str_remove_all(x, remove)
  x <-  stringr::str_squish(x)

  if (!is.null(atr_label))
    attr(x, "label") <- atr_label

  x
}

