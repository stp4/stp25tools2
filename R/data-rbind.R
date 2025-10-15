#' Bind multiple data.frames with additional metadata
#'
#' `Rbind2()` ist ein Wrapper um [dplyr::bind_rows()], der mehrere
#' `data.frame`s zu einem gemeinsamen Datensatz kombiniert und dabei
#' zusätzliche Features bietet:
#'
#' * fügt eine Spalte mit der Herkunft (`.id`) hinzu,
#' * erlaubt optional die Beibehaltung von Rownames,
#' * übernimmt Spaltenlabels (via `get_label()` und `set_label()`).
#'
#' @param ... Data frames oder tibbles, die gebunden werden sollen.
#' @param .id Name der Spalte, die die Herkunft kennzeichnet (Standard `"which"`).
#'   Wenn `NULL`, wird keine Herkunftsspalte hinzugefügt.
#' @param .names Optionaler Vektor mit Namen für die Datenquellen.
#'   Wenn `NULL`, werden die Namen aus den übergebenen Objekten ermittelt.
#' @param .use.label `logical`. Wenn `TRUE` (Default), werden Labels
#'   von den Eingabe-Dataframes übernommen.
#' @param include.rownames `logical`. Falls `TRUE`, wird eine zusätzliche
#'   Spalte `"Source"` mit den ursprünglichen Rownames ergänzt.
#' @return Ein `data.frame` (genauer: `tibble`), das die Daten kombiniert.
#' @seealso [dplyr::bind_rows()], [plyr::rbind.fill()]
#' @examples
#' df1 <- data.frame(a = 1, b = 2)
#' df2 <- data.frame(a = 2, c = 3, d = 5)
#'
#' Rbind2(df1, df2)
#' #>   which a  b  c  d
#' #> 1   df1 1  2 NA NA
#' #> 2   df2 2 NA  3  5
#'
#' df1 <- data.frame(CustomerId = 1:3, Product = "Oven")
#' df2 <- data.frame(CustomerId = 4:5, Product = "Television")
#' df3 <- data.frame(CustomerId = 6:7, Product = "Air conditioner", State = "NJ")
#'
#' Rbind2(df1, df2, df3)
#'
#' @export

Rbind2 <- function (...,
                    .id = "which",
                    .names = NULL,
                    .use.label = TRUE,
                    include.rownames = FALSE) {

  data <- dplyr::bind_rows(..., .id = .id)

  if (include.rownames)   {
    data <- cbind(data[1],
                  Source =  sub("(.*).....*", "\\1", rownames(data)),
                  data[-1])
  }

  if (!is.null(.id)) {
    tmp <- list(...)
    if (is.null(.names))
      .names <- names(tmp)

    if (is.null(.names))
      .names <- sapply(as.list(match.call()), deparse)[-1]

    data[[1]] <-
      as.character(factor(data[[1]], seq_along(.names), .names))
  }

  data[[1]] <- factor(data[[1]])

  if (.use.label) {
    label <- c(.id)
    names(label) <- .id
    for (dat in list(...)) {
      lbl <-  get_label(dat)
      label <-
        append(label, lbl[setdiff(names(lbl), names(label))])
      data <- set_label(data, label)
    }
  }

  data
}



