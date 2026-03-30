#' Apply Functions Over Data Frames
#'
#'
#' Dapply, dapply2: plyr::llply() + Label()
#'
#' @param x Objekt data.frame, formula
#' @param data data.frame
#' @param ... Namen der Spalten
#' @return data.frame
#' @export
#' @examples
#'
#'
#' df1 <- tibble::tibble(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7),
#'   C = c(1.6, 2.7, 3.8, 4.5, 5.6, 6.7)
#' ) |>
#'   Label(
#'     month = "Monat",
#'     student = "Schüler",
#'     A = "Deutsch",
#'     B = "Mathe"
#'   )
#'
#'
#'
#'
#'
#'
#' Dapply( ~ A + B, df1, ~ .x * 100)
#'
#' Dapply(~ A + B, df1, fun = function(x) x * 100)
#'
#' df1 |> Dapply( ~ A + B, fun = function(x) x * 100)
#'
#' df1 |> Dapply(A, B, fun = function(x) x * 100)
#'
#' df1 |> Dapply(A:C, fun = function(x) x * 100)
#'
#' dapply2(df1, ~ as.numeric(factor(.x)))
#'
#'
#' #' Alternative mit across::across
#'
#'
#' dplyr::mutate(df1, dplyr::across(c("A", "B"), .fns= function(x) x * 100 ))
#'
#'
Dapply <- function(...) {
  UseMethod("Dapply")
}



#' @rdname Dapply
#' @param data Data.frame
#' @export
#' @importFrom dplyr across mutate
#' @importFrom tidyselect all_of
Dapply.formula <- function(x,
                           data,
                           fun = function(y)
                             as.numeric(y),
                           ...) {
  values_from <- extracts_lhs(x, names(data))
  lvl <- get_label(data[values_from])
  rslt <-
    dplyr::mutate(data,
                  dplyr::across(tidyselect::all_of(values_from), .fns = fun))
  set_label(rslt, lvl)
}


#' @rdname Dapply
#' @export
Dapply.data.frame <- function(data,
                              ...,
                              fun = function(y)
                                as.numeric(y)) {
  dots <- rlang::enquos(...)
  first_expr <- rlang::quo_squash(dots[[1]])

  if (rlang::is_call(first_expr, "~")) {
    formula <- eval(first_expr, envir = rlang::caller_env())
    return(Dapply.formula(formula, data, fun = fun))
  }

  if (rlang::is_call(first_expr)) {
    lvl <- get_label(data)
    rslt <-
      dplyr::mutate(data,
                    dplyr::across(!!dots[[1]], .fns = fun))
  }
  else{
    values_from <- purrr::map_chr(dots, rlang::as_name)
    lvl <- get_label(data[values_from])
    rslt <-
      dplyr::mutate(data,
                    dplyr::across(.cols = tidyselect::all_of(values_from), .fns = fun))
  }

  set_label(rslt, lvl)
}

#' @rdname Dapply
#'
#' @description dapply2: Copie of plyr::llply()
#' @param fun   funktion function(x) as.numeric(x)
#' @export
#'
dapply2 <- function (data,
                     fun = function(x)
                       as.numeric(x),
                     ...) {
  lvl <- get_label(data)
  rslt <-
    dplyr::mutate(data,
                  dplyr::across(tidyselect::all_of(names(data)), .fns = fun)
                  )
  set_label(rslt, lvl)
}


#' Skalen zu einem Index zusammenfassen
#'
#' Berechnet einen zusammengesetzten Index aus mehreren Variablen
#' (z. B. Items einer Skala). Optional können Variablen rekodiert,
#' fehlende Werte behandelt und das Ergebnis als Prozentwert skaliert werden.
#'
#' @param ... Numerische Vektoren oder Faktoren, die zu einem Index
#'   zusammengefasst werden sollen.
#' @param na.rm Logisch. Sollen fehlende Werte bei der Berechnung
#'   ignoriert werden? Standard ist TRUE.
#' @param as_percent Logisch. Soll der Index auf den Bereich 0–1
#'   skaliert werden? Standard ist TRUE.
#' @param fun Aggregationsfunktion zur Bildung des Index
#'   (z. B. mean, median oder sum). Standard ist mean mit as_percent.
#' @param digits Anzahl der Nachkommastellen für das Ergebnis.
#'   Standard ist 2.
#' @param re_code Numerischer Index der Variablen, die rekodiert
#'   werden sollen (z. B. für invertierte Items). Standard ist NULL.
#' @param min_level Minimaler Skalenwert. Wird automatisch bestimmt,
#'   wenn nicht angegeben.
#' @param max_level Maximaler Skalenwert. Wird automatisch bestimmt,
#'   wenn nicht angegeben.
#'
#' @returns Ein numerischer Vektor mit dem berechneten Index.
#'
#' @details
#' Faktoren werden automatisch in numerische Werte umgewandelt.
#' Wenn `re_code` gesetzt ist, werden die entsprechenden Variablen
#' invertiert (Spiegelung an min/max).
#'
#' @export
#'
#' @examples
#' # Beispiel: Bildung eines Index aus mehreren Items
#' Anamnesebogen <- Anamnesebogen |>
#'   mutate(
#'     wechsel = Index(
#'       hitzewallungen.schwitzen,
#'       depressive.verstimmungen,
#'       reizbarkeit,
#'       angstlichkeit,
#'       herzbeschwerden,
#'       vergesslichkeit,
#'       haarausfall
#'     )
#'   )
#'
#' Anamnesebogen$wechsel
Index <- function(...,
                  na.rm = TRUE,
                  
                  as_percent = TRUE,
                  fun = mean,
                  digits = 2,
                  re_code = NULL,
                  min_level = NA,
                  max_level = NA) {
  dots <- list(...)
  if (is.na(min_level) & is.na(max_level) & is.factor(dots[[1]])) {
    min_level <- 1
    max_level <-  nlevels(dots[[1]])
  }
  dots <- sapply(dots, as.numeric)
  
  if (is.numeric(re_code))
    dots <- Umcodieren(dots, re_code, min_level, max_level)
  
  dots <-  apply(dots, 1, fun, na.rm = na.rm)
  if (as_percent) {
    dots <-  (dots - min_level) / (max_level - min_level)
    
  }
  
  round(dots, digits)
}
#' @noRd
Umcodieren <- function(x,
                       re_code,
                       min_level = NA,
                       max_level = NA) {
  if (is.na(max_level))
    max_level <- max(x, na.rm = TRUE)
  if (is.na(min_level))
    min_level <- min(x, na.rm = TRUE)
  mytempdata <- x[, re_code]
  
  if (is.numeric(mytempdata))
    x[, re_code] <- max_level + min_level - mytempdata
  else
    x[, re_code] <-
    apply(mytempdata, 2, function(item)
      max_level + min_level - item)
  return(x)
}



