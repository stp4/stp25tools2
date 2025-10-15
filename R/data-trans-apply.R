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


