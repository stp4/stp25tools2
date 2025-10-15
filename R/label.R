#' Label
#'
#' Setzen des attr(data, "label")
#' @param data data.frame
#' @param ... label in der Form a="Hallo, b="Welt"
#' @export
#' @examples
#'
#' DF <- data.frame(
#'   x = 1:6,
#'   y = 1:6,
#'   z = 1:6,
#'   BMI = c(1, 2, 3, 1, 2, 3),
#'   WHtR = gl(2, 3, labels = c("Amy", "Bob")),
#'   WHtR_1 = c(9, 7, 6, 8, 6, 9),
#'   bildprof = c(6, 7, 8, 5, 6, 7)
#' ) |>
#'   Label(
#'     BMI = "Body-Mass-Index",
#'     WHtR =  "Waist-Height-Ratio",
#'     WHtR_1 = "Waist-Height-Ratio",
#'     dummy = "Dummy",
#'     dummy2 = "Dummy"
#'   )
#'
#'
#'
#' get_label(DF)
#' get_label(DF)
#' DF <- set_label(DF, c(bildprof = "Bildungsprofil"))
#' get_label(DF)
#' DF <- delet_label(DF)
#' get_label(DF)
#'
Label <- function(data, ...) {
  lbl <- list(...)


  if (length(lbl) == 0) {
    message("Label: Keine label gefunden!")
    return(data)
  } else{
    if (is.null(names(lbl))) {
      message("Label: Keine Namen gefunden! Verwende daher names(data)")
      names(lbl) <-  names(data)[1:length(lbl)]
    }
    set_label(data, unlist(lbl))
  }
}


#' Remove Variable Labels
#'
#' @param data A data frame
#' @return The data frame with all variable labels removed
#' @export
delet_label <- remove_label <- function(data) {
  for (n in names(data)) {
    attr(data[[n]], "label") <- NULL
  }
  data
}



#' set_label
#'
#'
#' @param data data.frame
#' @param label attribut label
#' @export
#'
set_label <- function(data, label = NULL) {
  nms <- names(data)
  nl <- nms %in% names(label)
  diff_names <- setdiff(names(label), nms)

  if (length(diff_names) > 0)
    warning(
      "Aufpassen - nicht alle label haben den richtigen Namen!\n",
      paste(diff_names , collapse = ", ")
    )

  if (sum(nl) > 0) {
    for (n in nms[nl])
      attr(data[[n]], "label") <- label[[n]]
  }
  data
}


#' Get variable labels
#'
#' @param data A data frame
#' @return A named vector of labels
#' @export
#'
get_label <- function(data) {
  lbl <- lapply(data, attr, "label")
  unlabelled <- which(vapply(lbl, is.null, logical(1)))
  lbl[unlabelled] <- names(data)[unlabelled]
  unlist(lbl)
}

#' @rdname Label
#' @param pos an stringr::str_split_fixed
#'
#' @export
#'
trimm_label <-
  function(data, pattern = "\\.\\.\\. ", pos = 2) {
    lbl <- get_label(data)
    lbl_trm <-  stringr::str_split_fixed(lbl, pattern, n = pos + 1)

    if (pos == 1)
      lbl_trm <-  lbl_trm[, 1]
    else if (pos == 2)
      lbl_trm <- ifelse(lbl_trm[, 2] == "", lbl_trm[, 1], lbl_trm[, 2])
    else
      stop("Nur pos 1 oder 2 sind definiert.")
    names(lbl_trm) <- names(lbl)
    set_label(data, lbl_trm)

  }

#' @rdname Label
#' @param pattern,replacement Pattern to look for.
#'
#' @export
#'
gsub_label <- function(data,
                       pattern = "\\&amp;",
                       replacement = "&") {
  lbl <- get_label(data)

  lbl_trm <-  gsub(pattern, replacement, lbl)
  names(lbl_trm) <- names(lbl)

  set_label(data, lbl_trm)
}



# internal Test -----------------------------------------------------------



# @keywords internal
# internal_test_label <- function() {
#
#
#
# testthat::expect_warning(
# DF <- data.frame(
#   x = 1:6,
#   y = 1:6,
#   z = 1:6,
#   BMI = c(1, 2, 3, 1, 2, 3),
#   WHtR = gl(2, 3, labels = c("Amy", "Bob")),
#   WHtR_1 = c(9, 7, 6, 8, 6, 9),
#   bildprof = c(6, 7, 8, 5, 6, 7)
# ) |>
#   Label(
#     BMI = "Body-Mass-Index",
#     WHtR =  "Waist-Height-Ratio",
#     WHtR_1 = "Waist-Height-Ratio",
#     dummy = "Dummy",
#     dummy2 = "Dummy"
#   )
# )
#
#
# testthat::expect_equal(
# get_label(DF),
# c(x ="x" , y =  "y", z = "z", BMI = "Body-Mass-Index" ,
#   WHtR ="Waist-Height-Ratio", WHtR_1 ="Waist-Height-Ratio",
#   bildprof = "bildprof")
# )
#
#
#
# DF <- set_label(DF, c(bildprof = "Bildungsprofil"))
#
# testthat::expect_equal(
#   get_label(DF),
#   c(x ="x" , y =  "y", z = "z", BMI = "Body-Mass-Index" ,
#     WHtR ="Waist-Height-Ratio", WHtR_1 ="Waist-Height-Ratio",
#     bildprof = "Bildungsprofil")
# )
#
# DF <- delet_label(DF)
# testthat::expect_equal(
#   get_label(DF),
#   c(x ="x" , y =  "y", z = "z", BMI = "BMI" ,
#     WHtR ="WHtR", WHtR_1 ="WHtR_1",
#     bildprof = "bildprof")
# )
# }


# internal_test_label()



