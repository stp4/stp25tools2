#' Format a vector of provided numeric values
#'
#' Given a vector of data return as strings formatted as requested
#'
#'
#' Stolen from tangram
#'
#' @param x numeric; the data to format. Must work with quantile function.
#' @param digits the decimal
#' @param drop0leading leading zero
#' @param na.strings,na.symbol NA replace
#' @param decimal.mark an formatC default format="f"
#' @param ... weitere Argumente an formatC  drop0leading,format,decimal.mark
#'
#' @return character; formatted values as character strings
#' @export
#' @examples
#'
#'  \donttest{
#'  # Vector
#' x <- c(1, 0, 2.45896, 2548.256)
#' y <- c(0.1, 0.01 , 0.001, NA)
#' z <- c(1L, 2L, 3L)
#' render_f(x)
#' render_f(y)
#' render_f(z)
#' render_f(as.double(x))
#' render_f(letters[1:3])
#'
#' render_f(x, c(1, 2, 3, 4))
#' render_f(x, c(1, 2, 3))
#'
#' # Data.Frame
#' dat <- data.frame(Item = letters[1:4], x = x, y = y)
#' render_f(dat)
#' render_f(dat, digits = 2)
#' render_f(dat, digits = c(1, 2))
#' render_f(dat, digits = c(1, 2, 3, 4, 1))
#' render_f(dat,
#'          digits = list(c(1, 2), c(3)),
#'          drop0leading = list(FALSE, TRUE))
#'
#' # Matrix
#' render_f(cbind(y, x))
#' render_f(cbind(y, x) , digits = list(3, 0))
#' # List
#' mx <- list(x = x, y = y, z = rnorm(10))
#' render_f(mx)
#' render_f(mx, digits = 2)
#' render_f(mx, digits = list(2, 0, 1))
#'
#'}
render_f <- function(x, digits = NULL, ...) {
  UseMethod("render_f")
}

#' @rdname render_f
#'
#' @description
#' render_sigf:  Format numbers to significant figures nicely
#'
#' @export
#'
#' @examples
#'
#' render_sigf(c(123456, 23.2546, 1), 4)
#'
#'
render_sigf <- function(x, digits = 3) {
  out <- gsub("\\.$",
              "",
              formatC(
                signif(x, digits = digits),
                digits = digits,
                format = "fg",
                flag = "#"
              ))
  out[grepl(".", out, fixed = TRUE)] <-
    strtrim(out[grepl(".", out, fixed = TRUE)], digits + c(1, 2)[grepl("-", out, fixed = TRUE) + 1])
  out
}

# in tbll_extract.lda() verwendet
render_f_signif <- function(m, digits = 2) {
  apply(m, 2, render_sigf)
}

#' @rdname render_f
#' @export
 render_f.list <- function(x, digits = NULL, ...) {
  if (is.list(digits)) {
    Map(render_f, x, digits, ...)
  } else {
    lapply(x, render_f, digits = digits, ...)
  }
}

#' @rdname render_f
#' @export
render_f.matrix <- function(x, digits = NULL, ...) {
  if (!is.list(digits)) {
    apply(x, 2, render_f, digits=digits, ...)
  }
  else {
    as.matrix(data.frame(purrr::pmap(
      list(data.frame(x),
           digits = digits,
           ...),
      render_f
    )))
  }
}


#' @rdname render_f
#' @param digits  .numeric
#' @param ...  render_f
#' @export
render_f.data.frame <- function(x, digits = NULL, ...) {

 # num_cols <- sapply(x, is.numeric)
  num_cols <- which(vapply(x, is.numeric, logical(1)))

  if (!any(num_cols)) return(x)

  if (is.list(digits)) {
    x[num_cols] <- Map(render_f, x[num_cols], digits, ...)
  } else {
    x[num_cols] <- lapply(x[num_cols], render_f, digits = digits, ...)
  }
  x
}




#' @rdname render_f
#' @export
render_f.tbl_df  <- function(x, digits = NULL, ...) {
  num_cols <- which(vapply(x, is.numeric, logical(1)))
  if (!is.list(digits)) {
    x[num_cols]  <-
      lapply(x[num_cols], render_f , digits = digits, ...)
  }
  else {
    x[num_cols] <-
      purrr::pmap(list(x[num_cols], digits = digits, ...), render_f)
  }

  x
}


#' @rdname render_f
#' @export
render_f.numeric <- function(x, digits = NULL, ...) {
  if (is.null(digits))
    format_num(x, ...)
  else if (length(digits) == 1)
    format_num(x, digits = digits, ...)
  else
    mapply(format_num, x, digits = length_multiplier(digits, length(x)), ...)
}


#' @rdname render_f
#' @export
render_f.default <- function(x,
                             digits= NULL,
                             drop0leading = FALSE,
                             na.strings = "NA",
                             na.symbol = "",
                             decimal.mark = OutDec(),

                             ...) {
  x <- as.character(x)
  if (!is.null(na.strings))
    x[stringr::str_detect(x, na.strings)] <- na.symbol

  if (drop0leading)
    x <- drop_0_leading(x, decimal.mark)

  x
}

# helper ------------------------------------------------------------------


#' @noRd
OutDec <- function() {
  # if(!exists("get_opt")) return (".")
  if(is.null( get_opt("OutDec") ))  "."
  else get_opt("OutDec")

}

#' @noRd
format_num <- function(x,
                        digits = format_guess(x),
                        drop0leading  = FALSE,
                        format = "f",
                        decimal.mark = OutDec(),
                        na.strings = "NA",
                        na.symbol = "",
                        unit = NULL,
                        ...) {
  if (length(x) == 1)
    if (is.na(x))
      return(na.symbol)
  if(digits > 8 ) digits <- 2
  x <- formatC(x,
               digits = digits,
               format = format,
               decimal.mark = decimal.mark,
               ...)



  if (!is.null(na.strings))
    x[stringr::str_detect(x, na.strings)] <- na.symbol

  if (drop0leading)
    x <- drop_0_leading(x, decimal.mark)

  x
}


#' Laenge der digits anpassen
#'
#' @noRd
length_multiplier <- function(x,
                              n_out = NULL,
                              n  = length(x)) {
  if (n == n_out)
    x
  else if (n == 1)
    rep(x, n_out)
  else if (n > n_out)
    x[1:n_out]
  else if (n < n_out)
    c(x, rep(x[n], n_out - n))
  else
    NULL
}




# Guess the best format for a given set of numerical data
#
# Given a vector of data, default to 3 significant digits or all if maximum is greater
# than zero
#
# @param x numeric; basic math and quantile function must work on data passed in
# @return numeric; the digits past the decimal recommended for display
# @examples
#   stp25stat2:::format_guess(rnorm(100))
#   stp25stat2:::format_guess(rnorm(100, sd=1e-6))
#' @noRd
format_guess <- function(x) {
  d <- x[!is.na(x)]
  if (length(d) == 0)
    return(0) # Nothing, then just return 0 for rounding

  if (all(d == floor(d)))
    # Is it all whole numbers, then no decimals
    0
  else
  {
    consider <- quantile(abs(d), c(0.05, 0.5))
    if (sum(consider) == 0.0)
      3
    else
      # Otherwise use 3 significant digits of a representative smaller side quantile
      max(2 - max(floor(log10(consider))), 0)
  }
}

#' @keywords internal
#' Fuerende Null eliminieren
#' drop_0_leading("0,25689", ",")
#' @noRd
drop_0_leading <- function(x,
                           OutDec = OutDec()) {
  sub("^(-?)0\\.", "\\1.", x)
#  sub(glue::glue('^(-)?0[{OutDec}]'),
#      glue::glue('\\1{OutDec}'),
#      x)
}


