# mean --------------------------------------------------------------------

#' render_mean
#' Formatiere von Zahlen nach dem APA-Style ( American Psychological Association )
#' @noRd
rndr_median_iqr <- function(m,
                            iqr,
                            digits = get_opt("median", "digits"),
                            sep =    get_opt("median", "seperator"),
                            style =  get_opt("median", "style"),
                            ...) {

  paste0(render_f(m, digits, ...),
         " (IQR ",
         render_f(iqr, digits, ...),
         ")")
}



rndr_median_quant <- function(x,
                              digits = get_opt("median", "digits"),
                              sep =    get_opt("median", "seperator"),
                              style =  get_opt("median", "style"),
                              ...) {
  paste0(
    render_f(x[3], digits, ...),
    " (",
    render_f(x[2], digits, ...),
    sep,
    render_f(x[4], digits, ...),
    ")"
  )
}

rndr_median<-
  rndr_median_iqr_range <- function (m,
                                     iqr,
                                     mn,
                                     mx,
                                     digits = get_opt("median", "digits"),
                                     sep = get_opt("median", "seperator"),
                                     ...) {
    paste0(
      render_f(m, digits, ...),
      " (IQR ",
      render_f(iqr, digits, ...),
      sep,
      "range ",
      render_f(mn, digits, ...),
      " to ",
      render_f(mx, digits, ...),
      ")"
    )

  }

rndr_median_range <-
  function(x,
           digits = get_opt("median", "digits"),
           sep =    get_opt("median", "seperator"),
           style =  get_opt("median", "style"),
           ...) {
    paste0(
      render_f(x[3], digits, ...),
      " (",
      render_f(x[1], digits, ...),
      sep,
      render_f(x[5], digits, ...),
      ")"
    )
  }


rndr_mean <- function(m,
                      s,
                      digits = get_opt("mean", "digits"),
                      style = get_opt("mean", "style"),
                      ...) {

  # if (is.null(style))
  #   style <- get_opt("mittelwert", "mean.style")

  if(style == 1 )
    paste0(render_f(m, digits, ...),
           " (", render_f(s, digits, ...), ")")
  else
    paste0(render_f(m, digits, ...),
           get_opt("mean", "plusmin_str"),
           render_f(s, digits, ...))

}

rndr_mean_range <- function(m,
                            s,
                            mn,
                            mx,
                            digits = get_opt("mean", "digits"),
                            line_break = get_opt("mean", "seperator"),
                            style = get_opt("mean", "style"),
                            ...) {
  # noch nicht implementiert
  #  if (is.null(style)) style <- get_opt("mittelwert", "mean.style")

  paste0(
    render_f(m, digits, ...),
    " (SD ",
    render_f(s, digits, ...),
    line_break,
    "range ",
    render_f(mn, digits, ...),
    " to ",
    render_f(mx, digits, ...),
    ")"
  )

}


rndr_ods <- function(x, digits = get_opt("r", "digits")) {
  res <- render_f(x, digits = digits)
  res[which(x > 20)] <- ">20"
  res[which(x < .01)] <- "<0.01"
  res
}


rndr_CI <- function(ci,
                    digits = get_opt("r", "digits"),
                    sep =    get_opt("sep_element"),
                    sep_1 =  get_opt("brackets")[1],
                    sep_2 =  get_opt("brackets")[2]) {
  if (is.vector(ci)) {
    paste0(sep_1,
           render_f(ci[1], digits),
           sep,
           " ",
           render_f(ci[2], digits),
           sep_2)

  }
  else {
    paste0(sep_1,
           render_f(ci[, 1], digits),
           sep,
           " ",
           render_f(ci[, 2], digits),
           sep_2)
  }

}


rndr_CI2 <-  function(ci,
                      digits = 3,
                      sep =   ",",
                      sep_1 =  "[",
                      sep_2 =  "]",
                      format = "g") {
  cis <-  paste0(
    sep_1,
    render_f(ci[[1]],
             digits, format = format),
    sep,
    " ",
    render_f(ci[[2]],
             digits, format = format),
    sep_2
  )

  cis[is.na(ci[[1]])] <- NA
  cis
}


rndr_ods_CI <- function(ci,
                        digits =  get_opt("r", "digits"),
                        sep =   get_opt("sep_element"),
                        sep_1 = get_opt("brackets")[1],
                        sep_2 = get_opt("brackets")[2]) {
  paste0(sep_1,
         rndr_ods(ci[, 1], digits),
         sep,
         " ",
         rndr_ods(ci[, 2], digits),
         sep_2)


}


rndr_mean_CI <- function(m, ci, digits = get_opt("r", "digits" ), ...) {
  m <- render_f(m, digits)
  ci <-  rndr_CI(ci, digits)
  paste( m, ci )
}


rndr_r <- function(x, include.symbol = TRUE,
                   digits = get_opt("r", "digits" ),
                   drop0leading = !get_opt("r",  "lead.zero" ),
                   ...) {
  r2<- render_f(x,
                digits = digits,
                drop0leading  = drop0leading,
                ...)

  if (include.symbol)  paste0("r=", r2)
  else  r2
}

rndr_r2 <- function(x, include.symbol = TRUE, ...) {
  r2 <-  rndr_Effect_Size(x, ...)
  if (include.symbol) {
    paste(paste0(c(
      "R<sup>2</sup>", "adj.R<sup>2</sup>"
    ), "=",
    r2),
    collapse = ", ")
  }
  else
    r2
}


rndr_r2pseudo <- function(x, include.symbol = TRUE, ...) {
  r2 <- rndr_Effect_Size(x, ...)
  if (include.symbol)
    paste(paste0(names(r2), "=", r2), collapse = ", ")
  else
    r2
}


rndr_corr <- function(x, p, df) {
  paste0("r", rndr_df(df), "=", rndr_Effect_Size(x), ", ", rndr_P(p))
}


rndr_Effect_Size <- function(x,
                             digits = get_opt("r", "digits" )[1],
                             drop0leading = !get_opt("r",  "lead.zero" ),
                             ...) {
  render_f(x,
           digits = digits,
           drop0leading  = drop0leading,
           ...)


}



# percent -----------------------------------------------------------------

rndr_percent <- function(x, n,
                          digits = get_opt("percent", "digits") ,
                          symbol.trailing = get_opt("percent", "percentage_str"),
                          symbol.na = "n.a.",
                          style = get_opt("percent", "style"),
                          null_percent_sign = get_opt("percent", "null_percent_sign"),
                          small_values = x < 1 / (10 ^ digits)) {

  if( all(is.na(n)) ) return(symbol.na)

  prc <- format_num(x, digits = digits)
  n <- format_num(n, digits = 0)


  if (any(small_values))
    prc[which(small_values)] <- paste0("<", 1 / (10 ^ digits))

  if (is.character(symbol.trailing))
    prc <- paste(prc, symbol.trailing, sep = "")

  if (!is.null(null_percent_sign)) {
    prc[which(x == 0)] <- null_percent_sign
  }
  if (any(is.na(x)))
    prc[which(is.na(x))]  <- symbol.na

  switch(
    style,
    "1" = paste(prc, " (", n, ")", sep = ""),
    "2" = paste(n, " (", prc, ")", sep = ""),
    "3" = prc,
    "4" = n,
    "5" = paste(n, "/", n_total, sep = ""),
    NA
  )

}

