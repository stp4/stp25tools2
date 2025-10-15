
# p-Value -----------------------------------------------------------------


rndr_P <-
  function(x,
           include.symbol = TRUE,
           digits = get_opt("p", "digits"),
           with.stars = get_opt("p", "with.stars"),
           symbol.leading = c("p=", "p<"),
           mark.sig = get_opt("p", "mark.sig"),
           ...) {

    is_na<-is.na(x)
    p <- render_f(
      x,
      digits = digits,
      format = "f",
      drop0leading = TRUE
    )


    if (include.symbol)
      p <- ifelse(p == ".000",
                  paste0(symbol.leading[2], ".001"),
                  paste0(symbol.leading[1], p))
    else
      p <- ifelse(p == ".000", "<.001", p)


    if (with.stars)
      p <- paste0(p, rndr_Stars(x))

    if (!is.null(mark.sig))
      p <- add_marking(x, p)

    if(any(is_na)) p[is_na] <- ""

    p
  }

add_marking <- function(x, p,
                        symbol.leading =
                          if (which_output() == "html") "<b>" else " # ",
                        symbol.trailing = if (which_output() == "html") "</b>"else " # ") {
  ifelse(x <= .050,    paste0(symbol.leading, p, symbol.trailing), p)
}

rndr_Stars <- function (x,
                        stars.value =  get_opt("p", "stars.value"),
                        stars.symbols = get_opt("p", "stars.symbols")){
  p_sternchen <- function(x)  {
    stern <- as.character(cut(round(x, 3),
                              c(-Inf, stars.value, Inf),
                              c(stars.symbols, "")))
    stern[is.na(stern)] <- ""
    stern
  }

  if (is.vector(x)) {
    xnames <- names(x)
    x <- p_sternchen(x)
    names(x) <- xnames
  }
  else if (is.data.frame(x)) {
    xnames <- names(x)
    x <- data.frame(lapply(x, p_sternchen),
                    stringsAsFactors = FALSE)
    names(x) <- xnames
  }
  else if (is.matrix(x)) {
    xnames <- dimnames(x)
    x <- apply(x, 2, p_sternchen)
    dimnames(x) <- xnames

  }

  x
}


