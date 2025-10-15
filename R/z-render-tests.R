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






# Tests -------------------------------------------------------------------


rndr_Test_Statistic <-
  function (x,
            digits = get_opt("Fstat", "digits") ,
            drop0leading = !get_opt("Fstat", "lead.zero"),
            ...) {
    render_f(x,
             digits = digits,
             drop0leading  = drop0leading,
             ...)
  }

rndr_df <- function(df1, df2 = NULL) {
  sub <- sub_tiefgestellt()
  if (is.null(df2))
    paste0(sub[1], "(", render_f(df1, 0), ")", sub[2])
  else
    paste0(sub[1], "(", render_f(df1, 0), ", ", render_f(df2, 0), ")", sub[2])
}


rndr_F <-
  function(F_val,
           df1,
           df2,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <-
        paste0("F", rndr_df(df1, df2), "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_T <-
  function(F_val,
           df1,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("T", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_H <-
  function(F_val,
           df1,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("H", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_BP <-
  function(F_val,
           df1,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("BP", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_DW <-
  function(F_val,
           df1,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("DW",
                      "=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_W <-
  function(F_val,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("W=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_U <-
  function(F_val,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("U=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_shapiro <-
  function(F_val,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      F_val <- paste0("W=", rndr_Test_Statistic(F_val))
      if (is.null(p))
        F_val
      else
        paste0(F_val, sep, rndr_P(p))
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_lm <-
  function(F_val,
           df1,
           df2,
           p,
           r2,
           ar2,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic)
      paste0(
        "R2=",
        rndr_Effect_Size(r2),
        sep,
        "ad.R2=",
        rndr_Effect_Size(ar2),
        sep,
        rndr_F(F_val, df1, df2, p)
      )
    else
      rndr_P(p, FALSE)

  }


rndr_X <-
  function(x,
           df1,
           df2 = NULL,
           p = NULL,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic) {
      if (is.null(df2)) {
        if (!is.null(df1))
          x <-
            paste0(symbol_chi2(),
                   rndr_df(df1),
                   "=",
                   rndr_Test_Statistic(x))
        else
          x <- paste0(symbol_chi2(), "=", rndr_Test_Statistic(x))
      } else {
        x <-
          paste0(symbol_chi2(),
                 rndr_df(df1),
                 "=",
                 rndr_Test_Statistic(x))
      }
      if (!is.null(p))
        paste0(x, sep, rndr_P(p))
      else
        x
    }
    else{
      rndr_P(p, FALSE)
    }
  }


rndr_Chisq <- function(x, df, p)
  rndr_X(x, df, NULL, p)


rndr_Chisq_stars <-
  function(x, p) {
    paste0(rndr_Test_Statistic(x) , rndr_Stars(p))
  }


rndr_fischer <-
  function(x,
           p,
           sep = get_opt("sep_element"),
           include.test.statistic = get_opt("Fstat", "include.statistic")) {
    if (include.test.statistic)
      paste0("OR=", rndr_Test_Statistic(x), sep, rndr_P(p))
    else
      rndr_P(p, FALSE)
  }

