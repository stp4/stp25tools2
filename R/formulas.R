#' Create Analysis Formulas
#'
#' Internal function to create both original and cleaned formulas.
#'
#' @param measure_vars Original measure variables
#' @param measure_vars_clean Cleaned measure variable names
#' @param by Grouping specification
#'
#' @return List with two components:
#' \itemize{
#'   \item formula_in: Formula with original variable specifications
#'   \item formula: Formula with cleaned variable names
#' }
#' @keywords internal
#'
#' @examples
#' # example code
#'   stp25tools2:::create_formulas(
#' measure_vars = c("log(m1)", "m2", "m3", "m4"),
#' measure_vars_clean = c("log(m1)", "m2", "m3", "m4"),
#' by = ~ sex
#' )
#' stp25tools2:::create_formulas(
#'   measure_vars = c("log(m1)", "m2", "m3", "m4"),
#'   measure_vars_clean = c("log(m1)", "m2", "m3", "m4"),
#'   by = NULL
#' )
create_formulas <- function(measure_vars, measure_vars_clean, by=NULL, groups=NULL) {
  if (plyr::is.formula(by)) by <- rlang::f_rhs(by)

  if (!is.null(by)) group.vars <- all.vars(by)
  else group.vars <- by

  if(!is.null(groups)) if (!is.character(groups))
      groups <- all.vars(groups)

  formula_in    <- to_formula(measure_vars, by)
  formula       <- to_formula(measure_vars_clean, by)
  formula_trans <- ~ 1

  # Regelfall ist formula: m1 + m2 + m3 ~ sex + m4
  # seltener Spezialfall: log(m1) + m2 + m3 ~ factor(sex) + m4
  if (any(grepl("\\(", c(measure_vars_clean, group.vars)))) {
    if (is.null(by)) {
      new_rhs           <-  rlang::f_rhs(formula)
      formula_trans     <- rlang::new_formula(NULL, new_rhs)
    }
    else {
      new_lhs           <-  rlang::f_lhs(formula)
      formula_trans     <- rlang::new_formula(new_lhs, by)
    }
    formula  <- strip_functions(formula_trans)
    measure_vars_clean <-  unlist(lapply(
      measure_vars_clean,
      FUN = function(x) {
        sub(".*\\(([^()]*)\\).*", "\\1", x)
      }
    ))
    # warning("Sorry - das mache ich nicht. Transformationen bitte ueber Formulas.")
  }

  list(
    formula_in = formula_in,
    formula = formula,
    formula_trans = formula_trans,
    by = if( is.null(by)) NULL else rlang::new_formula(NULL, by),
    group.vars = group.vars,
    condition.vars = groups,
    measure_vars_clean = measure_vars_clean,
    measure_vars = measure_vars,
    all_vars = c(measure_vars_clean, group.vars, groups)
  )
}


#' Convert to formula
#' to_formula(letters[1:4], NULL)
#' ~a + b + c + d
#' to_formula(letters[1:4], letters[5:6])
#' a + b + c + d ~ e + f
#'
#' @param measure_vars Measurement variables for LHS
#' @param group_vars Grouping variables for RHS
#' @keywords internal
to_formula <- function(measure_vars, group_vars) {
 # cat("\n   to_formula\ngroup.vars: ")
 # print(group_vars)
 # group_vars[1] == "1"
  if (is.null(group_vars) | length(group_vars) == 0  ) {
    rhs <-   paste(measure_vars, collapse = "+")
   # cat("\n   ~ rhs")
   # print(paste( "~", rhs))
    formula(paste( "~", rhs))

  } else  {
    rhs <- paste(group_vars, collapse = "+")
    lhs <- paste(measure_vars, collapse = "+")
    #cat("\n   lhs ~ rhs")
   # print(paste(lhs, "~", rhs) )
    formula(paste(lhs, "~", rhs))
  }
}


to_formula(letters[1:4], NULL)
to_formula(letters[1:4], letters[5:6])


#' Expand dot notation in formulas
#'
#' @param formula Input formula
#' @param data Data frame with variables
#' @keywords internal
#' @noRd
expand_dot_formula <- function(formula, all_vars) {

  if(!any( grepl("\\.", all.names(formula)))) return(formula)

  # Formel in linke und rechte Seite zerlegen
  if (length(formula) == 3) {
    # Zweiseitige Formel (y ~ x)
    lhs <- formula[[2]]
    rhs <- formula[[3]]
    has_response <- TRUE
  } else {
    # Einseitige Formel (~x)
    lhs <- NULL
    rhs <- formula[[2]]
    has_response <- FALSE
  }

  # Funktion zur Ersetzung der Punkte
  replace_dots  <- function(expr) {
    if (identical(expr, quote(.))) {
      return(parse(text = paste(
        setdiff(all_vars, all.vars(formula)), collapse = " + "
      ))[[1]])
    } else if (is.call(expr)) {
      as.call(lapply(expr, replace_dots))
    } else {
      expr
    }
  }

  # Rechte Seite verarbeiten
  rhs <- replace_dots(rhs)

  # Linke Seite verarbeiten (falls vorhanden)
  if (has_response) {
    lhs <- replace_dots(lhs)
    lhs <- paste(deparse(lhs), collapse = " ")
    rhs <- paste(deparse(rhs), collapse = " ")
    # Neue Formel erstellen
    as.formula(paste(lhs, "~", rhs), env = environment(formula))
  } else {
    # Neue Formel ohne Response erstellen
    rhs <- paste(deparse(rhs), collapse = " ")
    as.formula(paste("~", rhs), env = environment(formula))
  }
}


#' Extract left-hand side of formula
#'
#' @param formula Input formula
#' @keywords internal
LHS <- function(formula) {
  if (length(formula) == 3) {
    formula[[2]]
  } else {
    formula[[2]]
  }
}


#' Extract right-hand side of formula
#'
#' @param formula Input formula
#' @keywords internal
#' @noRd
RHS <- function(formula) {
  if (length(formula) == 3) {
    formula[[3]]
  } else {
    NULL
  }
}



#' Mit hilfe von deepseek erstellt.
#' @import rlang
#' @keywords internal
#' @noRd
strip_functions <- function(formula,
                            keep = c("+", "-", "*", "/", ":", "|", "~"),
                            warn = TRUE) {
  # Process both sides of the formula
  new_lhs <- remove_functions(rlang::f_lhs(formula), keep = keep, warn = warn)
  new_rhs <- remove_functions(rlang::f_rhs(formula), keep = keep, warn = warn)

  rlang::new_formula(new_lhs, new_rhs)
}


#' Mit hilfe von deepseek erstellt.
#' @keywords internal
#' @noRd
remove_functions <- function(x, keep, warn) {

  if (is.call(x)) {
    if (as.character(x[[1]]) %in% keep) {
      # Keep the operator and process its arguments
      as.call(lapply(x, remove_functions, keep = keep, warn = warn))
    } else {
      if (warn && !is.name(x[[1]])) {
        warning("Removing function call: ", deparse(x))
      }
      # For other function calls, extract first argument
      remove_functions(x[[2]], keep = keep, warn = warn)
    }
  } else if (is.pairlist(x)) {
    as.pairlist(lapply(x, remove_functions, keep = keep, warn = warn))
  } else {
    x
  }
}



#' used in dapply

#' @keywords internal
#' @noRd
extracts_lhs <- function(x, data_names) {
  type_2_parts <- rlang::f_lhs(x)
  if (is.null(type_2_parts)) {
    rslt <- all.vars(rlang::f_rhs(x))
    if (rslt[1L] == ".")
      rslt <- data_names
  }
  else  {
    rslt <- all.vars(type_2_parts)
    if (rslt[1L] == ".") {
      rslt <-   setdiff(data_names , all.vars(rlang::f_rhs(x)))
    }
  }
  rslt
}
# extracts_lhs(~ . , letters[1:7])
# extracts_lhs(. ~  a + b + c , letters[1:7])
# extracts_lhs(a + b + c ~ . , letters[1:7])
# extracts_lhs(a + b + c ~ d , letters[1:7])
# extracts_lhs(~ a + b, letters[1:7])
#





# # Einfaches Beispiel
# fm1 <- log(m1) + m2[5] ~ sex+age
# strip_functions(fm1)
# # m1 + m2 ~ sex
#
# # Komplexeres Beispiel
 # fm2 <- sqrt(m1) + I(m2^2) + poly(m3, 2) ~ ns(age, 3) + sex
 # strip_functions(fm2)
# # m1 + m2 + m3 ~ age + sex
#
# # Mit Warnungen
# fm3 <- scale(m1) + log10(m2) ~ cut(age, 5)
# strip_functions(fm3, warn = TRUE)
# # Warning: Removing function call: scale(m1)
# # Warning: Removing function call: log10(m2)
# # Warning: Removing function call: cut(age, 5)
# # m1 + m2 ~ age

# @keywords internal
# internal_test_expect_equal <- function() {
#
# testthat::expect_equal(
#   create_formulas(
#     measure_vars = c("log(m1)", "m2", "m3", "m4"),
#     measure_vars_clean = c("log(m1)", "m2", "m3", "m4"),
#     by = ~ sex
#   ),
#   list(
#     formula_in = log(m1) + m2 + m3 + m4  ~ sex,
#     formula = m1 + m2 + m3 + m4 ~ sex,
#     formula_trans = log(m1) + m2 + m3 + m4  ~ sex,
#     by = ~ sex,
#     group.vars = "sex",
#     measure_vars_clean = c("m1", "m2", "m3", "m4"),
#     measure_vars = c("log(m1)", "m2", "m3", "m4"),
#     all_vars =     c("m1", "m2", "m3", "m4", "sex")
#   ) ,
#   ignore_attr = TRUE
# )
#
#
#   testthat::expect_equal(
#     create_formulas(
#       measure_vars = c("log(m1)", "m2", "m3", "m4"),
#       measure_vars_clean = c("log(m1)", "m2", "m3", "m4"),
#       by = NULL
#     ),
#
#     list(
#       formula_in = ~ log(m1) + m2 + m3 + m4,
#       formula =     ~ m1 + m2 + m3 + m4,
#       formula_trans =     ~ log(m1) + m2 + m3 + m4,
#       by = NULL,
#       group.vars = NULL,
#       measure_vars_clean = c("m1", "m2", "m3", "m4"),
#       measure_vars = c("log(m1)", "m2", "m3", "m4"),
#       all_vars =     c("m1", "m2", "m3", "m4")
#     ) ,
#     ignore_attr = TRUE
#   )
#
# }


#internal_test_expect_equal()



# expand_dot_formula( ~ . , letters[1:7])
# ~a + b + c + d + e + f + g
# expand_dot_formula( a +b +c ~ . , letters[1:7])
# a + b + c ~ d + e + f + g
# expand_dot_formula(. ~  a +b +c , letters[1:7])
# d + e + f + g ~ a + b + c



# all.vars(LHS(a + b ~ c + d))
# all.vars(LHS(~ c + d))
# all.vars(RHS(a + b ~ c + d))
# RHS(~ c + d)
# all.vars(RHS(. ~ c + d))


#
# names(create_formulas(
#   measure_vars = c("log(m1)", "m2", "m3", "m4"),
#   measure_vars_clean = c("log(m1)", "m2", "m3", "m4"),
#   by = ~ sex
# ))
#
# names(list(
#   formula_in = log(m1) + m2 + m3 + m4  ~ sex,
#   formula = m1 + m2 + m3 + m4 ~ sex,
#   formula_trans = log(m1) + m2 + m3 + m4  ~ sex,
#   by = ~ sex,
#   group.vars = "sex",
#   measure_vars_clean = c("m1", "m2", "m3", "m4"),
#   measure_vars = c("log(m1)", "m2", "m3", "m4"),
#   all_vars =     c("m1", "m2", "m3", "m4", "sex")
# ))
