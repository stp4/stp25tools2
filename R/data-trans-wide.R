#' Convert Long Data to Wide Format with Flexible Input Options
#'
#' A more user-friendly wrapper around `tidyr::pivot_wider()` that supports
#' both formula and variable name syntax for reshaping data.
#'
#' @param data A data frame to reshape
#' @param key Either a formula (e.g., month ~ student) or unquoted column name(s)
#' @param ... Columns to use as values (unquoted names)
#' @param values_fill Value to fill in missing combinations (default: NA)
#' @param names_sep Separator for new column names (default: "_")
#' @param names_vary Controls column name order ("fastest" or "slowest")
#'
#' @return A tibble in wide format
#' @export
#'
#' @examples
#' df <- data.frame(
#'   month = rep(month.abb[1:3], 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7),
#'   C = c(1, 3, 6, 3, 4, 7)
#' )
#'
#' # Using variable names
#' Wide(df, student, A, B, C)
#'
#' # Using formula syntax
#' Wide(df, month ~ student, A, B, C)
#' Wide(df, ~ student, A, B)
#'
Wide <- function(data,
                 key,
                 ...,
                 values_fill = NULL,
               #  id_cols = NULL,
                 names_sep = "_",
                 names_vary = "slowest") {

  # Input validation
  if (!is.data.frame(data)) {
    stop("First argument must be a data frame", call. = FALSE)
  }

  # Capture arguments
  key_expr <- rlang::enexpr(key)
  value_exprs <- rlang::enquos(...)

  # Determine values_from columns
  values_from <- handle_values_from(value_exprs, data, key_expr)

  # Process key specification (formula or column name)
  specs <- handle_key_spec(key_expr, data, values_from)

  print(  values_fill)

if(is.null(specs$id_cols))
rslt <-
  tidyr::pivot_wider(
    data = specs$data,
    names_from = tidyselect::all_of(specs$names_from),
    values_from = tidyselect::all_of(values_from),
    values_fill = values_fill,
    names_sep = names_sep,
    names_vary = names_vary
  )
  else{
    rslt<-
      tidyr::pivot_wider(
        data = specs$data,
        id_cols = tidyselect::all_of(specs$id_cols),
        names_from = tidyselect::all_of(specs$names_from),
        values_from = tidyselect::all_of(values_from),
        values_fill = values_fill,
        names_sep = names_sep,
        names_vary = names_vary
      )

  }

  # cleanup
  if (names(rslt)[1L] =="unique_group_id")  rslt <- rslt[-1]

  # namen in richtiger reigenfolge
  if (names_vary == "slowest")
    swap_names_after(rslt)
  else
    rslt



}

# Helper functions --------------------------------------------------------


 swap_names_after <- function(x) {
    input <- names(x)
    output <- ifelse(grepl("_", input), sub("(.*)_(.*)", "\\2_\\1", input), input)
    names(x) <- output
    x
  }




handle_values_from <- function(value_exprs, data, key_expr) {

  #print(value_exprs)
  if (length(value_exprs) > 0) {
    # Explicit values provided in ...
    #  cat("\nin length>0\n")
    values_from <- purrr::map_chr(value_exprs, rlang::as_name)
  } else if (rlang::is_formula(key_expr)) {
    # Implicit values - all columns not in formula
    # cat("\nin formula>0\n")

    formula_vars <- all.vars(key_expr)
    values_from <- setdiff(names(data), formula_vars)

    if (length(values_from) == 0) {
      stop("No value columns found - please specify values or check formula",
           call. = FALSE)
    }
  } else {
    stop("Values must be specified when using column name syntax",
         call. = FALSE)
  }

  # Verify columns exist
  missing_cols <- setdiff(values_from, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  values_from
}

handle_key_spec <- function(key_expr, data, values_from) {

  id_cols <- NULL
  if (rlang::is_formula(key_expr)) {
  #  formula_vars <- all.vars(key_expr)

    # Handle LHS of formula (if present)
    if (length(key_expr) == 3) {
       # Two-sided formula (y ~ x)
       names_from <- all.vars( rlang::f_rhs(key_expr)   )
       id_cols <- all.vars( rlang::f_lhs(key_expr)   )
       #   names_from <- formula_vars[-1]  # All except first var (LHS)
       #  id_cols <- formula_vars[1]      # First var is LHS
    } else {
      # One-sided formula (~x)
       cat("\nin formula\n")
      #print(  new_rhs <-  rlang::f_rhs(key_expr)   )
      names_from <-  all.vars(key_expr)
      id_cols <- NULL
    }

    # Select only needed columns
    keep_cols <- c(id_cols, names_from, values_from)
    data <- dplyr::select(data, tidyselect::all_of(keep_cols))

    # If no ID vars, create a row identifier
    if (is.null(id_cols)) {
      data <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(names_from))) |>
        dplyr::mutate(unique_group_id = dplyr::row_number()) |>
        dplyr::ungroup()

     # names_from <- c(names_from, "group_id")
    }

  } else {
    # Column name syntax
    names_from <- rlang::as_name(key_expr)
  }

  # Verify names_from columns exist
  missing_cols <- setdiff(names_from, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Key columns not found in data:", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  list(names_from = names_from,
       id_cols = id_cols,
       data = data)
}





