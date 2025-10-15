#' Reshape Wide Data to Long Format
#'
#' An enhanced version of `tidyr::pivot_longer()` that supports both formula and
#' variable name syntax, with additional features for handling labels and column specifications.
#'
#' @param x Either a data frame or a formula specifying the reshaping
#' @param data A data frame (required when x is a formula)
#' @param key Name for the new key column (default: "variable")
#' @param value Name for the new value column (default: "value")
#' @param use.label Logical indicating whether to use variable label (default: TRUE)
#' @param id.vars Columns to keep as identifier variables
#' @param ... Additional arguments passed to methods
#' @param by Formula specifying grouping variables (alternative to id.vars)
#' @param .list Optional list of data frames to bind before reshaping
#'
#' @return A data frame in long format
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
#' Long(df, A, B, by = ~month)
#'
#' # Using column positions
#' Long(df, 3, 4, 5, by = ~month)
#'
#' # Using formula syntax
#' Long(A + B ~ month, df, key = "student", value = "grade")
#'
#'
Long <- function(x, ...) {
  UseMethod("Long")
}

#' @rdname Long
#' @export
Long.formula <- function(x,
                         data,
                         key = "variable",
                         value = "value",
                         use.label = TRUE,
                         # include.total = FALSE,
                         # das macht Probleme mit anderen Teilen des Codes
                         # und zwar in Summarise_likert
                         # Tbll_likert, Tbll_lmulti
                         ...) {
  # Validate inputs
  if (missing(data)) stop("data argument is required with formula input")
  if (!is.data.frame(data)) stop("data must be a data frame")

  # Handle formula with constant RHS (e.g., a + b + c ~ 1)
  if (length(x) == 3L && x[[3L]] == 1) x[[3L]] <- NULL

  # Process formula
  x <- expand_dot_formula(x, names(data))


  rhs_vars <- all.vars(x[-3]) # Right-hand side variables
  lhs_vars <- all.vars(x[-2]) # Left-hand side variables
 # cat("\ndata\n")

  # Select relevant columns
  data <- data[unique(c(rhs_vars, lhs_vars))]

  # Handle variable -attribut label
  if (use.label) {
    lvl <- get_label(data[rhs_vars])
    if (length(unique(lvl)) != length(lvl)) {
      stop("Variable labels are not unique:\n",
           paste(utils::capture.output(print(lvl)), collapse = "\n"))
    }
  } else {
    lvl <- rhs_vars
    names(lvl) <- rhs_vars
  }


  # Reshape data
  result <- tidyr::pivot_longer(
    data,
    cols = tidyselect::all_of(rhs_vars),
    names_to = key,
    values_to = value
  )

  # Convert key to factor with labels

  # if (length(lhs_vars) != 0 & include.total) {
  #   total_result <- tidyr::pivot_longer(
  #     data[unique(rhs_vars)],
  #     cols = tidyselect::all_of(rhs_vars),
  #     names_to = key,
  #     values_to = value
  #   )
  #   result <-
  #     dplyr::bind_rows(result, total_result)
  #   result[[1]][is.na(result[[1]])] <- "Total"
  #   lvl <- c(lvl, Total = "Total")
  # }
  result[[key]] <- factor(result[[key]], levels = names(lvl), labels = lvl)

  result
}

#' @rdname Long
#' @export
Long.data.frame <- function(x,
                            ...,
                            by = NULL,
                            key = "variable",
                            value = "value",
                            id.vars = all.vars(by),
                            use.label = TRUE,
                          #  include.total = FALSE,
                            .list = NULL) {
  dots <- rlang::enquos(...)
  first_expr <-  rlang::quo_squash(dots[[1]])

  # Handle list input
  if (!is.null(.list)) {
    return(
      Long_rbind(
        x,
        .list,
        by = by,
        key = key,
        value = value
      )
    )
  }

  # Handle formula in measure.vars
  if (rlang::is_call(first_expr, "~")) {
    formula <- eval(first_expr, envir = rlang::caller_env())
    return(
      Long.formula(
        formula,
        data = x,
        key = key,
        value = value,
        use.label = use.label
      )
    )
  }else if(rlang::is_call(first_expr, "list" )){
    cat( "\nSwitch to Long_rbind\n")
    return(
      Long_rbind(
        data = x,
        eval(first_expr, envir = rlang::caller_env()),
        by = by,
        key = key,
        value = value
      )
    )

  }

  # Get measure variables
  measure_vars <- get_measure_vars(dots, data = x)

  # Clean up measure variable names
  measure_vars <- clean_names(measure_vars, data = x)

  # If no measure vars specified, use all non-id vars
  if (length(measure_vars) == 0) {
    measure_vars <- if (length(id.vars) == 0) names(x) else names(x[-id.vars])
  }

  # Check for duplicate measure vars
  if (length(unique(measure_vars)) != length(measure_vars)) {
    stop("Duplicate measure variables detected")
  }
  # cat("\nData:\n")
  # print(unique(c(measure_vars, id.vars)))
  # print(names(x))
  # Select relevant columns
  data <- x[unique(c(measure_vars, id.vars))]

  # Handle variable labels
  if (use.label) {
    lvl <- get_label(data[measure_vars])
    if (length(unique(lvl)) != length(lvl)) {
      warning("Oh je da sind wohl die Label bei den Items identisch!")
    }
  } else {
    lvl <- measure_vars
    names(lvl) <- measure_vars
  }

  # Reshape data
  result <- tidyr::pivot_longer(
    data,
    cols = tidyselect::all_of(measure_vars),
    names_to = key,
    values_to = value
  )


  # if (length(id.vars) != 0 & include.total) {
  #   total_result <- tidyr::pivot_longer(
  #     data[unique(measure_vars)],
  #     cols = tidyselect::all_of(measure_vars),
  #     names_to = key,
  #     values_to = value
  #   )
  #   result <-
  #     dplyr::bind_rows(result, total_result)
  #   result[[1]][is.na(result[[1]])] <- "Total"
  #   lvl <- c(lvl, Total = "Total")
  # }


  # Convert key to factor with labels
  result[[key]] <- factor(result[[key]], levels = names(lvl), labels = lvl)

  result
}





#' Reshape Wide Data to Long Format by Binding Rows
#'
#' Transforms wide-format data into long-format by binding rows according to a specified
#' list of column groups. Each group becomes a time point in the long format.
#'
#' @param data A data frame in wide format
#' @param .list A named list specifying column groups (e.g., list(t0 = c("col1", "col2")))
#' @param by Formula or character vector specifying ID variables to keep
#' @param key Name for the new key column (default: "variable")
#' @param value Names for the value columns (default: paste("value", seq_along(.list), sep = "."))
#' @param ... Additional arguments (currently unused)
#'
#' @return A data frame in long format
#' @export
#'
#' @examples
#'
#' df2 <- tibble::tribble(
#'   ~id, ~group, ~age, ~arg1, ~glu1, ~cys1, ~arg2, ~glu2, ~cys2, ~arg3, ~glu3, ~cys3,
#'   1, "Control", 29, 2, 12, 33, 7, 14, 35, 8, 14, 36,
#'   2, "Control", 28, 3, 13, 44, 7, 14, 35, 9, 14, 36,
#'   3, "Treat",   35, 4, 14, 55, 9, 14, 34, 8, 15, 35,
#'   4, "Treat",   55, 5, 15, 66, 9, 13, 35, 8, 14, 35
#' )
#'
#'  # das geht auch mit Long(data, ... )
#'
#' df2 |> Long_rbind(
#'   .list = list(
#'     t0 = c("arg1", "glu1", "cys1"),
#'     t1 = c(NA, "glu2", "cys2"),
#'     t2 = c("arg3", "glu3", "cys3")
#'   ),
#'   by = ~id + group + age,
#'   value = c("Argin", "Glutaminsr", "Cystein"),
#'   key = "Time"
#' )
#'
#'
#'
#' df2 <- tibble::tribble(
#'   ~id, ~group, ~age, ~arg.mean, ~arg.sd, ~cys.mean, ~cys.sd,
#'   1, "Control", 29,    5.2, 1.2, 33, 17,
#'   2, "Control", 28,    6.3, 1.3, 44, 12,
#'   3, "Treat",   35,    4.4, 1.4, 55, 19,
#'   4, "Treat",   55,    5.5, 1.5, 66, 11
#' )
#'
#' df2 |> Long_rbind(
#'   .list = list(
#'     Argin  = c("arg.mean", "arg.sd"),
#'     Glutaminsr   = c("cys.mean", "cys.sd")
#'
#'   ),
#'   by = ~id + group + age,
#'   value = c("Mean", "SD"),
#'   key = "Aminosr"
#' )
#'
Long_rbind <- function(data,
                       .list = list(NULL),
                       by = NULL,
                       key = "variable",
                       value = NULL,
                       ...) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!is.list(.list) || is.null(names(.list))) {
    stop(".list must be a named list")
  }

  if (length(unique(lengths(.list))) != 1) {
    stop("All elements in .list must have the same length. Use NA as placeholder.")
  }

  # Initialize result
  result <- NULL
  times_names <- names(.list)

  # Set default value names if not provided
  if (is.null(value)) {
    value <- paste("value", seq_along(.list[[1]]), sep = ".")
  } else if (length(value) != length(.list[[1]])) {
    stop("Length of 'value' must match the number of columns in each .list element")
  }



  # Process each time point
  for (time in times_names) {
    cols <- .list[[time]]

    # Handle NA placeholders
    if (any(is.na(cols))) {
      for (j in seq_along(cols)) {
        if (is.na(cols[j])) {
          new_col <- paste0("placeholder_", j)
          cols[j] <- new_col
          data[[new_col]] <- NA
        }
      }
    }

    # Verify columns exist
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Columns not found in data: ",
           paste(missing_cols, collapse = ", "))
    }

    # Create time-specific data
    temp_data <- data[cols]
    temp_data_key<- data.frame()
    names(temp_data) <- value
    temp_data[[key]] <- time
    temp_data <- cbind(temp_data[ncol(temp_data)],
                       temp_data[-ncol(temp_data)])

    # Add ID variables if specified
    if (!is.null(by)) {
      id_vars <- all.vars(by)
      temp_data <- cbind(data[id_vars], temp_data)
    }

    # Remove labels
    temp_data <- remove_label(temp_data)

    # Combine results
    result <- dplyr::bind_rows(result, temp_data)
  }

  # Convert key to factor with original time ordering
  result[[key]] <- factor(result[[key]], levels = times_names)

  result
}




# Helper functions --------------------------------------------------------


#' Get measure variables from dots
#'
#' @param dots Arguments (enquos)
#' @param data The input data frame
#' @return Character vector of measure variables
get_measure_vars <- function(dots, data) {
  #  dots <- rlang::enquos(...)
  if (length(dots) == 0) return(character(0))

  vars <- character(0)
  for (i in seq_along(dots)) {
    expr <- rlang::quo_get_expr(dots[[i]])
    if (is.numeric(expr)) {
      vars <- c(vars, names(data)[expr])
    } else if (is.character(expr)) {
      vars <- c(vars, expr)
    } else if (is.symbol(expr)) {
      vars <- c(vars, as.character(expr))
    }
  }
  vars
}

#' Clean variable names
#'
#' @param vars Character vector of variable names/positions
#' @param data The input data frame
#' @return Cleaned variable names
clean_names <- function(vars, data) {
  # Handle numeric indices
  num_vars <- suppressWarnings(as.numeric(vars))
  if (any(!is.na(num_vars))) {
    vars[!is.na(num_vars)] <- names(data)[num_vars[!is.na(num_vars)]]
  }

  # Remove empty/NA names
  vars <- vars[vars != "" & !is.na(vars)]

  # Return unique names
  unique(vars)
}


# df <- data.frame(
#   month = rep(month.abb[1:3], 2),
#   student = rep(c("Amy", "Bob"), each = 3),
#   A = c(9, 7, 6, 8, 6, 9),
#   B = c(6, 7, 8, 5, 6, 7),
#   C = c(1, 3, 6, 3, 4, 7)
# )
#
# # Using variable names
# Long(df, A, B, by = ~month, include.total = TRUE) -> x1
# Long(df, A, B, include.total = TRUE) -> x2
# #
# # Long( A +  B ~month,  df, include.total = TRUE)->x1
# # Long( ~A +  B,  df, include.total = TRUE)->x2
