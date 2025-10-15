#' Prepare Data for Statistical Analysis
#'
#' A versatile function that prepares data for statistical analysis using formula interface.
#' Supports both formula and data frame input methods, handles variable specifications,
#' and manages grouping structures.
#'
#' @note This documentation was developed with assistance from DeepSeek AI.
#'
#' @name prepare_data
#' @aliases prepare_data.formula prepare_data.data.frame
#'
#' @section Methods:
#' \describe{
#'   \item{`prepare_data.formula`}{Formula interface for data preparation}
#'   \item{`prepare_data.data.frame`}{Data frame interface for data preparation}
#' }
#'
#' @param x A formula specifying variables and structure (for formula method)
#' @param data A data frame containing the variables to be analyzed
#' @param ... Additional arguments passed to methods:
#'   - For formula method: currently unused
#'   - For data frame method: variables to include in analysis (can include specifications)
#' @param by Grouping variable(s) specified as formula or character (data frame method only)
#' @param na.action How to handle missing values (default: `na.pass`)
#' @param drop.unused.levels Logical indicating whether to drop unused factor levels
#' @param is_for_plot logical internal for auto_plot()
#' @return A list with components for analysis including:
#' \itemize{
#'   \item `data`: Prepared data frame
#'   \item `measure.vars`: Analysis variables
#'   \item `group.vars`: Grouping variables
#'   \item `formula`: Analysis formula
#'   \item `measure`: Statistical measures specifications
#'   \item `measure.test`: Statistical tests specifications
#'   \item `digits`: Suggested rounding digits
#'   \item And other metadata for analysis
#' }
#'
#' @details
#' The function provides two interfaces:
#'
#' 1. **Formula interface**:
#'    - Accepts formulas like `y ~ x` or `y1 + y2 ~ x1 + x2`
#'    - Supports `.` to include all variables
#'    - Handles variable specifications in brackets (e.g., `x[1,aov,median]`)
#'    - Processes headings specified as quoted strings
#'
#' 2. **Data frame interface**:
#'    - Accepts variables as separate arguments
#'    - Same support for specifications and headings
#'    - Grouping via `by` parameter
#'
#' Both methods:
#' - Handle missing variables with informative warnings/errors
#' - Preserve variable labels and attributes
#' - Support complex variable specifications
#'
#' @export
#'
#' @importFrom rlang quo_squash quo_get_expr is_call caller_env
#' @importFrom stats na.pass na.omit formula
#' @importFrom plyr is.formula
#' @importFrom stringr str_extract
#' @examples
#' df <- data.frame(
#'   u = factor(letters[1:3]),
#'   w = 1:3,
#'   x = 1:3,
#'   y = 1:3,
#'   z = 1:3
#' )
#'
#' # Formula interface
#' prepare_data(~x + y + z, df)
#' prepare_data(x + y ~ z, df)
#' prepare_data(. ~ z, df)  # Using dot for all variables
#'
#' # With specifications and headings
#' prepare_data(x[1,aov,median] + "Heading" + y ~ z, df)
#'
#' # Data frame interface
#' prepare_data(df, x, y, by = ~z)
#' prepare_data(df, x[1,aov,median], "Heading", y, by = ~z)
#'
#' # Handle missing variables
#' try(prepare_data(df, missing_var ~ z))
#' prepare_data(df, missing_var, by = ~z)  # Will warn but continue
NULL

#' @rdname prepare_data
#' @export
prepare_data <- function(...) {
  UseMethod("prepare_data")
}



#' @rdname prepare_data
#' @export
prepare_data.formula  <- function(x,
                                  data,
                                  ...,
                                  groups = NULL,
                                  na.action = NULL,
                                  drop.unused.levels = FALSE,
                                  is_for_plot = FALSE) {
  x <- expand_dot_formula(x, names(data))
  measure_vars <- LHS(x)

  by <- RHS(x)
 # if (!is.null(by))
  #  by <-   rlang::new_formula(NULL, by)
    #by <- formula(paste("~", paste(by, collapse = "+")))
  measure_vars<- paste(deparse(measure_vars), collapse = " ")
  measure_vars <- strsplit(gsub(" ", "", measure_vars), "\\+")[[1]]
 # cat("\n\n after\n")
 # print( measure_vars )

  sub_heading_l <- grepl("\"|'", measure_vars)
  if (any(sub_heading_l)) {
    sub_heading <-  gsub("\"", "", measure_vars[sub_heading_l])
    measure_vars[sub_heading_l] <-
      paste0("h__", seq_len(sum(sub_heading_l)), "__h")
  } else
    sub_heading <- NULL
if(!is_for_plot)
  make_my_list(data, measure_vars, sub_heading, by, groups,
               na.action, drop.unused.levels)
else  make_my_plot_list(data, measure_vars, sub_heading, by, groups,
                   na.action, drop.unused.levels)
}



#' @rdname prepare_data
#' @export
prepare_data.data.frame  <- function(data,
                                     ...,
                                     by = NULL,
                                     groups = NULL,
                                     na.action = NULL,
                                     drop.unused.levels = FALSE,
                                     is_for_plot = FALSE) {
  dots <- rlang::enquos(...)
  first_expr <-  rlang::quo_squash(dots[[1]])

  if (rlang::is_call(first_expr, "~")) {
    formula <- eval(first_expr, envir = rlang::caller_env())
    return(
      prepare_data.formula(
        formula,
        data,
        ...,
        groups = groups,
        na.action = na.action,
        drop.unused.levels = drop.unused.levels
      )
    )
  }

  measure_vars <- character(length(dots))
  sub_heading <- c()

  for (i in seq_along(dots)) {
    expr <- rlang::quo_get_expr(dots[[i]])
    if (!is.character(expr))
      measure_vars[i] <- rlang::as_label(rlang::quo_squash(expr))
    else {
      sub_heading <- append(sub_heading, rlang::quo_squash(expr))
      measure_vars[i] <- paste0("h__", length(sub_heading), "__h")
    }
  }

  measure_vars <- convert_numeric(measure_vars, names(data))



  if(!is_for_plot)
    make_my_list(data, measure_vars, sub_heading, by, groups,
                 na.action, drop.unused.levels)
  else  make_my_plot_list(data, measure_vars, sub_heading, by, groups,
                          na.action, drop.unused.levels)

}




#' @rdname prepare_data
#' @param data A data frame
#' @param measure_vars Character vector of measurement variables
#' @param sub_heading Character vector of section headings
#' @param by Grouping specification (formula or character)
#' @param groups Stratifizierung specification (formula or character)
#' @keywords internal
make_my_list <-  function(data,
                          measure_vars,
                          sub_heading,
                          by, groups,
                          na.action = NULL,
                          drop.unused.levels = FALSE) {
  data <- add_headings_to_data(data, sub_heading)
 # print(measure_vars)
  measure_vars_clean <- sapply(strsplit(measure_vars, "\\["), "[", 1)

  dupl_measure <- duplicated(measure_vars_clean)
  if(any(dupl_measure)) {
    measure_vars_clean <- measure_vars_clean[!dupl_measure]
    measure_vars  <- measure_vars[!dupl_measure]


    warning("prepare_data():\n Es wurden folgende Parameter mehrfach uebergeben:\n" ,
            paste( measure_vars_clean[dupl_measure], collapse =", "),
            "\n  Sollte das gewollt sein bitte bei \nTbll_desc(..., use.duplicated = TRUE) \nentsprechend die Einstellungen vornehmen."
    )
  }

 # print(list(measure_vars=measure_vars, measure_vars_clean=measure_vars_clean , by =by))
  formulas <- create_formulas(measure_vars, measure_vars_clean, by, groups)

 # print(formulas)
  extras <-  gsub("\\]| ", "", sapply(strsplit(formulas$measure_vars, "\\["), "[", 2))

  # Fehlende Variablen behandeln
  data <- handle_missing_vars(data, formulas$measure_vars_clean, formulas$group.vars)
  data <- tibble::as_tibble(data[formulas$all_vars])
  lbl <- get_label(data)
  my_clss <-  get_classes(data)

  measure <- which_measure(extras, my_clss[formulas$measure_vars_clean], formulas$measure_vars_clean)
  measure.test <- which_test(extras, my_clss[formulas$measure_vars_clean], formulas$measure_vars_clean)
  digits <- which_digits(extras, measure,formulas$ measure_vars_clean)
  data <- transform_data(formulas$formula_trans, data, na.action, drop.unused.levels)

  stp25Data <-
    list(
      data            = data,
      # 'A tibble:'
      measure.vars    = formulas$measure_vars_clean,
      # character
      group.vars      = formulas$group.vars,
      # character
      condition.vars  = formulas$condition.vars[1],
      # not used
      formula         = formulas$formula,
      # formula
      by              = formulas$by,
      # formula or language or symbol
      measure         = measure,
      measure.test    = measure.test,
      row_name        = lbl[formulas$measure_vars_clean],
      col_name        = lbl[formulas$group.vars],
      measure.class   = my_clss[formulas$measure_vars_clean],
      group.class     = if(is.null(formulas$group.vars)) NULL else my_clss[formulas$group.vars],
      condition.class = if(is.null(formulas$condition.vars)) NULL else class(data[[formulas$condition.vars[1]]]),
      # not used
      digits          = digits,
      N               = nrow(data),
      formula_in      = formulas$formula_in # formula
      # na.action = na.action,
      #drop.unused.levels = drop.unused.levels
    )

  class(stp25Data) <- c("stp25data2", "list")
  stp25Data
}



make_my_plot_list <-  function(data,
                          measure_vars,
                          sub_heading,
                          by, groups,
                          na.action = NULL,
                          drop.unused.levels = FALSE) {
  measure_vars_clean <- sapply(strsplit(measure_vars, "\\["), "[", 1)
  dupl_measure <- duplicated(measure_vars_clean)
  if(any(dupl_measure)) {
    measure_vars_clean <- measure_vars_clean[!dupl_measure]
    measure_vars  <- measure_vars[!dupl_measure]

  stop("prepare_data():\n Es wurden folgende Parameter mehrfach uebergeben:\n" ,
            paste( measure_vars_clean[dupl_measure], collapse =", ")
    )
  }

  formulas <- create_formulas(measure_vars, measure_vars_clean, by, groups)
  extras <-  gsub("\\]| ", "",
                  sapply(
                    strsplit(formulas$measure_vars, "\\["), "[", 2))
  extras <- match.arg(extras,
                      c("pie","dot","hist","box","bar", NA),
                      several.ok = TRUE)

  data <- handle_missing_vars(data,
                              formulas$measure_vars_clean,
                              formulas$group.vars)
  data <- tibble::as_tibble(data[formulas$all_vars])
  lbl <- get_label(data)
  my_clss <-  get_classes(data)

  measure <-ifelse(is.na(extras),
                   my_clss[formulas$measure_vars_clean], extras)
  names(measure) <- formulas$measure_vars_clean

  data <- transform_data(formulas$formula_trans,
                         data, na.action,
                         drop.unused.levels)

  stp25Data <-
    list(
      data            = data,
      measure.vars    = formulas$measure_vars_clean,
      group.vars      = formulas$group.vars,
      row_name        = lbl[formulas$measure_vars_clean],
      col_name        = lbl[formulas$group.vars],
      group.class     = if(is.null(formulas$group.vars)) NULL else my_clss[formulas$group.vars],
      measure         = measure,
      formula         = formulas$formula
    )

  class(stp25Data) <- c("stp25data2", "list")
  stp25Data
}




#' Add section headings to data
#'
#' @param data A data frame
#' @param sub_heading Character vector of headings to add
#' @keywords internal
#' @noRd
add_headings_to_data <- function(data, sub_heading) {
  if (!is.null(sub_heading)) {
    nn <- ncol(data)
    data[paste0("h__", seq_along(sub_heading), "__h")] <- "NA_character_"
    for (n in seq_along(sub_heading))
      attr(data[[n + nn]], "label") <- sub_heading[[n]]
  }
  data
}

#' Determine appropriate statistical measures
#'
#' @param extras Character vector of extra specifications
#' @param classes Vector of variable classes
#' @param names Character vector of variable names
#' @keywords internal
#' @examples
#' # example code
#'
#' stp25tools2:::which_measure( "", "numeric" , "BMI")
#' # BMI = "numeric"
#' @noRd
which_measure <- function(extras, classes, names) {
  measure <-
    stringr::str_extract(
      extras,
      paste(names(get_opt("measure")), collapse = "|"))
  names(measure) <- names
  ifelse(is.na(measure), classes, measure)
}

#' Determine appropriate statistical tests
#'
#' Configure using the config.yml file.
#'
#' @param extras Character vector of extra specifications
#' @param classes Vector of variable classes
#' @param names Character vector of variable names
#' @keywords internal
#' @examples
#' # example code
#' stp25tools2:::which_test("freq", "integer" , "BMI")
#' # c(BMI = "contest")
#' @noRd
which_test <- function(extras, classes, names) {
  # catTest <- c("factor", "freq", "logical", "multi", "ratio")
  #  conTest <- c("numeric", "integer", "mean", "median")

  catTest <- sapply( get_opt("test"), "==", "catTest")
  catTest <- names(catTest[catTest])
  conTest <- sapply( get_opt("test"), "==", "conTest")
  conTest <- names(conTest[conTest])
  test <-
    stringr::str_extract(
      extras,
      paste(get_opt("test_fun") , collapse ="|")
    )
  classes <- ifelse(classes == "character",
                    "notest",
                    ifelse(
                      classes %in% catTest,
                      "cattest",
                      ifelse(classes %in% conTest, "contest", "notest")
                    ))
  names(test) <- names
  ifelse(is.na(test), classes, test)
}


#' Determine appropriate rounding digits
#'
#' @param extras Character vector of extra specifications
#' @param measure Vector of statistical measures
#' @param names Character vector of variable names
#' @keywords internal
#' @examples
#' # example code
#'
#' stp25tools2:::which_digits("mean,5,anova", "factor", "BMI")
#' #c(BMI = 5)
#' stp25tools2:::which_digits("mean,anova", "numeric", "BMI")
#' #c(BMI = 2)
#'
#'#' @noRd
which_digits <- function(extras, measure, names) {
  digits <- as.integer(gsub("[^0-9]", "", extras))
  # Lookup-Tabelle für measure
  measure_map <- sapply(get_opt("measure"), get_opt,"digits")
  # Werte aus der Map holen
  measure_val <- unlist(measure_map[measure])
  # Falls nicht im Mapping vorhanden -> NA
  measure_val[is.na(measure_val)] <- NA

  digits <-  ifelse(is.na(digits), measure_val, digits)
  names(digits) <- names
  digits
}

#' Get simplified variable classes
#'
#' @param data A data frame
#' @keywords internal
get_classes <-
  function(data) {
    sapply(data, function(x)
      setdiff(class(x), c("labelled", "ordered")))
  }


#' Handle Missing Variables
#'
#' Internal function to check for and handle missing variables in the dataset.
#' Adds missing variables as NA columns with informative attributes and warnings.
#'
#' @param data The input data frame
#' @param vars Variables that should be present
#' @param groups Grouping variables that must be present
#'
#' @return Modified data frame with missing variables added as NA columns
#' @keywords internal
#'
#' @noRd
handle_missing_vars <- function(data, vars, groups) {
  missing_vars <- setdiff(vars, names(data))
  missing_groups <- setdiff(groups, names(data))
  if (length(missing_groups) > 0)
    stop(
      "Oh je - da hast die die falschen groups genommen! Die Variable '",
      missing_groups,
      "' existiert nicht!"
    )
  if (length(missing_vars) > 0) {
    nn <- ncol(data)
    data[missing_vars] <- "NA_character_"
    for (n in seq_along(missing_vars)) {
      warning(
        "Oh je - da hast du die falschen Variable genommen! Die Variable '",
        missing_vars,
        "' existiert nicht, ich mache aber trotzdem weiter."
      )

      attr(data[[n + nn]], "label") <- paste("Error:", missing_vars[n], "does not exist!")
    }
  }
  data
}


#' Convert numeric indices or range specifications to variable names
#'
#' @param measure Input specification (numeric indices, character names, or range strings)
#' @param var_names  Names (Data frame) whose variable names should be used
#'
#' @return Character vector of variable names
#'
#' @keywords internal
#' @noRd
convert_numeric  <- function(measure, var_names) {
  #measure <- as.character(measure)

  # Auto-detect numeric patterns if not specified
  me_as_num <- grepl("^[[:digit:]]", measure)

  # Early return if no numeric indices
  if (!any(me_as_num)) {
    return(measure)
  }

  # Process each element
  result <- lapply(seq_along(measure), function(i) {
    if (me_as_num[i]) {
      if (grepl(":", measure[i], fixed = TRUE)) {
        # Handle range specification (e.g., "1:3")
        range <- as.numeric(strsplit(measure[i], ":")[[1]])
        if (anyNA(range) || length(range) != 2) {
          stop("Invalid range specification: ", measure[i])
        }
        seq_range <- seq(range[1], range[2])
        var_names[seq_range]
      } else {
        # Handle single numeric index
        index <- suppressWarnings(as.numeric(measure[i]))
        if (is.na(index)) {
          measure[i] # Return original if not convertible
        } else {
          var_names[index]
        }
      }
    } else {
      measure[i] # Return character names as-is
    }
  })

  result <- unique(unlist(result))

  if(any(is.na(result)))    stop("Nein, nein - da ist wohl was schief gegengen! Die Nummer ", which(is.na(result)),
                                 " ist falsch!")
  # Flatten and remove duplicates
  result
}


#' @rdname prepare_data
#'
#' @description
#' copy from stats::model.frame.default
#'
#'
#' @param formula formula.
#' @param data data.frame.
#' @param na.action function.
#' @param drop.unused.levels logical
#'
#' @returns data.frame
transform_data <- function(formula,
                           data,
                           na.action = NULL,
                           drop.unused.levels = FALSE) {
  if (!any(grepl("\\(", formula))) {
    if (is.null(na.action) & !drop.unused.levels)
      return(data)
    else {
      data <- na.action(data)
      #copy & paste from stats::model.frame.default
      if (drop.unused.levels) {
        for (nm in names(data)) {
          x <- data[[nm]]
          if (is.factor(x) && length(unique(x[!is.na(x)])) <
              length(levels(x))) {
            ctr <- attr(x, "contrasts")
            data[[nm]] <- x[, drop = TRUE]
            if (!identical(attr(data[[nm]], "contrasts"), ctr))
              warning(
                gettextf(
                  "contrasts dropped from factor %s due to missing levels",
                  nm
                ),
                domain = NA,
                call. = FALSE
              )
          }
        }
      }

    }
    data
  }
  else{
    formula <-  Formula::Formula(formula)
    # Process model frame
    mf <- tryCatch({
      if (is.null(na.action)) {
        stats::model.frame(formula, data, drop.unused.levels = drop.unused.levels)
      } else {
        stats::model.frame(formula,
                           data,
                           na.action = na.action,
                           drop.unused.levels = drop.unused.levels)
      }
    }, error = function(e) {
      warning("Error in model.frame: ", e$message, call. = FALSE)
    })

    # Get all variables including those used in transformations
    all_vars <- all.vars(formula)

    # Handle transformations in formula
    transformed_vars <- setdiff(colnames(mf), all_vars)
    if (length(transformed_vars) > 0) {
      names(mf) <- all_vars

      #   warning("Some variables in formula are transformed (e.g., log()). ",
      #           "Original variable names may not match column names.",
      #           immediate. = TRUE)
    }

    # Convert to tibble if requested
    tibble::as_tibble(mf)

  }
  # Return with informative attributes
  # structure(
  #   mf,
  #   class = c(if (.tibble) "tbl_df", class(mf)),
  #   formula = formula,
  #   all_vars = all_vars,
  #   transformed_vars = transformed_vars
  # )
}




#' @rdname prepare_data
#' @export
print.stp25data2 <- function(x, ...) {

  past_pls <- function(x)  paste(paste(names(x), "=", x), collapse = ", ")
  cat("\nformula: ")
  print(x$formula)
  cat("\nmeasure.vars: ", paste(x$measure.vars, collapse = ", "))
  cat("\nmeasure: ", past_pls(x$measure))
#  cat("\nmeasure.class: ", paste(x$measure.class , collapse = ", "))
  cat("\ndigits: ", past_pls(x$digits))

#  rn <- paste(names(x$row_name), "", x$row_name )
  cat("\nrow_name: ", past_pls(x$row_name ))
  cat("\ngroup.vars: ", paste(x$group.vars, collapse = ", "), "\n")
  cat("\nby: ")
  print(str(x$by))

  #  cat("\ncol_name: ", paste(x$col_name, collapse=", "),"\n")
  print(head(x$data))
}




