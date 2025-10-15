#' @name Tbll_regression
#'
#' @title Enhanced Regression Results to Data Frame Converter
#'
#' @description
#' This function converts one or more regression model objects into a formatted
#' data frame using the parameters package for consistent parameter extraction.
#' It dynamically adapts output based on model type (lm, glm, etc.) and provides
#' extensive customization options.
#'
#' @param ... One or more regression model objects. Can be passed as unnamed
#'   arguments (will be named m1, m2, etc.) or as named arguments.
#' @param include.b Logical. If TRUE, includes coefficients. Default is TRUE.
#' @param include.p Logical. If TRUE, includes p-values. Default is FALSE.
#' @param include.ci Logical. If TRUE, includes confidence intervals. Default is TRUE.
#' @param include.se Logical. If TRUE, includes standard errors. Default is FALSE.
#' @param include.beta Logical. If TRUE, includes standardized coefficients (for lm models).
#'   Default is FALSE.
#' @param include.odds Logical. If TRUE, includes odds ratios (for glm models with logit link).
#'   Default follows include.b.
#' @param include.odds.ci Logical. If TRUE, includes confidence intervals for odds ratios.
#'   Default follows include.ci.
#' @param include.stars Logical. If TRUE, adds significance stars to the primary
#'   effect size measure (coefficients, betas, or odds ratios). Default is TRUE.
#' @param include.statistic Logical. If TRUE, includes test statistics (t, z values).
#'   Default is FALSE.
#' @param include.param Logical. If TRUE, includes model parameters section.
#'   Default is TRUE.
#' @param include.gof Logical. If TRUE, includes goodness-of-fit statistics.
#'   Default is TRUE.
#' @param model_names alternative Spalten bezeichnung
#' @param gof_vars was soll evaluiert werden
#'
#' @param digits insight::format_table
#' digits, ci_digits, p_digits, rope_digits, ic_digits
#' Number of digits for rounding or significant figures.
#'  digits = "scientific4", or digits = "signif5"
#'
#' @return A tibble with regression results. Structure depends on model types:
#' \itemize{
#'   \item \code{Parameter}: Variable names and GOF statistics
#'   \item For each model: Columns with requested statistics
#'   \item LM models: Coefficients, Beta (if requested), SE, CI, p-values
#'   \item GLM models: Coefficients, Odds Ratios (if logistic), SE, CI, p-values
#'   \item GOF section: AIC, BIC, R², RMSE, etc. (model-dependent)
#' }
#'
#' @details
#' The function uses parameters::model_parameters() and performance::model_performance()
#' for consistent extraction across different model types. Significance stars are
#' automatically applied to the most relevant effect size measure:
#' - LM models: Coefficients (or Betas if requested)
#' - GLM logistic models: Odds ratios (if requested) or coefficients
#' - Other GLM models: Coefficients
#'
#' Available GOF statistics depend on model type and may include:
#' AIC, AICc, BIC, R², adjusted R², Tjur's R², RMSE, Sigma, Number of observations
#'
#' @seealso
#' \code{\link[parameters]{model_parameters}}, \code{\link[performance]{model_performance}}
#'
#' @author Enhanced with assistance from Claude Sonnet 4 (claude-sonnet-4-20250514)
#'
#' Well, Claude actually only wrote the documentation. The proposed changes were all buggy and bloated. But it was a helpful learning process.
#'
#'
#' @export
#'
#' @importFrom parameters model_parameters
#' @importFrom performance model_performance
#' @importFrom insight format_table
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows full_join
#'
#' @examples
#' # Beispieldaten und Modelle
#' dat <- data.frame(
#'   treatment = gl(3, 3),
#'   outcome = gl(3, 1, 9),
#'   counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' )
#'
#' # Verschiedene Modelltypen
#' ols <- lm(vs ~ poly(mpg, 2) + cyl, data = mtcars)
#' binom <- glm(vs ~ poly(mpg, 2) + cyl, data = mtcars, family = binomial())
#'
#' # Grundlegende Verwendung
#' Tbll_regression(ols = ols, binom = binom)
#'
#' # Erweiterte Optionen
#' Tbll_regression(
#'   linear = ols,
#'   logistic = binom,
#'   include.beta = TRUE,
#'   include.odds = TRUE,
#'   include.odds.ci = TRUE,
#'   include.se = TRUE,
#'   include.p = TRUE,
#'   include.statistic = TRUE
#' )
#'
#' # Nur Parameter ohne GOF
#' Tbll_regression(ols, binom, include.gof = FALSE)
#'
#' # Nur GOF ohne Parameter
#' Tbll_regression(ols, binom, include.param = FALSE)
#' # Linear and logistic regression models
#' ols <- lm(vs ~ poly(mpg, 2) + cyl, data = mtcars)
#' binom <- glm(vs ~ poly(mpg, 2) + cyl, data = mtcars, family = binomial())
#'
#' # Basic usage
#' Tbll_regression(ols, binom)
#'
#' # Named models with custom options
#' Tbll_regression(
#'   linear = ols,
#'   logistic = binom,
#'   include.beta = TRUE,
#'   include.odds = TRUE,
#'   include.se = TRUE,
#'   include.p = TRUE
#' )
#' #'
#'
#' # Nur Parameter ohne GOF
#' rslt <- Tbll_regression(ols, binom, digits =5,
#'                         include.gof = FALSE, include.stars = FALSE)
#'
#' round(as.numeric(rslt[[2]]),3) == round(coef(ols), 3)
#' round(as.numeric(rslt[[3]]), 3) == round(coef(binom), 3)
Tbll_regression <- function(...,
                                      include.b = TRUE,
                                      include.ci = FALSE,
                                      include.se = FALSE,
                                      include.beta = FALSE,
                                      include.odds = TRUE,
                                      include.odds.ci = FALSE,
                                      include.p = FALSE,
                                      include.stars = !include.p,
                                      include.statistic = FALSE,
                                      include.param = TRUE,
                                      include.gof = TRUE,
                                      model_names =NULL,
                                      digits= 2,
                                      gof_vars = c("AIC", "AICc", "BIC",
                                                    "R2", "R2_adjusted", "Tjurs_R2",
                                                    "RMSE", "Sigma", "Obs")
                                      ) {

  models <- list(...)
  number_models <- length(models)
  # Modellnamen generieren
  if (is.null(model_names) | number_models != length(model_names)) {
    model_names <- if (!is.null(names(models))) {
      names(models)
    } else {
      paste0("m", seq_along(models))
    }
  }


  print(model_names)
  # Dynamisch verfügbare Parameter-Variablen generieren
  param_vars <-  "Parameter"
  if (include.b) param_vars <- c(param_vars, "Coefficient")
  if (include.beta) param_vars <- c(param_vars, "Std_Coefficient")
  if (include.odds) param_vars <- c(param_vars, "OR")
  if (include.se) param_vars <- c(param_vars, "SE")
  if (include.ci) param_vars <- c(param_vars, c("CI", "95% CI"))
  if (include.odds.ci) param_vars <- c(param_vars, "OR_CI")
  if (include.statistic) param_vars <- c(param_vars, "Statistic")
  if (include.p) param_vars <- c(param_vars, "p")


   # GOF (Goodness of Fit) Statistiken
  model_gof <- NULL

  if (include.gof) {

    for (i in seq_along(models)) {
      model <- models[[i]]
        gof <- render_f(performance::model_performance(model), 2)

         gof$Obs  <- as.character(nobs(model))
        #  print(str(gof))

        if (i == 1) {
          model_gof <- gof
        } else {
          model_gof <- dplyr::bind_rows(model_gof, gof)
        }
    }
    # Nur verfügbare GOF-Variablen behalten
    gof_vars <- intersect(gof_vars, names(model_gof))
    model_gof <- model_gof[gof_vars]
    model_gof <- t(model_gof)
    # colnames(model_gof) <- model_names
    model_gof <- cbind(data.frame(Parameter = gof_vars), model_gof)

  }

  #print(model_gof)

  # Parameter-Extraktion
  model_param <- NULL
  if (include.param) {
    for (i in seq_along(models)) {
      # Modelltyp bestimmen
      model <- models[[i]]
      model_class <- class(model)[1]
      print(model_class)
      #  model_family <- if ("glm" %in% class(model)) model$family$family else NULL
      is_logistic <- model_class == "glm"# && !is.null(model_family) && model_family == "binomial"


      model_name <- model_names[i]
      param_raw <- parameters::model_parameters(model)
      param <-
        tibble::as_tibble(
          insight::format_table(
            insight::standardize_names(param_raw),
            pretty_names = TRUE,
            stars = FALSE,
            stars_only = FALSE,
            digits = digits,
            ci_width = "auto",
            ci_brackets = TRUE,
            ci_digits = digits,
            p_digits = 3,
            rope_digits = digits,))


        # Standardized Coefficients (nur für lm-Modelle sinnvoll)
      if (include.beta && model_class == "lm") {
            param_std <-  parameters::model_parameters(model, standardize = "refit")
            param$Std_Coefficient <- render_f(param_std[[2]], 2)
          }

        # Odds Ratios (nur für logistische Regression)
      if (include.odds && is_logistic) {
        param_odds <-
          insight::standardize_names(parameters::model_parameters(
            model, exponentiate = TRUE))
        param$OR <- format_odds_ratio(param_odds$Coefficient, 2)
        # Odds Ratio Confidence Intervals
        if (include.odds.ci)
          param$OR_CI <- format_odds_ci(param_odds$CI_low, param_odds$CI_high, 2)
      }

        # Sterne hinzufügen
      if (include.stars ) {
         if(include.odds && is_logistic)
           param$OR <- add_significance_stars( param$OR, param$p)
         else if (include.b)
           param$Coefficient <- add_significance_stars( param$Coefficient, param$p)
        }


     param_vars_temp <- intersect(param_vars, names(param))
     param <- param[param_vars_temp]
     if(number_models > 1) names(param) <- c(param_vars_temp[1], paste0(model_name, "_", param_vars_temp[-1]))

    # Mergen mit vorherigen Modellen
     if (i == 1)
       model_param <-  param
     else
       model_param <- dplyr::full_join(model_param, param, by = "Parameter")
    }
  }


  # Ergebnisse zusammenfügen
  if (include.gof & include.param) {

    if(number_models > 1) names(model_gof) <-
      c(param_vars[1], paste0(model_names, "_", param_vars[2]))
    else  names(model_gof) <- param_vars[1:2]

    dplyr::bind_rows(model_param, c(Parameter = "Goodnes of fit"), model_gof)
  }
  else if (include.param)
    model_param
  else
    model_gof
}

# Funktion zum Hinzufügen von Sternen
add_significance_stars <- function(values, p_values) {
  if (is.null(values) || is.null(p_values)) return(values)
  stars <- sapply(p_values, function(p) {
    if (is.na(p)) return("")
    if (p <= 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    return("")
  })
  paste0(values, stars)
}

# Funktion zur Formatierung von Odds Ratios
format_odds_ratio <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0) return(x)

  sapply(x, function(val) {
    if (is.na(val)) return(NA_character_)

    # Numerischen Wert sicherstellen
    val <- as.numeric(val)

    if (val >= 10) {
      return(">10")
    } else if (val <= 0.01) {
      return("<0.01")
    } else {
      return(format(round(val, digits), nsmall = digits, scientific = FALSE))
    }
  })
}

# Funktion zur Formatierung von OR Konfidenzintervallen
format_odds_ci <- function(ci_low, ci_high, digits = 2) {
  if (is.null(ci_low) || is.null(ci_high) || length(ci_low) == 0) return(NULL)

  sapply(seq_along(ci_low), function(i) {
    low <- as.numeric(ci_low[i])
    high <- as.numeric(ci_high[i])

    if (is.na(low) || is.na(high)) return(NA_character_)

    # Formatierung der Grenzen
    low_formatted <- if (low <= 0.01) "<0.01"
    else if (low >= 10) ">10"
    else format(round(low, digits), nsmall = digits, scientific = FALSE)

    high_formatted <- if (high <= 0.01) "<0.01"
    else if (high >= 10) ">10"
    else format(round(high, digits), nsmall = digits, scientific = FALSE)

    return(paste0("[", low_formatted, ", ", high_formatted, "]"))
  })
}





