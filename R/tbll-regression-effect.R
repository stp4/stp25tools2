#' Effect Displays for Regression Models
#'
#' A wrapper around \code{effects::effect()} and \code{effects::allEffects()} that
#' provides enhanced output formatting, including case counts, formatted values,
#' and confidence intervals.
#'
#'
#' Alternative: https://larmarange.github.io/broom.helpers/articles/marginal_tidiers.html
#'
#' @param x A regression model object (e.g., from \code{lm()}, \code{glm()})
#' @param term Character vector or formula specifying the effect term(s) to display.
#'   If \code{NULL} (default), all effects are displayed. Formulas allow complex
#'   term specifications like \code{~ type * education + women}.
#' @param ... Additional arguments passed to \code{effects::effect()} or
#'   \code{effects::allEffects()}, such as \code{transformation = list(link = log, inverse = exp)}
#' @param include.se Logical indicating whether to include standard errors in output
#' @param include.ci Logical indicating whether to include confidence intervals in output
#' @param include.n Logical indicating whether to include case counts in output
#' @param digits Number of digits for formatting numeric values
#'
#' @return A data frame or list of data frames containing the effect displays with
#'   requested statistics. Each data frame includes:
#'   \itemize{
#'     \item Predictor variables
#'     \item Formatted average effects
#'     \item Confidence intervals (if requested)
#'     \item Case counts (if requested)
#'     \item Standard errors (if requested)
#'   }
#'
#' @export
#' @importFrom effects effect allEffects
#' @importFrom insight find_response
#'
#' @examples
#' library(car)  # For Prestige dataset
#'
#' # Create regression model
#' model <- lm(prestige ~ type * education + income + women, data = Prestige)
#'
#' # Display all effects with case counts
#' Tbll_effect(model, include.n = TRUE, digits = 0)
#'
#' # Display specific interaction term
#' Tbll_effect(model, term = "type * education", include.n = FALSE, digits = 0)
#'
#' # Display multiple terms using formula syntax
#' Tbll_effect(model, term = ~ type * education + women, digits = 0)
#'
#' # Display individual terms (not recommended for interactions)
#' Tbll_effect(model, term = c("type", "education"), digits = 0)
#'
#'
#'
#' DF <- data.frame(
#'   y=c(12, 15, 23, 44, 11, 44, 49, 27, 25, 8,
#'       11, 10, 28, 15, 34, 20, 31, 55, 15, 34,
#'       47, 11, 27, 9, 12, 15, 7, 15, 42, 14,
#'       19, 24, 20, 10, 38, 28, 36, 9, 31),
#'   iq =c(91, 95, 103, 116, 88, 116, 118, 106, 105, 82,
#'         88, 87, 107, 95, 111, 100, 109, 120, 95, 111,
#'         117, 88, 106, 85, 91, 95, 81, 95, 115, 94,
#'         99, 104, 100, 87, 113, 107, 112, 84, 109 )
#' )
#' DF$log.y =  log(DF$y)
#'
#' fit.linear <- lm(y ~ iq, DF)
#' fit.log.y <- lm(log.y ~ iq, DF)
#' fit.model.log<- lm(log(y)~iq, DF)
#'
#'
#'
#' Tbll_effect(fit.linear, "iq", include.ci = FALSE)
#' Tbll_effect(fit.log.y, "iq", include.ci = FALSE)
#' Tbll_effect(fit.model.log,"iq", include.ci = FALSE)
#'
#'
#' Tbll_effect(fit.log.y,"iq",include.ci = FALSE,
#'             transformation =
#'               list(link = log, inverse = exp)
#' )
#'
#' Tbll_effect(fit.model.log,"iq",include.ci = FALSE,
#'             xlevels=list(iq= c(90,100,110)),
#'             transformation = list(link = log, inverse = exp)
#' )
#'
#'
#' effects::effect("iq", fit.model.log,
#'                 xlevels=list(iq= c(90,100,110)),
#'                 transformation =
#'                   list(link = log, inverse = exp)
#' )
#'
#'
#'
#'
#'
#' # p1 <-
#' #   plot(effect("iq", fit.linear ,
#' #               partial.residuals = TRUE),
#' #        main = "y (linear)")
#' #
#' # p2 <-
#' #   plot(effect("iq", fit.log.y,
#' #               partial.residuals = TRUE),
#' #        main = "log")
#' #
#' # p3 <- plot(effect("iq", fit.log.y,
#' #                   partial.residuals = TRUE,
#' #                   transformation =
#' #                     list(link =  log, inverse = exp)),
#' #            main = "log + trans")
#' #
#' # p4 <- plot(effect("iq", fit.model.log ,
#' #                   partial.residuals = TRUE),
#' #            main = "log(y)")
#' #
#' # p5 <-   plot(effect("iq", fit.model.log ,
#' #                     partial.residuals = TRUE,
#' #                     transformation =
#' #                       list(link =  log, inverse = exp)),
#' #              main = "log(y) + trans")
#' #
#' # require(cowplot)
#' # plot_grid(p1, p4, p5, NULL, p2, p3, ncol = 3)
Tbll_effect <-
  function(x,
           term = NULL, # to effects
           ...,         # to effects
           include.se = FALSE,
           include.ci = TRUE,
           include.n = FALSE,
           digits = 2) {

    # allEffects: alles evaluieren
    if (is.null(term)) {
      rslt <- tbll_extract_eff(
        effects::allEffects(x, ...),
        include.se = include.se,
        include.ci = include.ci,
        include.n = include.n,
        digits = digits
      )
    }
    else {
   # effect: ausgewähltes evaluieren
      if (inherits(term, "formula")) {

        trm <- unlist(gsub(" ", "", strsplit(as.character(term), "\\+")[[2L]]))

        return(
          Tbll_effect(
          x,
          term = trm,
          ...,
          include.se = include.se,
          include.ci = include.ci,
          include.n = include.n,
          digits = digits
     ))


      } else if (!is.character(term)) {
        stop("Nur Formulas oder Character sind erlaubt!")
      }


      if (length(term) == 1) {
        rslt <- tbll_extract_eff(
          effects::effect(term = term[[1]], mod = x, ...),
          include.se = include.se,
          include.ci = include.ci,
          include.n = include.n,
          digits = digits
        )
      } else{
        rslt <- list()
        for (i in seq_along(term) ) {
          nms <- gsub("\\*", "_", term[i] )
         rslt[[i]] <-
             tbll_extract_eff(
              effects::effect(term = term[i], mod = x, ...),
              include.se = include.se,
              include.ci = include.ci,
              include.n = include.n,
              digits = digits
            )
        }
      }
    }

    rslt
  }



#' Extract and Format Effect Data
#'
#' Internal function to process effect objects from the effects package.
#' Converts effect objects to data frames and applies formatting options.
#'
#' @param x An effect object of class "efflist" or "eff"
#' @param include.fit Logical indicating whether to include fitted values
#' @param include.se Logical indicating whether to include standard errors
#' @param include.ci Logical indicating whether to include confidence intervals
#' @param include.n Logical indicating whether to include case counts
#' @param digits Number of digits for formatting
#' @param type Transformation type: "response" or "link"
#' @param ... Additional arguments (not used)
#'
#' @return Formatted data frame or list of data frames
#' @keywords internal
#' @noRd
tbll_extract_eff <-
  function(x,
           # class: "efflist" or "eff"
           include.fit = TRUE,
           include.se = FALSE,
           include.ci = TRUE,
           include.n = FALSE,
           digits = 2,
           type = c("response", "link")
           )  {
    type <- match.arg(type)
    rslt <- NULL
    caption <- paste("Regression Models Response: ", insight::find_response(x))
    note <- ""

    # liste mit data.frameveraenderte
    # Copie von effects::as.data.frame()
    if (inherits(x, "eff"))
      rslt[[1]] <- as_data_frame_eff(x, type = type)
    else
      rslt <- lapply(x, as_data_frame_eff, type = type)

    # data-wide-format add N and format digits
    for (i in seq_along(rslt)) {
      if (include.n)   rslt[[i]]$n <- extract_n(x, i)
      if (include.fit) rslt[[i]]$Avarage <- format_num(rslt[[i]]$fit, digits)
      if (include.ci)  rslt[[i]]$CI <- rndr_CI(rslt[[i]][c("lower", "upper")], digits)
      if (include.se) rslt[[i]]$SE <- format_num(rslt[[i]]$se, digits)
      ex <-  which(names(rslt[[i]]) %in% c("fit", "se", "lower", "upper"))

      rslt[[i]] <-
        prepare_output(
          rslt[[i]][-ex],
          caption = paste(caption, names(rslt)[i] ))
    }

    if(length(rslt) == 1)  rslt[[1]] else  rslt
  }



#' Convert Effect Object to Data Frame
#'
#' Internal function to convert effect objects to data frames with proper
#' transformation handling.
#'
#' @param x An effect object
#' @param type Transformation type: "response" or "link"
#'
#' @return Data frame with effect estimates
#'
#' @note  Das Orginal geht nicht effects::as.data.frame()
#' Stand: Tue Apr 20 11:53:58 2021
#' primaer habe ich x$transformation$inverse ergaenzt
#'
#'
#'
#' @noRd
as_data_frame_eff <-
  function (x, type = c("response", "link")) {

    type <- match.arg(type)
    linkinv <- if (is.null(x$link$linkinv))
      I
    else
      x$link$linkinv
    linkmu.eta <- if (is.null(x$link$mu.eta))
      function(x)
        NA
    else
      x$link$mu.eta
    xx <- x$x
    for (var in names(xx)) {
      if (is.factor(xx[[var]])) {
        xx[[var]] <- addNA(xx[[var]])
      }
    }
    x$x <- xx

    result <-
      switch(type,
             response = {
        data.frame(
          x$x,
          fit = x$transformation$inverse(x$fit),
          se =  x$transformation$inverse(x$fit) * x$se,
          lower = x$transformation$inverse(x$lower),
          upper = x$transformation$inverse(x$upper)
        )},
      link = {
        data.frame(
          x$x,
          fit = x$fit,
          se = x$se,
          lower = x$lower,
          upper = x$upper
        )
      })
    attr(result, "type") <- type
    result
  }


#' Extract Case Counts from Effect Object
#'
#' Internal function to extract number of cases from effect objects.
#'
#' @param x An effect object
#' @param i Index of the effect (for lists)
#'
#' @return Numeric vector of case counts
#' @keywords internal
#' @noRd
extract_n <- function (x, i) {

  if (inherits(x, "eff"))
    x <- list(i = x)
  y <- names(x[[i]]$data)[1L]
  groups <- names(x[[i]]$variables)
  fm <- formula(paste0(y, "~", paste0(groups, collapse = "+")))

  var_is_factor <- lapply(x[[i]]$variables, function(z)z$is.factor)
  var_source <-    lapply(x[[i]]$variables, function(z)z$levels)

  for (j in groups) {
    if (!var_is_factor[[j]]) {
      breaks <- var_source[[j]]
      breaks <- rowMeans(cbind(dplyr::lag(breaks)[-1],
                               dplyr::lead(breaks)[-length(breaks)]))
      x[[i]]$data[[j]] <- cut(x[[i]]$data[[j]], c(-Inf, breaks, Inf))
    }
  }
  rslt_n <- aggregate(
    fm,
    x[[i]]$data,
    FUN = function(n)length(n), drop = FALSE
  )
  rslt_n[[ncol(rslt_n)]]

}
