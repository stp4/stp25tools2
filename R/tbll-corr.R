#' @title Correlation Table Generator
#' @description R functions for creating formatted correlation tables with various options
#' @author Improved with assistance from Claude Sonnet 4 (claude-sonnet-4-20250514)
#' @note Enhanced version with improved error handling, documentation, and code structure
#'
#' @section Inspiration and Related Packages:
#' This script builds upon correlation analysis concepts from:
#' \itemize{
#'   \item \strong{Hmisc} (\url{https://cran.r-project.org/package=Hmisc}):
#'     Frank Harrell's comprehensive statistics package with rcorr() function
#'   \item \strong{corrr} (\url{https://github.com/tidymodels/corrr}):
#'     Tidy correlation matrices and network plots
#'   \item \strong{psych} (\url{https://cran.r-project.org/package=psych}):
#'     Procedures for personality, psychometric, and psychological research
#'   \item \strong{corrplot} (\url{https://github.com/taiyun/corrplot}):
#'     Visualization of correlation matrices
#' }
#'
#'
#' Create Formatted Correlation Table
#'
#' This function creates publication-ready correlation tables with flexible formatting
#' options. It supports correlation matrices, grouped correlations, and conditional
#' correlations using the Hmisc::rcorr() function as backend.
#'
#' @param ... Formula specification or data. Supports three formats:
#'   \itemize{
#'     \item \code{~ a + b + c}: Creates correlation matrix of variables a, b, c
#'     \item \code{a ~ b}: Correlates variable a with variable b
#'     \item \code{a + b + c ~ d}: Correlates variables a, b, c with variable d
#'   }
#' @param include.label Logical. If TRUE, includes variable labels in output.
#'   Default is TRUE.
#' @param include.mean Logical. If TRUE, includes means and standard deviations
#'   in correlation matrix. Default is FALSE.
#' @param include.n Logical. If TRUE, includes sample sizes for each correlation.
#'   Default is TRUE.
#' @param include.stars Logical. If TRUE, adds significance stars to correlations
#'   (*** p<0.001, ** p<0.01, * p<0.05, . p<0.1). Default is TRUE.
#' @param include.p Logical. If TRUE, includes explicit p-values instead of or
#'   in addition to significance stars. Default is FALSE.
#' @param cor_diagonale_up Logical. For correlation matrices, if TRUE shows
#'   correlations in upper triangle, if FALSE in lower triangle. Default is TRUE.
#' @param type Character. Correlation method, either "pearson" or "spearman".
#'   Default is "pearson".
#'
#' @return A tibble containing the formatted correlation results with the following
#' possible columns depending on the input format:
#' \itemize{
#'   \item For matrices: Source (variable names), numbered columns with correlations
#'   \item For grouped correlations: Characteristics, N, r, p.value (with group prefixes)
#'   \item Correlations are formatted to 2 decimal places
#'   \item P-values: <.001, .001-.01 (3 decimals), >.01 (2 decimals)
#' }
#'
#' @details
#' The function uses Hmisc::rcorr() for correlation calculations, which handles
#' missing values using pairwise deletion. For correlation matrices, the diagonal
#' can be displayed in the upper or lower triangle for better readability.
#'
#' Significance levels: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1
#'
#' @examples
#' # Create sample data
#' set.seed(1)
#' n <- 40
#' dat <- data.frame(
#'   group = gl(2, 20, labels = c("Control", "Treat")),
#'   a = rnorm(n, 0, 1),
#'   b = rnorm(n, 0, 1),
#'   c = rnorm(n, 0, 1),
#'   d = rnorm(n, 0, 0.5) + rnorm(n, 0, 1)
#' )
#'
#' # Correlation matrix
#' Tbll_corr(~ a + b + c, dat)
#'
#' # Single correlation
#' Tbll_corr(a ~ c, dat)
#'
#' # Multiple correlations with one variable
#' Tbll_corr(a + b + c ~ d, dat)
#'
#' # With custom options
#' Tbll_corr(~ a + b + c, dat,
#'          include.mean = TRUE,
#'          include.stars = FALSE,
#'          include.p = TRUE,
#'          type = "spearman")
#'
#' @seealso
#' \code{\link[Hmisc]{rcorr}}, \code{\link[stats]{cor}}, \code{\link[stats]{cor.test}}
#'
#' @export
Tbll_corr <-
  function(...,
           include.label = TRUE,
           include.mean = FALSE,
           include.n = TRUE,
           include.stars = TRUE,
           include.p = FALSE,
           cor_diagonale_up = TRUE,
           type = c("pearson", "spearman")) {
    X <- prepare_data(...)
 #   if (is.null(X$data))
 #     return(Info_Statistic("Correlation", "Hmisc", "rcorr"))
    type <-  match.arg(type)
    measure.vars <- X$measure.vars
    group.vars <- X$group.vars
    condition.vars <- X$condition.vars
    measure_data <-  apply(X$data[measure.vars], 2, as.numeric)

    if (is.null(group.vars))
      group_data <- NULL
    else
      group_data <- apply(X$data[group.vars], 2, as.numeric)

    if (is.null(condition.vars))
      condition_data <- NULL
    else
      condition_data <- X$data[[condition.vars[1]]]

    Hmisc_rcorr(
      measure_data = measure_data,
      group_data = group_data,
      condition_data = condition_data,
      measure.vars = measure.vars,
      group.vars = group.vars,
      condition.vars = condition.vars,
      row_name = X$row_name,
      cor_diagonale_up = cor_diagonale_up,
      include.stars = include.stars,
      include.p  = include.p,
      include.mean = include.mean,
      include.n = include.n,
      type = type
    )
  }




#' Internal Function for Correlation Calculations
#'
#' Internal workhorse function that performs the actual correlation calculations
#' using Hmisc::rcorr(). This function is called by Tbll_corr().
#'
#' @param measure_data,group_data,condition_data,group.vars,condition.vars Formula and data arguments passed from Tbll_corr()
#' @param row_name character
#' @param cor_diagonale_up Logical for triangle display in matrices
#' @param include.stars Logical for significance stars
#' @param include.p Logical for p-values
#' @param include.mean Logical for means and SDs
#' @param include.n Logical for sample sizes
#' @param type Character, correlation method
#'
#' @return Formatted tibble with correlation results
#' @keywords internal
Hmisc_rcorr <- function(measure_data,
                        group_data,
                        condition_data,
                        measure.vars,
                        group.vars,
                        condition.vars,
                        row_name,
                        cor_diagonale_up = TRUE,
                        include.stars = TRUE,
                        include.p  = FALSE,
                        include.mean = FALSE,
                        include.n = TRUE,
                        type) {
  N <- nrow(measure_data)
  ans <- NULL

  # Case 1: Conditional correlations (a + b ~ c | d)
  if (!is.null(condition.vars)) {
    # a + b ~ c | d
    if (is.null(group.vars))
      stop(
        "Oh nein das geht nicht du willst ja stratifizieren und das geht nur mit a +b ~c groups= ~d."
      )
    if (!is.factor(condition_data))
      stop("Achtung nur eine Faktor kann Gruppen bilden!")
    condition_data <- droplevels(condition_data)
    lvls <- levels(condition_data)
    for (i in seq_len(length(lvls))) {
      strata <- which(condition_data == lvls[i])
      ans_corr <-
        Hmisc::rcorr(measure_data[strata, ], type = type)
      k <- ncol(ans_corr$r)
      r <- ans_corr$r[-k, k]
      p <- ans_corr$P[-k, k]
      ans_corr <- data.frame(
        Characteristics = row_name ,
        N = round(ans_corr$n[-k, k], 0),
        r =  rndr_r(r, FALSE),
        p.value = rndr_P(p, FALSE),
        stringsAsFactors = FALSE
      )

      if (!include.n)
        ans_corr <- ans_corr[-2]

      names(ans_corr)[-1] <- paste0(lvls[i], "_", names(ans_corr)[-1])

      if (i == 1)
        ans <- ans_corr
      else
        ans <- cbind(ans, ans_corr[-1])

    }

  }
  # Case 2: Multiple correlations (a + b ~ c + d)
  else if (!is.null(group.vars)) {
    for (i in 1:(length(group.vars))) {
      ans_corr <- Hmisc::rcorr(measure_data,
                               group_data[, i],
                               type = type)
      k <- ncol(ans_corr$r)
      r <- ans_corr$r[-k, k]
      p <- ans_corr$P[-k, k]

      ans_corr <- data.frame(
        Characteristics = row_name ,
        N = round(ans_corr$n[-k, k], 0),
        r =  rndr_r(r, FALSE),
        p.value = rndr_P(p, FALSE),
        stringsAsFactors = FALSE
      )

      if (!include.n)
        ans_corr <- ans_corr[-2]

      if(!include.p) ans_corr <- ans_corr[-ncol(ans_corr)]

      names(ans_corr)[-1] <-
        paste0(group.vars[i], "_", names(ans_corr)[-1])

      if (i == 1)
        ans <- ans_corr
      else
        ans <- cbind(ans, ans_corr[-1])

    }
  }
  # Case 3: Correlation matrix (~ a + b + c)
  else{
    ans_list <- Hmisc::rcorr(measure_data, type = type)
    r <- rndr_r(ans_list$r, FALSE)
    # Format correlations with p-values/stars
    if (include.stars & !include.p) {
      r <- matrix(
        paste0(r, rndr_Stars(ans_list$P)),
        nrow = nrow(ans_list$r),
        dimnames = dimnames(ans_list$r)
      )
    } else if (include.p) {
      r <- matrix(
        paste0(
          r ,
          ", ",
          rndr_P(ans_list$P, TRUE, with.stars = include.stars)
        ),
        nrow = nrow(ans_list$r),
        dimnames = dimnames(ans_list$r)
      )
    } else
      NULL

    r <- format_diagonale(r, cor_diagonale_up)
    ans <- data.frame(Source = rownames(ans_list$r),
                      r,
                      stringsAsFactors = FALSE)

    # Labels
    my_num <- paste0("(", 1:length(row_name), ")")
    ans[, 1] <-   paste(my_num, row_name)
    colnames(ans)[2:ncol(ans)] <- my_num

    if (include.mean) {
      ans_mean <- apply(measure_data, 2, function(m) {
        rndr_mean(mean(m, na.rm = TRUE),
                  sd(m, na.rm = TRUE) ,
                  digits = get_opt("mean", "digits"),)
      })

      ans <- cbind(ans[1],
                   "M (SD)" = ans_mean,
                   ans[2:ncol(ans)],
                   stringsAsFactors = FALSE)
    }
  }



  prepare_output(ans,
                 caption = "Correlations",
                 note = type,
                 N = N)
}


format_diagonale <- function(mycorrtable,
                             cor_diagonale_up,
                             d = 1,
                             l = "") {
  diag(mycorrtable) <- d
  if (cor_diagonale_up)
    mycorrtable[lower.tri(mycorrtable)] <- l
  else
    mycorrtable[upper.tri(mycorrtable)] <- l

  mycorrtable
}
