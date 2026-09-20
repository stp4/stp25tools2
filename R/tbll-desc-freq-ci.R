#' @rdname Tbll_desc 
#' @description
#'  Tbll_desc_freq():  ist expliziet für die Evaluation von Haufigkeiten als 
#'  X-Table mit den 95 Prozent CIs. 
#'  Ausgabe der Spalten-Prozent mit Exact Binomial Test binom.test().
#'  
#' Percentages represent the proportion of respondents selecting each beverage; 
#' multiple responses were permitted. 95% confidence intervals were calculated
#' using the exact binomial method.
#' @param include.ci  logical. Tbll_desc_freq: 95-Prozent CIs für Tbll_desc_freq
#' @param p_adjust logical  or character. Tbll_desc_freq defaulte = "holm"  Adjust P-values for Multiple Comparisons
#' @export
#' @importFrom dplyr mutate group_by select 
#' @importFrom tidyselect all_of
#' @importFrom purrr map_dbl
#' @examples
#' 
#' DF <-
#' structure(list(
#'   q2.almdd = structure(
#'     c(TRUE, FALSE, FALSE, TRUE,TRUE, TRUE, TRUE, TRUE,
#'       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#'     label = "Alm Dudler"),
#'   q2.minrl = structure(
#'     c(TRUE, FALSE,FALSE, FALSE, FALSE, FALSE, FALSE,
#'       TRUE, TRUE, TRUE, FALSE, TRUE,TRUE, TRUE, TRUE),
#'     label= "Mineral"),
#'   q2.cola = structure(
#'     c(TRUE,TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE,
#'       TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
#'     label = "Cola"),
#'   q2.bier = structure(
#'     c(TRUE,FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
#'       FALSE, FALSE, FALSE,FALSE, TRUE, FALSE, FALSE, FALSE),
#'     label = "Bier"),
#'   q2.wein = structure(
#'     c(TRUE,FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
#'       FALSE, FALSE, FALSE,FALSE, FALSE, FALSE, FALSE, FALSE),
#'     label = "Wein")),
#'   row.names = c(NA,15L),
#'   class = "data.frame")
#' 
#' DF$sex <- factor(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2), 1:2, c("male", "female"))
#' DF<- rbind(DF,DF)
#' 
#' DF |>
#'   Tbll_desc_freq(
#'     q2.almdd,q2.minrl,q2.cola,
#'     q2.bier,q2.wein,
#'     by =  ~ sex,
#'     p_adjust = "bonferroni")  -> answer
#' # percent = prcnt, lower, upper
#' attr(answer, "data") |>
#'   filter( levels ==TRUE) |>
#'   ggplot(aes(x = percent, y = variable,  colour = sex)) +
#'   geom_errorbar(aes(xmin = lower, xmax = upper ), size = 0.8 ,
#'                 position = position_dodge(width = 0.5) ) + 
#'   geom_pointrange(aes(xmin = lower, xmax = upper ), size = 0.8 ,
#'                   position = position_dodge(width = 0.5) ) + 
#'   # facet_wrap(  sex  ~ .,
#'   #            scales = "free_y",
#'   #            space = "free_y",
#'   #            strip.position = "top") +
#'   labs(
#'     x = "Geschätzter Mittelwert mit 95%-KI", y = NULL
#'   ) +
#'   theme_bw(base_size = 12) +
#'   theme(
#'     strip.text = element_text(face = "bold", size = 11),
#'     panel.grid.minor = element_blank(),
#'     panel.spacing = unit(1, "lines")
#'   )
#' 
Tbll_desc_freq <- function(...,
                           prfx_in = "↳ ",
                           include.test = TRUE,
                           include.ci = TRUE,
                           include.label = TRUE,
                           include.n = FALSE,
                           conf.level = 0.95,
                           p_adjust = FALSE
                           
                           ) {
  answer <- NULL
  plot_data <- NULL
  # Objekt mit Daten measure, digits, labels
  X <-  prepare_data(...)
 
  if (!all(X$measure %in% c("factor", "logical")) | is.null(X$group.vars))  
     stop("Wir haben quasi eine X-Tabelle- nur Factoren sind erlaubt bei Haeufigkeiten und ich brauche eine Gruppe!")
   
  if (length(X$group.vars) > 1) {
    # sollte eine Gruppe 'group' heisen wird sie einfach überschrieben!
    X$data$group <- interaction2(X$data[X$group.vars], rev_order = TRUE)
    X$group.vars <- "group"
  } 
  
  caption <-  
    paste("Univariate description by", paste(X$col_name, collapse=" / "))
  note <-   
    if (include.ci)  
      paste0("Die ", conf.level*100, 
             "% Konfidenzintervalle wurden mit dem Binomial-Test (je Spalte) berechnet") else NULL
  
  if (!include.label) X$row_name <- X$measure.vars
  
  for (measure in X$measure.vars) {
    fm <- formula(paste("~", paste(measure, collapse = "+"), "+", X$group.vars))
    tab1 <- xtabs(fm, data = X$data, 
                  drop.unused.levels = TRUE)
    # calc percent 
    if (include.ci) {
       result <- 
        as.data.frame(tab1) |>
        dplyr::group_by(.data[[X$group.vars]]) |>
        dplyr::mutate(
          n = sum(Freq),
          prcnt = 100 * Freq / n,
          # Map over two inputs -> pair of vectors
          ci = purrr::map2(Freq, n, 
                           ~ stats::binom.test(.x, .y, conf.level = conf.level)$conf.int),
          lower = 100 * purrr::map_dbl(ci, 1),
          upper = 100 * purrr::map_dbl(ci, 2),
          percent = ff_percent_ci(prcnt, lower, upper, digits = X$digits[[measure]])
        ) 
       
       plot_dat <- result |>
         dplyr::mutate( variable =  X$row_name[[measure]]) |>
         dplyr::select(all_of(X$group.vars), variable,
                       levels = all_of(measure),
                       n = Freq, percent = prcnt, lower, upper)
         
       result <- result |>
        dplyr::select(all_of(X$group.vars), all_of(measure), n = Freq, percent) |>
        Wide(!!X$group.vars, n, percent)

    }
    else{
      result <- as.data.frame(tab1) |>
        dplyr::group_by(X$group.vars) |>
        dplyr::mutate(
          n = sum(Freq),
          prcnt = 100 * Freq / n,
          percent =   ff_percent(prcnt, digits = X$digits[[measure]])
        ) 
      plot_dat <- result |>
        mutate( variable =  X$row_name[[measure]]) |>
        dplyr::select(all_of(X$group.vars), variable,
                      levels = all_of(measure),
                      n = Freq, percent = prcnt)
      result <- result |>
        dplyr::select(all_of(X$group.vars), all_of(measure), n = Freq, percent) |>
        Wide(!!X$group.vars, n, percent)
    }
    
    names(result)[1] <- "Items"
    result[[1]] <- paste(prfx_in, result[[1]])
    
    result <- dplyr::bind_rows(
                data.frame(Items = X$row_name[[measure]]), 
                result)
    
    n_value <- seq(from = 2, to = ncol(result), by = 2)
    if(include.n)
      result[1, n_value] <-  colSums(result[n_value] , na.rm = TRUE)
    else {
      result <- result[-n_value]
      names(result) <- gsub("_percent", "", names(result))
      }
    
    if(X$measure[[measure]] == "logical") 
      result <- result[-2,]
    
    if (include.test) {
      # alternative aber fisher-Test
      #  print( summary(tab1) )
      # print(X$measure.test[[measure]])
      
      result$sig.Test <- NA
      tst <- auto_test(
        x =  X$data[[measure]],
        by = X$data[[X$group.vars]],
        measure.test = X$measure.test[[measure]]
      )
      note <- c(note, names(tst))
      result$sig.Test[1] <- tst
    }
    plot_data <- dplyr::bind_rows(plot_data, plot_dat)
    answer <- dplyr::bind_rows(answer, result)
  }
  
  if (is.character(p_adjust)  & include.test)
    answer <- P_adjust(answer, p_adjust, ncol(answer))
  else if (p_adjust)
    answer <- P_adjust(answer, "holm", ncol(answer))
  
  attr(answer, "stp25") <- "tbll_desc"
  attr(answer, "data") <- plot_data
  prepare_output(
    answer,
    caption = caption,
    note = if(is.null(note)) "" else paste(unique(note), collapse = ", "),
    N = nrow(X$data)
  )
}





# helper
ff_percent_ci <- function(percent, lower, upper, digits = 0) {
  sprintf(
    paste0("%.", digits, "f%% (%.", digits, "f–%.", digits, "f%%)"),
    percent, lower, upper
  )
}

ff_percent  <- function(percent, digits = 0) {
  #render_f(percent, digits = digits)
  sprintf(
    paste0("%.", digits, "f%%" ),
    percent, lower, upper
  )
}


#' @rdname Tbll_desc 
#' @description  P_adjust(): Adjust P-values for Multiple Comparisons
#' @param x data.frame. mit Ergebnissen aus Tbll_desc 
#' @param method  character. default = "holm"  correction method. Can be abbreviated.
#' @param pos integer. Position der Sig-Tests mit p-Werten
#' @export
P_adjust <- function(x, method = "holm", pos = ncol(x)) {
  sig <- x[[pos]]
  sig <- strsplit(sig, "p=|p<")
  p <- sig |> sapply("[", 2) |> as.numeric()
  sig <- sig |> sapply("[", 1)
  sig <- ifelse(is.na(sig), "", sig)
  p <- p.adjust(p , method = method)
  p <- ifelse(is.na(p), "", paste(stp25tools2:::rndr_P(p)))
  x[[ncol(x)]] <- paste0(sig, p)
  names(x)[pos] <- paste0(names(x)[pos], " (", method, ")")
  x
} 


Tbll_desc_mean <- function(...,
                           prfx_in = "↳ ",
                           include.test = TRUE,
                           include.ci = TRUE,
                           include.label = TRUE,
                           include.n = FALSE,
                           conf.level = 0.95,
                           p_adjust = FALSE
                           
) {
  answer <- NULL
  plot_data <- NULL
  # Objekt mit Daten measure, digits, labels
  X <-  prepare_data(...)
  
  if (!all(X$measure %in% c("numeric")) | is.null(X$group.vars))  
    stop("Nur Numeric sind erlaubt und ich brauche eine Gruppe!")
  
  if (length(X$group.vars) > 1) {
    # sollte eine Gruppe 'group' heisen wird sie einfach überschrieben!
    X$data$group <- interaction2(X$data[X$group.vars], rev_order = TRUE)
    X$group.vars <- "group"
  } 
  
  caption <-  
    paste("Univariate description by", paste(X$col_name, collapse=" / "))
  note <-   
    if (include.ci)  
      paste0("Die ", conf.level*100, 
             "% Konfidenzintervalle wurden mit dem T-Test berechnet") else NULL
  
  if (!include.label) X$row_name <- X$measure.vars
  
  for (measure in X$measure.vars) {
    fm <- formula(paste(measure, "~", X$group.vars))
    x <- 
    # calc percent 
   
      
      
    if (include.ci) {  }
      result <- 
        X$data |>
        dplyr::group_by(.data[[X$group.vars]]) |>
        dplyr::summarise(
          variable = X$row_name[[measure]],
          n =  length(na.omit(.data[[measure]])),
          mean = mean(.data[[measure]], na.rm=TRUE),
          sd =  sd(.data[[measure]], na.rm = TRUE),
          margin = qt((1 + conf.level) / 2, n - 1) * sd / sqrt(n),
         lower = mean - margin,
        upper = mean + margin
          
         #  lower1 =  stats::t.test(.data[[measure]], conf.level = conf.level)$conf.int[1] ,
         #  upper2 =  stats::t.test(.data[[measure]], conf.level = conf.level)$conf.int[2] ,
                          
        ) 
    
      # plot_dat <- result |>
      #   dplyr::mutate( variable =  X$row_name[[measure]]) |>
      #   dplyr::select(all_of(X$group.vars), variable,  
      #                 n, mean, lower, upper)
      
     result <- result |>
       dplyr::select(all_of(X$group.vars),
                     variable, 
                   n, mean, lower, upper) |>
       Wide(!!X$group.vars, n, mean, lower, upper)
      return(result)

    
    
    names(result)[1] <- "Items"
    result[[1]] <- paste(prfx_in, result[[1]])
    
    result <- dplyr::bind_rows(
      data.frame(Items = X$row_name[[measure]]), 
      result)
    
    n_value <- seq(from = 2, to = ncol(result), by = 2)
    if(include.n)
      result[1, n_value] <-  colSums(result[n_value] , na.rm = TRUE)
    else {
      result <- result[-n_value]
      names(result) <- gsub("_percent", "", names(result))
    }
    
    if(X$measure[[measure]] == "logical") 
      result <- result[-2,]
    
    if (include.test) {
      # alternative aber fisher-Test
      #  print( summary(tab1) )
      # print(X$measure.test[[measure]])
      
      result$sig.Test <- NA
      tst <- auto_test(
        x =  X$data[[measure]],
        by = X$data[[X$group.vars]],
        measure.test = X$measure.test[[measure]]
      )
      note <- c(note, names(tst))
      result$sig.Test[1] <- tst
    }
    plot_data <- dplyr::bind_rows(plot_data, plot_dat)
    answer <- dplyr::bind_rows(answer, result)
  }
  
  if (is.character(p_adjust)  & include.test)
    answer <- P_adjust(answer, p_adjust, ncol(answer))
  else if (p_adjust)
    answer <- P_adjust(answer, "holm", ncol(answer))
  
  attr(answer, "stp25") <- "tbll_desc"
  attr(answer, "data") <- plot_data
  prepare_output(
    answer,
    caption = caption,
    note = if(is.null(note)) "" else paste(unique(note), collapse = ", "),
    N = nrow(X$data)
  )
}

# require(dplyr)
# interaction2<- stp25tools2:::interaction2
# DF |>
#   Tbll_desc_mean(
#     harnstoff,
#     by = ~ group+sex,
#     include.n= TRUE
#   ) 

       
