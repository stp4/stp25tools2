#' Create stacked bar plots with counts or percentages
#'
#' `gg_stacked()` ist ein flexibler Wrapper um [ggplot2::geom_bar()],
#' mit Erweiterungen für Häufigkeitstabellen im „long“-Format.
#' Es werden automatisch Standardspalten wie `Item`, `Freq`, `levels`
#' oder `.grouping` erkannt, können aber auch manuell über Argumente
#' angegeben werden. Optional lassen sich Facetten, Reihenfolgen und
#' Labels (Zahlen oder Prozente) steuern.
#'
#' @param data Ein `data.frame` oder `tibble`, das mindestens die Variablen
#'   für die Kategorien (`x`), die Häufigkeiten (`weight`) und die Füllfarben (`fill`) enthält.
#' @param ... Optional ästhetische Mappings:
#'   - `x` : Kategorische Variable (Default: `"Item"`, falls vorhanden).
#'   - `weight` : Gewichtungsvariable, meist Häufigkeiten (Default: `"Freq"`).
#'   - `fill` : Gruppierungsvariable für die Balkensegmente (Default: `"levels"`).
#'   - `grouping` : Optionale Variable zur Bildung von Facetten (Default: `".grouping"`).
#' @param facet_formula,include.total Eine Facetten-Formel für [ggplot2::facet_grid()].
#'   Wird automatisch aus `grouping` abgeleitet, falls angegeben.
#'   Wenn include.total == FALSE dann wird der Level Total entfernt
#' @param include.order,include.order.last Optionaler Vektor zur Steuerung der 
#' Reihenfolge der Items. include.order.last numeric welcher Eintrag ist der letzte.
#' @param decreasing `logical`. Wenn `TRUE` (Default), werden Items absteigend sortiert.
#' @param include.reference Optional: Referenzlevel, das immer einbezogen wird.
#' @param palette Farbpalette für die Segmente, weitergereicht an
#'   [ggplot2::scale_fill_brewer()]. Standard `"BrBG"`.
#'   - Diverging: BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
#'   - Qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
#'   - Sequential: Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd
#'   
#'   
#' @param direction Richtung der Farbskala (`1` oder `-1`).
#' @param include.value `logical`. Ob Werte als Textlabels angezeigt werden. Default `TRUE`.
#' @param include.percent `logical`. Ob relative Häufigkeiten (Prozent) angezeigt werden.
#'   Falls `FALSE`, werden absolute Häufigkeiten gezeigt.
#' @param labels_size Textgröße für Labels.
#' @param labels_color Zeichenkette oder Vektor mit Farben für die Labels.
#'   Default: `c("black", "gray95")`.
#' @param labels_hide_below Schwellenwert (0–1). Labels für Segmente mit
#'   kleineren Anteilen werden ausgeblendet. Default `0.05`.
#' @param wrap Numerisch. Falls angegeben, werden lange Faktorlevels
#'   umgebrochen
#' @param width Balkenbreite (Default `0.90`).
#' @param item_levels Optional: Vektor mit expliziter Reihenfolge der Items.
#' @note Documentation created with assistance from ChatGPT.
#' @return Ein `ggplot`-Objekt mit horizontalen, gestapelten Balken.
#' @details
#' - Prozentdarstellung (`include.percent = TRUE`) nutzt `ggstats::StatProp`.
#' - Absolute Häufigkeiten (`include.percent = FALSE`) nutzen `stat = "count"`.
#' - Falls eine Gruppierungsvariable angegeben ist, werden die Items
#'   gruppenspezifisch mit [tidytext::reorder_within()] sortiert.
#'
#' @examples
#' library(tibble)
#' require(ggplot2)
#' # Beispiel mit Standardspalten
#' 
#' DF1 <- tibble(
#'   id = 1:6,
#'   .grouping = c(rep("Domain A", 4), "Domain B","Domain B"),
#'   Sex = rep("female", 6),
#'   Item = factor(rep(c("Fruit", "Vegetables", "Milk"), each = 2)),
#'   levels = rep(c(TRUE, FALSE), 3),
#'   Freq = c(55,55, 15, 85, 30,70)
#' )
#' DF2 <- tibble(
#'   id = 1:6,
#'   .grouping = c(rep("Domain A", 4), "Domain B","Domain B"),
#'   Sex = rep("male", 6),
#'   Item = factor(rep(c("Fruit", "Vegetables", "Milk"), each = 2)),
#'   levels = rep(c(TRUE, FALSE), 3),
#'   Freq = c(47,70, 20, 80, 30,70)
#' )
#' DF<- rbind(DF1,DF2)
#' DF
#' 
#' cowplot::plot_grid(
#'   gg_stacked(DF, 
#'              facet_formula =  ~Sex ) +  
#'     theme(legend.position="none"),
#'   gg_stacked(DF, 
#'              facet_formula = .grouping~Sex, include.percent = FALSE ) +  
#'     theme(legend.position="none")
#'   #+ scale_fill_discrete(limits = c(TRUE, FALSE))
#' )
#' 
#' # DF |>
#' # Tbll_multi(
#' #   "Soda (gas)",q2.almdd,q2.minrl,q2.cola,
#' #   "Alkohol",q2.bier,q2.wein,
#' #   by =  ~ sex) |>
#' #   Output2() |>
#' #   gg_stacked(DF)
#' 
#' 
#' 
#' @seealso [ggplot2::geom_bar()], [tidytext::reorder_within()]
#' @export
gg_stacked <- function(data,
                       ...,
                       facet_formula = NULL,
                       include.order = NULL,
                       decreasing = TRUE,
                       include.order.last = NULL,
                       include.reference = NULL,
                       
                       palette = "BrBG",
                       direction = 1,
                       include.value = TRUE,
                       include.percent = TRUE,
                       labels_size = 3.5,
                       labels_color = c("black", "gray95"),
                       labels_hide_below = .05,
                       wrap =40,
                       width = .90,
                       border = NA, # "white",
                       item_levels = NULL,
                       include.total = TRUE) {
  
  if (!is.null(attr(data, "tbll_likert"))) {
   # cat("\nAha - Hier kommt ein Tbll_multi() \n")
    data <-  attr(data, "tbll_likert")
    if (is.null(facet_formula))
      facet_formula <-  attr(data, "tbll")$facet_formula
    data <- attr(data, "data_long")
  }
  
  # Total in der ersten Spalte entfernen
  if (!is.null(facet_formula)  &  !include.total) {
    g_var <- all.vars(facet_formula)[1L]
    if (levels(data[[g_var]])[1L] == "Total") {
      which_total <- data[[g_var]] == "Total"
      data <- data[!which_total, ]
    }
  }
 
  # Capture dot-dot-dot arguments
  dots <- rlang::enquos(...)
  
  # Default mappings if not specified in ...
  mapping <- aes()
  # print(names(dots))
  # Check if Item is specified in dots
  if (any(names(dots) == "x")) {
    mapping$x <- dots$x
  }
  else if ("Item" %in% names(data)) {
    mapping$x <- quo(Item)
  }
  
  # Check if Freq is specified in dots
  if (any(names(dots) == "weight")) {
    mapping$weight <- dots$weight
  }
  else if ("Freq" %in% names(data)) {
    mapping$weight <- quo(Freq)
  }
  
  # Check if levels is specified in dots
  if (any(names(dots) == "fill")) {
    mapping$fill <- dots$fill
  }
  else if ("levels" %in% names(data)) {
    mapping$fill <- quo(levels)
  }
  
  # Check if .grouping is specified in dots
  if (any(names(dots) == "grouping")) {
    grouping_var <- rlang::eval_tidy(dots$grouping, data)
    if(is.null(facet_formula)) facet_formula <- formula(paste(dots$grouping, "~ ."))
  }
  else if (".grouping" %in% names(data)) {
    grouping_var <- data$.grouping
    if(is.null(facet_formula)) facet_formula <-  .grouping ~ .
  }
  else {
    grouping_var <- NULL
  }
  
  # bei Logikal lavels umdrehen
  if( is.logical(data[[rlang::as_name(mapping$fill)]])){
    data[[rlang::as_name(mapping$fill)]] <-
      factor(data[[rlang::as_name(mapping$fill)]], c(TRUE, FALSE))
  }
  
  
  # Calculate order based on mean Freq by Item
  if (!is.null(include.order)) {
    
    
    # !is.null(mapping$weight) && !is.null(mapping$x)
    data <- order_weighted( data,
                            item_var = rlang::as_name(mapping$x),
                            freq_var = rlang::as_name(mapping$weight),
                            levels_var = rlang::as_name(mapping$fill),
                            include.order,
                            decreasing,
                            include.reference
                            )
    
    if(is.numeric(include.order.last)) {
      #cat("Ordnen nach", include.order.last)
      item <- data[[rlang::as_name(mapping$x)]]
      data$mean_weight[item == levels(item)[include.order.last]] <- 0
      # data$Item <- forcats::fct_relevel(data$Item,
      #                            levels(data$Item)[include.order.last] , 
      #                            after = Inf)

    }
    
    
    
  } 
  else{
    # revers Order based on Item (factor)
    data$mean_weight <-
      nlevels( data[[rlang::as_name(mapping$x)]]) -
      as.numeric( data[[rlang::as_name(mapping$x)]] )
  }
  
  
  # Handle item levels if specified
  if (!is.null(item_levels)) {
    if (!is.null(mapping$x)) {
      data[[rlang::as_name(mapping$x)]] <-
        factor(
          data[[rlang::as_name(mapping$x)]],
          levels = item_levels
        )
    }
  }
  
  if(is.numeric(wrap)) {
    data[[rlang::as_name(mapping$x)]] <-
      wrap_string(data[[rlang::as_name(mapping$x)]], wrap)
  }
  
  
  # Handle label colors
  if (length(labels_color) != 1) {
    if (length(labels_color) == 0) {
      labels_color <- "black"
    }
    else {
      labels_color <- rep_len(labels_color, nrow(data))
    }
  }
  
  
#cat("\nCreate base plot\n")  
#print(head(data))
#cat("\nMapping\n")  
#print(mapping)

  
  # Create base plot
  if (!is.null(grouping_var)) {
    p <- ggplot(data) +
      aes(
        x = tidytext::reorder_within(
          !!mapping$x,
          mean_weight,
          grouping_var
        ),
        fill = !!mapping$fill,
        weight = !!mapping$weight
      ) +
      tidytext::scale_x_reordered()
  }
  else {
    p <- ggplot(data) +
      aes(
        x = reorder(!!mapping$x, mean_weight),
        fill = !!mapping$fill,
        weight = !!mapping$weight
      )
  }
  
  
  # Add bars based on percent or count
  # position_fill() stacks bars and standardises
  # position_stack() stacks bars 
  if (include.percent) {
    p <- p +
      geom_bar(colour = border,
               position = position_fill(reverse = TRUE),
               stat = ggstats::StatProp,
               complete = "fill",
               width = width
      ) +
      labs(x = NULL, y = NULL, fill = NULL) +
      scale_y_continuous(labels = ggstats::label_percent_abs())
    
    if (include.value) {
      p <- p +
        geom_text(
          mapping = aes(
            label = ggstats::label_percent_abs(
              hide_below = labels_hide_below,
              accuracy = 1
            )(after_stat(prop))
          ),
          stat = ggstats::StatProp,
          default_by = "y", # Gesamt oder Spalten-prozent or NULL or "total" 
          complete = "fill",
          position = position_fill(vjust = .5, reverse = TRUE),
          size = labels_size,
          color = labels_color
        )
    }
  }
  else {
    p <- p +
      geom_bar(colour = border,
               position = position_stack(reverse = TRUE),
               width = width
      ) +
      labs(x = NULL, y = NULL, fill = NULL)
    
    if (include.value) {
      p <- p +
        geom_text(
          mapping = aes(
            label = ggstats::label_number_abs(
              hide_below = labels_hide_below,
              accuracy = 1
            )(after_stat(count))
          ),
          stat = "count",
          position = position_stack(vjust = .5, reverse = TRUE),
          size = labels_size,
          color = labels_color
        )
    }
  }
  
  # Add facets if specified
  if (!is.null(facet_formula)) {
    p <- p +
      facet_grid(
        facet_formula,
        scales = "free",
        space = "free"
      )
  }
  
  # Final theme and styling
  p <- p +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )
  
  if (!is.null(palette)) {
    p <- p +
      scale_fill_brewer(palette = palette, direction = direction)
  }
  
  p + coord_flip()
}

#' Calculate Weighted Values Based on Factor Levels
#'
#' @param x A factor or logical vector to be weighted
#' @param include.order Logical or character specifying ordering direction.
#'        If character, can be "right" or "left" to specify direction.
#' @param decreasing Logical indicating if weights should decrease
#'        (only used when include.order is TRUE)
#' @param include.reference Numeric value specifying reference point for weighting.
#'        If NULL, uses simple sequential weights.
#'
#' @return A numeric vector of weights
#' @export
#'
#' @examples
#' # Basic usage with factor
#' calc_weight(factor(1:5))
#'
#' # With reference point
#' calc_weight(factor(1:7), include.reference = 3.5)
#'
#' # Left orientation
#' calc_weight(factor(c(TRUE, FALSE)), include.order = "left")
#'
calc_weight <- function(x,
                        include.order = TRUE,
                        decreasing = TRUE,
                        include.reference = NULL) {
  
  # Input validation
  if (!is.factor(x) && !is.logical(x)) {
    stop("x must be either a factor or logical vector")
  }
  
  if (is.logical(x)) {
    x <- factor(x, levels = c(FALSE, TRUE))
  }
  
  # Handle character ordering specification
  orientation_left <- FALSE
  
  if (is.character(include.order)) {
    include.order <- match.arg(include.order, c("right", "left"))
    if (include.order == "left") {
      decreasing <- FALSE
      orientation_left <- TRUE
    }
    include.order <- TRUE
  }
  else if ( !decreasing){
    orientation_left <- TRUE
  } else if (!include.order) {
    # Return equal weights if no ordering requested
    return(rep(1L, length(x)))
  }
  
  nlvls <- nlevels(x)
  m_weight <- seq_len(nlvls)
  
  # Handle reference point weighting
  if (!is.null(include.reference) && nlvls > 2L) {
    # Adjust weights based on reference point
    if (ceiling(include.reference) == floor(include.reference)) {
      # Integer reference point
      lw <- (include.reference + 1):nlvls
      rw <- 1:(include.reference)
      
    } else {
      # Non-integer reference point
      m_weight[ceiling(include.reference)] <- 0.001 # slight offset from zero
      lw <- ceiling(include.reference+1):nlvls
      rw <- 1:floor(include.reference)
      
    }
    
    m_weight[lw] <- seq_along(lw) / sum(seq_along(lw))
    m_weight[rw] <- -(rev(seq_along(rw))) / sum(seq_along(rw))
    
    #Flip weights if decreasing is FALSE
    if (!orientation_left) {
      m_weight <- m_weight * (-1)
    #  cat("\nm_weight decreasing\n")
    }
  } else {
    # Simple sequential weights normalized by sum
    # Reverse weights if needed
    if (!orientation_left) {
      m_weight <- rev(m_weight)
    }
    
    m_weight <- m_weight / sum(m_weight)
  }
  
  # Map weights to original vector
  weights <- m_weight[as.integer(x)]
  
  # For factors with reference point, return exact weights
  if (!is.null(include.reference) && nlvls > 2L) {
    return(round(weights,3))
  }
  
  # For simple cases, return normalized weights
  round(weights / sum(weights),3)
}



#' @param data data.frame.
#'
#' @param item_var,freq_var,levels_var character.
#'
#' @rdname order_weighted
#' @noRd
order_weighted <- function(data,
                           item_var = "Item",
                           freq_var = "Freq",
                           levels_var = "levels",
                           include.order = TRUE,
                           decreasing = TRUE,
                           include.reference = NULL
) {
  
  # # Sicherstellen, dass die Variablen im Datensatz existieren
  # if (!all(c(item_var, freq_var, levels_var) %in% names(data))) {
  #   stop("Nicht alle spezifizierten Variablen sind im Datensatz vorhanden")
  # }
  
  data |>
    dplyr::mutate(
      mean_weight = calc_weight(
        !!sym(levels_var),
        include.order,
        decreasing,
        include.reference
      )
    ) |>
    dplyr::group_by(!!sym(item_var)) |>
    dplyr::mutate(
      mean_weight = sum(!!sym(freq_var) * mean_weight) / sum(!!sym(freq_var))
    ) |>
    dplyr::ungroup()
}

# # Left orientation
# calc_weight(factor(c(TRUE, FALSE)), include.order = "left")
# # Basic usage with factor
# calc_weight(factor(1:5), "l")
# calc_weight(factor(1:5), "r")
# # 1:5
#
# # With reference point
# calc_weight(factor(1:5), include.reference = 3)
# calc_weight(factor(1:6), include.reference = 3)
# calc_weight(factor(1:5), "l", include.reference = 2.5)
# calc_weight(factor(1:6), include.reference = 2.5)


#' @rdname Tbll_multi
#' @param cutoff daten transformieren in prepare_upset_data
#' @description
#'  Helper for the preparation of `UpSetR::upset` plots.
#'
#' @export
#' @returns tibble
prepare_upset_data <- function(...,
                               use.level = 1,
                               cutoff  = NULL) {
  X <- prepare_data(...)
  dat <-   as.data.frame(lapply(
    X$data[X$measure.vars] ,
    FUN = function(x) {
      if (is.logical(x))
        as.integer(x)
      else if( is.integer(x) ) x
      else if (is.factor(x))
        as.integer(x == levels(x)[use.level])
      else if (is.numeric(x)) {
        if (is.null(cutoff ))
          stop("Du musch schu de 'cutoff ' angeben!")
        as.integer(x > cutoff )
      }
      else
        stop("Mai I woas nit woas tuan!")
    }
  ))
  colnames(dat) <-  X$row_name
  tibble::as_tibble(dat)
}

#' @rdname Tbll_multi
#'
#' @param data,intersect,ylab.inter,xlab.set,xlab.matrix multi_matrix_plot: Beschriftung in plot
#' @description
#'  Stolen from Michal Mrassowski https://github.com/krassowski/complex-upset
#'
#' @export
#'
#' @importFrom ComplexUpset upset upset_set_size intersection_size
#' @importFrom ggplot2 ylab
#' @examples
#'
#'  # library(ComplexUpset)
#'  DF <-
#' structure(list(
#'   q2.almdd = structure(
#'   c(TRUE, FALSE, FALSE, TRUE,TRUE, TRUE, TRUE, TRUE,
#'   TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#'                        label = "Alm Dudler"),
#'   q2.minrl = structure(
#'   c(TRUE, FALSE,FALSE, FALSE, TRUE, FALSE, FALSE,
#'   TRUE, TRUE, TRUE, FALSE, FALSE,TRUE, TRUE, TRUE),
#'                        label= "Mineral"),
#'   q2.cola = structure(
#'   c(TRUE,TRUE, TRUE, TRUE, FALSE, TRUE, FALSE,
#'   FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
#'                       label = "Cola"),
#'   q2.bier = structure(
#'   c(TRUE,FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
#'   FALSE, FALSE, FALSE,FALSE, TRUE, FALSE, FALSE, FALSE),
#'                       label = "Bier"),
#'   q2.wein = structure(
#'   c(TRUE,FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
#'    FALSE, FALSE, FALSE,FALSE, FALSE, FALSE, FALSE, FALSE),
#'                       label = "Wein")),
#'   row.names = c(NA,15L),
#'   class = "data.frame")
#'
#' dat <- prepare_upset_data(DF,
#' q2.almdd,q2.minrl,q2.cola,q2.bier,q2.wein )
#'
#' multi_matrix_plot(dat)
#'
multi_matrix_plot <- function(data,
                              intersect = colnames(data),
                              ...,
                              ylab.inter  = "Intersection Size",
                              xlab.set = "Set Size",
                              xlab.matrix = "") {
  ComplexUpset::upset(
    data,
    intersect = intersect,
    name = xlab.matrix,
    ...,
    set_sizes = (ComplexUpset::upset_set_size() + ggplot2::ylab(xlab.set)),
    base_annotations =
      list('Intersection size' = ComplexUpset::intersection_size() + ggplot2::ylab(ylab.inter))
  )
  
}


# require(dplyr)
# require(ggplot2)



# gg_stacked2 <- function(data,
#                        ...,
#                        facet_formula = NULL,
#                        include.order = NULL,
#                        decreasing = TRUE,
#                        include.reference = NULL,
#                        
#                        palette = "BrBG",
#                        direction = 1,
#                        include.value = TRUE,
#                        include.percent = TRUE,
#                        labels_size = 3.5,
#                        labels_color = c("black", "gray95"),
#                        labels_hide_below = .05,
#                        wrap =40,
#                        width = .90,
#                        border = NA, # "white",
#                        item_levels = NULL) {
#   
#   # Capture dot-dot-dot arguments
#   dots <- rlang::enquos(...)
#   
#   # Default mappings if not specified in ...
#   mapping <- aes()
#   # print(names(dots))
#   # Check if Item is specified in dots
#   if (any(names(dots) == "x")) {
#     mapping$x <- dots$x
#   }
#   else if ("Item" %in% names(data)) {
#     mapping$x <- quo(Item)
#   }
#   
#   # Check if Freq is specified in dots
#   if (any(names(dots) == "weight")) {
#     mapping$weight <- dots$weight
#   }
#   else if ("Freq" %in% names(data)) {
#     mapping$weight <- quo(Freq)
#   }
#   
#   # Check if levels is specified in dots
#   if (any(names(dots) == "fill")) {
#     mapping$fill <- dots$fill
#   }
#   else if ("levels" %in% names(data)) {
#     mapping$fill <- quo(levels)
#   }
#   
#   # Check if .grouping is specified in dots
#   if (any(names(dots) == "grouping")) {
#     grouping_var <- rlang::eval_tidy(dots$grouping, data)
#     if(is.null(facet_formula)) facet_formula <- formula(paste(dots$grouping, "~ ."))
#   }
#   else if (".grouping" %in% names(data)) {
#     grouping_var <- data$.grouping
#     if(is.null(facet_formula)) facet_formula <-  .grouping ~ .
#   }
#   else {
#     grouping_var <- NULL
#   }
#   
#   
#   
#   # Calculate order based on mean Freq by Item
#   if (!is.null(include.order)) {
#     # !is.null(mapping$weight) && !is.null(mapping$x)
#     data <- order_weighted( data,
#                             item_var = rlang::as_name(mapping$x),
#                             freq_var = rlang::as_name(mapping$weight),
#                             levels_var = rlang::as_name(mapping$fill),
#                             include.order,
#                             decreasing,
#                             include.reference)
#     
#     
#     
#   } else{
#     # revers Order based on Item (factor)
#     data$mean_weight <-
#       nlevels( data[[rlang::as_name(mapping$x)]]) -
#       as.numeric( data[[rlang::as_name(mapping$x)]] )
#     
#   }
#   
#   
#   # Handle item levels if specified
#   if (!is.null(item_levels)) {
#     if (!is.null(mapping$x)) {
#       data[[rlang::as_name(mapping$x)]] <-
#         factor(
#           data[[rlang::as_name(mapping$x)]],
#           levels = item_levels
#         )
#     }
#   }
#   
#   if(is.numeric(wrap)) {
#     data[[rlang::as_name(mapping$x)]] <-
#       wrap_string(data[[rlang::as_name(mapping$x)]], wrap)
#   }
#   
#   
#   # Handle label colors
#   if (length(labels_color) != 1) {
#     if (length(labels_color) == 0) {
#       labels_color <- "black"
#     }
#     else {
#       labels_color <- rep_len(labels_color, nrow(data))
#     }
#   }
#   
#   # Create base plot
#   if (!is.null(grouping_var)) {
#     p <- ggplot(data) +
#       aes(
#         x = tidytext::reorder_within(
#           !!mapping$x,
#           mean_weight,
#           grouping_var
#         ),
#         fill = !!mapping$fill,
#         weight = !!mapping$weight
#       ) +
#       tidytext::scale_x_reordered()
#   }
#   else {
#     p <- ggplot(data) +
#       aes(
#         x = reorder(!!mapping$x, mean_weight),
#         fill = !!mapping$fill,
#         weight = !!mapping$weight
#       )
#   }
#   
# #print(mapping)
#   
#   # Add bars based on percent or count
#   if (include.percent) {
#     p <- p +
#       geom_bar(colour = border,
#                position = position_fill(reverse = TRUE),
#                stat = ggstats::StatProp,
#                complete = "fill",
#                width = width 
#       ) +
#       labs(x = NULL, y = NULL, fill = NULL) +
#       scale_y_continuous(labels = ggstats::label_percent_abs())
#     
#     if (include.value) {
#       p <- p +
#         geom_text(
#                    mapping = aes(
#                     # x = Item, #fill = levels,
#                    label = ggstats::label_percent_abs(
#                      hide_below = labels_hide_below,
#                      accuracy = 1
#                    ) (after_stat(prop))
#                    ),
#                    
#                    stat = ggstats::StatProp,
#                     complete = "fill",
#                   position = position_fill(vjust = .5, reverse = TRUE),
#                   size = labels_size,
#                 color = labels_color
#         )
#       
#    
#       
#      #  p <- p + geom_text(stat = "prop", position = position_fill(.5))
#     }
#     
#     
#     
#     
#   }
#   else {
#     p <- p +
#       geom_bar(colour = border,
#                position = position_stack(reverse = TRUE),
#                width = width
#       ) +
#       labs(x = NULL, y = NULL, fill = NULL)
#     
#     if (include.value) {
#       p <- p +
#         geom_text(
#           mapping = aes(
#             label = ggstats::label_number_abs(
#               hide_below = labels_hide_below,
#               accuracy = 1
#             )(after_stat(count))
#           ),
#           stat = "count",
#           position = position_stack(vjust = .5, reverse = TRUE),
#           size = labels_size,
#           color = labels_color
#         )
#     }
#   }
#   
#   # Add facets if specified
#   if (!is.null(facet_formula)) {
#     p <- p +
#       facet_grid(
#         facet_formula,
#         scales = "free",
#         space = "free"
#       )
#   }
#   
#   # Final theme and styling
#   p <- p +
#     theme_bw() +
#     theme(
#       legend.position = "bottom",
#       panel.grid.major.y = element_blank()
#     )
#   
#   if (!is.null(palette)) {
#     p <- p +
#       scale_fill_brewer(palette = palette, direction = direction)
#   }
#   
#   p + coord_flip()
# }





#' @title Balkendiagramms mit relativen Häufigkeiten (Prozent)
#'
#' @description Erstellt ein Balkendiagramm basierend auf einer Kontingenztabelle (xtabs) und zeigt die relativen Häufigkeiten (prop.table) als Balken und Labels an. Die Funktion unterstützt bis zu vier Variablen für die Achse, Füllung und Facetting.
#'
#' @param data Ein data.frame, der die zu plottenden Variablen enthält.
#' @param formula Eine Formel (xtabs) 
#' der Art \code{~Var1}, \code{~Var1 + Var2}, \code{~Var1 + Var2 + Var3} 
#' oder \code{~Var1 + Var2 + Var3 + Var4}.
#'   \itemize{
#'     \item \code{Var1} wird zur X-Achse (Balken-Kategorien).
#'     \item \code{Var2} oder die linke Seite der Formel (z.B. \code{Var3 ~ Var2}) wird für das Facetting verwendet.
#'     \item Die dritte und vierte Variable werden für Facetting und/oder Füllung verwendet (siehe \code{aea_mapping}).
#'   }
#' @param margin Ein Vektor von Dimensionen (Zahlen), über die summiert werden soll, um die relativen Häufigkeiten zu berechnen (\code{prop.table}). Standardmäßig wird dies anhand der Anzahl der Variablen in der Formel geschätzt.
#' @param fill_col Füllfarbe für die Balken. Wird nur verwendet, wenn kein Füll-Mapping aus der Formel abgeleitet wird (d.h. bei \code{~Var1}).
#' @param labels_color Farbe für die Text-Labels.
#' @param facet_formula Eine optionale explizite Facetting-Formel 
#' (z.B. \code{Hersteller ~ Zylinder}). Überschreibt die automatische Ableitung.
#' @param horizontal Logischer Wert. Wenn \code{TRUE} (Standard), wird \code{coord_flip()} verwendet, um horizontale Balken zu erstellen.
#' @param include.value Logischer Wert. Gibt an, ob die Werte (Prozent oder Frequenz) als Text-Labels angezeigt werden sollen.
#' @param include.percent Logischer Wert. Wenn \code{TRUE} (Standard), werden Prozente (\code{prop.table}) berechnet und die Y-Achse als Prozent formatiert. Wenn \code{FALSE}, werden absolute Häufigkeiten verwendet.
#' @param labels_hide_below Numerischer Wert (als Dezimalzahl). Labels werden für Segmente ausgeblendet, deren Anteil unter diesem Wert liegt (nützlich, um Überlappungen zu vermeiden).
#' @param relevel Logischer Wert. Wenn \code{TRUE}, wird die erste
#'  Variable (\code{Var1}) umgekehrt nach der Häufigkeit sortiert, sodass der häufigste Wert bei \code{coord_flip} oben steht. Standard ist \code{TRUE}, wenn \code{horizontal=TRUE}.
#' @param ylab Beschriftung für die Y-Achse (Standard: "Prozent").
#' @param ylim Ein numerischer Vektor der Länge 2 für die Y-Achsenbegrenzung. 
#' Standard ist \code{c(0, 1)} (0 bis 100) bei \code{include.percent=TRUE}.
#' @param digits Die Anzahl der Nachkommastellen für die Prozent-Labels 
#'
#' @return Ein \code{ggplot} Objekt
#' 

#'
#' @examples
#' 
#' \dontrun{
#' library(ggplot2)
#' data(mpg)
#' 
#' # Einfache Verteilung (Prozent am Gesamt)
#' gg_barchart(mpg, ~class, margin=NULL)
#'
#' # Prozent pro Hersteller, ge-faceted nach Hersteller
#' gg_barchart(mpg, ~class + manufacturer, margin=2, labels_hide_below=0.01)
#' 
#' # Facetting mit zwei Variablen (class vs. drv, ge-faceted nach manufacturer)
#' # Note: Hier wird Füllung/Stapel nicht verwendet, da es zu komplex wird
#' gg_barchart(mpg, ~class + drv + manufacturer, margin=2)
#' }
#' 
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_text labs scale_y_continuous coord_flip facet_grid theme_bw
#' @importFrom scales percent
#' @importFrom rlang sym as_label !!
#' @importFrom dplyr mutate
#' @importFrom forcats fct_rev
#' @importFrom stats xtabs as.formula
#' 
gg_barchart <- function(data,
                        formula,  
                        margin = NULL,
                        fill_col = "#d8b365",  
                        labels_color = "black",
                        facet_formula = NULL,
                        horizontal = TRUE,
                        include.value = TRUE,
                        include.percent = TRUE,
                        background_col = "gray90",
                        include.background = if(is.null(background_col)) FALSE else TRUE,
                        labels_hide_below = .05,
                        relevel = horizontal,
                        ylab = if(include.percent) "Prozent" else "Anzahl",
                        ylim = c(0,1),
                        width = .9,
                        digits=0) {
  # xtabs und prop.table
  vars <- get_vars_from_formula(formula)
  margin <- guess_margin(vars, margin)
  
  if (include.percent)
    plot_data <- xtabs(formula, data) |>
    prop.table(margin) |>
    as.data.frame()
  else
    plot_data <- xtabs(formula, data) |>
    as.data.frame()
  
  mapping <- aea_mapping(vars, names(plot_data)) # Angepasste aea_mapping, um fill_var intern zu bestimmen
  facet_formula <- guess_facet(vars, facet_formula)
  
  # Konvertiere alle originalen Variablen in Faktoren
  for (var in all.vars(formula)) {
    if(!is.factor( plot_data[[var]]))
      plot_data[[var]] <- factor(plot_data[[var]])
  }
  
  # Optionale Neuanordnung der ersten Variable (X-Achse)
  if (relevel) {
    # fct_rev dreht die Levels. Da Sie sortiert haben, kehren Sie damit die Achse um
    plot_data <- plot_data |>
      mutate(
        !!mapping$x := fct_rev(!!mapping$x )
      )
  }
  
  
  
  
  
  p <- ggplot(plot_data, aes(x = !!mapping$x, y = Freq)) 
  # Hintergrundbalken (bis 100%)
  if(include.background){
    if(include.percent)
      max_y <- 1
    else 
      max_y <- sum(data$Freq, na.rm = TRUE)
    p <- p +  geom_col(
      aes(x = !!mapping$x, y = max_y),
      fill = background_col ,
      width = width,
      alpha = 0.3
    )
  }
  
  
  # two types of bar charts: geom_bar() and geom_col()
  # geom_col() uses stat_identity(): it leaves the data as is.
  
  
  p <- p + geom_col( 
    fill = fill_col, 
    color = fill_col,
    width=width)
  
  # Labels für einfache Balken (rechts/oben)
  if(include.value & include.percent)
    p <- p + geom_text(
      aes(
        # Position in der Mitte des Balkens
        y = Freq / 2,
        label = scales::percent(
          # Ausblenden kleiner Werte
          ifelse(Freq < labels_hide_below, NA, Freq), 
          accuracy = 1/10^digits)
      ),
      # Positionierung für mittigen Text (horizontal=TRUE -> hjust=0.5, aber 0 für Start)
      hjust =  0.5,   # if (horizontal) 0.5 else 0.5,
      vjust =  0.5,   # if (horizontal) 0.5 else 0.5,
      size = 3,
      color = labels_color
    )
  else if(include.value)
    p <- p + geom_text(
      aes(label = Freq),
      # Beschriftung Auserhalb
      hjust = if (horizontal) -0.4 else 0.5,
      vjust = if (horizontal) 0.5 else -.4,
      size = 3
    )
  
  # Skalierung, Achsendrehung und Facetting
  if(include.percent)
    p <- p +
    scale_y_continuous(labels = scales::percent,
                       limits = ylim)
  
  if (horizontal)
    p <- p + coord_flip()
  
  
  if (!is.null(facet_formula)) 
    p <- p + facet_grid(facet_formula, 
                        scales = "free_x")
  
  
  p <- p + labs(y = ylab, x = "") +
    theme_bw()  +
    theme(panel.grid = element_blank())
  
  
  return(p)
}



# 1. Extrahiert die Variablennamen aus der Formel
get_vars_from_formula <- function(formula) {
  # Konvertiert die Formel in eine Liste von Symbolen
  vars <- all.vars(formula)
  
  # Stellt sicher, dass mindestens eine Variable vorhanden ist
  if (length(vars) < 1) {
    stop("Die Formel muss mindestens eine Variable enthalten.")
  }
  return(vars)
}

# 2. Leitet die AES-Mappings ab
aea_mapping <- function(vars, data_names, fill_var=NULL) {
  # Die erste Variable wird zur X-Achse (der Balken)
  x_var <- sym(vars[1])
  
  # Die zweite Variable wird zur Füllung/Farbe (optional)
  if(is.null(fill_var) ){
    if (length(vars) == 4  ) fill_var <- sym(vars[4]) }
  
  # Alle restlichen Variablen werden für Facetting vorgeschlagen
  facet_vars <- if (length(vars) >= 3) vars[3:length(vars)] else NULL
  
  return(list(
    x = x_var,
    fill = fill_var,
    facet_vars = facet_vars
  ))
}

# 3. Leitet die Facet-Formel ab
guess_facet <- function(vars, facet_formula) {
  if (!is.null(facet_formula))
    return(facet_formula)
  
  
  if (length(vars) == 2) {
    facet_formula_str <- paste("~", vars[2])
    return(as.formula(facet_formula_str))
  }
  if (length(vars) >= 3) {
    facet_formula_str <- paste(vars[3], "~", vars[2])
    return(as.formula(facet_formula_str))
  }
  else
    return(NULL)
}

guess_margin <- function(vars, margin) {
  if (!is.null(margin))
    return(margin)
  if (length(vars) == 1)
    return(NULL)
  if (length(vars) == 2)
    return(2)
  if (length(vars) >= 3)
    return(c(2, 3))
  

}



#wrap_string <- stp25tools2::wrap_string
# require(ggplot2)
# require(tidyverse)
# # Beispiel mit Standardspalten
# DF1 <- tibble(
#   id = 1:6,
#    .grouping = c(rep("Domain A", 4), "Domain B","Domain B"),
#   Sex = rep("female", 6),
#   Item = factor(rep(c("Fruit", "Vegetables", "Milk"), each = 2)),
#   levels = rep(c(TRUE, FALSE), 3),
#   Freq = c(55,55, 15, 85, 30,70)
# )
# DF2 <- tibble(
#   id = 1:6,
#    .grouping = c(rep("Domain A", 4), "Domain B","Domain B"),
#   Sex = rep("male", 6),
#   Item = factor(rep(c("Fruit", "Vegetables", "Milk"), each = 2)),
#   levels = rep(c(TRUE, FALSE), 3),
#   Freq = c(47,70, 20, 80, 30,70)
# )
# DF<- rbind(DF1,DF2)
# DF
# 
# cowplot::plot_grid(
#   gg_stacked(DF, 
#              facet_formula =  ~Sex ) +  
#     theme(legend.position="none"),
#   gg_stacked(DF, 
#              facet_formula = .grouping~Sex, include.percent = FALSE ) +  
#     theme(legend.position="none")
#     #+ scale_fill_discrete(limits = c(TRUE, FALSE))
# )





