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
#' @param facet_formula Eine Facetten-Formel für [ggplot2::facet_grid()].
#'   Wird automatisch aus `grouping` abgeleitet, falls angegeben.
#' @param include.order Optionaler Vektor zur Steuerung der Reihenfolge der Items.
#' @param decreasing `logical`. Wenn `TRUE` (Default), werden Items absteigend sortiert.
#' @param include.reference Optional: Referenzlevel, das immer einbezogen wird.
#' @param palette Farbpalette für die Segmente, weitergereicht an
#'   [ggplot2::scale_fill_brewer()]. Standard `"BrBG"`.
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
#'
#' @return Ein `ggplot`-Objekt mit horizontalen, gestapelten Balken.
#' @details
#' - Prozentdarstellung (`include.percent = TRUE`) nutzt `ggstats::StatProp`.
#' - Absolute Häufigkeiten (`include.percent = FALSE`) nutzen `stat = "count"`.
#' - Falls eine Gruppierungsvariable angegeben ist, werden die Items
#'   gruppenspezifisch mit [tidytext::reorder_within()] sortiert.
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#'
#' # Beispiel mit Standardspalten
#' DF <- tibble(
#'   id = 1:6,
#'   .grouping = rep("Domain A", 6),
#'   Sex = rep(c("female","male"), 3),
#'   Item = factor(rep(c("Fruit", "Vegetables", "Milk"), each = 2)),
#'   levels = rep(c(TRUE, FALSE), 3),
#'   Freq = c(20,10, 15,25, 30,12)
#' )
#'
#' gg_stacked(DF)
#'
#' # Mit eigenen Spaltennamen und Facettierung
#' DF2 <- DF
#' names(DF2) <- c("id", "food_group", "Sex", "Frage", "factor_levels", "Anzahl")
#'
#' gg_stacked(
#'   DF2,
#'   x = Frage,
#'   weight = Anzahl,
#'   fill = factor_levels,
#'   grouping = food_group,
#'   facet_formula = food_group ~ Sex
#' )
#'
#' @seealso [ggplot2::geom_bar()], [tidytext::reorder_within()]
#' @export
gg_stacked <- function(data,
                       ...,
                       facet_formula = NULL,
                       include.order = NULL,
                       decreasing = TRUE,
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
                       item_levels = NULL) {

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



  # Calculate order based on mean Freq by Item
  if (!is.null(include.order)) {
   # !is.null(mapping$weight) && !is.null(mapping$x)
    data <- order_weighted( data,
                            item_var = rlang::as_name(mapping$x),
                            freq_var = rlang::as_name(mapping$weight),
                            levels_var = rlang::as_name(mapping$fill),
                            include.order,
                            decreasing,
                            include.reference)



  } else{
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


  # ggstats_label_percent_abs<-
  # function (..., hide_below = NULL)
  # {
  #   #print(list(...))
  #   function(x) {
  #
  #
  #       print(sum(x))
  #     res <- (scales::label_percent(...))(abs(x))
  #     if (!is.null(hide_below)) {
  #       res[abs(x) < hide_below] <- ""
  #     }
  #     print(res)
  #     res
  #   }
  # }



# Hier gibt es einen schweren Fehler
# die Zahlen bei der Ausgabe der Prozent simmen nicht.
# Es werden die Totalen Prozent ausgegeben.
# ggstats scheint aber zu funktionieren??
# hier ein Beispilecode
#
#  library(ggplot2)
#  library(ggstats)
# # displaying unobserved levels with complete
# d <- diamonds #|>
#  # dplyr::filter(!(cut == "Ideal" & clarity == "I1")) |>
#  # dplyr::filter(!(cut == "Very Good" & clarity == "VS2")) |>
#  # dplyr::filter(!(cut == "Premium" & clarity == "IF"))
#
#
# head(d)
#
# p <- ggplot(d) +
#   aes(y = clarity, fill = cut, by = clarity) +
#   geom_bar(position = "fill")
# p + geom_text(stat = "prop", position = position_fill(.5))
# p + geom_text(stat = "prop", position = position_fill(.5), complete = "fill")
#








  # Add bars based on percent or count
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
            ) (after_stat(prop))
          ),
          stat = ggstats::StatProp,
          complete = "fill",
          position = position_fill(vjust = .5, reverse = TRUE),
          size = labels_size,
          color = labels_color
        )

        # p <- p +
        #   geom_text(stat = "prop", position = position_fill(.5))
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
      cat("\nm_weight decreasing\n")
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



