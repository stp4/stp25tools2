#' Enhanced Likert and Multi-Response Bar Charts
#'
#' Creates customizable bar charts for Likert and multi-response data with support
#' for grouping, ordering, and various styling options.
#'
#' @param data A data frame in long format data A \code{likert_summary}
#' object from \code{Summarise_likert()}
#' @param ... Additional arguments passed to  ggplot
#' @param facet_formula Formula for faceting (e.g., .grouping ~ Sex)
#' @param include.reference Numeric reference line for Likert scaling
#' @param include.order Character specifying ordering direction ("right" or "left")
#' @param decreasing Logical indicating decreasing order
#' @param include.percent Logical to show percentages instead of counts
#' @param reverse_likert Logical to reverse Likert bar direction
#' @param width,border Proprieties of the bars with: numeric width of bars (0-1), border: color
#' @param wrap Numeric width for wrapping item labels
#' @param palette Character color palette name
#' @param direction Numeric direction for color palette (1 or -1)
#' @param item_levels Character vector for renaming item levels
#' @param labels_size Numeric size for value labels
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#'
#' #' require(ggstats)
#' library(ggplot2)
#' require(dplyr)
#'
#' # Basic usage
#' addmargins(xtabs( ~  clarity + cut , diamonds), 1)
#'
#' xtabs( ~ cut + clarity, diamonds) |>
#'   as.data.frame() |>
#'   gg_likertplot(
#'     x = clarity,
#'     fill = cut,
#'     weight = Freq,
#'     include.percent = TRUE,
#'     include.order = "l",
#'     include.reference = 2
#'   )
#'
#'
#' xtabs( ~ cut + clarity, diamonds) |>
#'   as.data.frame() |>
#'   mutate(.grouping = c(rep("A", 5 * 5), rep("B", 5 * 3))) |>
#'   gg_likertplot(
#'     x = clarity,
#'     fill = cut,
#'     weight = Freq,
#'     include.percent = TRUE,
#'     include.order = "l",
#'     include.reference = 2.5,
#'     facet_formula = .grouping ~ .
#'   )
#'
#' # Summarise_likert
#'
#' diamonds |>
#'    Wide( ~ clarity, cut) -> diamonds_wide
#'
#' # Create summary object
#' diamonds_likert <- diamonds_wide |>
#'   Summarise_likert(
#'     "Group A", SI2, SI1, VS1,
#'     "Group B", VS2, VVS2, VVS1, I1, IF,
#'     include.total = TRUE
#'   )
#'
#'
#'
#' # Create plot
#' plot(diamonds_likert)
#' # gg_likertplot(diamonds_likert)
#'
#' # Customized plot
#' plot(diamonds_likert,
#'      palette =  "Set2")
#'
#' # Access the ggplot object for further customization
#' p <- plot(diamonds_likert)
#' p +
#'   labs(title = "Diamond Quality Distribution",
#'        subtitle = "Data from ggplot2 diamonds dataset",
#'        tag ="A") +
#'   scale_fill_manual(values= RColorBrewer::brewer.pal(5, "Set2"))
#'
#'   # Plot speichern
#'   # ggsave("likert_plot.png", plot = plot(diamonds_likert))
#'
gg_likertplot <- function(data,
                          ...,
                          facet_formula = NULL,
                          include.order = NULL,
                          decreasing = FALSE,
                          include.reference = NULL,
                          include.percent = TRUE,
                          reverse_likert = FALSE,
                          wrap = 50,
                          palette = "RdBu",
                          direction = 1,
                          width = 0.90,
                          border = NA, # "white",
                          item_levels = NULL,
                          labels_size = 3.5
                        #  include.value = FALSE
                          ) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("\nOh je - data must be a data.frame()\n")
  }


  if(!is.null( attr(data, "tbll_likert") )) {
   # cat("\n Hier kommt ein Tbll_likert() \n")
    data <- attr(data, "tbll_likert")
    # stop("\nSorry - das mach ich nicht mehr  verwende einfach Summarise_likert()!!!\n")
  }

  # data muss in folge ein likert-long-data.frame sein
  if (!is.null(attr(data, "tbl"))) {
  # cat("\n Aha - da kommte ein Summarise_likert Objekt\n")
    if (is.null(facet_formula) &
        !is.null(attr(data, "tbll")$facet_formula))
      facet_formula <-  attr(data, "tbll")$facet_formula
    if (attr(data, "likert") == "wide") {
      data <- attr(data, "data_long")
    }
  }
 # else{
 #   cat("\n Aha - da kommte ein data.frame Objekt.
 #      Nur zur Sicherheit du weist eh das muss im lang-Format sein.")
 # }

  # Capture aesthetic mappings
  mapping <- ggplot2::aes()
  dots <- rlang::enquos(...)

  # Set default mappings if not specified
  if (any(names(dots) == "x")) {
    mapping$x <- dots$x
  } else if ("Item" %in% names(data)) {
    mapping$x <- rlang::quo(Item)
  } else {
    stop("Item variable not found in data or arguments")
  }

  # Set weight mapping (Freq)
  if (any(names(dots) == "weight")) {
    mapping$weight <- dots$weight
  } else if ("Freq" %in% names(data)) {
    mapping$weight <- rlang::quo(Freq)
  } else {
    stop("Freq variable not found in data or arguments")
  }

  # Set fill mapping (levels)
  if (any(names(dots) == "fill")) {
    mapping$fill <- dots$fill
  } else if ("levels" %in% names(data)) {
    mapping$fill <- rlang::quo(levels)
  } else {
    stop("levels variable not found in data or arguments")
  }


  # Check if .grouping is specified in dots
  if (any(names(dots) == "grouping")) {
    mapping$grouping  <- dots$grouping
    if(is.null(facet_formula)) facet_formula <- formula(paste(dots$grouping, "~ ."))

  }
  else if (".grouping" %in% names(data)) {
    mapping$grouping  <-  ".grouping"
    if(is.null(facet_formula)) facet_formula <-  .grouping ~ .
  }
  else {
    mapping$grouping  <- NULL

  }

  check_mapping_vars(data, mapping)

  # Handle item levels and wrapping
  data <- process_item_levels(data, mapping, item_levels, wrap)
  # Calculate ordering weights
  data <- calculate_weights(data, mapping, include.order, decreasing, include.reference)
  # Create base plot
  p <- create_base_plot(data, mapping)
  # Add bars based on percent or count
  p <- add_bars(p, include.percent, reverse_likert, include.reference, width, border, labels_size)
  # Apply styling
  p <- apply_styling(p, palette, direction, facet_formula)

  return(p)
}

# Helper functions --------------------------------------------------------

# Hilfsfunktion zum Überprüfen der Mappings
check_mapping_vars <- function(data, mapping) {
  # Alle relevanten Mappings aus mapping ziehen
  vars <- mapping[c("x", "fill", "weight", "grouping")]
  vars <- vars[!vapply(vars, is.null, logical(1))]   # NULL entfernen

  # Quos zu Strings konvertieren
  var_names <- vapply(vars, rlang::as_name, character(1))

  # Fehlende Spalten finden
  missing <- setdiff(var_names, names(data))

  if (length(missing) > 0) {
    stop(
      sprintf(
        "\nOh je - folgende Variablen fehlen im Dataframe: %s\nVerfügbar sind: %s",
        paste(missing, collapse = ", "),
        paste(names(data), collapse = ", ")
      ),
      call. = FALSE
    )
  }
}



process_item_levels <- function(data, mapping, item_levels, wrap) {
  item_var <- rlang::as_name(mapping$x)

  if (!is.null(item_levels)) {
    data[[item_var]] <- factor(data[[item_var]], levels = item_levels)
  }

  if (is.numeric(wrap)) {
    data[[item_var]] <- wrap_string(data[[item_var]], wrap)
  }

  return(data)
}

calculate_weights <- function(data,
                              mapping,
                              include.order,
                              decreasing,
                              include.reference) {
  if (!is.null(include.order)) {
    data <- order_weighted(
      data,
      item_var = rlang::as_name(mapping$x),
      freq_var = rlang::as_name(mapping$weight),
      levels_var = rlang::as_name(mapping$fill),
      include.order,
      decreasing,
      include.reference
    )
  } else {
    item_var <- rlang::as_name(mapping$x)
    data$mean_weight <- nlevels(data[[item_var]]) - as.numeric(data[[item_var]])
  }

  return(data)
}

create_base_plot <- function(data, mapping) {
  if (!is.null(mapping$grouping)) {

    grouping_var <- data[[mapping$grouping]]
    p <- ggplot2::ggplot(data) +
      ggplot2::aes(
        y = tidytext::reorder_within(!!mapping$x, mean_weight, grouping_var),
        fill = !!mapping$fill,
        weight = !!mapping$weight
      ) +
      tidytext::scale_y_reordered()
  } else {
    p <- ggplot2::ggplot(data) +
      ggplot2::aes(
        y =  stats::reorder(!!mapping$x, mean_weight),
        fill = !!mapping$fill,
        weight = !!mapping$weight
      )
  }

  return(p)
}

add_bars <- function(p,
                     include.percent,
                     reverse_likert,
                     include.reference,
                     width, border,
                     labels_size) {
  if (include.percent) {
    p <- p +
      ggplot2::geom_bar(colour = border,
        position = ggstats::position_likert(reverse = reverse_likert,
                                            cutoff = include.reference),
        stat = ggstats::StatProp,
        complete = "fill",
        width = width
      ) +
      ggplot2::scale_x_continuous(labels = ggstats::label_percent_abs())


  } else {
    p <- p +
      ggplot2::geom_bar(
        position =  ggstats::position_diverging(
          # position_likert_count(
          reverse = reverse_likert,
          cutoff = include.reference
        ),
        stat = "count",
        width = width
      ) +
      ggplot2::scale_x_continuous(labels = ggstats::label_number_abs())

  }

  return(p)
}

apply_styling <- function(p, palette, direction, facet_formula) {
  if (!is.null(palette)) {
    p <- p +
      ggplot2::scale_fill_brewer(palette = palette, direction = direction)
  }

  p <- p +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top",
                   panel.grid.major.y = ggplot2::element_blank())

  if (!is.null(facet_formula)) {
    p <- p +
      ggplot2::facet_grid(facet_formula, scales = "free", space = "free")
  }

  return(p)

}



