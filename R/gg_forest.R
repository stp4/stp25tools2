#' Forest Plot für Prävalenzen mit Konfidenzintervallen
#'
#' Diese Funktion erzeugt einen Forest Plot für Prävalenzen mit Konfidenzintervallen.
#' Die Punkte und Fehlerbalken können nach einer Gruppierungsvariable versetzt
#' dargestellt werden, und die Farben können flexibel angepasst werden.
#'
#' @param data Ein Dataframe mit den Prävalenzdaten. Muss mindestens die Spalten
#'   für Prävalenz, untere und obere CI-Grenzen enthalten.
#' @param ... Optionale aestetische Mappings. Kann \code{x}, \code{y}, \code{col},
#'   \code{xmin}, \code{xmax} enthalten. Falls nicht angegeben, werden Standardwerte
#'   verwendet.
#' @param main Titel des Plots. Default: "Prävalenz nach Risikofaktor".
#' @param digits Anzahl der Nachkommastellen für Prozentwerte. Default: 0.
#' @param xlab Beschriftung der x-Achse. Falls NULL, wird automatisch basierend auf
#'   der CI-Methode generiert.
#' @param color Farben für die Darstellung. Kann sein:
#'   \itemize{
#'     \item NULL: Verwendet die Standardfarbpalette (Brewer Set1)
#'     \item Einzelne Farbe als Zeichenkette (z.B. "steelblue"): Alle Elemente in dieser Farbe
#'     \item Vektor von Farben: Benutzerdefinierte Farben für jede Gruppe
#'     \item Name einer Brewer-Palette (z.B. "Set2", "Dark2"): Verwendet diese Palette
#'   }
#' @param dodge_width Breite des Versatzes zwischen gruppierten Elementen.
#'   Größere Werte führen zu größeren Abständen. Default: 0.5.
#' @param show_legend Soll die Legende angezeigt werden? Default: TRUE.
#' @param legend_title Titel der Legende. Falls NULL, wird keine Überschrift angezeigt.
#' @param point_size Größe der Punkte im Plot. Default: 3.
#' @param errorbar_size Dicke der Fehlerbalken. Default: 1.
#' @param text_size Schriftgröße der Prozentwerte. Default: 3.5.
#'
#' @return Ein ggplot-Objekt, das den Forest Plot darstellt.
#'
#' @details
#' Die Funktion unterstützt verschiedene Methoden für Konfidenzintervalle:
#' \itemize{
#'   \item Clopper-Pearson (Standard, wenn Spalten "clopper.pearson_low/upr" vorhanden)
#'   \item Wilson (wenn Spalten "wilson.score_low/upr" vorhanden)
#'   \item Agresti-Coull (wenn Spalten "agresti.coull_low/upr" vorhanden)
#'   \item Wald (wenn Spalten "wald_low/upr" vorhanden)
#' }
#'
#' Wenn das Argument \code{col} übergeben wird, werden die Punkte und Fehlerbalken
#' entsprechend der Gruppierungsvariable versetzt dargestellt. Die Farben können
#' über das \code{color}-Argument angepasst werden.
#'
#' @examples
#' \dontrun{
#' #' library(stp25tools2)
#' # Test-Daten
#' test1 <- epi.tests(c(670, 202, 74, 640))
#' test2 <- epi.tests(c(650, 202, 74, 660))
#' 
#' data <- Tbll_xtabs_2x2(
#'   Histologie = test1,
#'   Stanzbiopsie = test2,
#'   select = c("se", "sp", "lr.pos", "pv.pos")
#' )
#' data
#' 
#' data |> gg_forest( ncol) 
#' }
#'
#' @import ggplot2
#' @importFrom rlang enquo enquos as_name
#' @importFrom scales percent_format
#' @export
gg_forest <- function(data,
                       ...,
                       main = "",
                       digits = 2,
                       facet_formula = NULL,
                       xlab = NULL,
                       color = NULL,
                       dodge_width = 0.5,
                       show_legend = TRUE,
                       legend_title = NULL,
                       point_size = 3,
                       errorbar_size = point_size / 3,
                       text_size = 3.5,
                       nrow = NULL,
                       ncol = NULL) {
  
  
  # Überprüfen der Eingaben
  if (!is.data.frame(data)) {
    stop("'data' muss ein Dataframe sein.")
  }
  
  if (dodge_width <= 0) {
    warning("'dodge_width' sollte positiv sein. Verwende Default-Wert 0.5.")
    dodge_width <- 0.5
  }
  
  att_in <- attr(data, "plot")
  if (!is.null(att_in))  {
    ci <-  paste0(attr(data, "ci") * 100, "%-CI")
    data <- att_in
  }
  else
    ci <- ""
  
  # Capture dot-dot-dot arguments
  dots <- rlang::enquos(...)
  
  # Default mappings if not specified in ...
  mapping <- aes()
  nmsdta <- names(data)
  nmsdots <- names(dots)
  
# x-axis Punkte estimate (numeric) --------------------------------------------------------

   
  if ("x" %in% nmsdots) {
    mapping$x <- dots$x
  }
  else if ("praevalenz" %in% nmsdta) {
    mapping$x <- quo(praevalenz)
    mapping$stat_code <- quo(stat_code)
    mapping$statistic <- quo(statistic)
    mapping$statistic_type <-  quo(statistic_type)
    mapping$est_label <-  quo(est_label)
    data$statistic_type <- get_statistic_type(data$stat_code)
    data <- dplyr::mutate(
      data,
      stat_code = "tp",
      statistic_type = "percent",
      est_label = paste0(round(!!mapping$x), "%")
    )
    
    stop("\n das geht noch nicht wegen der xlim auf 100%")
  } 
  else if ("est" %in% nmsdta) {
    mapping$x <- quo(est)
  }
  else {
    stop("Keine Estimate-Spalte gefunden. Bitte 'x' Mapping angeben.")
  }
  

# was wird evaluiert - statistic ------------------------------------------


  if (all(c("statistic", "stat_code") %in% nmsdta)) {
    mapping$stat_code <- quo(stat_code)
    mapping$statistic <- quo(statistic)
    mapping$statistic_type <-  quo(statistic_type)
    mapping$est_label <-  quo(est_label)
    data$statistic_type <- get_statistic_type(data$stat_code)
    
    # data$statistic 
    # als Faktor mit den Levels in der gewünschten Reihenfolge
    
    prc_fm <- function(x, digits=0){
       ifelse(x < 99, paste0(round(x, digits), "%"),
              ifelse(x==100,  "100%", "99%"))
    }
    
    data <- dplyr::mutate(data,
                          statistic= factor(data$statistic, levels = unique( statistic )),
                          est_label =
                            ifelse(statistic_type == "percent",
                                   prc_fm(!!mapping$x, 0),
                                   signif(!!mapping$x, 2)
                            ))
    
    
    if( length(unique(data$statistic) == 1)) facet_formula  <- ~ statistic
    
      }
  else {
    stop("Keine statistic-Spalte gefunden. Bitte 'x' Mapping angeben.")
  }
  

# y-axis (factor) ---------------------------------------------------------

  
  if ("y" %in% nmsdots) {
    mapping$y <- dots$y
  }
  else if ("variable" %in% nmsdta) {
    mapping$y <- quo(variable)
  } 
  else if ("parameter" %in% nmsdta) {
    mapping$y <- quo(parameter)
    # nur in Tbll_2x2- objekt
    data$parameter <- factor(data$parameter, rev(unique(data$parameter)))
  }else {
    stop("Keine y-Achsen-Spalte gefunden. Bitte 'y' Mapping angeben.")
  }
  
  

# groups (farbe) ----------------------------------------------------------

  
  # Wichtig: col wird über das Mapping gesetzt
  if ("col" %in% nmsdots) {
    mapping$col <- dots$col
    has_color_mapping <- TRUE
  } else {
    mapping$col <- NULL
    has_color_mapping <- FALSE
  }
  
  

  

# CI-Mappings Fehlerbalken ---------------------------------------------------

 
  if (all(c("xmin", "xmax") %in% nmsdots)) {
    mapping$xmin <- dots$xmin
    mapping$xmax <- dots$xmax
    if (is.null(xlab))
      xlab <- "Prävalenz (95% CI)"
  }
  else if (all(c("lower", "upper") %in% nmsdots)) {
    mapping$xmin <- dots$lower
    mapping$xmax <- dots$upper
    if (is.null(xlab))
      xlab <- "Estimate (95% CI)"
  }
  else if (all(c("lower", "upper") %in% nmsdta)) {
    mapping$xmin <- quo(lower)
    mapping$xmax <- quo(upper)
    if (is.null(xlab))
      xlab <- "Estimate (95% CI)"
  }
  else if (any(grepl("clopper", nmsdta))) {
    mapping$xmin <- quo(clopper.pearson_low)
    mapping$xmax <- quo(clopper.pearson_upr)
    if (is.null(xlab))
      xlab <- paste0("Prävalenz mit ", ci, " (Clopper-Pearson)")
  }
  else if (any(grepl("wilson", nmsdta))) {
    mapping$xmin <- quo(wilson.score_low)
    mapping$xmax <- quo(wilson.score_upr)
    if (is.null(xlab))
      xlab <- paste0("Prävalenz mit ", ci, " (Wilson)")
  }
  else if (any(grepl("agresti", nmsdta))) {
    mapping$xmin <- quo(agresti.coull_low)
    mapping$xmax <- quo(agresti.coull_upr)
    if (is.null(xlab))
      xlab <- paste0("Prävalenz mit ", ci, " (Agresti-Coull)")
  }
  else if (any(grepl("wald", nmsdta))) {
    mapping$xmin <- quo(wald_low)
    mapping$xmax <- quo(wald_upr)
    if (is.null(xlab))
      xlab <- paste0("Prävalenz mit ", ci, " (Wald)")
  }
  else {
    stop("Keine CI-Spalten gefunden. Bitte xmin/xmax angeben.")
  }
  
#  mapping$x_lim1 <- quo(x_lim1)
#  mapping$x_lim2 <- quo(x_lim2)
  data <- dplyr::mutate(data,
                        x_lim1=  
                          ifelse(!!mapping$xmin>90, 90,
                          ifelse(!!mapping$xmin>80, 80,
                          ifelse(!!mapping$xmin>70, 70,
                          ifelse (!!mapping$xmin>60, 60, 
                          ifelse (!!mapping$xmin>50, 50,
                          round(!!mapping$xmin - abs((!!mapping$xmin *.40)), 1
                                )))))),
                        x_lim2 = ifelse( !!mapping$xmax>60, 
                                         100, round(!!mapping$xmax*1.2,1))
                      )

  
  # return(list(
  #   data = head(data),
  #   mapping= mapping))
  
  
# Create base plot --------------------------------------------------------
  if (!has_color_mapping) {
    p <-
      ggplot(
        data, 
        aes(
          x = !!mapping$x, 
          y = !!mapping$y))
  } else {
    p <-
      ggplot(data,
             aes(
               x = !!mapping$x,
               y = !!mapping$y,
               color = !!mapping$col,
               group = !!mapping$col   # Wichtig für position_dodge
             ))
  }

 
  # Positionierung für versetzte Darstellung
  dodge_pos <- position_dodge(width = dodge_width)
  
  # Basiselemente des Plots
  # p <- p + 
  #   geom_vline(
  #     xintercept = c(0, 50),
  #     linetype = "dashed",
  #     color = "gray70",
  #     alpha = 0.7
  #   )
  
  # Punkte hinzufügen (mit oder ohne Farbe)
  if (has_color_mapping) {
    p <- 
      p + 
      geom_point(
        aes(color = !!mapping$col),
        size = point_size,
        position = dodge_pos)
  } else {
    p <- p + 
      geom_point(
        size = point_size,
        position = position_nudge(x = 0),
        color = if (!is.null(color) && length(color) == 1) color else "steelblue"
    )
  }
 
  # Fehlerbalken hinzufügen (mit oder ohne Farbe)
  if (has_color_mapping) {
    
    cat( "\n\n  geom_errorbar \n")
    p <- p + geom_errorbar(
      aes(
        xmin = !!mapping$xmin,
        xmax = !!mapping$xmax,
        color = !!mapping$col
      ),
      width = errorbar_size/2,
      linewidth = errorbar_size,
      position = dodge_pos,
      show.legend = FALSE,  # Farbe nur in Punkten zeigen
      orientation = "y"
    )
  } else {
    p <- p + geom_errorbar(
      aes(
        xmin = !!mapping$xmin,
        xmax = !!mapping$xmax
      ),
      width = errorbar_size/2,
      linewidth = errorbar_size,
      position = position_nudge(x = 0),
      color = if (!is.null(color) && length(color) == 1) color else "steelblue",
      orientation = "y"
    )
  }
 
  # Prozentwerte als Text hinzufügen
  if (has_color_mapping) {
    p <- p + geom_text(
      aes(
        label = est_label,
        x = !!mapping$x,
        group = !!mapping$col
      ),
      size = text_size,
      position = dodge_pos,
      vjust = -0.6,
      show.legend = FALSE
    )
  } else {
    p <- p + geom_text(
      aes(
        label = est_label,
        x = !!mapping$x
      ),
      size = text_size,
      # position = position_nudge(y = 0.2),
      position = dodge_pos,
      
      vjust = -0.6,
      show.legend = FALSE
    )
  }
 
  
  if(!is.null(facet_formula)) {

    
    scale_list <- data |>
      group_by(statistic) |>
      summarise(
        x_min = min(x_lim1),
        x_max = max(x_lim2)
      ) |>
      purrr::pmap(function(statistic, x_min, x_max) {
        scale_x_continuous(
          limits = c(x_min, x_max),
          expand = expansion(mult = c(0.02, 0.05))
        )
      })
    
    
    p <- p + 
      facet_wrap(facet_formula, 
                 nrow = nrow,
                 ncol = ncol,
                 scales = "free_x",
                 
                 ) +
      ggh4x::facetted_pos_scales(x = scale_list)
    
  }
  else{
  
  
  
  # Skalierung und Design
  p <- p +
    scale_x_continuous(
    #  labels = scales::percent_format(accuracy = 1),
    #  limits = c(!!mapping$x_lim1, !!mapping$x_lim2),
    #  breaks = seq(0, 1, 0.2),
      expand = expansion(mult = c(0.02, 0.05))  # Weniger Padding
    ) 
  }
  
  p <- p +
    labs(
      title = main,
      x = xlab,
      y = NULL,
      color = legend_title
    ) +
   # theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.y = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = if (show_legend) "top" else "none"
    )
  
     return(p)
  
  
  # Farbmanagement
  if (has_color_mapping && !is.null(color)) {
    if (length(color) == 1 && color %in% rownames(RColorBrewer::brewer.pal.info)) {
      # Brewer-Palette verwenden
      n_groups <- length(unique(data[[rlang::as_name(mapping$col)]]))
      p <- p + scale_color_brewer(palette = color)
    } else if (length(color) == 1) {
      # Einzelne Farbe für alle Gruppen
      n_groups <- length(unique(data[[rlang::as_name(mapping$col)]]))
      p <- p + scale_color_manual(values = rep(color, n_groups))
    } else if (length(color) > 1) {
      # Benutzerdefinierter Farbvektor
      p <- p + scale_color_manual(values = color)
    }
  } else if (has_color_mapping) {
    # Standard Brewer-Palette
    p <- p + scale_color_brewer(palette = "Set1")
  }
  
  return(p)
}





# Bestimmt Statistik-Typen
get_statistic_type <- function(statistic) {
  # Prozent-basierte Statistiken (0-1)
  percent_stats <- c(
    "se", "sp", #"Sensitivity", "Specificity",
    "ap",
    "tp",    #"Apparent prevalence", "True prevalence",
    "diag.ac",   #"Diagnostic accuracy",
    "pv.pos",  # "Positive predictive value",
    "pv.neg",  # "Negative predictive value",
    "p.rout",   #"Proportion of disease positive",
    "p.rin",  #"Proportion of disease negative",
    "p.tpdn",   #"True positive / disease negative",
    "p.tndp" ,   #"True negative / disease positive",
    "p.dntp",    #"Disease negative / true positive",
    "p.dptn"    #"Disease positive / true negative"
  )
  
  # Likelihood Ratios (typischerweise 0-10 oder 0-100)
  likelihood_stats <- c(
    "lr.pos", #" Positive likelihood ratio",
    "lr.neg"  # "Negative likelihood ratio"
  )
  
  # Verhältnis-basierte Statistiken
  ratio_stats <- c(
    "diag.or",   #"Diagnostic odds ratio",
    "nndx", #"Number needed to diagnose",
    "youden"  #  "Youden's index"
  )
  
  sapply(statistic, function(stat) {
    if (stat %in% percent_stats)
      return("percent")
    if (stat %in% likelihood_stats)
      return("likelihood")
    if (stat %in% ratio_stats)
      return("ratio")
    return("other")
  })
}




# Bild |>
#   Tbll_xtabs_2x2( "  A: Inkl. alle negativ" = ~ Bildgebung_A + Histologie,
#                   "  B: Inkl. proportional" = ~ Bildgebung_B + Histologie,
#                   "  C: Inkl. alle positiv" = ~ Bildgebung_C + Histologie,
#                   "  D: eindeutiger Befund"  = ~ Bildgebung_D + Histologie
#   ) |>
#   Output()  |> 
#   gg_forest(ncol=2)  +
#   ggplot2::theme_bw(base_size = 12)
# SavePlot(w=8, h=2.2*3)
 