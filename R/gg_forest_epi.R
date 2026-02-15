#' Forest Plot für epiR Statistiken mit intelligentem Facet-Handling
#'
#' Erstellt einen Forest Plot für die extrahierten epiR Statistiken
#' mit automatischer Prüfung der Facet-Kompatibilität.
#'
#' @param data Dataframe von `extract_epi()`
#' @param facet_formula Formel für Facets (Standard: NULL). 
#'   Nur erlaubt wenn Statistiken vergleichbare Skalierungen haben.
#'   Beispiele:
#'   - `~ statistic`: Ein Facet pro Statistik (nur bei kompatiblen Statistiken)
#'   - `parameter ~ statistic`: Parameter als Reihen, Statistiken als Spalten
#' @param sort_by Sortieren nach Wert (TRUE) oder originaler Reihenfolge (FALSE)
#' @param color Farbe für Punkte und Balken. Bei Facets wird Farbe ignoriert.
#' @param title Plot-Titel
#' @param xlab Beschriftung der x-Achse
#' @param show_labels Sollen die Werte als Text angezeigt werden?
#' @param scales Skalierung der Facets: "fixed", "free", "free_x", "free_y"
#' @param strip_position Position der Facet-Labels: "top", "bottom"
#' @param force_facet Facets erzwingen auch bei inkompatiblen Skalierungen (FALSE)
#' @param show_reference Referenzlinien anzeigen (bei Prozent-Statistiken bei 0.5)
#'
#' @return Ein ggplot2 Objekt
#' @export
#'
#' @examples
#' \dontrun{
#' library(epiR)
#' library(ggplot2)
#'
#' test1 <- epi.tests(c(670, 202, 74, 640))
#' test2 <- epi.tests(c(650, 202, 74, 660))
#'
#' data <- extract_epi(
#'   Histologie = test1,
#'   Stanzbiopsie = test2,
#'   select = c("se", "sp", "lr.pos", "pv.pos")
#' )
#'
#' # Nur Sensitivität und Spezifität (kompatible Skalierung)
#' data_se_sp <- data[data$statistic %in% c("Sensitivity", "Specificity"), ]
#' forest_plot_epi(data_se_sp, facet_formula = ~ statistic)
#'
#' # Inkompatible Statistiken - Facet wird automatisch deaktiviert
#' forest_plot_epi(data, facet_formula = ~ statistic)
#'
#' # Nur Prozent-basierte Statistiken
#' data_percent <- data[data$statistic %in% 
#'   c("Sensitivity", "Specificity", "Positive predictive value"), ]
#' forest_plot_epi(data_percent, facet_formula = ~ statistic)
#' }
forest_plot_epi <- function(data,
                            facet_formula = NULL,
                            sort_by = TRUE,
                            color = "steelblue",
                            title = "Forest Plot: Diagnostische Testcharakteristika",
                            xlab = "Wert (95% Konfidenzintervall)",
                            show_labels = TRUE,
                            scales = "fixed",
                            strip_position = "top",
                            force_facet = FALSE,
                            show_reference = TRUE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 ist benötigt für diese Funktion")
  }
  
  # Sicherstellen, dass Daten vorhanden sind
  if (is.null(data) || nrow(data) == 0) {
    stop("Keine Daten zum Plotten vorhanden")
  }
  
  # Helper-Funktionen --------------------------------------------------------
  
  # Bestimmt Statistik-Typen
  get_statistic_type <- function(statistic) {
    # Prozent-basierte Statistiken (0-1)
    percent_stats <- c(
      "Sensitivity", "Specificity", 
      "Apparent prevalence", "True prevalence",
      "Diagnostic accuracy", "Positive predictive value",
      "Negative predictive value",
      "Proportion of disease positive", 
      "Proportion of disease negative",
      "True positive / disease negative",
      "True negative / disease positive",
      "Disease negative / true positive",
      "Disease positive / true negative"
    )
    
    # Likelihood Ratios (typischerweise 0-10 oder 0-100)
    likelihood_stats <- c(
      "Positive likelihood ratio",
      "Negative likelihood ratio"
    )
    
    # Verhältnis-basierte Statistiken
    ratio_stats <- c(
      "Diagnostic odds ratio",
      "Number needed to diagnose",
      "Youden's index"
    )
    
    sapply(statistic, function(stat) {
      if (stat %in% percent_stats) return("percent")
      if (stat %in% likelihood_stats) return("likelihood")
      if (stat %in% ratio_stats) return("ratio")
      return("other")
    })
  }
  
  # Prüft ob Facets erlaubt sind
  check_facet_compatibility <- function(data, facet_formula, force_facet) {
    if (is.null(facet_formula)) return(FALSE)
    
    # Extrahiere Variablen aus der Formel
    if (inherits(facet_formula, "formula")) {
      vars <- all.vars(facet_formula)
    } else {
      stop("facet_formula muss eine Formel sein oder NULL")
    }
    
    # Wenn statistic in der Formel ist
    if ("statistic" %in% vars) {
      # Bestimme Statistik-Typen in den Daten
      stat_types <- unique(get_statistic_type(data$statistic))
      
      # Prüfe Kompatibilität
      if (length(stat_types) > 1) {
        # Gemischte Typen - prüfe ob kompatibel
        if (force_facet) {
          warning("Facets mit gemischten Statistik-Typen können zu schlechter Darstellung führen.",
                  "\nVerwende 'force_facet = FALSE' oder filtere Daten auf einen Typ.")
          return(TRUE)
        } else {
          # Prüfe auf kompatible Kombinationen
          # Prozent + Prozent: OK
          # Likelihood + Likelihood: OK
          # Prozent + Likelihood: Nicht kompatibel
          # Prozent + Ratio: Nicht kompatibel
          
          if (all(stat_types %in% c("percent"))) {
            # Nur Prozent-Statistiken - kompatibel
            return(TRUE)
          } else if (all(stat_types %in% c("likelihood"))) {
            # Nur Likelihood Ratios - kompatibel
            return(TRUE)
          } else if (all(stat_types %in% c("ratio"))) {
            # Nur Ratio-Statistiken - potentiell kompatibel
            return(TRUE)
          } else {
            # Inkompatible Kombination
            message("Facets deaktiviert: Inkompatible Statistik-Typen (",
                    paste(unique(data$statistic), collapse = ", "), ")\n",
                    "Verwende entweder nur Prozent-Statistiken (Sensitivity, Specificity, etc.) ",
                    "oder nur Likelihood Ratios.")
            return(FALSE)
          }
        }
      } else {
        # Nur ein Statistik-Typ - immer kompatibel
        return(TRUE)
      }
    }
    
    # Wenn statistic nicht in der Formel ist, Facets immer erlaubt
    return(TRUE)
  }
  
  # Erstelle geeignetes Label
  create_label <- function(est, lower, upper, stat_type) {
    if (stat_type == "percent") {
      sprintf("%.1f%%\n(%.1f%%, %.1f%%)", 
              est * 100, lower * 100, upper * 100)
    } else if (stat_type == "likelihood") {
      sprintf("%.2f\n(%.2f, %.2f)", est, lower, upper)
    } else {
      sprintf("%.2f\n(%.2f, %.2f)", est, lower, upper)
    }
  }
  
  # Hauptfunktion ------------------------------------------------------------
  
  # Daten vorbereiten
  
  plot_data <- attr(data, "plot")
  if (!is.null(plot_data))  {
    ci <-  paste0(attr(data, "ci") * 100, "%-CI")
    
  }
  else{
    ci <- ""
    plot_data <- data
  }
  # Statistik-Typ hinzufügen
  plot_data$stat_type <- get_statistic_type(plot_data$statistic)
  
  # Prüfe Facet-Kompatibilität
  use_facet <- check_facet_compatibility(plot_data, facet_formula, force_facet)
  
  if (!use_facet && !is.null(facet_formula)) {
    message("Plotting without facets due to incompatible statistic types.")
  }
  
  # Sortieren wenn gewünscht
  if (sort_by) {
    if (use_facet && "statistic" %in% all.vars(facet_formula)) {
      # Innerhalb jeder Statistik sortieren
      plot_data <- plot_data %>%
        dplyr::group_by(statistic) %>%
        dplyr::arrange(est, .by_group = FALSE) %>%
        dplyr::ungroup()
    } else {
      # Global sortieren
      plot_data <- plot_data[order(plot_data$est), ]
    }
    
    # Parameter als Faktor mit sortierten Levels
    plot_data$parameter <- factor(plot_data$parameter, 
                                  levels = unique(plot_data$parameter))
  }
  
  # Bestimme ob einheitliche Skalierung möglich ist
  stat_types <- unique(plot_data$stat_type)
  
  # Basis-Plot
  if (use_facet && length(unique(plot_data$statistic)) > 1) {
    # Bei Facets mit verschiedenen Statistiken: Farbe nach Parameter
    p <- ggplot2::ggplot(
      plot_data, 
      ggplot2::aes(x = est, y = parameter, color = parameter)
    )
  } else {
    # Einfacher Plot: feste Farbe
    p <- ggplot2::ggplot(
      plot_data, 
      ggplot2::aes(x = est, y = parameter)
    )
  }
  
  # Referenzlinien hinzufügen
  if (show_reference) {
    if (length(stat_types) == 1 && stat_types == "percent") {
      # Nur Prozent-Statistiken: Referenz bei 0.5
      p <- p + ggplot2::geom_vline(
        xintercept = 0.5, 
        linetype = "dashed", 
        color = "gray70", 
        alpha = 0.5
      )
    } else if (!use_facet) {
      # Bei gemischten Typen ohne Facets: Keine Referenz
      # (könnte bei Bedarf erweitert werden)
    }
  }
  
  # Punkte und Fehlerbalken
  if (use_facet && length(unique(plot_data$statistic)) > 1) {
    # Bei Facets: Farbe nach Parameter
    p <- p +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = lower, xmax = upper),
        height = 0.2, 
        size = 1
      )
  } else {
    # Ohne Facets oder nur eine Statistik: feste Farbe
    p <- p +
      ggplot2::geom_point(size = 3, color = color) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = lower, xmax = upper),
        height = 0.2, 
        color = color, 
        size = 1
      )
  }
  
  # Labels hinzufügen wenn gewünscht
  if (show_labels) {
    plot_data$label_text <- mapply(
      create_label,
      plot_data$est,
      plot_data$lower,
      plot_data$upper,
      plot_data$stat_type
    )
    
    p <- p + 
      ggplot2::geom_text(
        data = plot_data,
        ggplot2::aes(
          label = label_text,
          x = upper
        ),
        hjust = -0.1, 
        vjust = 0.5,
        size = 3,
        color = "gray30",
        lineheight = 0.8
      )
  }
  
  # Facets hinzufügen wenn erlaubt und sinnvoll
  if (use_facet) {
    p <- p + ggplot2::facet_grid(
      facet_formula,
      scales = scales,
      space = ifelse(scales == "fixed", "fixed", "free_x")
    )
  }
  
  # X-Achse skalieren basierend auf Statistik-Typen
  if (use_facet) {
    # Bei Facets: Skalierung kontrollieren
    if (scales %in% c("free_x", "free")) {
      # Freie Skalierung - ggplot kümmert sich automatisch
      p <- p + ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.2))
      )
    } else {
      # Feste Skalierung - nur wenn alle Statistiken gleichen Typ haben
      if (length(stat_types) == 1) {
        if (stat_types == "percent") {
          p <- p + ggplot2::scale_x_continuous(
            labels = scales::percent_format(accuracy = 1),
            limits = c(0, 1),
            expand = ggplot2::expansion(mult = c(0.05, 0.1))
          )
        } else if (stat_types == "likelihood") {
          # Für Likelihood Ratios: angemessene Limits
          max_val <- max(plot_data$upper, na.rm = TRUE) * 1.1
          p <- p + ggplot2::scale_x_continuous(
            limits = c(0, max_val),
            expand = ggplot2::expansion(mult = c(0.05, 0.1))
          )
        } else {
          # Andere Typen: automatische Limits
          p <- p + ggplot2::scale_x_continuous(
            expand = ggplot2::expansion(mult = c(0.05, 0.2))
          )
        }
      } else {
        warning("Feste Skalierung (scales = 'fixed') mit gemischten Statistik-Typen ",
                "kann zu schlechter Darstellung führen. Verwende scales = 'free_x'.")
        p <- p + ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(0.05, 0.2))
        )
      }
    }
  } else {
    # Ohne Facets: Skalierung basierend auf Daten
    if (length(stat_types) == 1 && stat_types == "percent") {
      p <- p + ggplot2::scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1),
        expand = ggplot2::expansion(mult = c(0.05, 0.15))
      )
    } else if (length(stat_types) == 1 && stat_types == "likelihood") {
      max_val <- max(plot_data$upper, na.rm = TRUE) * 1.1
      p <- p + ggplot2::scale_x_continuous(
        limits = c(0, max_val),
        expand = ggplot2::expansion(mult = c(0.05, 0.1))
      )
    } else {
      p <- p + ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.15))
      )
    }
  }
  
  # Theme und Labels
  p <- p +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 11, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 10),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold", 
        size = 11,
        margin = ggplot2::margin(t = 5, b = 5)
      ),
      strip.background = ggplot2::element_rect(
        fill = "gray90", 
        color = NA
      ),
      panel.spacing = ggplot2::unit(10, "points")
    )
  
  # Strip position setzen
  if (use_facet) {
    if (strip_position == "top") {
      p <- p + ggplot2::theme(strip.placement = "outside")
    } else if (strip_position == "bottom") {
      p <- p + ggplot2::theme(
        strip.placement = "outside",
        strip.text.y = ggplot2::element_text(angle = 0)
      )
    }
  }
  
  # Farbpalette setzen wenn nötig
  if (use_facet && length(unique(plot_data$statistic)) > 1) {
    n_params <- length(unique(plot_data$parameter))
    if (n_params <= 8) {
      p <- p + ggplot2::scale_color_brewer(
        palette = "Set2",
        name = "Test"
      )
    } else {
      p <- p + ggplot2::scale_color_viridis_d(
        name = "Test",
        option = "plasma"
      )
    }
    
    # Legende anpassen
    p <- p + ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.box.spacing = ggplot2::unit(2, "mm")
    )
  }
  
  return(p)
}


#' Hilfsfunktion: Filtere Daten für kompatible Facets
#'
#' Filtert Daten für Facet-Plots basierend auf Statistik-Typen.
#'
#' @param data Dataframe von `extract_epi()`
#' @param statistic_type Typ der Statistiken: "percent", "likelihood", oder "all"
#' @param specific_stats Spezifische Statistiken auswählen (optional)
#'
#' @return Gefilterter Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' data <- extract_epi(test1, test2, select = c("se", "sp", "lr.pos", "pv.pos"))
#' 
#' # Nur Prozent-Statistiken
#' data_percent <- filter_for_facets(data, "percent")
#' 
#' # Nur Sensitivität und Spezifität
#' data_se_sp <- filter_for_facets(data, specific_stats = c("Sensitivity", "Specificity"))
#' }
filter_for_facets <- function(data, 
                              statistic_type = c("percent", "likelihood", "all"),
                              specific_stats = NULL) {
  
  statistic_type <- match.arg(statistic_type)
  
  # Helper-Funktion für Statistik-Typen (wie in forest_plot_epi)
  get_statistic_type <- function(statistic) {
    percent_stats <- c(
      "Sensitivity", "Specificity", 
      "Apparent prevalence", "True prevalence",
      "Diagnostic accuracy", "Positive predictive value",
      "Negative predictive value",
      "Proportion of disease positive", 
      "Proportion of disease negative",
      "True positive / disease negative",
      "True negative / disease positive",
      "Disease negative / true positive",
      "Disease positive / true negative"
    )
    
    likelihood_stats <- c(
      "Positive likelihood ratio",
      "Negative likelihood ratio"
    )
    
    sapply(statistic, function(stat) {
      if (stat %in% percent_stats) return("percent")
      if (stat %in% likelihood_stats) return("likelihood")
      return("other")
    })
  }
  
  if (!is.null(specific_stats)) {
    # Spezifische Statistiken auswählen
    filtered <- data[data$statistic %in% specific_stats, ]
  } else {
    # Nach Typ filtern
    data$stat_type <- get_statistic_type(data$statistic)
    
    if (statistic_type == "percent") {
      filtered <- data[data$stat_type == "percent", ]
    } else if (statistic_type == "likelihood") {
      filtered <- data[data$stat_type == "likelihood", ]
    } else {
      filtered <- data
    }
    
    # Stat_type Spalte entfernen
    filtered$stat_type <- NULL
  }
  
  if (nrow(filtered) == 0) {
    warning("Keine Daten nach Filterung übrig.")
  }
  
  return(filtered)
}


#' Beispiel für kompatible Facet-Plots
#'
#' @return Liste mit Beispiel-Plots
#' @export
#'
#' @examples
#' \dontrun{
#' example_compatible_facets()
#' }
example_compatible_facets <- function() {
  if (!requireNamespace("epiR", quietly = TRUE)) {
    stop("epiR ist benötigt für dieses Beispiel")
  }
  
  library(epiR)
  
  # Test-Daten erstellen
  test1 <- epi.tests(c(670, 202, 74, 640))
  test2 <- epi.tests(c(650, 202, 74, 660))
  
  # Alle möglichen Statistiken extrahieren
  data <- extract_epi(
    Histologie = test1,
    Stanzbiopsie = test2,
    select = c("se", "sp", "lr.pos", "lr.neg", "pv.pos", "pv.neg", "diag.ac")
  )
  
  plots <- list()
  
  # 1. Nur Sensitivität und Spezifität (kompatibel)
  data_se_sp <- filter_for_facets(data, specific_stats = c("Sensitivity", "Specificity"))
  plots$sens_spec <- forest_plot_epi(
    data_se_sp,
    facet_formula = ~ statistic,
    title = "Sensitivität und Spezifität"
  )
  
  # 2. Nur Likelihood Ratios (kompatibel)
  data_lr <- filter_for_facets(data, specific_stats = c("Positive likelihood ratio", 
                                                        "Negative likelihood ratio"))
  plots$likelihood <- forest_plot_epi(
    data_lr,
    facet_formula = ~ statistic,
    title = "Likelihood Ratios",
    show_reference = FALSE
  )
  
  # 3. Nur Prozent-basierte Statistiken
  data_percent <- filter_for_facets(data, statistic_type = "percent")
  plots$percent <- forest_plot_epi(
    data_percent,
    facet_formula = ~ statistic,
    title = "Prozent-basierte Statistiken"
  )
  
  # 4. Gemischte Daten (ohne Facet)
  plots$mixed <- forest_plot_epi(
    data,
    facet_formula = ~ statistic,  # Wird automatisch deaktiviert
    title = "Gemischte Statistiken (kein Facet)"
  )
  
  # 5. Mit force_facet (mit Warnung)
  plots$forced <- forest_plot_epi(
    data,
    facet_formula = ~ statistic,
    force_facet = TRUE,
    scales = "free_x",
    title = "Gemischte Statistiken (force_facet = TRUE)"
  )
  
  return(plots)
}

# 
# # Test der Funktion ---------------------------------------------------------
# 
#   library(epiR)
#   library(stp25tools2)
#   # Test-Daten
#   test1 <- epi.tests(c(670, 202, 74, 640))
#   test2 <- epi.tests(c(650, 202, 74, 660))
#   
#   data <- Tbll_xtabs_2x2(
#     Histologie = test1,
#     Stanzbiopsie = test2,
#     select = c("se", "sp", "lr.pos", "pv.pos")
#   )
#   data
#   
#   # Verschiedene Szenarien testen
#   
#   # 1. Kompatible Statistiken (nur Prozent)
#  # data_percent <- data[data$statistic %in% c("Sensitivity", "Specificity"), ]
#   forest_plot_epi(data, facet_formula = ~ statistic)
# 
#   
#   # # 2. Inkompatible Statistiken (automatisch ohne Facet)
#   # p2 <- forest_plot_epi(data, facet_formula = ~ statistic)
#   # print(p2)
#   # 
#   # # 3. Force facet (mit Warnung)
#   # p3 <- forest_plot_epi(data, facet_formula = ~ statistic, force_facet = TRUE, scales = "free_x")
#   # print(p3)
#   # 
#   # # 4. Ohne Facet (Standard)
#   # p4 <- forest_plot_epi(data)
#   # print(p4)
#   # 
#   # # 5. Nur Likelihood Ratios
#   # data_lr <- data[data$statistic %in% c("Positive likelihood ratio"), ]
#   # p5 <- forest_plot_epi(data_lr, facet_formula = ~ statistic, show_reference = FALSE)
