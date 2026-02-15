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
#' # Daten laden
#' data <- structure(
#'   list(
#'     Sex = structure(
#'       c(1L, 1L, 1L, 2L, 2L, 2L),
#'       levels = c("male", "female"),
#'       class = "factor"
#'     ),
#'     variable = structure(
#'       c(1L, 2L, 3L, 1L, 2L, 3L),
#'       levels = c("Adipositas", "Bewegungsmangel", "Nikotinabusus"),
#'       class = "factor"
#'     ),
#'     event = c("bösartig", "bösartig", "bösartig", "bösartig", "bösartig", "bösartig"),
#'     event_count = c(35, 24, 29, 42, 26, 36),
#'     total = c(43, 43, 43, 50, 50, 50),
#'     praevalenz = c(0.8139535, 0.5581395, 0.6744186, 0.84, 0.52, 0.72),
#'     clopper.pearson_low = c(0.6659855, 0.3987539, 0.5145602, 0.7088737, 0.3741519, 0.5750946),
#'     clopper.pearson_upr = c(0.9160876, 0.7092188, 0.8092372, 0.9282992, 0.6633949, 0.8376894)
#'   ),
#'   row.names = c(1L, 3L, 5L, 2L, 4L, 6L),
#'   class = "data.frame"
#' )
#'
#' # 1. Gruppiert nach Geschlecht mit Standardfarben
#' gg_praevalenz(data, col = Sex)
#'
#' # 2. Gruppiert nach Geschlecht mit benutzerdefinierten Farben
#' gg_praevalenz(data, col = Sex, color = c("darkblue", "darkred"))
#'
#' # 3. Ohne Gruppierung mit fester Farbe
#' gg_praevalenz(data, color = "steelblue")
#'
#' # 4. Mit angepasstem Versatz und Brewer-Palette
#' gg_praevalenz(data, col = Sex, dodge_width = 0.7, color = "Set2")
#'
#' # 5. Ohne Legende und mit angepassten Größen
#' gg_praevalenz(data, col = Sex, show_legend = FALSE, 
#'               point_size = 4, errorbar_size = 1.5)
#'
#' # 6. Mit manuellen aestetics
#' gg_praevalenz(data, x = praevalenz, y = variable, 
#'               col = Sex, xmin = clopper.pearson_low, 
#'               xmax = clopper.pearson_upr)
#' }
#'
#' @import ggplot2
#' @importFrom rlang enquo enquos as_name
#' @importFrom scales percent_format
#' @export
gg_praevalenz <- function(data,
                          ...,
                          main = "Prävalenz nach Risikofaktor",
                          digits = 0,
                          xlab = NULL,
                          color = NULL,
                          dodge_width = 0.5,
                          show_legend = TRUE,
                          legend_title = NULL,
                          point_size = 3,
                          errorbar_size = 1,
                          text_size = 3.5) {
  
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
  
  if ("x" %in% nmsdots) {
    mapping$x <- dots$x
  }
  else if ("praevalenz" %in% nmsdta) {
    mapping$x <- quo(praevalenz)
  } 
  else if ("est" %in% nmsdta) {
    mapping$x <- quo(est)
  }
  else {
    stop("Keine Prävalenz-Spalte gefunden. Bitte 'x' Mapping angeben.")
  }
  
  if ("y" %in% nmsdots) {
    mapping$y <- dots$y
  }
  else if ("variable" %in% nmsdta) {
    mapping$y <- quo(variable)
  } 
  else if ("parameter" %in% nmsdta) {
    mapping$y <- quo(parameter)
  }else {
    stop("Keine y-Achsen-Spalte gefunden. Bitte 'y' Mapping angeben.")
  }
  
  # Wichtig: col wird jetzt über das Mapping gesetzt
  if ("col" %in% nmsdots) {
    mapping$col <- dots$col
    has_color_mapping <- TRUE
  } else {
    mapping$col <- NULL
    has_color_mapping <- FALSE
  }
  
  # CI-Mappings
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
  
  # Create base plot
  if (!has_color_mapping) {
    p <- ggplot(data, aes(x = !!mapping$x, y = !!mapping$y))
  } else {
    p <- ggplot(data, aes(
      x = !!mapping$x,
      y = !!mapping$y,
      color = !!mapping$col,
      group = !!mapping$col   # Wichtig für position_dodge
    ))
  }
  
  # Positionierung für versetzte Darstellung
  dodge_pos <- position_dodge(width = dodge_width)
  
  # Basiselemente des Plots
  p <- p + 
    geom_vline(
      xintercept = 0.5,
      linetype = "dashed",
      color = "gray70",
      alpha = 0.7
    )
  
  # Punkte hinzufügen (mit oder ohne Farbe)
  if (has_color_mapping) {
    p <- p + geom_point(
      aes(color = !!mapping$col),
      size = point_size,
      position = dodge_pos
    )
  } else {
    p <- p + geom_point(
      size = point_size,
      position = position_nudge(x = 0),
      color = if (!is.null(color) && length(color) == 1) color else "steelblue"
    )
  }
  
  # Fehlerbalken hinzufügen (mit oder ohne Farbe)
  if (has_color_mapping) {
    p <- p + geom_errorbarh(
      aes(
        xmin = !!mapping$xmin,
        xmax = !!mapping$xmax,
        color = !!mapping$col
      ),
      height = 0.2,
      size = errorbar_size,
      position = dodge_pos,
      show.legend = FALSE  # Farbe nur in Punkten zeigen
    )
  } else {
    p <- p + geom_errorbarh(
      aes(
        xmin = !!mapping$xmin,
        xmax = !!mapping$xmax
      ),
      height = 0.2,
      size = errorbar_size,
      position = position_nudge(x = 0),
      color = if (!is.null(color) && length(color) == 1) color else "steelblue"
    )
  }
  
  # Prozentwerte als Text hinzufügen
  if (has_color_mapping) {
    p <- p + geom_text(
      aes(
        label = paste0(round(!!mapping$x * 100, digits = digits), "%"),
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
        label = paste0(round(!!mapping$x * 100, digits = digits), "%"),
        x = !!mapping$x
      ),
      size = text_size,
     # position = position_nudge(y = 0.2),
      position = dodge_pos,
      
      vjust = -0.6,
      show.legend = FALSE
    )
  }
  
  # Skalierung und Design
  p <- p +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      expand = expansion(mult = c(0.02, 0.05))  # Weniger Padding
    ) +
    labs(
      title = main,
      x = xlab,
      y = NULL,
      color = legend_title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
     axis.text.y = element_text(size = 12, face = "bold"),
       panel.grid.minor = element_blank(),
      legend.position = if (show_legend) "top" else "none"
    )
  
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

#data |> gg_praevalenz() + facet_grid(~statistic )

# 
# DF <- data.frame(
#   Sex = gl(2, 50, labels = c("male", "female")),
#   Adipositas = factor(rbinom(n = 100, 1, prob = .2), 0:1, c("bösartig", "gutartig")),
#   Bewegungsmangel = factor(rbinom(n = 100, 1, prob = .4), 0:1, c("bösartig", "gutartig")),
#   Nikotinabusus = factor(rbinom(n = 100, 1, prob = .3), 0:1, c("bösartig", "gutartig"))
# ) 



# DF <- data.frame(
#   Sex = gl(2, 50, labels = c("male", "female")),
#   Adipositas = factor(rbinom(n = 100, 1, prob = .2), 0:1, c("bösartig", "gutartig")),
#   Bewegungsmangel = factor(rbinom(n = 100, 1, prob = .4), 0:1, c("bösartig", "gutartig")),
#   Nikotinabusus = factor(rbinom(n = 100, 1, prob = .3), 0:1, c("bösartig", "gutartig"))
# ) 
# DF <- DF[-c(1:7),]
# # Formatierte Ausgabe
# DF |> Summarise(
#   Adipositas,
#   Bewegungsmangel,
#   Nikotinabusus,
#   #  by=~sex,
#   key = "Risikofaktor",
#   fun = function(x) Praevalenz(x)
# )
# 
# # Risikofaktor Anteil Praevalenz (%) Clopper-Pearson CI
# # 1      Adipositas  82/93           88.2       [79.8, 93.9]
# # 2 Bewegungsmangel  57/93           61.3       [50.6, 71.2]
# # 3   Nikotinabusus  70/93           75.3       [65.2, 83.6]
# 
# # Numerische Ausgabe
# DF |> Summarise(
#   Adipositas,
#   Bewegungsmangel,
#   Nikotinabusus,
#   key = "Risikofaktor",
#   fun = praevalenz
# )
# # Risikofaktor    event event_count total praevalenz clopper.pearson_low
# # 1      Adipositas bösartig          82    93  0.8817204           0.7982136
# # 2 Bewegungsmangel bösartig          57    93  0.6129032           0.5062386
# # 3   Nikotinabusus bösartig          70    93  0.7526882           0.6523714
# # clopper.pearson_upr
# # 1           0.9394522
# # 2           0.7121691
# # 3           0.8363151
# 
# 
# # 
# DF  |> Tbll_praevalenz(
#   Adipositas,
#   Bewegungsmangel,
#   Nikotinabusus,
#   key = "Risikofaktor"
# ) |>
#   Output() |> # Ausgabe der Tabelle 
#   gg_praevalenz(, color = "steelblue") # erstellen der Grafik
# 
# # Tab 1: Prävalenz 
# # Risikofaktor Anteil Praevalenz.... Clopper.Pearson.CI
# # 1      Adipositas  73/93           78.5       [68.8, 86.3]
# # 2 Bewegungsmangel  54/93           58.1       [47.4, 68.2]
# # 3   Nikotinabusus  68/93           73.1       [62.9, 81.8]
# # 
# # 95 % CI 
# 
# 
# DF  |> 
#   Summarise(
#     Adipositas + Bewegungsmangel + Nikotinabusus ~ Sex, 
#     fun = praevalenz
#   )  -> data
# 
# 
# # Beispiele für die Verwendung:
# 
# # 1. Gruppiert nach Sex mit Standardfarben
# gg_praevalenz(data, col = Sex)