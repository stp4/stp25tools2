#' Lattice-Matrix-Plot
#'
#'
#' @param ... Variablen und Daten
#' @param ylab,xlab default ist ""
#' @param type c("histogram", "boxplot")
#' @param par.settings sefault ist  par.settings = bw_theme((farbe()))
#' @param include.n noch nicht implemeniert
#' @param cex.main,cex.scales Ueberschrift und Scales
#' @param ncol an grid.arrange
#' @param grid.arrange  logical Arrange multiple grobs on a page
#' @importFrom gridExtra grid.arrange
#' @importFrom lattice histogram xyplot barchart bwplot dotplot stripplot
#' @importFrom latticeExtra marginal.plot
#'
#'
#' @return lattice Plot
#' @export
#'
#' @examples
#'
#' enviro <- lattice::environmental
#'
#' enviro <- transform(
#'   enviro,
#'   smell = cut(
#'     enviro$ozone,
#'     breaks = c(0, 30, 50, Inf),
#'     labels = c("ok", "hmmm", "yuck"),
#'     ordered = TRUE
#'   ),
#'   is.windy = factor(
#'     enviro$wind > 10,
#'     levels = c(TRUE, FALSE),
#'     labels = c("windy", "calm")
#'   )
#' )
#' #head(enviro)
#'
#' auto_plot(enviro, ozone, radiation, is.windy, wind, smell, by =  ~ temperature)
#'
#' auto_plot(ozone ~ radiation + is.windy + wind + smell, enviro)
#'
#' auto_plot(enviro, ozone[box], radiation[hist], is.windy, wind, smell, temperature)
#'
#' auto_plot(enviro, ozone, radiation, is.windy, wind, by =  ~ smell)
#'
#' # das ist eine Liste mit plots
#' p1 <-
#'   auto_plot(enviro,
#'             ozone,
#'             radiation,
#'             is.windy,
#'             wind,
#'             smell,
#'             temperature,
#'             grid.arrange = FALSE)
#'
#'
#' for (i in seq_along(p1)) {
#'   cat("\n", names(p1[i]))
#'   print(p1[[i]])
#'   #  SavePlot(  names(p1[i]) , w=3.6, h=2.9)
#' }
#'
#'
auto_plot <- function(...) {
  UseMethod("auto_plot")
}


#' @rdname auto_plot
#' @export
auto_plot.lm<- function(x, ...){
  auto_plot(formula(terms(x)), x$model, ...)
}


#' @rdname auto_plot
#' @export
auto_plot.default <- function(...,
                      origin = 0,
                      xlab = NULL,
                      ylab = NULL,
                      type = c("p", "r"),
                      cex.main = 1,
                      cex.scales = 0.75,
                      ncol = NULL,
                      default.scales = list(abbreviate = TRUE,
                                            minlength = 5,
                                            cex = cex.scales),
                      # relation = "free",
                      # rot = 30,tick.number = 3,
                      # y = list(draw = FALSE)
                      layout = NULL,
                      lattice.options = list(layout.heights = list(
                        axis.xlab.padding = list(x = 0),
                        xlab.key.padding = list(x = 0)
                      )),

                      col = farbe(),
                      col.bar = "gray50",

                      par.settings =  bw_theme(col=col, col.bar =col.bar),
                    #  stack = FALSE,
                      include.n = TRUE,
                      par.strip.text = NULL,
                      wrap.main=NULL,



                      grid.arrange = TRUE, # Arrange multiple grobs on a page
                      levels.logical = c(TRUE, FALSE),
                      labels.logical = levels.logical,
                      include.label = TRUE,

                      include.percent = FALSE,
                      # multi_barplot
                      include.reorder = FALSE,
                      include.reorder.last = NULL,

                      main=""

                      ) {
  X <- prepare_data(..., is_for_plot = TRUE)

  if (all(sapply(X$data[X$measure.vars], class) == "logical")){

    return(
      multi_barplot(
        ...,
        reorder = include.reorder,
        last = include.reorder.last,
        main = main,
        #   ylab = ylab,
        include.percent = include.percent,
        origin = origin,
        xlab =  if (is.null(xlab)) {if (include.percent) "Percent" else "Count"} else xlab
      ))
  }


  if (!is.null(wrap.main))
    X$row_name <- wrap_string(X$row_name, wrap.main)

  if (length(xlab) == 1)
    xlab <- rep(xlab, length(X$measure.vars))

  if (length(ylab) == 1)
    ylab <- rep(ylab, length(X$measure.vars))


  if (is.null(X$group.vars) |
      (length(X$group.vars) == 1) |
      (length(X$measure.vars) > length(X$group.vars))) {
  #  print(X$measure)

    res <- multi_av_plot(
      X$data,
      X$measure.vars,
      X$group.vars,
      X$row_name,
      X$col_name,
      X$group.class,
      X$measure,
      #reorder ,plot.points, ref, cut,
      origin,
      xlab,
      ylab,
      type,
      #subset,as.table,subscripts,
      default.scales,
      lattice.options,
      par.settings,
      include.n,
      cex.main,
      layout,
      par.strip.text,
      include.percent,
      levels.logical,
      labels.logical#,
     # stack = stack
    )
  }
  else{
    #  cat("\nmulti_uv_plot\n")
    if (is.null(xlab))
      xlab <-  rep(X$col_name[1], length(X$measure.vars))
    res <- multi_uv_plot(
      X$data,
      X$group.vars,
      X$measure.vars,
      X$col_name,
      X$row_name,
      X$measure,
      X$group.class,
      origin,
      xlab,
      ylab,
      type,
      default.scales,
      lattice.options,
      par.settings,
      include.n,
      cex.main,
      layout,par.strip.text,
      include.percent,
      levels.logical,
      labels.logical
    )
  }


  if (grid.arrange) {
    if (length(res) > 0) {
      if (is.null(ncol))
        ncol <- ifelse(length(res) < 4, length(res),
                       ifelse(length(res) < 10, 3, 4))
      gridExtra::grid.arrange(grobs = res, ncol = ncol)
    }
    else {
      plot(1)
    }
  }
  else {
    names(res) <- X$measure.vars
    res
  }

}


multi_av_plot <- function(data,
                          measure.vars,
                          group.vars,
                          row_name,
                          col_name,
                          group.class,
                          measure,
                          origin,
                          xlab,
                          ylab,
                          type,
                          default.scales,
                          lattice.options,
                          par.settings,
                          include.n, cex.main,layout,
                          par.strip.text,
                          include.percent,
                          levels.logical,
                          labels.logical,
                          ...) {
 # cat("\n  multi_av_plot\n")
 # print(group.vars)
  z <-  group.vars[1]
  if ( !is.null(group.vars)) if(group.class[1] == "logical") {
    data[[z]] <- factor(data[[z]], levels.logical, labels.logical)
    group.class[1] <- "factor"
  }
  res <- list()
  stack <- FALSE

 # print(z)
  for (i in seq.int(length(measure.vars))) {
    y <- measure.vars[i]


    if (is.null(z)) {

      if (measure[i] %in% c("numeric", "hist") ) {
        res[[i]] <-
          lattice::histogram(
            formula(paste("~", y)),
            data,
            type = "count",
            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else if (measure[i] %in% c("factor", "bar", "logical")) {

        if( measure[i] == "logical" ) {
          data[[y]] <- factor(data[[y]], levels.logical, labels.logical )
          stack <- TRUE # not used  !
          }

        tab <-  xtabs(formula(paste("~", y)), data)

        if (include.percent) {
          tab <- as.data.frame(prop.table(tab)*100)
          }
        else tab <- as.data.frame(tab)

        res[[i]] <-
          lattice::barchart(
            formula(paste("Freq~", y)),
            data = tab,
            main = list(label=row_name[i], cex=cex.main),
            stack = FALSE,
            origin = origin,
            horizontal = FALSE,
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            layout=layout,
            par.strip.text=par.strip.text,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else if ( measure[i] == "box"){
        res[[i]] <-
          lattice::bwplot(
            formula(paste("~", y)),
            data,

            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else if ( measure[i] == "pie"){
        cat("\npiechart 1\n")
        stop("Neee sich hasse Tortendiagreamme!")
      }
      else if ( measure[i] == "dot"){
        tab <- as.data.frame(xtabs(formula(paste("~", y)), data))


        if(stack)  stop( "Die Funktion stack habe ich hier noch nicht fertig!!!!")
        res[[i]] <-
          lattice::dotplot(
            formula(paste("Freq~", y)),
            data = tab,

            main = list(label=row_name[i], cex=cex.main),
            stack = stack,
            origin = origin,
            horizontal = FALSE,
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else{}
    }
    else{

      if (group.class[1] == "factor") {

        if (measure[i] %in% c("numeric", "box")) {

          res[[i]] <-
            lattice::bwplot(
              formula(paste(y, "~", z)),
              data,

              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if ( measure[i] == "hist" ) {
          res[[i]] <-
            lattice::histogram(
              formula(paste("~", y, "|", z)),
              data,
              ylab = ylab[i],
              xlab = xlab[i],
              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings
            )
        }
        else if ( measure[i] %in% c("factor", "bar","logical")) {
          if( measure[i] == "logical" ){
            data[[y]] <- factor(data[[y]], levels.logical, labels.logical )
            stack <- TRUE
          }

          tab <- xtabs(formula(paste("~", y, "+", z)), data)

          if (include.percent)
            tab <- as.data.frame(prop.table(tab, 2) * 100)
          else
            tab <- as.data.frame(tab)
         # Logical
          if (stack) {
            names(tab)[1] <- "my_group"
            res[[i]] <-
              lattice::barchart(
                formula(paste("Freq ~ ", z)),
                data = tab,
                groups = my_group,
                col = c("gray40", "gray90"),
                main = list(label = row_name[i], cex = cex.main),
                stack = stack,
                origin = origin,
                horizontal = FALSE,
                par.settings = par.settings,
                default.scales = default.scales,
                lattice.options = lattice.options,
                layout = layout,
                par.strip.text = par.strip.text,
                xlab = xlab[i],
                ylab = ylab[i]
              )
          }
          else {
            res[[i]] <-
              lattice::barchart(
                formula(paste("Freq~", y, "|", z)),
                data = tab,
                main = list(label = row_name[i], cex = cex.main),
                stack = FALSE,
                origin = origin,
                horizontal = FALSE,
                par.settings = par.settings,
                default.scales = default.scales,
                lattice.options = lattice.options,
                layout = layout,
                par.strip.text = par.strip.text,
                xlab = xlab[i],
                ylab = ylab[i]
              )
          }
        }
        else if ( measure[i] == "pie"){

          cat("\npiechart 2\n")
          stop("Neee sich hasse Tortendiagreamme!")

        }
        else if ( measure[i] == "dot"){

          res[[i]] <-
            lattice::stripplot(
              formula(paste(y, "~", z)),
              data,

              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              panel = function(x, y, ...) {
                panel.stripplot(x, y, ..., jitter.data = TRUE)
              },
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )

        }
        else if ( measure[i] == "dens" ){
          # noch nicht implementiert
          res[[i]] <-  densityplot(
            formula(paste("~", y)),
            data,
            outer = TRUE,
            subscripts = TRUE,
            groups = data[[ z ]],

            plot.points = FALSE,
            ref = TRUE,
            cut = 0,
            as.table = TRUE,

            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i],
            ...
          )


        }
        else {}
      }
      else{
        if (measure[i] %in% c("numeric", "dot")) {
        res[[i]] <-
            lattice::xyplot(
              formula(paste(y, "~", z)),
              data,
              type = type,
              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if (measure[i] == "factor" | measure[i] == "box") {
          res[[i]] <-
            lattice::bwplot(
              formula(paste(y, "~", z)),
              data,

              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if( measure[i] =="hist") {
          res[[i]] <-
            lattice::histogram(
              formula(paste("~", y, "|", z)),
              data,

              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if ( measure[i] =="bar") {
          tab <-
            as.data.frame(xtabs(formula(paste(
              "~", y, "+", z
            )), data))
          res[[i]] <-
            lattice::barchart(
              formula(paste("Freq~", y, "|", z)),
              data = tab,

              main = list(label=row_name[i], cex=cex.main),
              stack = stack,
              origin = origin,
              horizontal = FALSE,
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              layout=layout,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if ( measure[i] =="pie"){

          cat("\npiechart\n")
          stop("Neee sich hasse Tortendiagreamme!")
        }
        else {}

      }
    }
  }
  res
}


#' Hilfsfunktion
#'
#' Im wesentlichen ist das eine Kopie von oben nur die Formeln sind vertauscht
#' und die auswahl an verschiede Plots ist nicht möglich.
#' @noRd
multi_uv_plot <- function(data,
                          measure.vars,
                          group.vars,
                          row_name,
                          col_name,
                          group.class,
                          measure,
                          origin,
                          xlab,
                          ylab,
                          type,
                          default.scales,
                          lattice.options,

                          par.settings,
                          include.n,
                          cex.main,layout,
                          par.strip.text,
                          include.percent,
                          levels.logical,
                          labels.logical
                          ) {
  z <-  group.vars[1]
  res <- list()


  for (i in seq.int(length(measure.vars))) {
    y <- measure.vars[i]
    ylab <- col_name[1]
    if (group.class[1] == "factor") {
      if (measure[i] == "numeric") {

        res[[i]] <-
          lattice::bwplot(
            formula(paste(z, "~", y)),
            data,

            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab,
            ylab = ylab
          )

      }
      else if (measure[i] %in% c("factor", "bar", "logical")) {

        if( measure[i] == "logical" )
          data[[y]] <- factor(data[[y]], levels.logical, labels.logical)
        # hier fehlt noch logical wie oben
        tab <-
          as.data.frame(xtabs(formula(paste("~", z, "+", y)), data))

          if (include.percent) tab <- as.data.frame(prop.table(tab,2)*100)
          else tab <- as.data.frame(tab)

        res[[i]] <-
          lattice::barchart(
            formula(paste("Freq~", z, "|", y)),
            data = tab,

            main = list(label=row_name[i], cex=cex.main),
            stack = FALSE,
            origin = origin,
            horizontal = FALSE,
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab,
            ylab = ylab
          )
      }
    }
    else{
      if (measure[i] == "numeric") {
        res[[i]] <-
          lattice::xyplot(
            formula(paste(z, "~", y)),
            data,
            type = type,

            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab,
            ylab = ylab
          )
      }
      else if (measure[i] %in% c("factor", "bar", "logical")) {

          if( measure[i] == "logical" ) data[[y]] <- factor(data[[y]], levels.logical, labels.logical )

        res[[i]] <-
          lattice::bwplot(
            formula(paste(z, "~", y)),
            data,

            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab,
            ylab = ylab
          )
      }
    }
  }
  res
}



#' Marginal Plots with Lattice
#'
#' Ein Wrapper um \code{latticeExtra::marginal.plot}, der Daten aus \code{prepare_data()} entgegennimmt
#' und eine komfortable Möglichkeit bietet, marginale Verteilungen von Messvariablen
#' darzustellen. Gruppen können automatisch über numerische Variablen (per \code{cut()})
#' oder Faktoren gebildet werden.
#'
#' @param ... Daten und Formeln, die an \code{prepare_data()} übergeben werden.
#'   Typischerweise eine Formel im Stil \code{y1 + y2 + ... ~ x} oder
#'   \code{y1, y2, ... , by = ~ x}.
#' @param include.label,strip.names Optional, ein Character-Vektor gleicher Länge wie die Zahl der
#'   Messvariablen. Wird verwendet, um die Panel-Strips umzubenennen.
#' @param cut.breaks Falls die Gruppenvariable numerisch ist: Anzahl oder Vektor von
#'   Breaks, die an \code{cut()} übergeben werden.
#' @param cut.labels Optional, Character-Vektor mit Labels für die durch \code{cut()}
#'   erzeugten Gruppen.
#' @param auto.key Liste, wird an \code{latticeExtra::marginal.plot} weitergereicht
#'   und steuert die Legende.
#' @param par.settings Liste mit Lattice-Theming-Optionen (z. B. von \code{bw_theme()}).
#' @param plot.points Logisch oder Zeichenkette, ob Rohdatenpunkte im Plot angezeigt
#'   werden sollen. Wird an \code{panel.densityplot} weitergereicht.
#' @param origin Numerischer Wert, Startpunkt der Histogrammbalken.
#' @param xlab,ylab Achsenbeschriftungen.
#' @param type Plot-Typ, Standard \code{"p"} bei Gruppen, \code{"h"} sonst.
#' @param main Titel des Plots.
#' @param subset Logischer Vektor oder Ausdruck zur Subset-Selektion der Daten.
#' @param as.table Logisch, ob Panels tabellarisch wie Daten gedruckt werden sollen.
#' @param subscripts Logisch, ob Subscripts für Panel-Funktionen übergeben werden.
#' @param default.scales Liste mit Skalierungsoptionen für die Achsen (Relation, Tickanzahl etc.).
#' @param layout Layout-Spezifikation für Panels (z. B. \code{c(nrow, ncol)}).
#' @param lattice.options Weitere Lattice-Optionen, z. B. Abstände von Achsen und Labels.
#'
#' @return Ein Objekt der Klasse \code{"trellis"}, erzeugt durch \code{latticeExtra::marginal.plot}.
#'   Das Objekt kann wie üblich mit \code{print()} oder direkt im Plot ausgegeben werden.
#'
#' @seealso \code{\link[latticeExtra]{marginal.plot}}, \code{\link{xyplot}}, \code{\link{densityplot}}
#'
#' @export
#' @importFrom latticeExtra marginal.plot
#'
#' @examples
#' enviro <- lattice::environmental
#'
#' enviro <- transform(
#'   enviro,
#'   temperature = (temperature - 32) / 1.8,
#'   smell = cut(
#'     ozone,
#'     breaks = c(0, 30, 50, Inf),
#'     labels = c("ok", "hmmm", "yuck"),
#'     ordered_result = TRUE
#'   ),
#'   is.windy = factor(
#'     wind > 10,
#'     levels = c(TRUE, FALSE),
#'     labels = c("windy", "calm")
#'   ),
#'   wind = wind * 0.44704
#' )
#'
#' # Einfaches Beispiel
#' marginal_plot(enviro, ozone + radiation + is.windy + wind + smell ~ temperature)
#'
#' # Mit eigenen Cut-Breaks
#' marginal_plot(enviro, ozone + radiation ~ temperature,
#'               cut.breaks = 4,
#'               cut.labels = c("extrem kalt", "kalt", "warm", "heiss"))
#'
#' # Mit Strip-Namen und eigener Legende
#' marginal_plot(enviro,
#'               ozone, radiation, is.windy, wind, smell,
#'               by = ~ temperature,
#'               strip.names = c("Ozon [ppm]", "Radiation", "is Windy", "Wind speed [m/s]", "Smell"),
#'               main = "Distribution",
#'               cut.breaks = c(-Inf, 20, 25, 30, Inf),
#'               cut.labels = c("bis 20°C", "20°C bis 25°C", "25°C bis 30°C", "über 30°C"),
#'               auto.key = list(lines = TRUE, title = "Temperatur"))
marginal_plot <- function(...,
                          strip.names = NULL,
                          include.label = if(is.null(strip.names)) TRUE else FALSE,
                          cut.breaks = 3,
                          cut.labels = NULL,
                         # type = c("p", if (is.null(groups)) "h"),
                          par.settings = bw_theme(farbe()),
                          auto.key = list(lines = TRUE),
                           # plot.points, ref, cut	->   passed to panel.densityplot
                           plot.points = FALSE, #"jitter",
                          # ref = TRUE,
                         #  cut = 0,
                         # from, to, cut
                         #  Controls range over which density is evaluated.
                         #  Passed on as arguments to density.

                           origin = 0,
                           xlab = NULL,
                           ylab = NULL,
                           type = NULL,
                           main = "marginal distributions",
                           subset = TRUE,
                           as.table = TRUE,
                           subscripts = TRUE,
                           default.scales = list(
                             relation = "free",
                             abbreviate = TRUE,
                             minlength = 5,
                             rot = 0,
                             cex = 0.75,
                            tick.number = 3
                             # y = list(draw = FALSE)
                           ),
                           layout = NULL,
                           lattice.options = list(layout.heights = list(
                             axis.xlab.padding = list(x = 0),
                             xlab.key.padding = list(x = 0)
                           ))) {
  X <- prepare_data(...)
  groups <- NULL
  if (!is.null(X$group.vars)) {
    groups = X$data[[X$group.vars]]
    if (!is.factor(groups))
     # das geht nicht #groups <- equal.count(groups )
     groups <- cut(groups, breaks = cut.breaks, labels = cut.labels)

    if (is.null(type))
      type <- "p"
  } else{
    if (is.null(type))
      type <- "h"
  }


  measure.data <-  X$data[X$measure.vars]

  if (include.label) {
    names(measure.data) <- X$row_name
  }
  else  if (!is.null(strip.names)) {
    if (length(X$measure.vars) == length(strip.names))
      names(measure.data) <- strip.names
    else
      warning("Oje - die strip.names muessen gleich lang wie die measure.vars sein")
  }

#print(measure.data)
  latticeExtra::marginal.plot(
    measure.data,
    data = X$data,
    groups = groups,
    main = main,
    auto.key = auto.key,
    par.settings = par.settings,

    plot.points = plot.points,
  #  ref = ref,
  #  cut = cut,
    origin = origin,
    xlab = xlab,
    ylab = ylab,
    type =  type,

    subset = subset,
    as.table = as.table,
    subscripts = subscripts,
    default.scales = default.scales,
    layout = layout,
    lattice.options = lattice.options

  )

}


#' multi_barplot
#'
#' Adaption von `lattice::barchart()` und Feineinstellung können
#' mit `lattice::update.trellis` erstellt werden.
#'
#' @param ... an Summarise
#' @param reorder,last  an reorder2
#' @param main,ylab,origin,xlab an Lattice
#' @param include.percent summary
#' @return lattice plot
#' @export
#'
#' @examples
#'
#' n <-99
#' DF <- data.frame(
#'   Magazines = rbinom(n, 1,prob=.75),
#'   Comic.books =rbinom(n, 1,prob=.25),
#'   Fiction = rbinom(n, 1,prob=.5),
#'   Sonstiges = rbinom(n, 1,prob=.35)
#' ) |> transform(sex = cut(rnorm(n), 2, c("m", "f")))
#'
#'
#' multi_barplot(  DF, .~ sex, last="Sonstiges")
#'
#'
multi_barplot <-
  function (...,
            reorder = TRUE,
            last = NULL,
            main = NULL,
            ylab = "",
            include.percent = FALSE,
            origin = 0,
            xlab = if (include.percent) "Percent" else "Count",
            xlim = NULL,
            ylim = NULL,
            wrap = TRUE,
            use.level = 1) {
    dat_sum <- Summarise(
        ...,
        fun = function(x) {
          n_tot <- length(x)
          x <- na.omit(x)
          if (is.factor(x))
            x <-  x == levels(x)[use.level]
          if (include.percent)
            mean(x) * 100
          else
            sum(x)
        }
      )
    #print(dat_sum)
    dat_sum <- dat_sum[rev(seq_len(nrow(dat_sum))), ]
    #print(dat_sum)
    if (reorder)
      dat_sum$variable <-
      reorder2(dat_sum$variable, dat_sum$value, last = last)

    if (is.logical(wrap)) {
      if (wrap)
        dat_sum$variable <- wrap_factor(dat_sum$variable, 35)
    } else if (is.numeric(wrap)) {
      dat_sum$variable <- wrap_factor(dat_sum$variable, wrap)
    }

    fm <- "variable ~ value"
    if (ncol(dat_sum) > 2)
      fm <- paste(fm, "|", names(dat_sum)[1])

    if (is.null(xlim) & is.null(ylim)) {
      p <- lattice::barchart(
        formula(fm),
        dat_sum,
        origin = origin,
        xlab = xlab,
        main = main
      )
    } else     if (!is.null(xlim) & is.null(ylim)) {
      p <- lattice::barchart(
        formula(fm),
        dat_sum,
        origin = origin,
        xlab = xlab,
        xlim = xlim,
        main = main
      )
    } else{
      stop("ylim noch nicht implementiert!!")
    }

    return(p)
  }


#
# n <-99
# DF <- data.frame(
#   Magazines = rbinom(n, 1,prob=.2)==1,
#   Comic.books =rbinom(n, 1,prob=.95)==1,
#   Fiction = rbinom(n, 1,prob=.1)==1,
#   Sonstiges = rbinom(n, 1,prob=.35)==1
# ) #|> transform(sex = cut(rnorm(n), 2, c("m", "f")))
#
# #sum(DF$Magazines)
# #table(DF$Magazines)
# #stp25stat2::Tbll_desc(DF)
# multi_barplot(  DF, reorder=TRUE , last="Sonstiges")
#
#
#
# n <-99
# DF <- data.frame(
#   Magazines = rbinom(n, 1,prob=.75),
#   Comic.books =rbinom(n, 1,prob=.25),
#   Fiction = rbinom(n, 1,prob=.5),
#   Sonstiges = rbinom(n, 1,prob=.65)
# ) |> transform(sex = cut(rnorm(n), 2, c("m", "f")))
#
#
# multi_barplot(  DF, .~ sex, last="Sonstiges", include.total=TRUE)




# Exampels ----------------------------------------------------------------

#
#
# enviro <- lattice::environmental
#
# enviro <- transform(
#   enviro,
#   temperature = (temperature - 32) / 1.8,
#   smell = cut(
#     ozone,
#     breaks = c(0, 30, 50, Inf),
#     labels = c("ok", "hmmm", "yuck"),
#     ordered = TRUE
#   ),
#   is.windy = factor(
#     wind > 10,
#     levels = c(TRUE, FALSE),
#     labels = c("windy", "calm")
#   ),
#   wind = wind * 0.44704
# ) |> Label(
#   ozone=  "Ozon [ppm]",
#   radiation= "Radiation",
#   is.windy="is Windy",
#   wind=  "Wind speed [m/s]",
#   smell="Smell")
#
#
# p1 <-  marginal_plot(enviro,
#                 ozone, radiation, is.windy, wind, smell,
#                 by = ~ temperature,
#                 strip.names = c("Ozon [ppm]", "Radiation", "is Windy", "Wind speed [m/s]", "Smell"),
#                 main = "Distribution",
#                 cut.breaks = c(-Inf, 20, 25, 30, Inf),
#                 cut.labels = c("bis 20°C", "20°C bis 25°C", "25°C bis 30°C", "über 30°C"),
#                 auto.key = list(lines = TRUE, title = "Temperatur"))
#
#
# expect_equal(p1$legend$right$args$text,
#              c("bis 20°C","20°C bis 25°C", "25°C bis 30°C", "über 30°C")
# )
#
#  auto_plot(enviro, ozone, radiation, is.windy, wind, smell, by =  ~ temperature)
# #
# # auto_plot(ozone ~ radiation + is.windy + wind + smell, enviro)
# #
# enviro |>
#   auto_plot(ozone[box], radiation[hist], is.windy[pie], wind, temperature,
#             by = ~ smell)

