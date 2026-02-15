#' Enhanced Plot Function for 'effects' Objects
#'
#' A wrapper around \code{effects::plot.eff}, \code{effects::allEffects},
#' and \code{effects::predictorEffects} that simplifies graphical customization
#' and layout control for model effect plots.
#'
#' @name plot_effect
#' @description
#' This function provides a convenient interface to create and customize
#' effect plots from regression models. It combines the flexibility of
#' \code{effects::plot.eff()} with the convenience of \code{effects::allEffects()}
#' and \code{effects::predictorEffects()}, and optionally arranges multiple
#' plots using \pkg{cowplot}.
#'
#' @param x A fitted model object or an object of class \code{"efflist"}.
#' @param formula A formula specifying the effects to plot (e.g. \code{~ a + b + c*d}).
#' @param predictor A one-sided formula specifying predictor effects,
#'   as in \code{effects::predictorEffects()}.
#' @param partial.residuals Logical; if \code{TRUE}, partial residuals are added to the plot.
#' @param main Optional main title(s) for the plot(s).
#' @param factor.names Logical; whether to include factor names in strip labels.
#' @param rel_widths,rel_heights Numeric values passed to \code{cowplot::plot_grid()}
#'   to control relative plot dimensions.
#' @param ylab,xlab Character; axis labels for the plot.
#' @param labels Optional named vector of labels used for axes and legends.
#' @param cex Overall text size multiplier.
#' @param axis.padding Numeric; additional space around the plot axes (see
#'   \code{lattice::lattice.options}).
#' @param multiline Logical; whether to display multiple lines in each panel
#'   for factors with multiple levels.
#' @param x.var Integer or character; specifies the x-variable for interaction terms.
#' @param space Character string indicating legend placement (e.g., \code{"right"}).
#' @param columns Number of columns in the legend layout.
#' @param xlevels List specifying the number or values of levels for numeric predictors,
#'   e.g. \code{list(x1 = c(2, 4.5, 7), x2 = 4)}.
#' @param select,remove Character vector; variables to include or exclude from plotting.
#' @param order Character vector specifying the order of plotted predictors.
#' @param plot Logical; if \code{TRUE}, produces a plot; if \code{FALSE}, returns the
#'   underlying plot object(s).
#' @param rug Logical; whether to add rug plots for observed data.
#' @param nrow,ncol Number of rows and columns in the \pkg{cowplot} layout.
#' @param lty,lty.factor Line type(s) for effect lines.
#' @param axes List of axis specifications (created automatically if omitted).
#' @param key.args List of arguments controlling the plot legend
#'   (e.g., position, number of columns, size).
#' @param par.settings List of lattice graphical settings
#'   (see \code{\link[lattice]{trellis.par.set}}).
#' @param bottom.padding,top.padding,left.padding,right.padding Numeric values
#'   controlling lattice panel margins.
#' @param ... Additional arguments passed to \code{effects::plot.eff()} or
#'   \code{plot2.efflist()}.
#'
#' @return A lattice effect plot, a \pkg{cowplot} object, or a list of plots.
#'
#' @seealso
#'   \code{\link[effects]{plot.eff}},
#'   \code{\link[effects]{allEffects}},
#'   \code{\link[effects]{predictorEffects}},
#'   \code{\link[cowplot]{plot_grid}}.
#' @note Documentation created with assistance from ChatGPT.
#' @examples
#' require(dplyr)
#' 
#' mtcars2 <- mtcars |>
#'   mutate(
#'     vs   = factor(vs, labels = c("V-shaped", "straight")),
#'     am   = factor(am, labels = c("automatic", "manual")),
#'     cyl  = ordered(cyl),
#'     gear = ordered(gear),
#'     carb = ordered(carb),
#'     mpg =mpd * 0.425143707,
#'     wt = wt *0.4535923744953
#'   ) |> Label(
#'     mpg  = "Fuel consumption (km/l)",
#'     cyl  = "Number of cylinders",
#'     disp = "Displacement (cu.in.)",
#'     hp   = "Gross horsepower",
#'     drat = "Rear axle ratio",
#'     wt   = "Weight (kg)",
#'     qsec = "1/4 mile time",
#'     vs   = "Engine",
#'     am   = "Transmission",
#'     gear = "Number of forward gears",
#'     carb = "Number of carburetors"
#'   )
#' 
#' fit <- lm(mpg ~ hp * wt + vs + am * cyl, data = mtcars2)
#' 
#' # Example using allEffects
#' plot_effect(
#'   fit,
#'   labels = get_label(mtcars2),
#'   main = letters[1:3],
#'   space = "right",
#'   columns = 1,
#'   rel_widths = c(3, 4),
#'   rel_heights = c(5, 6)
#' )
#' 
#' # Example using specific effect
#' plot_effect(fit, ~ cyl * am)
#' 
#' # Example using predictor effects
#' plot_effect(fit, predictor = ~ am * cyl)
#' 
#' # Comparison: flexplot visualization
#' # flexplot::visualize(fit, "model", mpg ~ cyl | am)
#'
#' @export
#' @importFrom effects allEffects effect predictorEffects
#' @importFrom cowplot plot_grid
#' @importFrom purrr map
#' @importFrom lattice lattice.getOption lattice.options
#' 
plot_effect <-
  function(x,
           formula = NULL,
           partial.residuals = FALSE,
           predictor =NULL,
           ...
  ) {
    rslt<- list()
    term <- list()
    
    if (!is.null(predictor))
      return(plot_allEffects(x, 
                             predictor, 
                             ..., 
                             partial.residuals = partial.residuals))
    
    if (is.null(formula)) {
      rslt <- effects::allEffects(x,
                                  partial.residuals = partial.residuals)
    }
    else {
      if (inherits(formula, "formula")) {
        trm <- gsub(" ", "", strsplit(as.character(formula), "\\+")[[2L]])
        for (i in trm) {
          term[i] <- strsplit(i, "\\*")
        }
      } else if (!is.character(formula)) {
        stop("Nur Formulas oder Character sind erlaubt!")
      }
      if (length(term) == 1) {
        
        rslt <- list( 
          term =
            effects::effect(term = term[[1]],  
                            mod = x,
                            partial.residuals = partial.residuals)
        )
        
      } else{
        rslt <- list()
        for (i in seq_along(term)) {
          rslt[[names(term)[i]]] <-
            effects::effect(term = term[[i]], 
                            mod = x,
                            partial.residuals = partial.residuals)
        }
        rslt
      }
    }
    
    plot2.efflist(rslt, ...)
  }

#' @rdname plot_effect
#' 
# @param xlevels effects::effect the number of levels for any focal numeric predicto  xlevels=list(x1=c(2, 4.5, 7), x2=4)
# @param predictor  formula.  ~ ., a predictor effects::predictorEffects
plot_allEffects <- function(x,
                            predictor = NULL,
                            main = NULL,
                            factor.names = FALSE,
                            rel_widths = 1,
                            rel_heights = 1,
                            ylab = NULL,
                            xlab = NULL,
                            labels = NULL,
                            cex = 1.1,
                            axis.padding = .4,
                            multiline = NULL,
                            x.var = 1,
                            space = "right",
                            columns = 1,
                            xlevels = NULL,
                            select = NULL, remove =NULL,
                            order = NULL,
                            ...) {

  if (inherits(x, "efflist"))
    plot2(
      x,
      main = main,
      factor.names = factor.names,
      rel_widths = rel_widths,
      rel_heights = rel_heights,
      ylab = ylab,
      xlab = xlab,
      labels = labels,
      cex = cex,
      axis.padding = axis.padding,
      multiline = multiline,
      x.var = x.var,
      space = space,
      columns = columns,
      select = select, remove = remove,
      order = order,
      ...
    )
  else if( is.null(predictor))
    plot2(
      effects::allEffects(x, xlevels = xlevels),
      main = main,
      factor.names = factor.names,
      rel_widths = rel_widths,
      rel_heights = rel_heights,
      ylab = ylab,
      xlab = xlab,
      labels = labels,
      cex = cex,
      axis.padding = axis.padding,
      multiline = multiline,
      x.var = x.var,
      space = space,
      columns = columns,
      select = select, remove = remove,
      order = order,
      ...
    )
  else if( inherits(predictor, "formula") )
  plot2(
    effects::predictorEffects(x, predictor, xlevels = xlevels),
    main = main,
    factor.names = factor.names,
    rel_widths = rel_widths,
    rel_heights = rel_heights,
    ylab = ylab,
    xlab = xlab,
    labels = labels,
    cex = cex,
    axis.padding = axis.padding,
    multiline = multiline,
    x.var = x.var,
    space = space,
    columns = columns,
    select = select, remove = remove,
    order = order,
    ...
  )
  else return(class(x))
}


#' @rdname plot_effect
plot2 <- function(...) {
  UseMethod("plot2")
}



#' @rdname plot_effect
plot2.default <- function(...) {
  plot(...)
}


#' @rdname plot_effect
#' 
plot2.efflist <-
  function (x,
            main = NULL,
            factor.names = FALSE,
            nrow = NULL,
            ncol = NULL,
            rel_widths = 1,
            rel_heights = 1,
            ylab = NULL,xlab = NULL,labels = NULL,
            xlim = NULL,
            ticks = NULL, y.ticks = if(is.list(ticks)) ticks else list(at=ticks),
            x.ticks = NULL,
            
            cex = 1.1,
            cex.x = cex * .8,
            cex.y = cex * .8,
            cex.xlab = cex,
            cex.ylab = cex,
            cex.strip = cex * 1.2,
            cex.title = cex,
            cex.key = cex * 0.75,
            cex.points = cex,
            lty = 1,
            lty.factor = 0, 
            multiline = NULL,
            x.var = 1,
            space = "right",
            columns = 1,
            axes = NULL,
            axis.padding = .4,
            plot = TRUE,
            rug = FALSE,
            layout = NULL,
            key.args = list(
              space = space,
              columns = columns,
              cex.title = cex.title,
              cex = cex.key
            ),
            bottom.padding = 0,
            top.padding = 0,
            left.padding = .4,
            right.padding = 1,
            par.strip.text = list(),
            #https://stackoverflow.com/questions/13026196/how-to-nicely-rescale-lattice-figures
            
            #  par.settings = ggplot2like(),
            par.settings = list(
              add.text      = list(cex = cex.strip),
              par.zlab.text = list(cex = cex.xlab),
              par.ylab.text = list(cex = cex.ylab),
              par.xlab.text = list(cex = cex.xlab),
              layout.heights =
                list(bottom.padding = bottom.padding,
                     top.padding    = top.padding),
              layout.widths =
                list(left.padding =  left.padding,
                     right.padding =  right.padding)
              #   axis.line = list(col="gray"),
              #   axis.text = list(col= "red"),
              #   strip.background =list( col = 'grey80')
            ),
            select = NULL, remove =NULL,
            order = NULL,
            ...)
  {
    plotlist <- list()
    param <- purrr::map(x, \(xfit) names(xfit$variables))
    x <- x[!duplicated(param)]
    effects_all <- effects <- gsub(":", "*", names(x))
    
    if(!is.null(remove)){
      if(is.character(remove))
        effects <- setdiff(effects, remove)
      else stop( "Bei remove sind nur die Parameter als Character-Namen erlaubt!")
      cat("\nremove: ", remove, "\n")
      pos <- which(effects_all %in% effects)
      x <- x[ pos]
     
    }
    if(!is.null(select)){
      if(is.character(select))
        effects <- intersect(effects,select )
      else stop( "Bei select sind nur die Parameter als Character-Namen erlaubt!")
      
      cat("\nselect: ", effects, "\n")
      pos <- which(effects_all %in% effects)
      x <- x[ pos]
    }
    if(!is.null(order)){
      if(is.numeric(order))  effects <- effects[order]
      else stop( "Bei order sind nur die Parameter als Numeric-Position erlaubt!")
    }
 
    eff_names <-
      c(x[[1]]$response, unlist(strsplit(effects, "\\*")))
    
    # xlab und ylab zum Vektor label zusammenbauen
    if (!is.null(labels)) {
      # wenn zb cut(x, 5) kommt wird das leehrzeichen von effect verworfen
      names(labels) <- gsub("\\s", "", names(labels))
      labels <- labels[eff_names]
      unlabl <- which(is.na(labels))
      labels[unlabl] <- eff_names[unlabl]
      names(labels)[unlabl] <- eff_names[unlabl]
      
         
    }
    else{
      # ylab und xlab aufdröseln
      labels <- eff_names
      if (is.list(xlab))
        xlab <- unlist(xlab)
      
      names(labels) <- eff_names
      if (!is.null(ylab)) {
        labels[1] <- ylab
      }
      
      if (!is.null(xlab)) {
        xnames <- names(xlab)
        if (length(xlab) > 1) {
          if (is.null(xnames))
            stop("Die xlabs muessen namen haben!")
          else if (any(xnames == ""))  {
            print(xlab)
            stop("Alle xlabs muessen namen haben!")
          }
        }
        for (i in xnames)
          labels[i] <- xlab[i]
      }
    }
    

    reset_axis <-
      lattice::lattice.getOption("axis.padding")$numeric
    lattice::lattice.options(axis.padding = list(numeric = axis.padding))
    
    # bei Modellen mit log-transformierten werten müssen 
    # die labels bereinigt werden
    if(any(grepl("\\(", names(labels)))) {
      names(labels) <- gsub("^[^(]*\\(\\s*([^,)]+)\\s*[,)].*", "\\1", names(labels))
    }

    
    for (i in seq_along(effects)) {
    
       is_fctr <- unlist(purrr::map(x[[i]]$variables, \(x) x$is.factor))
       effects_i <- unlist(strsplit(effects[i], "\\*"))
  
       if(any(grepl("\\(", effects_i))){ 
         effects_i <- gsub("^[^(]*\\(\\s*([^,)]+)\\s*[,)].*", "\\1", effects_i)
         }
   
      # lty
      if (all(is_fctr)) lty2 <- lty.factor 
       else lty2 <- lty
      # Multiline
      if (is.null(multiline)) {
        if (any(is_fctr))
          multiline_i <- FALSE
        else
          multiline_i <- TRUE
      }
      else
        multiline_i <- multiline
      
      # Axes: make a list for the axix
      axes_i <- list(y = list(lab = labels[1],
                              cex = cex.y),
                     x = list(cex = cex.x))
      
      for (j in effects_i)
        axes_i$x[[j]]$lab <- labels[[j]]
        

      if (length(effects_i) == 2 & multiline_i) 
        key.args$title <- labels[[effects_i[-x.var]]]
      
      if (!is.null(xlim)) {
         if (is.list(xlim))
           for (j in effects_i)
             axes_i$x[[j]]$lim <- xlim[[j]]
         else {
           for (j in effects_i)
             axes_i$x[[j]]$lim <- xlim 
           #  warning("xlim: Hier sollte eine Liste mit Namen 
           #  uebergeben werden. list(sex = c(1 ,2) } ")
           }
          
      }
       if (!is.null(x.ticks)) {
         if (is.list(x.ticks))
           for (j in effects_i) axes_i$x[[j]]$ticks <- x.ticks[[j]]
         else stop ("xticks: hier muss eine Liste mit Namen uebergeben werden. list(x = c(1,2)  ")
       }
      
      # layout
      layout.i <- NULL
       if (!is.null(layout)) {
         is_lay <- effects_i %in% names(layout)
         if (!multiline_i & any(is_lay))
           layout.i <- layout[[
             effects_i[
               which(is_lay)][1L]]]
       }  
      
      # plot der einzelnen Effekte und uebergabe an eine liste
      plotlist[[effects[i]]]  <-
        update(
          effects:::plot.eff(
            x[[i]],
            factor.names = factor.names,
            main = "",
            rug = rug,
            lty = lty2,
            ticks =y.ticks,
            axes = axes_i,
            multiline = multiline_i,
            key.args = key.args,
            x.var = x.var,
            layout =  layout.i,
            ...
          ),
          par.settings = par.settings,
          par.strip.text = par.strip.text
        )
    }
    
    lattice::lattice.options(
      axis.padding = list(numeric = reset_axis))
    
    if (plot)
      cowplot::plot_grid(
        plotlist = plotlist,
        nrow = nrow,
        ncol = ncol,
        rel_widths = rel_widths,
        rel_heights  = rel_heights,
        labels = main,
        hjust = 0 
      )
    else
      plotlist
  }



