#' Custom Black-and-White Theme for Lattice Plots
#'
#' Creates a customized black-and-white theme for lattice graphics with sensible defaults
#' that work well for academic publications and presentations.
#'
#' @name set_lattice
#'
#' @param ... Additional theme parameters to override
#'
#' @return A lattice theme object
#' @importFrom utils modifyList
#' @importFrom grDevices grey.colors
#' @importFrom lattice standard.theme trellis.par.set
#' @importFrom grDevices hsv
#' @export
#'
#' @examples
#'
#' library(lattice)
#' axis.padding   <- lattice.getOption("axis.padding")
#' layout.heights <- lattice.getOption("layout.heights")
#' layout.widths  <- lattice.getOption("layout.widths")
#'
#' #' Abstand der inneren Labels in effect::plot() (ist ja ein lattice-plot)
#' lattice.options(axis.padding =list(numeric=0.3))
#'
#' #' Abstand oben, unten, links, rechts
#' lattice.options(
#'   layout.widths =
#'     list(
#'       left.padding=list(x=.4),
#'       right.padding=list(x=1)
#'     )
#'   )
#'
#' #' reset
#' lattice.options(
#'   layout.widths = layout.widths,
#'   layout.heights =layout.heights,
#'   axis.padding =axis.padding
#'   )
#'
#'
#' require(lattice)
#' #lattice::trellis.par.set(standard_theme())
#' lattice::trellis.par.set(bw_theme(farbe()))
#' barchart(yield ~ variety | site, data = barley,
#'          groups = year, layout = c(1,6), stack = TRUE,
#'          auto.key = list(space = "right"),
#'          ylab = "Barley Yield (bushels/acre)",
#'          scales = list(x = list(rot = 45)))
#' #'
#' his <- data.frame(variable = LETTERS[1:5],  value = c(2, 3, 5, 6, 4))
#'
#' barchart(
#'   variable ~ value ,
#'   his,
#'   xlab = "Prozent nicht zutreffend",
#'   origin = 0,
#'   par.settings =
#'     bw_theme(col.bar  =  "#708C98FF")
#' )
#'
#'
#'  #lattice::trellis.par.set(
#'  #  effects::effectsTheme(
#'  #
#'  #    strip.background = list(col = "transparent"),
#'  #    strip.shingle = list(col = "black"),
#'  #    clip = list(strip = "off"),
#'  #    superpose.line = list(lwd = c(2, rep(1, 6))),
#'  #    col = grey.colors(7, start = 0.3, end = 0.9)
#'  #    ))
#'
#'

NULL

#' @rdname set_lattice
#' @param theme mein lattice theme
#' @export
set_lattice <- function(theme = bw_theme()) {
  lattice::trellis.par.set(theme)
  invisible(theme)
}

#' @rdname set_lattice
#' @export
my_theme <- function(
    col = c("#0C5BB0FF","#15983DFF","#EC579AFF","#FA6B09FF","#667840FF"),
    col.bar  = "#708C98FF",
    ...) {
  bw_theme(col = col, col.bar = col.bar,...)
}

#' @rdname set_lattice
#' @export
reset_lattice <- function() {
  # weil beim standart theme die transpazenz fehlt.
  lattice::trellis.par.set(standard_theme())
  invisible(standard_theme())
}


#' @rdname set_lattice
#' @export
standard_theme <- function() {
  theme <- lattice::standard.theme()
  theme$background$col <- "transparent"
  theme
}



#'
#' @rdname set_lattice
#' @export
ggplot_theme <-
  function(col = c("#00BA38","#00BFC4","#619CFF","#F564E3","#F8766D","#B79F00"),
           col.bar = NULL ,
           pch = 15:18,
           lty = 1:3,
           cex = 1,
           cex.main = 1.2,
           cex.axis = 0.8,
           cex.xlab = 1,
           cex.ylab = 1,
           cex.add = 0.8,
           strip.background.col = c("grey80", "grey70", "grey60"),
           ...) {
  #  @importFrom latticeExtra ggplot2like
  #  theme <- latticeExtra::ggplot2like(n = 6, h.start = 120)
  #  latticeExtra::ggplot2like(n = 6, h.start = 120) |> dput()
    theme <-
      list(grid.pars = list(),
        fontsize = list(text = 12, points = 8),
        background = list(alpha = 1, col = "transparent"),
        panel.background = list(col = "grey90"),
        clip = list(panel = "on", strip = "on"),
        add.line = list(alpha = 1, col = "black", lty = 1, lwd = 1),
        add.text = list(alpha = 1, cex = 0.8, col = "black", font = 1,lineheight = 1.2),
        plot.polygon = list(alpha = 1, col = "grey20", border = "transparent", lty = 1, lwd = 1),
        box.dot = list(alpha = 1, col = "grey20", cex = 1, font = 1, pch = "|"),
        box.rectangle = list(alpha = 1, col = "grey20", fill = "white", lty = 1, lwd = 1),
        box.umbrella = list(alpha = 1, col = "grey20", lty = 1, lwd = 1),
        dot.line = list(alpha = 1, col = "white", lty = 1, lwd = 1),
        dot.symbol = list(alpha = 1, cex = 0.8, col = "black", font = 1, pch = 19),
        plot.line = list(alpha = 1, col = "black", lty = 1, lwd = 1),
        plot.symbol = list(alpha = 1, cex = 0.6, col = "black", font = 1, pch = 19,fill = "transparent"),
        reference.line = list(alpha = 1,col = "white", lty = 1, lwd = 1),
        strip.background = list(alpha = 1,
                                col = c("grey80", "grey70", "grey60")),
        strip.shingle = list(alpha = 1,
                             col = c("grey60", "grey50", "grey40")),
        strip.border = list(alpha = 1, col = "transparent", lty = c(1, 1, 1, 1, 1, 1, 1), lwd = c(1, 1, 1, 1, 1, 1, 1)),
        superpose.line = list(alpha = 1,
                              col = c("#00BA38", "#00BFC4", "#619CFF", "#F564E3", "#F8766D", "#B79F00"),
                              lty = c(1, 1, 1, 1, 1, 1, 1), lwd = c(1, 1, 1, 1, 1, 1, 1)),
        superpose.symbol = list(alpha = c(1, 1, 1, 1, 1, 1, 1), cex = 0.6,
                                col = c("#00BA38", "#00BFC4", "#619CFF", "#F564E3", "#F8766D", "#B79F00"),
                                fill = c("#CCFFFF", "#FFCCFF", "#CCFFCC", "#FFE5CC", "#CCE6FF", "#FFFFCC", "#FFCCCC"),
                                font = c(1, 1, 1, 1, 1, 1, 1), pch = 19),
        superpose.polygon = list(alpha = c(1,1, 1, 1, 1, 1, 1),
                                 col = c("#00BA38", "#00BFC4", "#619CFF", "#F564E3", "#F8766D", "#B79F00"),
                                 border = "transparent", lty = c(1, 1, 1, 1, 1, 1, 1), lwd = c(1, 1, 1, 1, 1, 1, 1)),
        regions = list(alpha = 1,
                    col = c(
                      "#3B4FB8", "#3C4EB6", "#3D4DB4", "#3E4DB3", "#404CB1", "#414CB0", "#424BAE",
                      "#434BAC", "#454AAB", "#464AA9", "#4749A8", "#4849A6", "#4A48A4", "#4B48A3", "#4C47A1", "#4D47A0", "#4F469E", "#50469C", "#51459B",
                      "#524599", "#544498", "#554396", "#564394", "#574293", "#594291", "#5A4190", "#5B418E", "#5C408C", "#5E408B", "#5F3F89", "#603F88",
                      "#613E86", "#633E84", "#643D83", "#653D81", "#663C80", "#683C7E", "#693B7C", "#6A3B7B", "#6B3A79", "#6D3978", "#6E3976", "#6F3874", "#703873", "#723771", "#733770", "#74366E", "#75366C", "#77356B",
                      "#783569", "#793468", "#7A3466", "#7C3365", "#7D3363", "#7E3261", "#7F3260", "#81315E", "#82315D", "#83305B", "#843059", "#862F58",
                      "#872E56", "#882E55", "#892D53", "#8B2D51", "#8C2C50", "#8D2C4E", "#8E2B4D", "#902B4B", "#912A49", "#922A48", "#932946", "#952945",
                      "#962843", "#972841", "#982740", "#9A273E", "#9B263D", "#9C263B", "#9D2539", "#9F2438", "#A02436", "#A12335", "#A22333", "#A42231", "#A52230", "#A6212E", "#A7212D", "#A9202B", "#AA2029", "#AB1F28",
                      "#AC1F26", "#AE1E25", "#AF1E23", "#B01D21", "#B11D20", "#B31C1E", "#B41C1D", "#B51B1B", "#B71B1A")),
        shade.colors = list(alpha = 1,

        palette =
          function (irr, ref, height, saturation = 0.9) {
            hsv(h = height, s = 1 - saturation * (1 - (1 - ref)^0.5),v = irr)
            }),
        axis.line = list(alpha = 1, col = "transparent", lty = 1, lwd = 1),
        axis.text = list(alpha = 1, cex = 0.8, col = "grey50", font = 1, lineheight = 0.9),
        axis.components = list(left = list(tck = 1, pad1 = 1, pad2 = 1),
                               top = list(tck = 1, pad1 = 1, pad2 = 1),
                               right = list(tck = 1, pad1 = 1, pad2 = 1),
                               bottom = list(tck = 1, pad1 = 1, pad2 = 1)),
        layout.heights = list(top.padding = 1, main = 1, main.key.padding = 1, key.top = 1, xlab.top = 1, key.axis.padding = 1, axis.top = 1, strip = 1, panel = 1, axis.panel = 1, between = 1, axis.bottom = 1, axis.xlab.padding = 1, xlab = 1, xlab.key.padding = 1, key.bottom = 1, key.sub.padding = 1, sub = 1, bottom.padding = 1),
        layout.widths = list(left.padding = 1, key.left = 1, key.ylab.padding = 1, ylab = 1, ylab.axis.padding = 1, axis.left = 1, axis.panel = 1, strip.left = 1, panel = 1,
        between = 1, axis.right = 1, axis.key.padding = 1, ylab.right = 1, key.right = 1, right.padding = 1),
        box.3d = list(alpha = 1, col = "black", lty = 1, lwd = 1),
        par.title.text = list(alpha = 1, cex = 1.2, col = "#000000", font = 1, lineheight = 1),
        par.xlab.text = list(alpha = 1, cex = 1, col = "black", font = 1, lineheight = 1),
        par.ylab.text = list(alpha = 1, cex = 1, col = "black", font = 1, lineheight = 1),
        par.zlab.text = list(alpha = 1, cex = 1, col = "black", font = 1, lineheight = 1),
        par.main.text = list(alpha = 1, cex = 1.2, col = "black",font = 2, lineheight = 1),
        par.sub.text = list(alpha = 1, cex = 1, col = "black", font = 2, lineheight = 1))


    if (is.null(col.bar))
      col.bar <- theme$plot.polygon$col

    theme$strip.background$col = strip.background.col

    theme$axis.text$cex = 0.8
    theme$axis.text$lineheight = 0.9
    theme$axis.text$col = "#000000"

    theme$superpose.symbol$col = col
    theme$superpose.symbol$pch = pch


    theme$superpose.polygon$col = col
    theme$superpose.polygon$border = "transparent"

    theme$plot.polygon$col = col.bar

    theme$superpose.line$col = col
    theme$superpose.line$lty = lty

    theme$box.dot$pch = 19
    theme$box.dot$cex = cex
    theme$box.rectangle$col = col.bar
    theme$superpose.symbol$fill = col

    theme$plot.symbol$pch = 1


    theme$par.xlab.text$cex <- cex.xlab
    theme$par.ylab.text$cex <- cex.ylab
    theme$par.main.text$cex <- cex.main
    theme$axis.text$cex <- cex.axis

    # text in auto.key
    theme$add.text$cex <- cex.add


    val <- list(...)
    if (length(val == 0))
      theme
    else
      modifyList(theme, val)
  }

#' @param bottom,top,left,right theme layout.heights and layout.widths Numeric vector of length 4 for plot padding (bottom, left, top, right)
#' @param col Color palette for plot elements (default: grey scale)
#' @param col.n Number of colors to generate if col is a palette name
#' @param col.bar Color for bars in barcharts
#' @param pch Vector of point symbols
#' @param lty Vector of line types
#' @param lwd Vector of line widths
#' @param strip.background.col,strip.border.col,strip.text.col Background, Border, Text color for strips
#' @param box Logical indicating whether to draw plot borders
#' @param cex,cex.main,cex.axis,cex.strip,cex.symbol,cex.xlab,cex.ylab,cex.add Base scaling factor for
#' title, axis labels, x and y labels, strip labels
#' @param strip.lineheight numeric strip lineheight
#'
#' @rdname set_lattice
#' @export
bw_theme <- function(col = grDevices::grey.colors(7, start = 0.3, end = 0.9),
                     col.n=5,
                     col.bar = "grey50",
                     pch = 15:18,
                     lty = 1:3,
                     lwd =c(2, rep(1, 6)),

                     cex = 1,
                     cex.main = 1.2,
                     cex.axis = 0.8,
                     cex.xlab = 1,
                     cex.ylab = 1,
                     cex.strip = 0.8,

                     cex.add = cex.strip,
                     cex.symbol =  0.8,

                     strip.lineheight = 1.2,
                     strip.background.col = "transparent",
                     strip.border.col = "#000000",
                     strip.text.col = "#000000",

                     bottom = 1,
                     top = 1,
                     left = 1,
                     right = 1,

                     box = NULL,
                     ...) {

  if (length(col) == 1) {
    col <-  if (grepl("[A-Z]", substr(col, 1, 1)))
      farbe(tolower(col), col.n)
  }

  theme <- lattice::standard.theme(color = FALSE)
  # background
  theme$strip.background$col <- strip.background.col
  # symbol
  theme$superpose.symbol$pch <- pch
  theme$superpose.symbol$fill <- col
  theme$superpose.symbol$col <- col
  theme$superpose.symbol$cex <- cex.symbol

  # balken
  theme$superpose.polygon$col <- col
  theme$superpose.polygon$border <- "transparent"
  theme$plot.polygon$col <- col.bar
  theme$plot.polygon$border <- "transparent"

  # lienien
  theme$superpose.line$col <- col
  theme$superpose.line$lty <- lty
  theme$superpose.line$lwd <- lwd

  # box-plot
  theme$box.dot$pch <- 19
  theme$box.dot$cex <- cex.symbol
  theme$plot.symbol$pch <- 1
  # text in auto.key und strip
  theme$add.text$cex <- cex.add

  # strip.default
  theme$strip.shingle$col <- col
  theme$strip.background$col <- strip.background.col
  theme$strip.border$col <- strip.border.col
  theme$add.text$col <-  strip.text.col

  theme$par.xlab.text$cex <- cex.xlab
  theme$par.ylab.text$cex <- cex.ylab
  theme$par.main.text$cex <- cex.main
  theme$axis.text$cex <- cex.axis


  # effects::effectsTheme
  # strip.background = list(col = gray(seq(0.95, 0.5, length = 3)))
  # strip.shingle = list(col = "black")
  theme$clip$strip <-"off"
  #  superpose.line = list(lwd = c(2, rep(1, 6)))


  # box um die Grafik
  if (!is.null(box)) {
    theme$box.3d$col <- "transparent"
    theme$strip.border$col <- "transparent"
    theme$axis.line$col <- "transparent"
  }

  # layout
  theme$layout.heights$top.padding <- top
  theme$layout.heights$bottom.padding <- bottom
  theme$layout.widths$left.padding <- left
  theme$layout.widths$right.padding <- right

  theme$layout.heights$strip <- strip.lineheight

  val <- list(...)
  if (length(val == 0))
    theme
  else
    utils::modifyList(theme, val)
}







#' Color Palette Generator
#'
#' Provides various color palettes including custom, RColorBrewer, and grayscale options.
#' Supports Likert scales and colorblind-friendly palettes.
#'
#' @param type Palette type:
#'   - Custom: "pirat", "ggplot", "sex", "sex.mf"
#'   - RColorBrewer: "dark", "pastel", "Reds", "Blues", "Greens", etc.
#'   - Grayscale: "bw", "grays"
#'   - Special: "cb" (colorblind-friendly), "likert" (Likert scales)
#' @param n Number of colors (minimum 3, maximum depends on palette)
#' @param include.names Logical, whether to include color names (may cause issues with ggplot)
#' @param ... Additional arguments for Likert scales (name, middle, middle.color)
#'
#' @return Character vector of hex color codes (or named vector if include.names = TRUE)
#' @export
#'
#' @examples
#' #' Basic usage
#' par(mfrow=c(3,3))
#'
#' barplot(cbind(1:5, rep(3,5)), horiz = TRUE, main= "pirat",
#'         col=farbe("pirat", 5))
#'
#' barplot(cbind(1:8, rep(3,8)), horiz = TRUE, main= "likert",
#'         col=farbe("likert", 8, name="RdBl", middle=4,  middle.color= "#00FF00"))
#'
#' barplot(cbind(1:5, rep(3,5)), horiz = TRUE, main= "ggplot",
#'         col=farbe("ggplot", 5))
#'
#'
#' barplot(cbind(1:5, rep(3,5)), horiz = TRUE, main= "Greens",
#'         col=farbe("Greens", 5))
#' barplot(cbind(1:8, rep(3,8)), horiz = TRUE, main= "color.blinde",
#'         col=farbe("color.blinde", 8))
#'
#' barplot(cbind(1:2, rep(1,2)), horiz = TRUE, main= "sex",
#'         col=farbe("sex", 2))
#'
#'
#' barplot(cbind(1:5, rep(3,5)), horiz = TRUE, main= "bw",
#'         col=farbe("bw", 5))
#' barplot(cbind(1:5, rep(3,5)), horiz = TRUE, main= "pastel",
#'         col=farbe("pastel", 5))
#' barplot(cbind(1:5, rep(3,5)), horiz = TRUE, main= "dark",
#'         col=farbe("dark", 5))
farbe <- function(
    type = c(
      "pirat", "ggplot", "dark", "dunkel",
      "pastel", "hell", "cb", "color.blinde",
      "sex", "sex.mf", "bw", "grays", "likert",
      "Reds", "Blues", "Greens","Blues", "Greys", "Oranges", "Purples"),
    n = 5,
    include.names=FALSE,
    ...) {

  cbPalette <- c(
    orange     = "#E69F00",
    skyblue    = "#56B4E9",
    green      = "#009E73",
    yellow     = "#F0E442",
    blue       = "#0072B2",
    vermillion = "#D55E00",
    purple     = "#CC79A7",
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854",
    "#FFD92F",
    "#E5C494",
    "#B3B3B3",
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666"
  )

  type <-  match.arg(type, several.ok = FALSE)
  col <-  switch(
    type,
    pirat = c(
      blue1     = "#0C5BB0FF",
      green    =  "#15983DFF" ,
      pink   =    "#EC579AFF",
      orange    = "#FA6B09FF",
      green1   =  "#667840FF" ,
      red1      = "#B91226FF" ,
      blue2     = "#149BEDFF" ,
      green2    = "#A1C720FF",
      yellow    = "#FEC10BFF",
      turquoise = "#16A08CFF",
      poop    =   "#9A703EFF"  ,
      purple2   = "#972C8DFF",
      orange2  =  "#FF6435FF"  ,
      brown     = "#6A1D1AFF",
      purple =    "#5A5895FF" ,
      salmon    = "#D86C4FFF" ,
      darkgreen = "#006A40FF" ,
      brown1    = "#88775FFF" ,
      red     =   "#EE0011FF"  ,
      green     = "#5FB233FF",
      gray1    =  "#323337FF" ,
      gray2    =  "#534C53FF",
      blue    =   "#3F516AFF"  ,
      grayblue  = "#708C98FF"
    )[1:n],
    ggplot =  c("#00BA38",
                "#00BFC4",
                "#619CFF",
                "#F564E3",
                "#F8766D",
                "#B79F00"),

    dark = RColorBrewer::brewer.pal(n, "Dark2"),
    #   "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" "#666666"
    dunkel = RColorBrewer::brewer.pal(n, "Dark2"),
    pastel = RColorBrewer::brewer.pal(n, "Pastel1"),
    hell = RColorBrewer::brewer.pal(n, "Pastel1"),
    cb = cbPalette[1:n],
    color.blinde = cbPalette[1:n],

    bw =   grDevices::grey.colors(n, start = 0.3, end = 0.9),
    sex =  c(pink  = "#EC579AFF",
             blue2 = "#149BEDFF"),

    sex.mf = c(blue2 = "#149BEDFF",
               pink  = "#EC579AFF"),

    grays   =  grDevices::grey.colors(n, start = 0.3, end = 0.9),
    likert = likert_col(n=n, ...),

    brewer_pal2(n, type )
  )

  if(!include.names) as.character(col)
  else col

}

#' @rdname farbe
#' @param name A palette name from the lists below "RdBl"  ist   RdBl = c("Reds", "Blues")
#' @param middle,middle.color reference   "gray65"
#' @importFrom grDevices rgb
#'
#' @examples
#'
#'
#'
#' farbe("likert")
#' ##stp25settings:::likert_col(5 )
#' #"#FC9272"    "#FEE0D2"     "gray90"    "#DEEBF7"    "#9ECAE1"
#'
#'
#' farbe("likert", n=4,  name="GrRd", middle=2)
#' ##stp25settings:::likert_col(4 , "GrRd", middle=2)
#' #"#A1D99B"     "gray90"    "#FEE0D2"    "#FC9272"
#'
likert_col <- function(n = 5,
                       name =  "RdBl" ,
                       # c("RdBl", "BlRd",
                       #          "RdGr", "GrRd",
                       #          "GrBl", "BlGr",
                       #          "Bw"),
                       middle = mean(1:n),
                       middle.color =  "gray90") {
  # cat("\n n = ", n, " middle = " ,middle, "\n")
  if (length(name) == 1) {
    name <-
      switch(
        name,
        RdBl = c("Reds", "Blues"),
        BlRd = c("Blues", "Reds"),
        RdGr = c("Reds", "Greens"),
        BlGr = c("Blues", "Greens"),
        GrBl = c("Greens", "Blues"),
        GrRd = c("Greens", "Reds"),
        c("Greys", "Greys")
      )
  }
  if (is.odd(middle)) {
    c(rev(brewer_pal2(n = middle - 1, name = name[1])),
      middle.color = middle.color,
      brewer_pal2(n = n - middle, name = name[2]))
  }
  else{
    c(rev(brewer_pal2(n = floor(middle), name = name[1])),
      brewer_pal2(n = n - floor(middle), name = name[2]))
  }

}

brewer_pal2<- function(n,
                       name="Blues"){
  my_color <-
    switch(
      name,
      Reds = switch(
        n,
        rgb(
          c( 252),
          c( 146),
          c( 114),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252),
          c(224, 146),
          c(210, 114),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252, 222),
          c(224, 146, 45),
          c(210, 114, 38),
          maxColorValue = 255
        ),
        rgb(
          c(254,  252, 251, 203),
          c(229, 174, 106, 24),
          c(217, 145, 74, 29),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252, 251,222, 165),
          c(229, 174, 106, 45, 15),
          c(217, 145, 74, 38, 21),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252,  252, 251, 222, 165),
          c(229, 187, 146, 106, 45, 15),
          c(217, 161, 114, 74, 38, 21),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252, 252, 251, 239, 203, 153),
          c(229, 187, 146, 106, 59, 24, 0),
          c(217, 161, 114, 74, 44, 29, 13),
          maxColorValue = 255
        ),
        rgb(
          c(255, 254, 252, 252, 251, 239, 203, 153),
          c(245, 224, 187, 146, 106, 59, 24, 0),
          c(240, 210, 161, 114, 74, 44, 29, 13),
          maxColorValue = 255
        ),
        rgb(
          c(255, 254, 252, 252, 251, 239, 203, 165, 103),
          c(245, 224, 187, 146, 106,  59, 24, 15, 0),
          c(240, 210, 161, 114,  74,  44, 29, 21, 13),
          maxColorValue = 255
        )
      )


      ,



      Blues = switch(
        n ,
        rgb(
          c(158),
          c(202),
          c(225),
          maxColorValue = 255
        ),
        rgb(
          c(222, 158),
          c(235, 202),
          c(247, 225),
          maxColorValue = 255
        ),
        rgb(
          c(222, 158, 49),
          c(235, 202, 130),
          c(247, 225, 189),
          maxColorValue = 255
        ),
        rgb(
          c(239, 189, 107, 33),
          c(243, 215, 174, 113),
          c(255, 231, 214, 181),
          maxColorValue = 255
        ),
        rgb(
          c(239, 189, 107, 49, 8),
          c(243, 215, 174, 130, 81),
          c(255, 231, 214, 189, 156),
          maxColorValue = 255
        ),
        rgb(
          c(239, 198, 158, 107, 49, 8),
          c(243, 219, 202, 174,
            130, 81),
          c(255, 239, 225, 214, 189, 156),
          maxColorValue = 255
        ),
        rgb(
          c(239, 198, 158, 107, 66, 33, 8),
          c(243, 219, 202,
            174, 146, 113, 69),
          c(255, 239, 225, 214, 198, 181,
            148),
          maxColorValue = 255
        ),
        rgb(
          c(247, 222, 198,
            158, 107, 66, 33, 8),
          c(251, 235, 219, 202, 174,
            146, 113, 69),
          c(255, 247, 239, 225, 214, 198, 181,
            148),
          maxColorValue = 255
        ),
        rgb(
          c(247, 222, 198,
            158, 107, 66, 33, 8, 8),
          c(251, 235, 219, 202, 174,
            146, 113, 81, 48),
          c(255, 247, 239, 225, 214, 198,
            181, 156, 107),
          maxColorValue = 255
        )
      ),





      Greens = switch(
        n,
        rgb(
          c(161),
          c(217),
          c(155),
          maxColorValue = 255
        ),
        rgb(
          c(229, 161),
          c(245, 217),
          c(224, 155),
          maxColorValue = 255
        ),
        rgb(
          c(229, 161, 49),
          c(245, 217, 163),
          c(224, 155, 84),
          maxColorValue = 255
        ),
        rgb(
          c(237,186, 116, 35),
          c(248, 228, 196, 139),
          c(233, 179,118, 69),
          maxColorValue = 255
        ),
        rgb(
          c(237, 186, 116,49, 0),
          c(248, 228, 196, 163, 109),
          c(233, 179, 118,84, 44),
          maxColorValue = 255
        ),
        rgb(
          c(237, 199, 161,116, 49, 0),
          c(248, 233, 217, 196, 163, 109),
          c(233,192, 155, 118, 84, 44),
          maxColorValue = 255
        ),
        rgb(
          c(237,199, 161, 116, 65, 35, 0),
          c(248, 233, 217, 196,171, 139, 90),
          c(233, 192, 155, 118, 93, 69, 50),
          maxColorValue = 255
        ),
        rgb(
          c(247, 229, 199, 161, 116,65, 35, 0),
          c(252, 245, 233, 217, 196, 171, 139,90),
          c(245, 224, 192, 155, 118, 93, 69, 50),
          maxColorValue = 255
        ),
        rgb(
          c(247, 229, 199, 161, 116, 65, 35, 0, 0),
          c(252,245, 233, 217, 196, 171, 139, 109, 68),
          c(245,224, 192, 155, 118, 93, 69, 44, 27),
          maxColorValue = 255
        )
      ),


      Greys = switch(
        n,
        rgb(c(189),
            c(189),
            c(189),
            maxColorValue = 255),
        rgb(c(240, 189),
            c(240, 189),
            c(240, 189),
            maxColorValue = 255),

        rgb(c(240, 189, 99),
            c(240, 189, 99),
            c(240, 189, 99), maxColorValue = 255),
        rgb(
          c(247, 204, 150, 82),
          c(247, 204, 150, 82),
          c(247, 204, 150, 82),
          maxColorValue = 255
        ),
        rgb(
          c(247, 204, 150, 99, 37),
          c(247, 204, 150, 99, 37),
          c(247, 204, 150, 99, 37),
          maxColorValue = 255
        ),
        rgb(
          c(247, 217, 189, 150, 99, 37),
          c(247, 217, 189, 150, 99, 37),
          c(247, 217, 189, 150, 99, 37),
          maxColorValue = 255
        ),
        rgb(
          c(247, 217, 189, 150, 115, 82, 37),
          c(247, 217, 189, 150, 115, 82, 37),
          c(247, 217, 189, 150, 115, 82, 37),
          maxColorValue = 255
        ),
        rgb(
          c(255, 240, 217, 189, 150, 115, 82, 37),
          c(255, 240, 217, 189, 150, 115, 82, 37),
          c(255, 240, 217, 189, 150, 115, 82, 37),
          maxColorValue = 255
        ),
        rgb(
          c(255, 240, 217, 189, 150, 115, 82, 37, 0),
          c(255, 240, 217, 189, 150, 115, 82, 37, 0),
          c(255, 240, 217, 189, 150, 115, 82, 37, 0),
          maxColorValue = 255
        )
      ),



      Oranges = switch(
        n ,
        rgb(
          c(253),
          c(174),
          c(107),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253),
          c(230, 174),
          c(206, 107),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 230),
          c(230, 174,   85),
          c(206, 107, 13),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 253, 217),
          c(237, 190, 141,  71),
          c(222, 133,  60,   1),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 253, 230, 166),
          c(237, 190, 141,  85,  54),
          c(222, 133,  60,  13,   3),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 253, 253, 230, 166),
          c(237, 208, 174, 141,  85,  54),
          c(222, 162, 107,  60,  13,   3),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 253, 253, 241, 217, 140),
          c(237, 208, 174, 141, 105,  72,  45),
          c(222, 162, 107,  60,  19,   1,   4),
          maxColorValue = 255
        ),
        rgb(
          c(255, 254, 253, 253, 253, 241, 217, 140),
          c(245, 230, 208, 174, 141, 105,  72,  45),
          c(235, 206, 162, 107, 60,   19,   1,   4),
          maxColorValue = 255
        ),
        rgb(
          c(255, 254, 253, 253, 253, 241, 217, 166, 127),
          c(245, 230, 208, 174, 141, 105,  72,  54,  39),
          c(235, 206, 162, 107,  60,  19,   1,   3,   4),
          maxColorValue = 255
        )
      ),


      Purples = switch(
        n,
        rgb(
          c( 188 ),
          c( 189 ),
          c( 220 ),
          maxColorValue = 255
        ),
        rgb(
          c(239, 188),
          c(237, 189),
          c(245, 220),
          maxColorValue = 255
        ),

        rgb(
          c(239, 188, 117),
          c(237, 189, 107),
          c(245, 220, 177),
          maxColorValue = 255
        ),
        rgb(
          c(242, 203, 158, 106),
          c(240, 201, 154,  81),
          c(247, 226, 200, 163),
          maxColorValue = 255
        ),
        rgb(
          c(242, 203, 158, 117,  84),
          c(240, 201, 154, 107,  39),
          c(247, 226, 200, 177, 143),
          maxColorValue = 255
        ),
        rgb(
          c(242, 218, 188, 158, 117,  84),
          c(240, 218, 189, 154, 107,  39),
          c(247, 235, 220, 200, 177, 143),
          maxColorValue = 255
        ),
        rgb(
          c(242, 218, 188, 158, 128, 106,  74),
          c(240, 218, 189, 154, 125,  81,  20),
          c(247, 235, 220, 200, 186, 163, 134),
          maxColorValue = 255
        ),
        rgb(
          c(252,  239, 218, 188, 158, 128, 106,  74),
          c(251,  237, 218, 189, 154, 125,  81,  20),
          c(253,  245, 235, 220, 200, 186, 163, 134),
          maxColorValue = 255
        ),
        rgb(
          c(252, 239, 218, 188, 158, 128, 106,  84,  63),
          c(251, 237, 218, 189, 154, 125,  81,  39,   0),
          c(253, 245, 235, 220, 200, 186, 163, 143, 125),
          maxColorValue = 255
        )
      )
    )
  names(my_color ) <- paste0(name, 1:n)
  my_color
}






