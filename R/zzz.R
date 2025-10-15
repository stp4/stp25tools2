
# usethis::use_vignette("Wide_Long")


# Initialize options when package is loaded

#  .onLoad - Fuer interne Initialisierung
#   Wird aufgerufen, wenn das Namespace geladen wird (einmal pro R-Session)
#  .onAttach - Fuer User-Kommunikation
#   Wird aufgerufen, wenn das Paket mit library() oder require() attachiert wird
.onLoad <- function(libname, pkgname) {
  # Niemals cat() oder print() in .onLoad - das stoert den Ladevorgang
  init_stp25_options()
}


.onAttach  <- function(libname, pkgname) {
  oldc <- getOption("contrasts")
  set_opt(default_contrasts = oldc)


  contrasts =  c("contr.Treatment", "contr.poly")
  options(contrasts = contrasts)
  # User-Kommunikation
  packageStartupMessage( pkgname,
    " geladen. \nPfad: ",libname, "\n\n",
    "I changed the contrasts from ",
    paste(oldc, collapse = ", " ),"\nto ", paste(contrasts, collapse = ", " ),
    "!\n\n")
}



#' Reset contrasts
#'
#' @export
reset_contrasts<- function(){
#  get_opt("default_contrasts")
  options(contrasts = get_opt("default_contrasts"))

  options()$contrasts
}




# .onAttach <- function(libname, pkgname) {
#   suppressPackageStartupMessages({
#     options(oldc = getOption("oldc"))
#     oldc <<- "some_value"
#   })
# }




#' Initialize stp25 options from config file
#' config_path Path to config.yml file
#' @export
#' @importFrom yaml read_yaml
init_stp25_options <- function() {
  # Suche die config.yml im installierten Paket
  config_path <- system.file( "config.yml", package = "stp25tools2")

  if (config_path == "") {
    warning("config.yml nicht gefunden. Verwende Standard-Optionen.")
    # Fallback-Optionen setzen
    # dput(get_opt())
    options(stp25.options =
              list(
                output = "",
                fig_folder = "Fig/",
                html_folder = "Results/",
                center = TRUE,
                sep_element = ", ",
                brackets = c("[", "]"),
                test_fun = c(
                  "contest","wilcox","utest","htest","kruskal",
                  "ttest","aov","anova","cattest","chisq","fisher",
                  "ordtest","binomial","notest","shapiro","kstest"
                ),
                test_fun_contest = c(
                  "contest","wilcox","utest","htest","kruskal","ttest","aov","anova"
                ),
                test_fun_cattest = c("cattest", "chisq", "fisher", "ordtest", "binomial"),
                test_fun_notest = "notest",
                test_fun_ordtest = "ordtest",
                test_fun_disttest = c("shapiro", "kstest"),
                test_fun_cortest = c("pearson", "kendall", "spearman"),
                table = list(
                  stubhead = "Item",
                  measure.name.m = "m",
                  measure.name.sd = "sd",
                  measure.name.total = "Total",
                  measure.name.statistics = "Statistics",
                  caption.nr.prefix = NULL,
                  include.n = FALSE,
                  include.tabel.number = TRUE
                ),
                percent = list(
                  digits = 0L,
                  lead.zero = TRUE,
                  style = 1L,
                  percentage_str = "%",
                  null_percent_sign = ".",
                  include_name = "",
                  include_level_multi = TRUE,
                  useNA = "no"
                ),
                mean = list(
                  digits = 2L,
                  lead.zero = TRUE,
                  plusmin_str = " +- ",
                  style = 1L,
                  seperator = ", ",
                  include_name = "(mean)"
                ),
                median = list(
                  digits = 2L,
                  lead.zero = TRUE,
                  seperator = ", ",
                  style = "IQR",
                  include_name = "(median)"
                ),
                Fstat = list(
                  digits = 2L,
                  lead.zero = TRUE,
                  include.statistic = TRUE
                ),
                r = list(digits = 2L, lead.zero = FALSE),
                r2 = list(digits = 2L, lead.zero = FALSE),
                p = list(
                  digits = 3L,
                  lead.zero = FALSE,
                  stars.value = c(0.001, 0.01, 0.05),
                  stars.symbols = c("***", "**", "*"),
                  with.stars = FALSE,
                  mark.sig = NULL
                ),
                measure = list(
                  numeric = "mean",
                  integer = "mean",
                  factor = "percent",
                  logical = "percent",
                  mean = "mean",
                  median = "median",
                  multi = "percent",
                  ratio = "percent",
                  freq = "percent"
                ),
                measure_fun = list(
                  numeric = "mean_tbll",
                  integer = "mean_tbll",
                  factor = "prct_tbll",
                  logical = "multi_tbll",
                  mean = "mean_tbll",
                  median = "median_tbll",
                  multi = "multi_tbll",
                  ratio = "prct_tbll",
                  header = "emty_tbll",
                  freq = "prct_tbll",
                  character = "emty_tbll"
                ),
                test = list(
                  numeric = "conTest",
                  integer = "conTest",
                  factor = "catTest",
                  logical = "catTest",
                  mean = "conTest",
                  median = "conTest",
                  multi = "catTest",
                  ratio = "catTest",
                  header = "notest",
                  freq = "catTest"
                )
              )
              )
    return()
  }

  config <- yaml::read_yaml(config_path)
  options(stp25.options = config$default)
}


#' Set package options
#'
#' Configure output settings for tables and statistics.
#'
#' @param ... Named lists of options to set. Each name corresponds to an option group
#'            (e.g., "table", "mean"), and the list contains key-value pairs for settings.
#'
#' @return Invisibly returns the previous values of the modified options
#' @examples
#' set_opt(
#'   table = list(measure.name.m = 'Average'),
#'   median = list(digits = 3),
#'   mean = list(digits = 4, style = 2)
#' )
#' @export
set_opt <- function(...) {
  new <- list(...)

  if ("prozent" %in% names(new)) {
    stop("prozent gibs nicht mehr! Verwende percent!")
  }

  if (any(names(new) == "fig_folder")) {
    new$fig_folder <- paste0(clnsng(new$fig_folder), .Platform$file.sep)
  }

  if (any(names(new) == "html_folder")) {
    new$html_folder <- paste0(clnsng(new$html_folder), .Platform$file.sep)
  }

  stp25.options(new)
}

#' Get package options
#'
#' Retrieve current package options or specific settings.
#'
#' @param name (Optional) Name of the option group to retrieve
#' @param type (Optional) Specific parameter within the group to retrieve
#' @return Requested options or values
#' @examples
#' get_opt()
#' get_opt("mean")
#' get_opt("mean", "digits")
#' @export
get_opt <- function(name = NULL, type = NULL) {
  opt <- getOption("stp25.options")

  if (!is.null(name)) {
    opt <- opt[[name]]
  }

  if (!is.null(type)) {
    opt <- opt[[type]]
  }

  opt
}

stp25.options <- function(...) {
  new <- list(...)

  if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) {
    new <- new[[1]]
  }

  old <- getOption("stp25.options")

  if (length(new) == 0) {
    return(old)
  }

  nm <- names(new)
  if (is.null(nm)) {
    return(old[unlist(new)])
  }

  isNamed <- nm != ""
  if (any(!isNamed)) {
    nm[!isNamed] <- unlist(new[!isNamed])
  }

  retVal <- old[nm]
  names(retVal) <- nm
  nm <- nm[isNamed]

  options(stp25.options = updateList(old, new[nm]))
  invisible(retVal)
}

updateList <- function(x, val) {
  if (is.null(x)) x <- list()
  utils::modifyList(x, val)
}


#' Helper in farbe
#' @noRd
is.odd <- function(x)
  trunc(x) - x == 0

# Cs, XLS



#' Character strings from unquoted names
#'
#' Erweiterung der  Hmisc Cs-Funktion um trim.ws.
#' Erlaubt Variablen mit Komma oder Plus(+) oder Leerzeichen abzuternnen.
#' @param ... Vektor oder String oder Formula
#' @return Vektor
#' @export
#' @examples
#'
#' Cs(sd, fr, fg)
#' Cs(sd, fr, fg, "hju nh")
#' Cs("  Hallo Welt ")
#' Cs(~ sd + fr + fg)

Cs<-
  function (...)
  {
    x <- as.character(sys.call())[-1]
    if (length(x) == 1)
      strsplit(gsub("^[[:blank:]]*", "",
                    gsub(
                      "[[:blank:]]*$", "",
                      gsub("[\t\n\\~\\+\\:/]", "  ", x)
                    ))
               , " +")[[1]]
    else
      x

  }



#' @rdname Cs
#'
#' @description XLS:
#'
#' Nummernindex aus Excel - Spaltenbezeichnung
#'
#' Extrahiert aus Buchstaben die Spaltennummer
#' @param ... liste mit den Spaltennamen A:BB
#' @export
#' @examples
#' as.roman(1968)
#' #strsplit("A:V", "\\:")
#' XLS(a, B)
#' XLS(a, B, c:f, g:h,i, r:z)
#' XLS(A:Z)
#'
XLS <- function(...) {
  letter_num <- function(ltr) {
    which(myLetters %in% ltr)
  }

  myLetters = c(LETTERS,
                unlist(lapply(LETTERS, function(abc)
                  paste0(abc, LETTERS))))

  ltr <- toupper(as.character(sys.call())[-1])


  xrange <- grep("\\:", ltr)
  n <- 0
  if (length(xrange)) {
    for (i in seq_along(xrange)) {
      posn <- xrange[i] + n - i + 1
      mltr <- unlist(strsplit(ltr[posn], "\\:"))
      myRange <- myLetters[letter_num(mltr[1]):letter_num(mltr[2])]
      ltr <- append(ltr, myRange, after = posn)
      ltr <- ltr[-posn]
      n <- n + length(myRange)
    }
  }

  letter_num(ltr)
}



#' @rdname Cs
#' @param x dataframe ofe factor
#' @description
#' paste_names:  paste names
#'
#' @param collapse  an  character string to separate the results ", "
#'
#' @return character
#' @export
#'
paste_names <-
  function(x,
           collapse = ", ",
           ...) {
    paste0(names(x) , collapse = collapse)
  }


#' @rdname Cs
#' @description
#' paste_names_levels:
#' Copy and Paste.
#'
#' @param collapse  an  character string to separate the results ", "
#' @param abbreviate logical.  in paste_names_levels
#'
#' @return character
#' @export
#'
paste_names_levels <- function(x,
                               abbreviate = TRUE,
                               collapse = ", ",
                               ...) {
  strg <-  get_label(x)
  if (abbreviate)
    paste0(names(strg), " = '", abbreviate(as.vector(strg), 15), "'", collapse = collapse)
  else
    paste0(names(strg), " = '", abbreviate(as.vector(strg), 15), "'", collapse = collapse)
}



#' prepare_output
#'
#' @param x data.frame
#' @param caption character
#' @param note character
#' @param N number
#' @param labels optional
#' @param rgroup optional
#' @param n.rgroup optional
#' @param ... not used
#'
#' @returns tibble, data.frame
#' @export
prepare_output <- function(x,
                           caption = "",
                           note = "",
                           N = NULL,
                           labels = NA,
                           #  include.n =  get_opt("caption"),
                           rgroup = NULL,
                           n.rgroup = NULL,
                           # class = NULL,
                           ...) {
  if (is.null(note))  note <- ""
  if (is.null(caption)) caption <- ""

  attr(x, "caption") <-  caption
  attr(x, "note") <- note
  attr(x, "N") <- N
  attr(x, "labels") <- labels
  attr(x, "rgroup") <- rgroup
  attr(x, "n.rgroup") <- n.rgroup


  x <-  tibble::as_tibble(x, .name_repair = "unique", rownames=NA)

  x
}


# @importFrom stp25settings which_output
# @export
#stp25settings::which_output
#
#' which_output
#'
#' @return values are text, latex, html, markdown_html markdown, pandoc, and rst
#' @export
#' @importFrom knitr pandoc_to
#'
#' @examples
#'
#' which_output()
#'
which_output <- function()
{
  in_opt <- get_opt("output")
  in_formats <- c("text", "markdown", "md", "pandoc", "rst",
                  "html", "docx", "word", "latex")
  if (is.null(in_opt))
    in_opt <- ""
  in_formats <- in_formats[pmatch(in_opt, in_formats)]
  in_formats <- switch(in_formats, md = "markdown", word = "docx",
                       in_formats)
  if (is.na(in_formats)) {
    # out <- knitr:::out_format()
    if(!exists("opts_knit")) out <- NULL
    else out <- opts_knit$get("out.format")
    if (is.null(out)) {
      if (options()$prompt[1] == "HTML> ") {
        in_formats <- "html"
      }
      else {
        in_formats <- "text"
      }
    }
    else {
      in_formats <- knitr::pandoc_to()
      in_formats <- switch(in_formats,

                           html = "markdown_html",
                           beamer = "latex",
                           guess_in_format(in_formats))
    }
  }
  in_formats[1]
}

#' @noRd
guess_in_format <- function(x) {
  if (any(grepl("gfm", x))) "markdown_html"
  else "text"
}

utils::globalVariables(c(
 'oldc',
 'notest',
 'ordtest',
 'disttest',
 'cortest',
 'mean_weight',
'Freq',
 'Item',
 'aes',
 'geom_bar',
  'position_fill',
  'labs',
  'scale_y_continuous',
  'geom_text',
  'after_stat',
 'prop',
  'position_stack',
  'count',
 'facet_grid',
 'theme_bw',
 'theme',
 'element_blank',
  'scale_fill_brewer',
 'coord_flip',
'ggplot', 'n_total', 'opts_knit'




))
