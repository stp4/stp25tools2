# plot_methods.R


#' @rdname gg_stacked
#'
#' @param x stp25likert-Objekt
#' @export
plot.stp_multi <- function(x, ...) {
  # Überprüfen ob gg_likertplot verfügbar ist
  if (!exists("gg_stacked", mode = "function")) {
    stop("gg_stacked function not found. Please make sure it's loaded.")
  }

  # gg_likertplot aufrufen
  gg_stacked(x, ...)
}

# plot Methode für likert_summary Objekte
#' @rdname gg_likertplot
#'
#' @param x stp25likert-Objekt
#' @export
plot.stp_likert <- function(x, ...) {
  # Überprüfen ob gg_likertplot verfügbar ist
  if (!exists("gg_likertplot", mode = "function")) {
    stop("gg_likertplot function not found. Please make sure it's loaded.")
  }

  # gg_likertplot aufrufen
  gg_likertplot(x, ...)
}


