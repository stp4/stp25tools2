#' Erstellt Filter für CONSORT-Diagramme
#'
#' Diese Funktionen erstellen Ausschlusskriterien und Follow-up-Filter für die 
#' Verwendung mit dem `consort`-Package zur Erstellung von CONSORT-Diagrammen.
#'
#' @param data Ein Dataframe, im Wide - Format der die Studiendaten enthält.
#' Bei einem Long-Format muss vorher ins Wide transformiert werden.
#' @param ... Eine Reihe von Formeln im Format `bedingung ~ "ausschlussgrund"`.
#'   Die linke Seite definiert die Ausschlussbedingung, die rechte Seite den
#'   Beschreibungstext für das CONSORT-Diagramm.
#' @param .trialno Character, Name der Spalte für die Teilnehmernummerierung
#' @param .exclusion Character, Name der Spalte für die Ausschlussgründe
#' @param .filter Character, Name der logischen Spalte die anzeigt ob ein 
#'   Teilnehmer in die nächste Phase kommt
#'
#' @return Ein modifizierter Dataframe mit zusätzlichen Spalten für die 
#'   Filterung und Ausschlussgründe, geeignet für `consort::consort_plot()`
#'
#' @details
#' Die Funktionen arbeiten mit folgenden Logik:
#' - `Exclusion()`: Definiert Ausschlusskriterien für die Baseline-Phase
#' - `Followup()`: Definiert Ausschlusskriterien für die Follow-up-Phase
#' - Die Levels der Faktoren werden in der Reihenfolge der übergebenen Formeln beibehalten
#' - Teilnehmer die keine Ausschlusskriterien erfüllen erhalten `FILTER = TRUE`
#'
#' @examples
#' \dontrun{
#' library(consort)
#' library(tidyverse)
#' 
#' set.seed(0815)
#' n <- 317
#' 
#' DF <- data.frame(
#'   docu = rbinom(n, 1, .15) == 1,
#'   group = sample(c("HM", "M/UM", "10K"), n, replace = TRUE),
#'   sex = sample(c("male", "female"), n, replace = TRUE),
#'   age = round(runif(n, 15, 80)),
#'   outcome = round(ifelse(rbinom(n, 1, .01), NA, rnorm(n)), 2),
#'   measure = round(ifelse(rbinom(n, 1, .1), NA, rnorm(n)), 2)
#' )
#' 
#' # Fehlende Werte simulieren
#' DF$group[sample.int(n, 5)] <- NA
#' DF$sex[sample.int(n, 5)] <- NA
#' DF$outcome[1:2] <- NA
#' 
#' # Ausschlusskriterien anwenden
#' DF <- DF |>
#'   Exclusion(
#'     docu ~ "missing documentation",
#'     age < 18 ~ "age <18", 
#'     is.na(group) ~ "missing data runners",
#'     is.na(sex) ~ "missing data sex"
#'   ) |>
#'   Followup(
#'     is.na(outcome) ~ "Outcome missing",
#'     is.na(measure) ~ "Measure missing"
#'   )
#' 
#' # CONSORT-Diagramm erstellen
#' out <- consort_plot(
#'   data = DF,
#'   orders = c(
#'     trialno = "Population",
#'     exclusion = "Excluded", 
#'     group = "Randomised",
#'     lost_followup = "Not evaluable for\n the final analysis",
#'     trialno = "Final Analysis"
#'   ),
#'   side_box = c("exclusion", "lost_followup"),
#'   allocation = "group",
#'   cex = 0.9
#' )
#' plot(out)
#' 
#' # Gefilterten Datensatz für Analyse verwenden
#' DF_analysis <- DF |> dplyr::filter(FILTER)
#' 
#' # Beschiftungen Auslesen aus dem
#' # consort_plot Objekt
#' rslt <- list()
#' for( i in names(out) ){
#'   rst <- strsplit(out[[i]]$text, '\n')[[1]]
#'   prev_node <- out[[i]]$prev_node
#'   if(is.null(prev_node))
#'     rslt[[i]] <- rst
#'   else
#'     rslt[[prev_node]] <- append(rslt[[prev_node]], rst)
#' }
#' 
#' rslt 
#' 
#' 
#' }
#'
#' @seealso
#' \code{\link[consort]{consort_plot}}
#' , 
#' \code{\link[stp25tools2]{consort2}}
#' 
#' @rdname consort_filters
#' @export
#' @importFrom rlang enquos eval_tidy expr get_expr is_formula f_rhs
#' @importFrom dplyr case_when
#' 
Filter2 <- function(data, ..., 
                    .trialno = "trialno", 
                    .exclusion = "exclusion",
                    .filter = "FILTER") {
  dots <- rlang::enquos(...)
  
  # Sicherstellen, dass data ein data.frame ist
  if (!is.data.frame(data)) {
    stop("data muss ein data.frame sein")
  }
  else {
    N <- nrow(data)
    if (!(.filter %in% names(data))) {
      data[[.filter]] <- rep(FALSE, N)
    }
    if (!(.trialno %in% names(data))) {
      data[[.trialno]] <- seq_len(N)
    } else {
      stop("Ein Filter kann nur einmal mit dem selben Namen ausgefuehrt werden")
    }
    
    if (!(.exclusion %in% names(data))) {
      data[[.exclusion]] <- rep(NA, N)
    } else {
      stop("Ein Filter kann nur einmal mit dem selben Namen ausgefuehrt werden")
    }
  }
  
  exc1 <- rlang::eval_tidy(
    rlang::expr(dplyr::case_when(!!!dots)),
    data = data
  )
  
  # Levels in der Reihenfolge extrahieren, wie sie in ... übergeben wurden
  level_order <- character(length(dots))
  for (i in seq_along(dots)) {
    expr <- rlang::get_expr(dots[[i]])
    if (rlang::is_formula(expr)) {
      level_order[i] <- as.character(rlang::f_rhs(expr))
    }
  }
  
  # Nur gültige Levels behalten (ohne leere Strings)
  level_order <- level_order[level_order != ""]
  
  # Faktor mit Levels in der Übergabe-Reihenfolge
  if (!all(is.na(exc1))) {
    data[[.exclusion]] <- factor(exc1, levels = level_order)
    data[[.filter]][is.na(exc1)] <- TRUE
  }
  
  data
}

#' @rdname consort_filters
#' @export
Exclusion <- function(data, ...) {
  Filter2(
    data,
    ...,
    .trialno = "trialno", 
    .exclusion = "exclusion"
  )
}

#' @rdname consort_filters
#' @export
Followup <- function(data, ...) {
  Filter2(
    data,
    ...,
    .trialno = "followup",
    .exclusion = "lost_followup"
  )
}

