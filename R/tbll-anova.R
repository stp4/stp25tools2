#' ANOVA-Ergebnisse extrahieren und als formatierte Tabelle ausgeben
#'
#' Diese Funktion extrahiert ANOVA-Ergebnisse aus verschiedenen Modelltypen
#' (aov, lm, car::Anova) und gibt sie als formatierte Tabelle aus.
#' Sie unterstützt verschiedene Quadratsummen-Typen (Type I, II, III).
#'
#' @param ... Ein oder mehrere Modelle (aov, lm, oder car::Anova-Objekte).
#'   Bei mehreren Modellen können benannte Argumente verwendet werden.
#' @param include.eta Logisch, ob Effektstärken eingeschlossen werden sollen.
#'   Standard: TRUE
#' @param include.power Logisch, ob Teststärke (Power) eingeschlossen werden soll.
#'   Standard: FALSE
#' @param include.sumsq Logisch, ob Quadratsummen eingeschlossen werden sollen.
#'   Standard: TRUE
#' @param include.meansq Logisch, ob mittlere Quadratsummen eingeschlossen werden sollen.
#'   Standard: FALSE
#' @param es_type Charaktervektor der Effektstärken-Typen. Mögliche Werte:
#'   "eta" (Eta-Quadrat), "omega" (Omega-Quadrat), "epsilon" (Epsilon-Quadrat).
#'   Standard: "eta"
#' @param type Numerisch, Typ der Quadratsummen (1, 2 oder 3). Standard: 2
#' @param model_names Optionaler Vektor mit Modellnamen für die Ausgabe bei
#'   mehreren Modellen.
#'
#' @return Ein data.frame mit den ANOVA-Ergebnissen als formatierte Tabelle.
#'
#' @details
#' \strong{Quadratsummen-Typen (Type I, II, III):}
#'
#' Die verschiedenen Typen von Quadratsummen unterscheiden sich in ihrer
#' Behandlung von unausgeglichenen Designs und Interaktionen:
#'
#' \describe{
#'   \item{\strong{Type I (Sequential)}}{
#'     Werden sequentiell berechnet, wobei die Reihenfolge der Terme im Modell
#'     wichtig ist. Jeder Term wird nach allen vorherigen Termen im Modell
#'     adjustiert. Diese Methode ist anfällig für Reihenfolge-Effekte.
#'     
#'     \emph{R:} \code{summary(aov(...))} verwendet standardmäßig Type I.
#'     \emph{SPSS:} In UNIANOVA entspricht dies der Einstellung "Sequential".
#'   }
#'   \item{\strong{Type II (Hierarchical)}}{
#'     Jeder Term wird nach allen anderen Termen im Modell adjustiert, außer
#'     Termen, die den aktuellen Term enthalten. Diese Methode ist geeignet
#'     für balancierte Designs und Modelle ohne Interaktionen.
#'     
#'     \emph{R:} \code{car::Anova(model, type = 2)}
#'     \emph{SPSS:} Standard-Einstellung in GLM (General Linear Model)
#'   }
#'   \item{\strong{Type III (Marginal)}}{
#'     Jeder Term wird nach allen anderen Termen im Modell adjustiert,
#'     einschließlich Interaktionen. Diese Methode ist am konservativsten
#'     und wird für unbalancierte Designs und Modelle mit Interaktionen
#'     empfohlen, erfordert aber korrekte Kontrast-Spezifikation.
#'     
#'     \emph{R:} \code{car::Anova(model, type = 3)}
#'     \emph{SPSS:} Verfügbar in GLM als Option, erfordert spezielle Kontraste
#'   }
#' }
#'
#' \strong{Vergleich zwischen R und SPSS:}
#'
#' \itemize{
#'   \item \strong{R standard (aov):} Verwendet Type I Quadratsummen, die 
#'         reihenfolgeabhängig sind. Die Reihenfolge der Terme in der Formel
#'         beeinflusst die Ergebnisse.
#'         
#'   \item \strong{SPSS standard (GLM):} Verwendet Type II Quadratsummen als
#'         Standard. Dies entspricht dem hierarchischen Ansatz, bei dem 
#'         Haupteffekte auch bei vorhandenen Interaktionen getestet werden.
#'         
#'   \item \strong{SPSS UNIANOVA:} Bietet beide Typen an, mit Type III als
#'         häufig verwendete Option für unbalancierte Designs.
#' }
#'
#' Für Modelle mit Interaktionen oder unbalancierten Designs wird Type III
#' empfohlen, wobei Effekt-Kodierung (z.B. contr.sum) verwendet werden sollte.
#' In R muss für Type III-Analysen typischerweise das \code{car}-Paket verwendet
#' werden, da die Basis-R-Funktionen nur Type I unterstützen.
#'
#' @examples

#' 
#' require(car)
#' 
#' # contr.treatment (Referenzgruppen-Kodierung):
#' # Intercept = Mittelwert der Referenzgruppe
#' # Wie unterscheidet sich Gruppe X von der Referenzgruppe?
#' # Referenzgruppe eine spezielle Bedeutung hat (z.B. Kontrollgruppe)
#' 
#' options(contrasts = c("contr.treatment", "contr.poly"))
#' mod1_aov <- aov(conformity ~ fcategory*partner.status, data=Moore) 
#' mod1_lm <- lm(conformity ~ fcategory*partner.status, data=Moore)
#' 
#' 
#' # contr.sum (Effekt-Kodierung):
#' # Orthogonale Hypothesen Ergebnisse hängen nicht von der Reihenfolge der Terme ab
#' # Intercept = Gesamtmittelwert über alle Gruppen
#' # Wie weicht Gruppe X vom Gesamtmittelwert ab?
#' # Type III Tests ANOVA 
#' 
#' op <- options(contrasts = c("contr.sum", "contr.poly"))
#' mod2_aov <- aov(conformity ~ fcategory*partner.status, data=Moore)
#' 
#' mod2_lm <- lm(conformity ~ fcategory*partner.status, 
#'               data=Moore,
#'               contrasts=list(fcategory=contr.sum, partner.status=contr.sum))
#' 
#' # Beide Kontrastarten liefern identische F-Werte und p-Werte
#' 
#' Tbll_anova(mod1_lm) # achtung ANOVA ist falsch
#' Tbll_anova(mod2_lm) # achtung ANOVA ist falsch
#' 
#' # aber 
#' levels(Moore$fcategory)
#' # Referenzgruppe ist anderst daher unterschiedliche T -Werte 
#' Tbll_regression(mod1_lm, include.statistic = TRUE, include.p = TRUE)
#' Tbll_regression(mod2_lm, include.statistic = TRUE, include.p = TRUE)
#' 
#' # aber identische Effecte 
#' Tbll_effect(mod1_lm)
#' Tbll_effect(mod2_lm)
#' 
#' 
#' 
#' # Korrektes Modell
#' Tbll_anova(mod2_lm, type=3) 
#' Tbll_anova(mod2_aov, type=3) 
#' 
#' 
#' # Beispiel mit iris-Datensatz
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' # Einfache ANOVA - R Standard (Type I)
#' model1 <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' Tbll_anova(model1, type = 1)  # Entspricht summary(aov(...))
#'
#' # SPSS-äquivalent (Type II)
#' Tbll_anova(model1, type = 2)
#'
#'
#'
#' # Komplexeres Beispiel mit Interaktionen
#' require(car)
#' mod <- lm(conformity ~ fcategory * partner.status, data = Moore,
#'          contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
#'
#' Tbll_anova(mod, type = 1)  # R aov-style
#' Tbll_anova(mod, type = 2)  # SPSS GLM standard
#' Tbll_anova(mod, type = 3)  # SPSS UNIANOVA style
#' 
#'
#' @seealso
#' \code{\link[car]{Anova}}, \code{\link[parameters]{model_parameters}},
#' \code{\link[stats]{aov}}, \code{\link[stats]{lm}}
#'
#' @export
#' @importFrom car Anova
#' @importFrom parameters model_parameters 
#' @importFrom insight find_response find_interactions
Tbll_anova <-
  function(...,
           # names = NULL,
           include.eta = TRUE,
           include.power = FALSE,
           include.sumsq = TRUE,
           include.meansq = FALSE,
           es_type = "eta",
           type = 2,
           model_names =NULL) {
    dots <- list(...)
    if (!include.eta)
      es_type <- NULL
    
    if (length(dots) == 1) {
      dots[[1]] |>
        extract_param_aov(es_type = es_type,
                          type = type,
                          include.power = include.power,
                          include.sumsq = include.sumsq,
                          include.meansq = include.meansq)
    }
    else{
      rslt <-  NULL
      
      number_models <- length(dots)
      # Modellnamen generieren
      if (is.null(model_names) | number_models != length(model_names)) {
        model_names <- if (!is.null(names(dots))) {
          names(dots)
        } else {
          paste0("m", seq_len(number_models))
        }
      }
      
 
      for (i in  seq_len(number_models)) {
        rst <-
          dplyr::bind_rows(
            data.frame(Response = model_names[i]),
            dots[[i]] |>
              extract_param_aov(
                es_type = es_type,
                type = type,
                include.power = include.power,
                include.sumsq = include.sumsq,
                include.meansq = include.meansq
              )
          )
       # rst[1, 1] <- model_names[i]
        rslt <- dplyr::bind_rows(rslt, rst)
      }
      rslt
    }
  }


extract_param_aov <- function(x,
                              es_type,
                              type,
                              include.sumsq = TRUE,
                              include.meansq = FALSE,
                              include.power = FALSE,
                              ...) {

  # hier kommt ein car::Anova - Objekt
  if (inherits(x, "anova")) {
    response <- ""
    anova_type <- ""
    intrc <- NULL
  }
  else {
    response <- paste0(", Response: ", insight::find_response(x))
    anova_type <- paste0("(Type ", type[1], " tests)")
    intrc <- insight::find_interactions(x)
  }
    if(!is.null(intrc) & type != 3) {
      print(intrc)
      stop("\n\nFalsche ANOVA type = ",type, 
           " bei Interactionen muss \ntype = 3 und 'contr.sum' gesetzt werden!\n")
     }
  # lm - objekt
  if (!(inherits(x, "aov")) & inherits(x, "lm")) {
    
    if (type == 1)
      x <- anova(x)
    else
      x <- car::Anova(x, type = type)

    rslt <-
      parameters::model_parameters(x,
                                   es_type = es_type, 
                                   power = include.power,
                                   verbose = TRUE)
  }
  else {
    # model_parameterstype fuehrt intern car::Anova aus
    # wenn ein car::Anova kommt wird type verworfen
    #  cat("  model_parameters(x, ", type, ")\n")
    rslt <-
      parameters::model_parameters(x,
                                   type = type,
                                   es_type = es_type,
                                   power = include.power,
                                   verbose = TRUE)
  }
  rslt$p <- rndr_P(rslt$p, include.symbol = FALSE)
  rslt$df <- render_f(rslt$df, digits =0)
  rslt <-
    dplyr::mutate(rslt, dplyr::across(
      tidyselect::all_of(names(rslt)),
      .fns = function(x) {
        if (is.numeric(x))
          render_f(x, digits = 2)
        else
          x
      }
    ))
  
  if (!include.meansq)
    rslt <-  rslt[!(names(rslt) %in% "Mean_Square")]
  if (!include.sumsq)
    rslt <-  rslt[!(names(rslt) %in% "Sum_Squares")]
  
  prepare_output(dplyr::relocate(rslt, p, .after = dplyr::last_col()),
                 caption = paste0("Anova ", anova_type, response)
                 )
  
}

