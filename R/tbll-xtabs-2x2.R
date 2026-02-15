#https://www.epirhandbook.com/en/new_pages/tables_presentation.html
#'
#'https://cran.r-universe.dev/articles/epiR/epiR_diagnostic_tests.html
#'
#'https://cran.r-universe.dev/articles/epiR/epiR_measures_of_assoc.html


 



#' @title Extrahiere und präsentiere Statistiken aus epiR::epi.tests-Objekten
#'
#' @description
#' 
#' Tbll_2x2
#' 
#' Input: 
#' 1.) data.frame + mehere Namen der Variablen  -> Prävalenz Berechnung 
#' 2.) data + mehere formula vom type ~ a + b -> 2x2 Berechnung der 
#' 3.) Vector (singel)
#' 4.) xtabs oder tabel, oder 2x3
#' 
#'   # Prüft ob Namen vorhanden sind oder generiert sie
#'   
#' Tbll_xtabs_2x2
#' 
#'         LR+ > 10: Starker Hinweis auf das Vorliegen der Krankheit
#'         LR- < 0,1: Starker Hinweis auf das Fehlen der Krankheit
#' 
#' `Tbll_xtabs_2x2.epi.tests()` bereitet diese Statistiken in einer formatierten Tabelle auf,
#' geeignet für Berichte oder die Visualisierung.
#' 
#'  
#' `extract_epi()` extrahiert detaillierte Statistiken aus einem oder mehreren
#' `epi.tests`-Objekten für die weitere Analyse.
#' 
#' Prävalenzanalyse mit Konfidenzintervallen
#'
#' Eine umfassende Suite von Funktionen zur Berechnung, Formatierung und Visualisierung
#' von Prävalenzen mit verschiedenen Methoden für Konfidenzintervalle.
#'
#'
#' @details
#' Die Funktionen sind für die Aufbereitung von Ergebnissen diagnostischer Tests
#' aus dem `epiR`-Paket konzipiert. Sie extrahieren Schätzwerte mit zugehörigen
#' Konfidenzintervallen für eine benutzerdefinierte Auswahl an Kennzahlen.
#' `Tbll_xtabs_2x2.epi.tests()` formatiert die Ergebnisse automatisch: Prävalenz-basierte
#' Statistiken (z.B. Sensitivität, Spezifität) werden standardmäßig als
#' Prozentwerte dargestellt, andere Kennzahlen bleiben als Dezimalzahlen.
#'
#' @param ... Ein oder mehrere Objekte der Klasse `epi.tests`. Kann benannt
#'   oder unbenannt übergeben werden. Bei unbenannten Objekten werden Namen
#'   automatisch generiert (z.B. Test1, Test2).
#' @param select Ein Zeichenvektor, der angibt, welche Statistiken extrahiert
#'   werden sollen. Standardmäßig werden Sensitivität (`"se"`) und Spezifität
#'   (`"sp"`) ausgegeben.
#'   Verfügbare Kennzahlen sind:
#'   \itemize{
#'     \item `"ap"`: Schein-Prävalenz (Apparent prevalence)
#'     \item `"tp"`: Wahre Prävalenz (True prevalence)
#'     \item `"se"`: Sensitivität (Sensitivity)
#'     \item `"sp"`: Spezifität (Specificity)
#'     \item `"diag.ac"`: Diagnostische Treffsicherheit (Diagnostic accuracy)
#'     \item `"diag.or"`: Diagnostische Chance (Diagnostic odds ratio)
#'     \item `"nndx"`: Number needed to diagnose
#'     \item `"youden"`: Youden's Index
#'     \item `"pv.pos"`: Positiver prädiktiver Wert (Positive predictive value)
#'     \item `"pv.neg"`: Negativer prädiktiver Wert (Negative predictive value)
#'     \item `"lr.pos"`: Positives Likelihood-Verhältnis (Positive likelihood ratio)
#'     \item `"lr.neg"`: Negatives Likelihood-Verhältnis (Negative likelihood ratio)
#'     \item `"p.rout"`: Anteil der Erkrankten (Proportion of disease positive)
#'     \item `"p.rin"`: Anteil der Nicht-Erkrankten (Proportion of disease negative)
#'     \item `"p.tpdn"`: True positive / disease negative
#'     \item `"p.tndp"`: True negative / disease positive
#'     \item `"p.dntp"`: Disease negative / true positive
#'     \item `"p.dptn"`: Disease positive / true negative
#'   }
#' @param digits Ganzzahl. Die Anzahl der Dezimalstellen für die Formatierung
#'   der numerischen Werte. Standard ist `2`.
#' @param as_percent Logischer Wert. Sollen die Werte als Prozentzahlen
#'   formatiert werden? Der Standard (`NULL`) führt zu einer automatischen
#'   Entscheidung basierend auf der Kennzahl (Prozent für Prävalenz-basierte
#'   Statistiken). `TRUE` erzwingt die Prozent-Formatierung für alle Werte,
#'   `FALSE` unterdrückt sie.
#' @param include.n Logischer Wert. Sollen die Fallzahlen (`n`) in der
#'   finalen Ausgabetabelle von `Tbll_xtabs_2x2.epi.tests()` enthalten sein? Standard ist `FALSE`.
#'
#' @return
#' `extract_epi()` gibt ein `data.frame` (im tibble-Format) mit den folgenden
#' Spalten zurück:
#' \itemize{
#'   \item `parameter`: Name des Tests (aus den Argumentnamen oder generiert).
#'   \item `statistic`: Vollständiger, lesbarer Name der Kennzahl.
#'   \item `stat_code`: Kurzcode der Kennzahl (z.B. "se").
#'   \item `est`: Rohwert der Schätzung.
#'   \item `lower`: Untere Grenze des Konfidenzintervalls.
#'   \item `upper`: Obere Grenze des Konfidenzintervalls.
#'   \item `conf.level`: Konfidenzniveau, das für die Berechnung verwendet wurde.
#'   \item `method`: Methode, die für die Berechnung der Konfidenzintervalle
#'         verwendet wurde (z.B. "exact", "wilson").
#'   \item `n`: Gesamtfallzahl der Kontingenztabelle.
#' }
#'
#' `Tbll_xtabs_2x2.epi.tests()` gibt ein Objekt zurück, das für die Ausgabe durch
#' `stp25output2::Output()` vorbereitet ist. Es enthält eine formatierte
#' Tabelle, bei der jede Zeile einem Test (`parameter`) entspricht und die
#' Spalten die ausgewählten Kennzahlen (`statistic`) enthalten. Jede Zelle zeigt
#' den geschätzten Wert mit Konfidenzintervall. Das Objekt besitzt außerdem
#' das Attribut `"plot"`, das den von `extract_epi()` erzeugten Dataframe
#' enthält, um eine direkte Weiterverarbeitung (z.B. mit `gg_praevalenz()`)
#' zu ermöglichen.
#'
#' @examples
#' \dontrun{
#' library(epiR)
#' # Epikurve Objekte erstellen
#' test1 <- epi.tests(c(670, 202, 74, 640))
#' test2 <- epi.tests(c(650, 202, 74, 660))
#' test3 <- epi.tests(c(690, 182, 64, 650))
#'
#' # Daten extrahieren
#' daten <- extract_epi(
#'   Histologie = test1,
#'   Stanzbiopsie = test2,
#'   MRT = test3,
#'   select = c("se", "sp")
#' )
#'
#' # Formatierte Tabelle erstellen und ausgeben epi.tests
#' Tbll_xtabs_2x2(
#'   Histologie = test1,
#'   Stanzbiopsie = test2,
#'   MRT = test3,
#'   select = c("se", "sp")
#' ) |> stp25output2::Output()
#'
#' # Extrahierten Dataframe für Visualisierung nutzen
#' # library(ggplot2)
#' # daten |> gg_praevalenz() + facet_grid(~statistic)
#' }
#'
#' @seealso
#' Die zugrundeliegende Funktion zum Erstellen der Testobjekte:
#' [epiR::epi.tests()].
#'
#' @name Tbll_xtabs_2x2
#' @aliases Praevalenz praevalenz  Tbll_praevalenz
NULL
 
#' @export
Tbll_xtabs_2x2 <- function(x, ...) {
  UseMethod("Tbll_xtabs_2x2")
}

#' @rdname Tbll_xtabs_2x2
#' @export
Tbll_xtabs_2x2.data.frame <- function(x,
                           ...,
                           # epi
                           method = c("clopper-pearson", "wilson", "agresti", "jeffreys"),
                           conf.level = 0.95,
                           select = c("se", "sp", "pv.pos", "pv.neg", "lr.pos","lr.neg"),
                           digits = 1L,
                           use_data_names = FALSE
                           ) {
  method <-  match.arg(method)
 
    dots <- rlang::enquos(...)
    # Parameter-Namen extrahieren oder generieren
    nms <- names(dots)
   # print(dots == "")
    if( all(nms== "")) use_data_names <- TRUE
    nms <- test_no_name(nms, n = length(dots))
    
    first_expr <-  rlang::quo_squash(dots[[1]])
    xtabs_2x2 <- NULL
    xtabs_Nx2 <- NULL
    epi_test_rst <- NULL
    
    # Formel ~ a + b
    if (rlang::is_call(first_expr, "~")) {
      
      # Test was kommt
      for (i in seq_along(dots)) {
        formula_l <- eval(dots[[i]], envir = rlang::caller_env())
        if (length(formula_l) == 2L)
          xtb <- xtabs(formula_l , x)
        # 2x2 tabelle weiter mit epi
        if (all(dim(xtb) == c(2, 2))) {
          xtabs_2x2[[nms[i]]] <- xtb
          epi_test_rst[[nms[i]]] <-
            epiR::epi.tests(xtb, method = method, conf.level = conf.level)
        }
        # 3x2 Tabelle (inkonklusiv bei der Test-Variable)
        else if (all(dim(xtb) == c(3, 2)))
          xtabs_Nx2[[nms[i]]] <- xtb
      }
      # weiter mit epi für 2x2- CIs
      if (!is.null(xtabs_2x2) & is.null(xtabs_Nx2)) {
        
        # Namen aus der x-tabelle
        if(use_data_names)  
          model_names <-
            sapply(xtabs_2x2, function(x) paste(names(dimnames(x)), collapse ="/" ))
        else model_names <- NULL
        
        epi_data_frame <-  extract_epi(select = select, dots = epi_test_rst)
        return(
          Tbll_xtabs_2x2.epi.tests(
            digits = digits,
            epi_data_frame = epi_data_frame,
            model_names = model_names
            
          ))
      }
      # 3x2 Tabelle mit Inkonklusiv ecaluierung als 2x2 Tabelle ohne CIs
      else if (is.null(xtabs_2x2) & !is.null(xtabs_Nx2)) {
        rslt <- NULL
        for (i in names(xtabs_Nx2)) {
          # Namen aus der x-tabelle
          if(use_data_names)  
            param <- paste(names(dimnames(xtabs_Nx2[[i]])), collapse ="/" )
          else param <- i
          
          rslt[[i]] <- cbind(Parameter = param, 
                             predictive_value(xtabs_Nx2[[i]]))
        }
        return(
          prepare_output(do.call(rbind, rslt), 
                         caption = "Diagnostische Kennzahlen")
          )
      }
      else{
        cat("\nIch weis nicht was ich machen soll - ich bekomme 2x2 und 3x2 Tabellen??\n")
        return(list(xtabs_2x2 = xtabs_2x2, xtabs_Nx2 = xtabs_Nx2))
      }
    }
    else{
      # hier kommen einzelne Parameter zu testen der Prävalent 1x2 Tabelle
      return(
        Tbll_praevalenz(x, ...,
                        ci_method =   method,
                        conf.level = conf.level,
                        digits = digits)
      )
    }
    
}




  # das brauch ich vieleicht einmal später
  
  # else if (is.vector(x)) {
  #   if (is.logical(x)) {
  #     referenz <- 2L
  #   }
  #   else if (is.numeric(x)) {
  #     data <- data == 1
  #     referenz <- 2L
  #   }
  #   else{
  #     referenz <- 1L
  #   }
  #   
  #   x <- table(x)
  #   
  #   item <- NULL
  # }
  # else if (is.factor(data)) {
  #   referenz <- 1L
  #   x <- table(x)
  #   item <- NULL
  # }
  # else if (inherits(x, "table")) {
  #   if (is.na(referenz))  referenz <- 1L
  #   item <- NULL
  # }
  # else{
  #   X <- stp25tools2::prepare_data(x,...)
  #   
  #   if(length( names(X$data)) == 1L){
  #     x <- table(X[[1]])
  #     print(x)
  #   }
  #   else return(X)
  #   
  # }



# Tbll epi-test -----------------------------------------------------------


#' @export
Tbll_xtabs_2x2.epi.tests <-
  function(... ,
           select = c("se", "sp", "pv.pos", "pv.neg", "lr.pos", "lr.neg") ,
           digits = 2,
           as_percent = NULL,
           include.n = FALSE,
           epi_data_frame = NULL,
           model_names = NULL) {
    if (is.null(epi_data_frame))
      rst <-  extract_epi(..., select = select)
    else
      rst <- epi_data_frame
    
    # Bestimmen ob als Prozent formatieren
    if (is.null(as_percent)) {
      percent_stats <- c("ap", "tp", "se", "sp", "diag.ac", "pv.pos", "pv.neg")
      as_percent <- rst$stat_code %in% percent_stats
      
      rst$est <- ifelse(as_percent, rst$est * 100, rst$est)
      rst$lower <- ifelse(as_percent, rst$lower * 100, rst$lower)
      rst$upper <- ifelse(as_percent, rst$upper * 100, rst$upper)
      dig_prc <- if (digits < 2)
        0
      else
        digits - 2
      digits <- ifelse(as_percent, dig_prc , digits)
    } else if (as_percent) {
      rst$est <-   rst$est * 100
      rst$lower <-  rst$lower * 100
      rst$upper <-   rst$upper * 100
    }
    
    rst$value <-
      rndr_mean_CI(rst$est, cbind(rst$lower, rst$upper), digits = digits)
    conf.level <-
      paste("Conf. level:", paste(unique(rst$conf.level), collapse = ", "))
    method <- paste("Method:", paste(unique(rst$method), collapse = ", "))
    
    outp <-  c("stat_code", "est", 
               "lower", "upper", "conf.level", "method")
    if (!include.n)
      outp <- c(outp , "n")
    
    result <- rst[-which(names(rst) %in% outp)] |>
      Wide(parameter ~ statistic)
    
    if (!is.null(model_names))
      result$parameter <- model_names
    
    attr(result, "plot") <-  rst
    #attr(result, "conf.level") <- conf.level
    prepare_output(result,
                   caption = "Diagnostic Test",
                   note = paste(conf.level, method))
  }

#' helper
#' @noRd
extract_epi <- function(...,
                        select = c("se", "sp"),
                        dots = NULL
) {
  cat ("\n in extract_epi \n")
  if (is.null(dots)) {
    dots <- list(...)
    # Sicherstellen, dass alle Objekte epi.tests sind
    for (i in seq_along(dots)) {
      if (!inherits(dots[[i]], "epi.tests")) {
        stop("Alle Argumente müssen epi.tests Objekte sein. Argument ",
             i,
             " ist: ",
             class(dots[[i]]))
      }
    }
    # Parameter-Namen extrahieren oder generieren
    nms <- names(dots)
    nms <- test_no_name(nms)
  }
  else{
    nms <- names(dots)
  } 
  # Liste für Ergebnisse
  results <- list()
  
  # Durch jedes epi.tests Objekt iterieren
  for (i in seq_along(dots)) {
    epi_obj <- dots[[i]]
    param_name <- nms[i]
    # Sicherstellen, dass Detail-Informationen vorhanden sind
    if (!"detail" %in% names(epi_obj)) {
      warning("epi.tests Objekt ", param_name, " hat keine 'detail' Komponente")
      next
    }
    
    detail <- epi_obj$detail
    
    # Nur ausgewählte Statistiken extrahieren
    if (!is.null(select)) {
      idx <- detail$statistic %in% select
      if (!any(idx)) {
        warning("Keine der ausgewählten Statistiken in ", param_name, " gefunden")
        next
      }
      detail <- detail[idx, ]
    }
    
    # Durch jede Statistik iterieren
    for (j in seq_len(nrow(detail))) {
      stat <- detail$statistic[j]
      est <- detail$est[j]
      lower <- detail$lower[j]
      upper <- detail$upper[j]
      
      # Vollständigen Statistik-Namen holen
      full_stat_name <- get_full_stat_name(stat)
      
      # Ergebnis zur Liste hinzufügen
      results[[length(results) + 1]] <- data.frame(
        parameter = param_name,
        statistic = full_stat_name,
        stat_code = stat,
        est = est,
        lower = lower,
        upper = upper,
        #  label_est = label_est,
        #  label_ci = label_ci,
        conf.level = epi_obj$conf.level,
        method = epi_obj$method,
        n = epi_obj$tab[3,3],
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Alle Ergebnisse kombinieren
  if (length(results) == 0) {
    stop("Keine Daten konnten extrahiert werden.")
  }
  
  result_df <- do.call(rbind, results)
  # Reihenfolge nach Parameter und Statistik
 # result_df <- result_df[order(result_df$parameter, result_df$statistic), ]
  
  #if( !is.null(model_names)) { result_df$parameter <- model_names}
  # Row names zurücksetzen
  rownames(result_df) <- NULL
  
  return(result_df)
}



# Tbll Pravalence ---------------------------------------------------------


#' Generiere Prävalenz-Statistik-Tabellen
#'
#' Eine flexible Funktion zur Erstellung von formatierte Tabellen mit Prävalenz-
#' Statistiken und Konfidenzintervallen. Die Funktion kann direkt mit ggplot
#' zur Visualisierung verwendet werden.
#'
#' @param ... Eine Formel oder mehrere Variablen zur Analyse. Format:
#'   - `var1 + var2 ~ group_var` für Gruppierung
#'   - `var1[measure, digits] + ...` für spezifische Formatierung
#' @param fun Funktion zur Berechnung (Standard: `praevalenz`)
#' @param key Name der Schlüsselspalte (Standard: "Risikofaktor")
#' @param clopper.pearson Logisch, ob Clopper-Pearson KI berechnet werden soll (Standard: TRUE)
#' @param wilson.score Logisch, ob Wilson-Score KI berechnet werden soll (Standard: FALSE)
#' @param agresti.coull Logisch, ob Agresti-Coull KI berechnet werden soll (Standard: FALSE)
#' @param wald Logisch, ob Wald KI berechnet werden soll (Standard: FALSE)
#' @param conf.level Konfidenzniveau (Standard: 0.95)
#' @param digits Anzahl der Nachkommastellen für die Formatierung (Standard: 1)
#'
#' @return Ein Dataframe mit formatierten Prävalenzstatistiken, der Attribute
#'   für die Visualisierung enthält:
#'   \itemize{
#'     \item `attr(..., "plot")`: Daten für die Visualisierung
#'     \item `attr(..., "conf.level")`: Verwendetes Konfidenzniveau
#'   }
#'
#' @details
#' Die Funktion kombiniert verschiedene KI-Methoden:
#' \itemize{
#'   \item **Clopper-Pearson**: Exakte Methode, immer sicher und korrekt
#'   \item **Wilson-Score**: Ausgezeichnete Approximation, besonders für kleine Stichproben
#'   \item **Agresti-Coull**: Gute Approximation, konservativer als Wilson
#'   \item **Wald**: Einfache Normalapproximation, vermeiden bei Prävalenzen <10\% oder >90\%
#' }
#'
#' Die Ausgabe kann direkt mit \code{\link{gg_praevalenz}} visualisiert werden.
#'
#' @examples
#' \dontrun{
#' library(stp25output2)
#' library(stp25tools2)
#' 
#' # Beispieldaten
#' DF <- data.frame(
#'   Sex = gl(2, 50, labels = c("male", "female")),
#'   Adipositas = factor(rbinom(n = 100, 1, prob = .2), 0:1, c("bösartig", "gutartig")),
#'   Bewegungsmangel = factor(rbinom(n = 100, 1, prob = .4), 0:1, c("bösartig", "gutartig")),
#'   Nikotinabusus = factor(rbinom(n = 100, 1, prob = .3), 0:1, c("bösartig", "gutartig"))
#' )
#' 
#' # 1. Einfache Tabelle
#' DF |> Tbll_praevalenz(
#'   Adipositas,
#'   Bewegungsmangel,
#'   Nikotinabusus,
#'   key = "Risikofaktor"
#' ) |> Output()
#' 
#' # 2. Mit Gruppierung nach Geschlecht
#' DF |> Tbll_praevalenz(
#'   Adipositas + Bewegungsmangel + Nikotinabusus ~ Sex,
#'   key = "Risikofaktor"
#' ) |> Output()
#' 
#' # 3. Mit mehreren KI-Methoden
#' DF |> Tbll_praevalenz(
#'   Adipositas,
#'   Bewegungsmangel,
#'   wilson.score = TRUE,
#'   agresti.coull = TRUE
#' ) |> Output()
#' 
#' # 4. Direkte Visualisierung
#' DF |> Tbll_praevalenz(
#'   Adipositas,
#'   Bewegungsmangel,
#'   Nikotinabusus,
#'   key = "Risikofaktor"
#' ) |> gg_praevalenz(color = "steelblue")
#' }
#'
#' @seealso,
#' \code{\link{gg_praevalenz}} für die Visualisierung
#'
#' @importFrom stats na.omit
#' @export
#' @rdname Tbll_xtabs_2x2
Tbll_praevalenz <- function(x, ...,
                            key = "Risikofaktor",
                            clopper.pearson = TRUE,
                            wilson.score = FALSE,
                            agresti.coull = FALSE,
                            wald = FALSE,
                            conf.level = 0.95,
                            ci_method = NULL,
                            digits = 1L) {
  if(!is.null(ci_method)){
    clopper.pearson <- wilson.score <- agresti.coull <- wald <- FALSE
    if( ci_method == "clopper-pearson") clopper.pearson <-TRUE
    else if( ci_method == "wilson") wilson.score <-TRUE
    else if( ci_method == "agresti") clopper.pearson <-TRUE
  }
  
  result <- Summarise(
    x, ..., key = key,
    fun = function(x)
      Praevalenz(
        x,
        clopper.pearson = clopper.pearson,
        wilson.score = wilson.score,
        agresti.coull = agresti.coull,
        wald = wald,
        conf.level = conf.level,
        digits = digits
      )
  )
  
  attr(result, "plot") <- Summarise(x, ...,
                                    fun = function(x)
                                      praevalenz(
                                        x,
                                        clopper.pearson = clopper.pearson,
                                        wilson.score = wilson.score,
                                        agresti.coull = agresti.coull,
                                        wald = wald,
                                        conf.level = conf.level,
                                        digits = digits
                                      ))
  attr(result, "conf.level") <- conf.level
  prepare_output(result,
                 caption = "Prävalenz", 
                 note = paste(conf.level * 100, "% CI"))
  
}

#' Formatierte Prävalenz mit Konfidenzintervallen
#'
#' Hilfsfunktion zur Berechnung und Formatierung von Prävalenzen mit
#' Konfidenzintervallen als Zeichenketten für die Tabellenausgabe.
#'
#' @examples
#' \dontrun{
#' DF <- data.frame(
#'   sex = gl(2, 50, labels = c("male", "female")),
#'   Adipositas = rbinom(n=100, 1, prob=.2),
#'   Bewegungsmangel =rbinom(n=100, 1, prob=.4),
#'   Nikotinabusus = rbinom(n=100, 1, prob=.3)
#' )
#'
#' # Formatierte Ausgabe
#' DF |> Summarise(
#'   Adipositas,
#'   Bewegungsmangel,
#'   Nikotinabusus,
#'   key = "Risikofaktor",
#'   fun = Praevalenz
#' )
#'
#' # Mit allen KI-Methoden
#' Praevalenz(DF$Adipositas,
#'            clopper.pearson = TRUE,
#'            wilson.score = TRUE,
#'            agresti.coull = TRUE,
#'            wald = TRUE)
#' }
#'
#' @seealso \code{\link{Tbll_praevalenz}} für die Tabellenerstellung,
#'          \code{\link{praevalenz}} für die numerische Berechnung
#'
#' @importFrom stats na.omit
#' @export
#' @rdname Tbll_xtabs_2x2
Praevalenz <- function(x,
                       ...,
                       clopper.pearson = TRUE,
                       wilson.score = FALSE,
                       agresti.coull = FALSE,
                       wald = FALSE,
                       conf.level = 0.95,
                       digits = 1L) {
  x <- na.omit(x)
  rst <-  praevalenz(
    x,
    clopper.pearson = clopper.pearson,
    wilson.score = wilson.score,
    agresti.coull = agresti.coull,
    wald = wald,
    conf.level = conf.level
  )
  
  ans <-
    c(
      'Anteil' = paste0(rst$event_count , "/", rst$total),
      'Praevalenz (%)' =  render_f(rst$praevalenz  * 100, digits)
    )
  if (clopper.pearson) {
    CI.95 <- paste(
      render_f(rst$clopper.pearson_low * 100, digits),
      render_f(rst$clopper.pearson_upr * 100, digits),
      sep = ", "
    )
    ans <- c(ans, "Clopper-Pearson CI" = paste0("[", CI.95, "]"))
  }
  if (agresti.coull) {
    CI.95 <- paste(
      render_f(rst$agresti.coull_low * 100, digits),
      render_f(rst$agresti.coull_upr * 100, digits),
      sep = ", "
    )
    ans <- c(ans, "Agresti-Coull CI" = paste0("[", CI.95, "]"))
  }
  
  if (wald) {
    CI.95 <- paste(render_f(rst$wald_low * 100, digits),
                   render_f(rst$wald_upr * 100, digits),
                   sep = ", ")
    ans <- c(ans, "Wald CI" = paste0("[", CI.95, "]"))
  }
  
  if (wilson.score) {
    CI.95 <- paste(
      render_f(rst$wilson.score_low * 100, digits),
      render_f(rst$wilson.score_upr * 100, digits),
      sep = ", "
    )
    ans <- c(ans, "Wilson-Score CI" = paste0("[", CI.95, "]"))
  }
  
  ans
}


#' Numerische Berechnung von Prävalenzen mit Konfidenzintervallen
#'
#' Berechnet Prävalenzen mit verschiedenen Methoden für Konfidenzintervalle
#' und gibt die Ergebnisse als numerische Werte für die weitere Verarbeitung
#' (z.B. Visualisierung) zurück.
#' 
#' @importFrom stats binom.test qnorm xtabs
#' @importFrom stats na.omit
praevalenz <- function(data,
                       x,
                       clopper.pearson = TRUE,
                       wilson.score = FALSE,
                       agresti.coull = FALSE,
                       wald = FALSE,
                       conf.level = 0.95,
                       digits = 1L,
                       referenz = NA) {
  if (is.vector(data)) {
    if (is.logical(data)) {
      referenz <- 2L
    }
    else if (is.numeric(data)) {
      data <- data == 1
      referenz <- 2L
    }
    else{
      referenz <- 1L
    }
    
    x <- table(data)
    
    item <- NULL
  }
  else if (is.factor(data)) {
    referenz <- 1L
    x <- table(data)
    item <- NULL
  }
  else if (is.data.frame(data)) {
    if (is.character(x))
      x <- formula(paste("~", x))
    item <- all.vars(x)
    if (is.na(referenz)) {
      if (is.logical(data[[item]]))
        referenz <- 2L
      else if (is.numeric(data[[item]])) {
        data[[item]] <- data[[item]] == 1
        referenz <- 2L
      }
    }
    else
      referenz <- 1L
    x <- xtabs(x, data)
    
    
  }
  else if (inherits(data, "table")) {
    x <- data
    if (is.na(referenz))  referenz <- 1L
    item <- NULL
  }
  else{
    stop(" Nur table, xtabs oder data + formula sind erlaubt")
  }
  
  
  
  
  referenz2 <- ifelse(referenz == 1, 2, 1)
  event_name <- names(x)[referenz]
  event_count <- x[referenz]
  non_event_count <- x[referenz2]
  gesamt <- sum(x)
  praevalenz <- event_count / gesamt
  
  
  
  # Z-Wert basierend auf Konfidenzniveau
  z <- qnorm(1 - (1 - conf.level) / 2)
  
  # Ergebnisliste initialisieren
  rslt <- list(
    parameter = item,
    event = event_name,
    event_count = as.numeric(event_count),
    total = as.numeric(gesamt),
    praevalenz = as.numeric(praevalenz)
  )
  
  # Sicherstellen, dass x die richtige Struktur hat
  if (length(x) != 2) {
    if (clopper.pearson) {
      rslt$clopper.pearson_low <- NA
      rslt$clopper.pearson_upr <- NA
    }
    if (wilson.score) {
      rslt$wilson.score_low <-  NA
      rslt$wilson.score_upr <-   NA
    }
    if (agresti.coull) {
      rslt$agresti.coull_low <-  NA
      rslt$agresti.coull_upr <-   NA
    }
    if (wald) {
      rslt$wald_low <-  lower_wald
      rslt$wald_upr <-   upper_wald
    }
    
    warning("Die Tabelle muss genau zwei Kategorien haben.")
  }
  
  
  
  
  
  # 1. Clopper-Pearson (exakte Methode)
  if (clopper.pearson) {
    ci_exakt <- binom.test(event_count, gesamt, conf.level = conf.level)
    rslt$clopper.pearson_low <- ci_exakt$conf.int[1]
    rslt$clopper.pearson_upr <- ci_exakt$conf.int[2]
  }
  
  # 2. Wilson-Score Intervall
  if (wilson.score) {
    # Wilson-Score Formel
    p <- praevalenz
    n <- gesamt
    
    # Zentrierter Wert
    center <- (event_count + z^2 / 2) / (n + z^2)
    
    # Standardfehler für Wilson
    se_wilson <- (z / (n + z^2)) * sqrt(event_count * (1 - p) + z^2 / 4)
    
    # Konfidenzintervall
    lower_wilson <- center - se_wilson
    upper_wilson <- center + se_wilson
    
    # Sicherstellen, dass Werte zwischen 0 und 1 liegen
    lower_wilson <- max(0, lower_wilson)
    upper_wilson <- min(1, upper_wilson)
    
    rslt$wilson.score_low <-  lower_wilson
    rslt$wilson.score_upr <-   upper_wilson
  }
  
  # 3. Agresti-Coull Methode
  if (agresti.coull) {
    # Agresti-Coull adjustierte Werte
    n_tilde <- gesamt + z^2
    p_tilde <- (event_count + z^2 / 2) / n_tilde
    
    # Standardfehler
    se_ac <- z * sqrt(p_tilde * (1 - p_tilde) / n_tilde)
    
    # Konfidenzintervall
    lower_ac <- p_tilde - se_ac
    upper_ac <- p_tilde + se_ac
    
    # Sicherstellen, dass Werte zwischen 0 und 1 liegen
    lower_ac <- max(0, lower_ac)
    upper_ac <- min(1, upper_ac)
    
    rslt$agresti.coull_low <-  lower_ac
    rslt$agresti.coull_upr <-   upper_ac
  }
  
  # 4. Wald-Methode (Normalapproximation)
  if (wald) {
    # Standardfehler für Wald
    se_wald <- sqrt(praevalenz * (1 - praevalenz) / gesamt)
    
    # Konfidenzintervall
    lower_wald <- praevalenz - z * se_wald
    upper_wald <- praevalenz + z * se_wald
    
    # Sicherstellen, dass Werte zwischen 0 und 1 liegen
    lower_wald <- max(0, lower_wald)
    upper_wald <- min(1, upper_wald)
    
    rslt$wald_low <-  lower_wald
    rslt$wald_upr <-   upper_wald
  }
  
  # Als Liste zurückgeben
  if (is.null(item))
    rslt[-1]
  else
    rslt
}




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


# Tbll epi-spezial --------------------------------------------------------


#' @param x  xtabs 3X2
#' @noRd
predictive_value <- function(x){
  # Test ob die Labels übereinstimmen
  nm_2x2 <- intersect(dimnames(x)[[1]], dimnames(x)[[2]])

 # cat("\n  predictive_value  \n\n")
#  print(str(x))
#   print(nm_2x2)
#   print(colnames(x))
#   print(rownames(x))
# print( 
#    list(which(colnames(x) %in% nm_2x2), 
#         which(rownames(x) %in% nm_2x2))
# )
#
#
#print( x[which(colnames(x) %in% nm_2x2), which(rownames(x) %in% nm_2x2)]
   #    
  #     )

  nm_inkonklusiv <- setdiff(dimnames(x)[[1]], dimnames(x)[[2]])
  
  if(length(nm_2x2) != 2) {
    cat("\n Das geht nicht - ich brauce 2x2  + inclusive!\n")
    return(x)
  }
  # 2x2 Tabelle aufsplitten
  x_2x2 <- 
    addmargins(
      x[which(colnames(x) %in% nm_2x2), which(rownames(x) %in% nm_2x2)]
    )
  x_3x2 <- addmargins(x)
  
  # Zwischenwerte berechnen
  total_3x2 <- x_3x2[4,3]
  total_2x2 <- x_2x2[3,3]
  tp  <- x_2x2[1,1] # a <- 123 True Positive: Test positiv UND Krankheit vorhanden
  fp  <- x_2x2[1,2] # b <-  53 False Positive: Test positiv ABER Krankheit nicht vorhanden
  fn  <- x_2x2[2,1] # c <-  42 False Negative: Test negativ ABER Krankheit vorhanden
  tn  <- x_2x2[2,2] # d <-  27 True Negative: Test negativ UND Krankheit nicht vorhanden
  
  # Inkonklusiv   
  ink  <- x_3x2[3,3]
  inkp <- x_3x2[3,1]
  inkn <- x_3x2[3,2]
  
  # Real
  P   <-  x_3x2[4,1] # Real Positive
  N   <-  x_3x2[4,2] # Real Negative
  
  prevalence <-  x_3x2[4, 1] / x_3x2[4, 3]
  
  #ppv <- tp/ (tp + fp)
  #npv <- tn / (fn + tn)
  
  positiv <- tp + fp + inkp   #True Positive+ False Positive
  pv.pos <- tp / positiv   
  negativ <- fn + tn + inkn  # False Negative + True Negative
  pv.neg <- tn / negativ   
  
  # FDR <- 1 - PPV
  # FOR <- 1 - NPV #False omission rate (FOR)
  
  sp <- TNR <-  tn / N   #True negative rate (TNR),
  se <- TPR <-  tp / P  # True positive rate (TPR)
  youden <- sp + se - 1 # Youden Index
  
  FPR <-  fp / N # False positive rate (FPR)
  FNR <-  fn / P  # False negative rate (FNR)
  
  lr.pos <- TPR/FPR
  lr.neg <- FNR/TNR
  
  Inkonklusiv <- ink / total_3x2 
  
 
 # get_full_stat_name
  
  data.frame(
    Kennzahl = get_full_stat_name(
      c("pv.pos", "pv.neg", "incl", "se", "sp", "lr.pos", "lr.neg")
      ),
    Anzahl = c(
      paste0(tp, "/", positiv)  ,
      paste0(tn, "/", negativ),
      paste0(ink, "/", total_3x2),
      paste0(tn, "/", N),
      paste0(tp, "/", P),
      "", ""
    ),
    Prozent = c(paste0(stp25tools2::render_f(
      c(pv.pos, pv.neg, Inkonklusiv, se, sp) * 100, 1
    ), "%"), render_f(c(lr.pos, lr.neg), 2)))
}




#' helper
#' Prüft ob Namen vorhanden sind oder generiert sie
#' @noRd
test_no_name <- function(names, n) {
  if (is.null(names)) {
    return(paste0("Test", seq_len(n)))
  }
  
  # Leere Namen ersetzen
  needs_name <- names == ""
  if (any(needs_name)) {
    names[needs_name] <- paste0("Test", which(needs_name))
  }
  make.unique(names)
}



#' Vollständige Statistik-Namen
#' | Kürzung|Langform| 
#' |--------|--------|  
#' | PPV / NPV|Positive/Negative predictive value| 
#' | LR+ / LR-|Likelihood ratio|Das Plus/ 
#' | Sens. / Spec.|Sensitivity / Specificity| 
#' | DOR|Diagnostic odds ratio| 
#' | NND|Number needed to diagnose| 
#' | D+ / D-|Disease positive / negative| 
#' @noRd
get_full_stat_name <- function(stat) {

  
  # stat_names <- c(
  #   "ap" = "Apparent prevalence",
  #   "tp" = "True prevalence", 
  #   "se" = "Sensitivity",
  #   "sp" = "Specificity",
  #   "diag.ac" = "Diagnostic accuracy",
  #   "diag.or" = "Diagnostic odds ratio",
  #   "nndx" = "Number needed to diagnose",
  #   "youden" = "Youden's index",
  #   "pv.pos" = "Positive predictive value",
  #   "pv.neg" = "Negative predictive value",
  #   "lr.pos" = "Positive likelihood ratio",
  #   "lr.neg" = "Negative likelihood ratio",
  #   "p.rout" = "Proportion of disease positive",
  #   "p.rin" = "Proportion of disease negative",
  #   "p.tpdn" = "True positive / disease negative",
  #   "p.tndp" = "True negative / disease positive", 
  #   "p.dntp" = "Disease negative / true positive",
  #   "p.dptn" = "Disease positive / true negative"
  # )
  
  stat_names <- c(
    "ap"      = "App. Prev.",
    "tp"      = "True Prev.", 
    "se"      = "Sens.",
    "sp"      = "Spec.",
    "diag.ac" = "Acc.",
    "diag.or" = "DOR",
    "nndx"    = "NND",
    "youden"  = "Youden's J",
    "pv.pos"  = "PPV",
    "pv.neg"  = "NPV",
    "lr.pos"  = "LR+",
    "lr.neg"  = "LR-",
    "p.rout"  = "Prop. D+",
    "p.rin"   = "Prop. D-",
    "p.tpdn"  = "TP/D-",
    "p.tndp"  = "TN/D+", 
    "p.dntp"  = "D-/TP",
    "p.dptn"  = "D+/TN",
    "incl"    = "Incl."
  )
  return(stat_names[stat])
  # if (stat %in% names(stat_names)) {
  #   return(stat_names[stat])
  # } else {
  #   return(stat)
  # }
}

# get_full_stat_name(
#   c("pv.pos", "pv.neg", "incl", "se", "sp", "lr.pos", "lr.neg")
# )







