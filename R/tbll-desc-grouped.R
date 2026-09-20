#' @title Tabellen mit deskriptiven Statistiken und univariaten Tests
#'
#' @description
#' Erstellt eine formatierte Übersichtstabelle mit Mittelwerten, Standardabweichungen
#' oder Prozentangaben, getrennt nach Gruppen (z. B. Geschlecht) und stratifiziert
#' nach einer weiteren Variable (z. B. Messzeitpunkt).
#'
#' Abhängig vom Skalenniveau der Variablen werden automatisch geeignete
#' univariate Signifikanztests durchgeführt (z. B. \eqn{t}-Test, Wilcoxon-Test, Chi²-Test).
#'
#' Die Funktion akzeptiert sowohl Formeln als auch Variablennamen als Argumente.
#' Es kann also entweder die lange oder kompakte Schreibweise verwendet werden:
#'
#' ```
#' Tbll_desc_grouped(
#'   h0, bmi, smoker, fev1.pr,
#'   by = ~ sex,
#'   groups = ~ time
#' )
#'
#' # oder äquivalent:
#' Tbll_desc_grouped(
#'   h0 + bmi + smoker + fev1.pr ~ sex | time
#' )
#' ```
#'
#' @details
#' Die Funktion dient der Erstellung von tabellarischen Übersichten über
#' kontinuierliche und kategoriale Variablen, mit optionaler Durchführung
#' univariater Tests zwischen Gruppen innerhalb einer Stratifikation.
#'
#' Die zugrunde liegenden Tests werden automatisch anhand des Datentyps
#' (kontinuierlich, ordinal, kategorial) gewählt:
#'
#' - **Kategoriale Variablen:** Chi²-Test oder Fisher-Test
#' - **Kontinuierliche Variablen:** t-Test, Wilcoxon-Test oder ANOVA, je nach Setting
#' - **Ordinale Variablen:** Kruskal-Wallis-Test
#'
#' Wenn mehrere Tests innerhalb einer Tabelle durchgeführt werden, handelt es sich
#' um *multiples Testen*. Daher **sollte das Ergebnis ausschließlich explorativ interpretiert werden**.
#'
#' **Wichtiger Hinweis:**
#' Bei multiplen Tests steigt die Wahrscheinlichkeit von α-Fehlern.
#' Es wird ausdrücklich empfohlen, die Ergebnisse mit Vorsicht zu interpretieren
#' oder entsprechende Korrekturen (z. B. Bonferroni) nachträglich anzuwenden.
#'
#' @param ... Eine Kombination aus:
#'   - Variablennamen (z. B. `bmi, smoker, fev1.pr`)
#'   - Optionalen Formeln für Gruppierung und Stratifizierung, z. B.
#'     `by = ~ sex` und `groups = ~ time`,
#'     oder in kompakter Form `bmi + smoker ~ sex | time`.
#' @param include.label Logisch; wenn `TRUE` (Standard), werden Variablenlabels
#'   (sofern vorhanden) in der Tabelle angezeigt.
#' @param include.total Logisch; wenn `TRUE`, wird eine zusätzliche Spalte mit Gesamtergebnissen angezeigt.
#' @param include.test Logisch oder Zeichenkette; steuert, ob und welche Tests
#'   durchgeführt werden. Mögliche Werte sind:
#'   `"contest"`, `"cattest"`, `"ordtest"`, `"disttest"`, `"cortest"`, `"notest"`.
#' @param digits Anzahl der Nachkommastellen für numerische Werte.
#' @param use.level Gibt an, welche Ebene bei Faktoren berücksichtigt wird.
#' @param use.duplicated Logisch; steuert, ob doppelte Variablen berücksichtigt
#'
#' Die Argumente \code{by} und \code{groups} definieren:
#' \itemize{
#'   \item \code{by}: Gruppierungsvariable, gegen die getestet wird (z. B. \code{sex})
#'   \item \code{groups}: Stratifizierungsvariable, nach der die Analysen getrennt werden (z. B. \code{time})
#' }
#'
#'  
#' @note
#' Diese Funktion berechnet univariate Signifikanztests für mehrere Variablen,
#' typischerweise getrennt nach Gruppen und Strata. Die resultierenden p-Werte
#' sind nicht multipeltest-korrigiert und dürfen nicht als Beleg für tatsächliche
#' Unterschiede interpretiert werden.
#'
#' Die Ergebnisse sind daher **rein explorativ** zu verstehen.
#' Die Berechnung erfolgt lediglich, weil solche Tests häufig gefordert werden –
#' sie sind jedoch **statistisch nicht empfehlenswert**.
#'
#' Es existiert kein Korrekturverfahren, das die grundlegende Fehlinterpretation
#' der univariaten p-Werte „repariert“. Das Problem liegt nicht in der
#' fehlenden Adjustierung, sondern in der falschen Schlussfolgerung:
#' ein kleiner p-Wert bedeutet nicht automatisch, dass ein relevanter Unterschied
#' oder ein kausaler Zusammenhang besteht.
#'
#' Zur kritischen Diskussion der Problematik vgl. u.a.:
#' - Wasserstein, R. L., & Lazar, N. A. (2016). *The ASA Statement on p-Values: Context, Process, and Purpose.*  
#'   The American Statistician, 70(2), 129–133. <doi:10.1080/00031305.2016.1154108>
#'
#' **Kurz gesagt:** p-Werte können Hinweise auf Unterschiede geben, aber keine
#' verlässliche Evidenz für Relevanz oder Wahrheit liefern. Ihre unreflektierte
#' Nutzung in multiplen univariaten Analysen führt leicht zu Fehlinterpretationen.

#' @return
#' Gibt ein \code{tibble} mit deskriptiven Statistiken und ggf. Signifikanztests zurück.
#' Typischerweise enthält die Ausgabe folgende Spalten:
#' \describe{
#'   \item{Items}{Variablenname oder Label}
#'   \item{Groups}{Name der Vergleichsgruppe}
#'   \item{Strata-Spalten}{Eine Spalte pro Level der Stratifizierung (z. B. \code{Ph1}, \code{Ph2}, \code{Ph3})}
#' }
#'
#' @seealso
#' \code{\link{prepare_data}}, \code{\link{Tbll_desc}}, \code{\link{Tbll_test}}
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Beispiel 1: getrennte Argumente
#' set.seed(42)
#' DF_long <- tibble::tibble(
#'   id      = 1:100,
#'   sex     = factor(sample(c("male", "female"), 100, TRUE)),
#'   time    = factor(sample(c("Ph1", "Ph2", "Ph3"), 100, TRUE)),
#'   bmi     = round(rnorm(100, 25, 4), 1),
#'   smoker  = factor(sample(c("Never", "Former", "Current"), 100, TRUE)),
#'   fev1.pr = round(rnorm(100, 100, 15), 1),
#' )
#' 
#' # Beispiel
#' 
#' DF_long |>
#'   Tbll_desc_grouped(
#'     bmi[1],
#'     smoker,
#'     fev1.pr,
#'     by = ~ sex,
#'     groups = ~ time,
#'     include.test = TRUE
#'   )
#' }
#'
Tbll_desc_grouped <- function(...,
                         include.label = TRUE,
                         include.total = FALSE,
                         include.test = FALSE,
                         digits = NULL,
                         use.level = 1,
                         use.duplicated = FALSE
                       ) {

  X <- prepare_data(...)
  X <- base_setting(X, use.duplicated, include.measure=NULL)

  if (is.character(include.test)) {
    include.test <- gsub("[^[:alpha:]]", "", tolower(include.test))
    which_test <-
      match.arg(include.test,
                c(contest, cattest, notest, ordtest, disttest, cortest))
    X$measure.test <- rep(which_test, length(X$measure.test))
    if (which_test %in% disttest) {
      include.test <- FALSE
      include.normality.tests <- TRUE
    } else{
      include.test <- TRUE
    }
  }


if( is.null(X$group.vars))
  stop("Hier muss zwingend eine by  also by = ~ Group vorhanden sein!")

if(is.null(X$condition.vars))
  stop("Hier muss zwingend eine Guppenvariable also groups = ~ Group vorhanden sein!")


  multiple_test_tbl(
    X$data,
    X$measure.vars,
    X$group.vars,
    X$condition.vars,
    X$measure,
    X$measure.test,
    use.level = use.level,
    X$digits,
    include.total = include.total,
    include.label = include.label,
    include.test =include.test
  )

}




#' Tabel like in Word dokument
#'
#' @param data Long Data.frame
#' @param vars character, measure variable
#' @param groups character, grouping for sig Test
#' @param condition character, conditional variable
#'
#' @return tibble
#'
#' @noRd
multiple_test_tbl <- function(data,
                              vars,
                              groups,
                              condition,
                              measure,
                              measure.test,
                              use.level,
                              digits,
                              include.total,
                              include.label,
                              include.test,
                              name = "Items") {
  data <- data[c(vars, groups, condition)]

  if (include.label)
    lbl <- get_label(data)
  else {
    lbl <- vars
    names(lbl) <- vars

  }
  my_groups <- levels(data[[groups]])
  results <- lapply(vars, function(i) {
    x <- data[[i]]
    if (is.character(x)) {
      tibble(!!name := lbl[i])
    }
    else   {
      tibble::as_tibble(
        calc_desc_pvalue(
          data,
          x = i,
          groups,
          condition,
          lbl[i],
          measure[i],
          measure.test[i],
          use.level,
          digits[i],
          include.test =include.test
        )
      )
    }
  })
  ans <- dplyr::bind_rows(results)

  if (include.total)
    ans
  else
    ans[-ncol(ans)]


}


calc_desc_pvalue <- function(data,
                       x,
                       groups,
                       condition,
                       lbl,
                       measure,
                       measure.test,
                       use.level,
                       digits,
                       include.test = FALSE) {
  stat_rslt <- NULL
  p.value <- NULL
  my_groups = levels(data[[groups]])
  my_condition = levels(data[[condition]])
  length_data <- nrow(data)
  if (measure %in% c("freq", "factor")) {
    measure <- "multi"
    lbl <- paste0(lbl, ": ", levels(data[[x]])[use.level])
  }
  dat <- split(data[c(x, groups)], data[[condition]])



  if (include.test) {
    # p - value
    for (j in my_condition) {
      d <-  na.omit(dat[[j]])
      # include.total
      if (is.null(p.value)) {
        if (length_data < 12)
          p.value <- "-"
        else
          p.value <- auto_test(x = data[[x]],
                               by = data[[groups]],
                               measure.test = measure.test)
      }

      if (nrow(d) < 12)
        p <- "-"
      else
        p <- auto_test(x = d[[1]],
                       by = d[[2]],
                       measure.test = measure.test)
      p.value <- c(p.value, p)
    }

    # reorder p-values
    p_total <- p.value[1]
    p.value <- p.value[-1]
    names(p.value) <- names(dat)
    p.value <- c(Items = "",
                 Groups = "p.value",
                 p.value,
                 Total = p_total)

  }


  # Mean and Percent
  for (k in my_groups) {
    sbst <- data[[groups]] == k
    dat <- split(data.frame(x = data[sbst, x]), data[sbst, condition])

    dat$Total <- na.omit(data[sbst, x])
    stat <- NULL

    for (cndt in names(dat)) {
      if (nrow(na.omit(dat[[cndt]])) == 0)
        tbl_part_i <-  "-"
      else
        tbl_part_i <-
          list_rbind(purrr::pmap(
            list(
              x = dat[[cndt]],
              digits = digits,
              measure = measure,
              row_name = lbl,
              use.level = use.level
            ),
            stp25tools2:::prct_or_mean
          ))$m[1]

      stat <- c(stat, tbl_part_i)
    }
    names(stat) <- names(dat)

    if (is.null(stat_rslt))
      stat_rslt <-  c(Items = as.character(lbl), Groups = k, stat)
    else
      stat_rslt <- rbind(stat_rslt, c(Items = "", Groups = k, stat))

  }

  rbind(stat_rslt, p.value)

}

base_setting <- function(X, use.duplicated, include.measure) {
  if (use.duplicated) {
    # erlaube doppelte parameter
    in_vars <- strsplit(as.character(X$formula)[2L], " \\+ ")[[1L]]
    X$measure.vars <- in_vars
    X$measure.class <- X$measure.class[in_vars]
    X$digits <- X$digits[in_vars]
    X$measure <- X$measure[in_vars]
    X$row_name <- X$row_name[in_vars]
    X$measure.test <- X$measure.test[in_vars]
  }

  if (!is.null(include.measure)) {
    h__h <- X$measure == "character"
    if (any(h__h))
      X$measure[which(h__h)] <- "character"
    X$measure[which(!h__h)] <- include.measure[1]
  }

  X
}


#
#
# if (!("stp25tools2" %in% .packages())) {
#   library(tidyverse)
#   library(stp25tools2)
# }
#
# setwd("C:/Users/wpete/Dropbox/1_Projekte/943_Abdelrahman_Omar")
#
# load("Processed data/AbdelrahmanOmar.Rdata")
#
#
# set_opt(
#   table =    list(
#     wrap = TRUE,
#     #  wrap_results = TRUE,
#     #  include.tabel.number = TRUE,
#     measure.name.m = 'Average',
#     #  measure.name.total = 'Summe',
#     measure.name.statistics = 'P-value',
#     stubhead = 'Items'
#   ),
#   Fstat = list(small.sampel = FALSE, include.statistic = FALSE),
#   median = list(digits = 2, include_name = ''),
#   mean = list(
#     digits = 1,
#     style = 3,
#     include_name = ''
#   ),
#   p = list(digits = 3, mark.sig = TRUE),
#   percent = list(
#     digits = 0,
#     style = 2,
#     percentage_str = '%',
#     null_percent_sign = '.',
#     # include_name = '',
#     include_level_multi = FALSE
#   )
# )
#
#
# setwd("C:/Users/wpete/Dropbox/1_Projekte/943_Abdelrahman_Omar")
# # -- Load Data ----------------------------------------------
# #
# load("Processed data/AbdelrahmanOmar.Rdata")
#
#
#
#
# # Variable names ----------------------------------------------------------
#
# vars <- Cs(
#   h0,
#   sex,
#   Male,
#   bmi,
#   BMI,
#   #age,
#   h1,
#   smoker,
#   #Never.Smokers,Former.Smoker, Current.Smoker,
#   pack,
#   #parental.smoke, eaely.smoke, sec.smoke,
#   # h2,
#   # any.respiratory, emotional.b, daily.b, extertional.b,
#   #  wheezing, cough, sputum, asthma, r.asthma, bronchitis,
#   #  allergy, allergic.b, atopic.b, h3, eosinophiles, feno, ige,
#   h4,
#   fev1.pr,
#   fvc.pr #fev1.l, fvc.l, fev1.fvc.pr, fev1.fvc.l,
#   # h5, histamines, ltra, inhaltives, ics,
#   # h6, prntl.asthma, prntl.copd, prntl.allergies,
#   #  h7, urban, road,
#   #  dust,
#   # ses.score, ses,
#   # activity, Physically.active, nutrition, Healthy.nutrition
# )
#
#
#
#
#
# DF_long |> #filter( sptgroups %in% levels(sptgroups)[1:2] ) |>
#   Tbll_desc_grouped(
#     h0,
#     bmi,
#     #age,
#     #  h1,
#     smoker,
#     #Never.Smokers,Former.Smoker, Current.Smoker,
#     # pack,
#     fev1.pr,
#     by = ~ sex,
#     groups = ~ time
#   )

# A tibble: 7 × 5
# Items                     Groups Ph1          Ph2          Ph3
# <chr>                     <chr>  <chr>        <chr>        <chr>
#   1 "Demographie"             NA     NA           NA           NA
# 2 "BMI"                     male   26 ± 4       26 ± 4       27 ± 4
# 3 ""                        female 24 ± 5       25 ± 5       26 ± 5
# 4 "Smokers: Never Smokers"  male   1166 (45.9%) 1110 (46.1%) 1009 (43.2%)
# 5 ""                        female 1464 (53.8%) 1392 (53.7%) 1233 (50.3%)
# 6 "FEV1, %predGLI, post-BD" male   100.8 ± 12.8 102.0 ± 13.6 104.3 ± 14.2
# 7 ""                        female 102.8 ± 12.6 104.8 ± 13.4 106.5 ± 14.1
