#' Extended CONSORT utilities 
#' 
#' 
#' prepare_consort, add_box2, add_side_box2, add_split2
#'
#' Erweiterte Hilfsfunktionen für das Paket **consort**, insbesondere:
#'
#' * `prepare_consort()` – Formatieren von Label-/n-Daten in konsistenter CONSORT-Form
#' * `add_box2()` – erweiterte Box-Funktion mit Listen-Unterstützung und automatischer n-Propagation
#' * `add_side_box2()` – erweiterte Side-Box-Funktion inkl. Listen-Unterstützung
#' * `add_split2()` – erweiterte Split-Funktion
#'
#' Alle Funktionen unterstützen den **Pipe-Workflow** (`|>`) und führen
#' eine konsistente Verwaltung von `n.total` intern fort.
#'
#' @details
#'
#' ## `prepare_consort()`
#' Hilfsfunktion, welche die Eingangswerte (`label`, `n`) in formatierten Text
#' für CONSORT-Boxen umwandelt.  
#'
#' Unterstützt:
#'
#' * Boxen (`box`)
#' * Side-Boxen (`side_box`)
#' * Split-Boxen (`split`)
#'
#' Numerische Werte (`n`) werden automatisch:
#' * formatiert (`big.mark`, `decimal.mark`)
#' * berechnet, wenn der erste Wert `NA` ist
#'
#' ---
#'
#' ## `add_box2()`
#' Erweiterte Version von `consort::add_box()`, unterstützt zusätzlich:
#'
#' * Übergabe von `label` **oder Listen von Labels**
#' * Übergabe von `n` **oder Listen von n-Vektoren**
#' * automatisches Formatieren über `prepare_consort()`
#' * Berechnung und Speicherung von `node1$n.total` für spätere Schritte
#'
#' ---
#'
#' ## `add_side_box2()`
#'
#' Entspricht der Logik von `add_side_box()`, erweitert um:
#'
#' * Listen für `label`
#' * Listen für `n`
#' * automatische Summenberechnung für ersten NA-Eintrag
#'
#' ---
#'
#' ## `add_split2()`
#'
#' Erweiterte Form von `add_split()`, nutzt ebenfalls `prepare_consort()`  
#' und propagiert `n.total`.
#'
#' ---
#'
#' @param prev_box Ein bestehendes CONSORT-Objekt (oder `NULL` für den Start).
#' @param ... Übergabe weiterer Argumente, insbesondere:
#' * `label`: Character-Vektor oder Liste von Character-Vektoren
#' * `n`: Numerischer Vektor oder Liste numerischer Vektoren
#' @param txt Optionaler Text, der `label`/`n` überschreibt.
#' @param n_box Glue-Template zur Anzeige von `n` in Boxen.
#' @param n_split Glue-Template zur Anzeige von `n` in Side-Boxes oder Splits.
#' @param big.mark Tausendertrennzeichen.
#' @param decimal.mark Dezimaltrennzeichen.
#' @param just Textausrichtung (wie in `consort`).
#' @param text_width Breite des Textblocks (wie in `consort`).
#' @param side Seite für Side-Boxen (`left`, `right` etc.).
#'
#' @return
#' * `prepare_consort()` gibt eine benannte Liste formatierten Textes zurück.  
#' * `add_box2()`, `add_side_box2()`, `add_split2()` geben ein erweitertes
#'   CONSORT-Objekt mit gespeicherten `n.total` zurück.
#' @seealso
#' \code{\link[consort]{consort_plot}}, 
#' \code{\link[stp25tools2]{consort_filters}}
#' @examples
#' \dontrun{
#'
#' options(txt_gp = grid::gpar(cex = 0.8))
#'
#' add_box2(
#'   label = "Participants\nin the LEAD study",
#'   n = 15056
#' ) |>
#'   add_side_box2(
#'     label = c("Excluded", " - dropouts", " - invalid/missing SPT"),
#'     n = c(NA, 8881, 650)
#'   ) |>
#'   add_box2(
#'     label = "Valid / non-missing SPT"
#'   ) |>
#'   add_split2(
#'     label = c("Treatment", "Control"),
#'     n = c(100, 120)
#'   ) |>
#'   add_side_box2(
#'     label = list(
#'       c("Excluded:",
#'         "\u2022 MRI not collected",
#'         "\u2022 Tissues not collected",
#'         "\u2022 Other"),
#'       c("Excluded (n=7):",
#'         "\u2022 MRI not collected",
#'         "\u2022 Tissues not collected")
#'     ),
#'     n = list(
#'       c(NA, 3, 4, 8),
#'       c(NA, 3, 4)
#'     )
#'   ) |>
#'   add_box2(
#'     label = list("Analysed", "Analysed"))
#'
#' }
#'
#' @name consort2
NULL


#' @rdname consort2
#' @export
#' @importFrom glue glue
prepare_consort <- function(... ,
                            n_box = "n = {n}",
                            n_split = ": {n}",
                            big.mark = ",",
                            decimal.mark = getOption("OutDec")) {
  dots <- list(...)
  new_list <- list()
  for (i in seq_along(dots)) {
    j <- names(dots)[i]
    nm <- paste0("m", i)
    if (grepl("side_box", j)) {
      nm <- paste0(nm, "_side_box")
      
      spl <- NULL
      if (is.na(dots[[i]]$n[1]))
        dots[[i]]$n[1] <- sum(dots[[i]]$n, na.rm = TRUE)
      for (k in seq_along(dots[[i]]$label)) {
        sp  <-  glue(
          dots[[i]]$label[k],
          n_split,
          n = format(
            dots[[i]]$n[k],
            decimal.mark = decimal.mark,
            big.mark =
              big.mark,
            scientific = FALSE
          ),
          .sep = ""
        )
        spl <- c(spl, sp)
      }
      new_list[[nm]] <- spl
    }
    else if (grepl("label_box", j)) {
      nm <- paste0(nm, "_label_box")
      new_list[nm] <- dots[[i]]$label
    }
    else if (grepl("split", j)) {
      nm <- paste0(nm, "_split")
      spl <- NULL
      if (is.na(dots[[i]]$n[1]))
        dots[[i]]$n[1] <- sum(dots[[i]]$n, na.rm = TRUE)
      for (k in seq_along(dots[[i]]$label)) {
        sp  <-  glue(
          dots[[i]]$label[k],
          n_box,
          n = format(
            dots[[i]]$n[k],
            decimal.mark = decimal.mark,
            big.mark =
              big.mark,
            scientific = FALSE
          ),
          .sep = "\n"
        )
        spl <- c(spl, sp)
      }
      new_list[[nm]] <- spl
    }
    else{
      nm <- paste0(nm, "_box")
      new_list[nm] <- glue(
        dots[[i]]$label,
        n_box,
        n = format(
          dots[[i]]$n,
          decimal.mark = decimal.mark,
          big.mark = big.mark,
          scientific = FALSE
        ),
        .sep = "\n"
      )
    }
    
  }
  
  
  new_list
}

#' @rdname consort2
#' @export
#' @import consort
#' @import grid
add_box2 <- function(prev_box = NULL,
                     ...,
                     txt = "Hallo Welt!",
                     n_box =   "n = {n}",
                     n_split = ": {n}",
                     big.mark =  ",",
                     decimal.mark =  getOption("OutDec"),
                     just = c("center", "left", "right"),
                     text_width = NULL
) {
  
  # Ausgangswert für n_total aus vorheriger Box
  if (is.null(prev_box)) 
    n_total <- NA
  else
    n_total <- prev_box$node1$n.total
  
  #cat("add_box2:", n_total, "\n")
  
  dots <- list(...)
  
  if (length(dots) > 0) {
    
    # =======================================
    # 1) FALL: label ist LISTE → mehrere Boxen erzeugen
    # =======================================
    if (is.list(dots$label)) {
      
      txt <- NULL
    
        if (is.null(dots$n)) {
          dots$n <- n_total
          
        }
          
          
      for (i in seq_along(dots$label)) {
        
        label_i <- dots$label[[i]]
        n_i      <- dots$n[[i]]
       
            
        
        # neuen Textblock erzeugen
        one_block <- prepare_consort(
          box = list(label = label_i, n = n_i),
          n_box = n_box,
          n_split = n_split,
          big.mark = big.mark,
          decimal.mark = decimal.mark
        )
        
        # jeder Block ist Länge 1
        txt <- append(txt, unlist(one_block))
      }
      
      # n_total berechnen (≈ Summe über erste n-Werte)
      n_total <- sum(sapply(dots$n, function(v) v[1]), na.rm = TRUE)
      
    } else {
      
      # =======================================
      # 2) FALL: label NICHT Liste (Standardverhalten)
      # =======================================
      
      if (is.null(dots$n)) {
        dots$n <- n_total
      } else {
        n_total <- sum(dots$n, na.rm = TRUE)
      }
      
      txt <- prepare_consort(
        box = dots,
        n_box = n_box,
        n_split = n_split,
        big.mark = big.mark,
        decimal.mark = decimal.mark
      )
      
      txt <- unlist(txt)
    }
  }
  
  # =======================================
  # BOX ZEICHNEN
  # =======================================
  box <- consort::add_box(
    prev_box,
    txt = txt,
    just = just,
    text_width = text_width
  )
  
  box$node1$n.total <- n_total
  
  box
}


 

#' @rdname consort2
#' @export
add_side_box2 <- function(prev_box ,
                     ...,
                     txt = "Hallo Welt!",
                     n_box =   "n = {n}",
                     n_split = ": {n}",
                     big.mark =  ",",
                     decimal.mark =  getOption("OutDec"),
                     side = NULL,  
                     just = c("center", "left", "right"),
                     text_width = NULL
) {
  n_total <- prev_box$node1$n.total
 # cat("add_side_box2:", n_total, "\n")
  dots <- list(...)
 
  if (length(dots) > 0) {
    if(is.list(dots$label)) {
      txt <- NULL
      for( i in seq_along(dots$label)){
        
        n_l <- dots$n[[i]]
        label_l <- dots$label[[i]]
        
        if (is.na(n_l[1])) 
          n_l[1] <- sum(n_l, na.rm = TRUE)
        
       n_total[i] <- n_total[i] - n_l[1]
        
        label_l <- prepare_consort(
          side_box  = list(label=label_l, n = n_l),
          n_box = n_box,
          n_split = n_split,
          big.mark = big.mark,
          decimal.mark = decimal.mark
        )
        # hier kommen mehere Zeilen
        label_l <- paste(unlist(label_l), collapse = "\n")
        txt <- append(txt, label_l)
      }
    }
    else{
    # Erster Eintarag ist 'Excluded' und der kann hier berechnet werden
    if (is.na(dots$n[1])) 
      dots$n[1] <- sum(dots$n, na.rm = TRUE)
    
    n_total <- n_total - dots$n[1]

    txt <- prepare_consort(
      side_box  = dots,
      n_box = n_box,
      n_split = n_split,
      big.mark = big.mark,
      decimal.mark = decimal.mark
    )
    # hier kommen mehere Zeilen
    txt <- paste(unlist(txt), collapse = "\n")
    }
  }
  
  box <- consort::add_side_box(
    prev_box,
    txt = txt,
    side = side,
    text_width = text_width)
  
  box$node1$n.total <- n_total

  box
}

 

#' @rdname consort2
#' @export
add_split2 <- function(prev_box ,
                          ...,
                          txt = "Hallo Welt!",
                          n_box =   "n = {n}",
                          n_split = ": {n}",
                          big.mark =  ",",
                          decimal.mark =  getOption("OutDec"),
                          side = NULL,  
                          just = c("center", "left", "right"),
                          text_width = NULL
) {
  n_total <- prev_box$node1$n.total
  #cat("add_split:", n_total, "\n")
  dots <- list(...)
  
  if (length(dots) > 0) {
  
    # Erster Eintarag ist 'Excluded' und der kann hier berechnet werden
    #if (is.na(dots$n[1])) 
    #  dots$n[1] <- sum(dots$n, na.rm = TRUE)
    
    n_total <-  dots$n
    
    txt <- prepare_consort(
      split  = dots,
      n_box = n_box,
      n_split = n_split,
      big.mark = big.mark,
      decimal.mark = decimal.mark
    )
    
  }
  
  box <- consort::add_split(
    prev_box,
    txt = txt,
    just = just,
    text_width = text_width)
  
  box$node1$n.total <- n_total
  
  box
}



 

  


                                        