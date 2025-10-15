clnsng <- function(x) {
  x <- gsub("\u00e4", "ae", x)
  x <- gsub("\u00fc", "ue", x)
  x <- gsub("\u00f6", "oe", x)
  x <- gsub("\u00dc", "Ue", x)
  x <- gsub("\u00c4", "Ae", x)
  x <- gsub("\u00d6", "Oe", x)
  x <- gsub("\u00df", "ss", x)
  x <- gsub(" ", "_", x)
  gsub("[^[:alnum:]_ ]", "", x)
}


#' clean_space
#'
#' Leerzeichen entfernen
#'
#' @param x string
#'
#' @noRd
clean_space <- function(x) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  sub(",", ".", x)
}


# Fix and Clean -------------------------------------------------------------------


#' Fix names of data.frames
#'
#' Insbesondere bei `readxl::read_excel` werden die Spaltennamen
#' genau so übernommen, wie sie in Excel vorkommen, also mit allen Sonderzeichen.
#' Diese Funktion packt die Namen in die Labels und bereinigt die Spaltennamen.
#' `clean_names()` enthält die Funktion `clean_character()`.
#' #'
#' @param data Data.frame
#' @param tolower  lower case
#' @param abbreviate,name_length Abbreviate strings to at least name_length characters,
#'
#' @return Data.frame
#' @export
#'
#' @examples
#'
#' # fl_new <- "data/Mai_Markiert_LT_Cases_Jan2018-Dez2022.xlsx"
#' # DF <- readxl::read_excel(fl_new, range = "A1:z15", sheet = 1) |>
#' #   fix_names()
fix_names <- function(data,
                      tolower = TRUE,
                      abbreviate = TRUE,
                      name_length = 25
                      ) {
  labl <- names(data)

  mns <- clean_character(
    labl,
    ascii = TRUE,
    tolower = tolower,
    abbreviate = abbreviate,
    minlength = name_length

  )

  names(labl) <- mns
  names(data) <- mns
  set_label(data, labl)
}

#' Clean and standardize character strings
#'
#' This function converts character strings into standardized,
#' machine-friendly names (e.g., for variable or file names).
#'
#' Steps:
#' - Transliterates characters to ASCII
#' - Converts to lower case (optional)
#' - Applies user-defined replacements
#' - Removes unwanted characters
#' - Collapses multiple separators
#' - Optionally abbreviates long names
#' - Ensures uniqueness (optional)
#'
#' @param x Character vector to be cleaned.
#' @param tolower Logical, convert to lower case? (default: TRUE)
#' @param abbreviate Logical, abbreviate names to minlength? (default: FALSE)
#' @param minlength Minimum length for abbreviations. (default: 12)
#' @param ascii Logical, Transliterates characters to ASCII
#' @param replace Named character vector of replacements.
#' @param unique Logical, make names unique? (default: TRUE)
#'
#' @return A cleaned character vector.
#' @examples
#' clean_character(c("Ölß", "Extra lang mit Müll!!!", "#1", "äüö_ ga"))
#'
#' @export
clean_character <- function(x,
                            tolower    = TRUE,
                            abbreviate = FALSE,
                            minlength  = 12,
                            ascii = FALSE,
                            replace    = c(
                              "%" =  "_pct",
                              "#" =  "_cnt",
                              "\u00A8" = "",
                              "&+" = "_and_",
                              "@+" = "_at_",
                              "\u00e4" = "ae",
                              "\u00fc" = "ue",
                              "\u00f6" = "oe",
                              "\u00dc" = "Ue",
                              "\u00c4" = "Ae",
                              "\u00d6" = "Oe",
                              "\u00df" = "ss"
                            ),
                            unique     = TRUE) {

  # transliterate to ASCII
  if (ascii) x <- stringi::stri_trans_general(x, "latin-ascii")

  # lower case if requested
  if (tolower) {
    x <- tolower(x)
  }

  # apply custom replacements
  if (!is.null(replace)) {
    x <- stringr::str_replace_all(x, replace)
  }

  # replace all non-alphanumeric with "."
  x <- stringr::str_replace_all(x, "[^a-zA-Z0-9]+", ".")
  # collapse multiple "." into single
  x <- stringr::str_replace_all(x, "\\.+", ".")
  # remove leading/trailing "."
  x <- stringr::str_replace_all(x, "^\\.|\\.$", "")

  # abbreviate if requested
  if (abbreviate) {
    x <- abbreviate(x, minlength = minlength, named = FALSE, strict = TRUE)
    x <- stringr::str_replace_all(x, "\\.+", ".")
    x <- stringr::str_replace_all(x, "^\\.|\\.$", "")
  }



  # make unique if requested
  if (unique) {
    x <- make.names(x, unique = TRUE)
  }

  # trim whitespace
  x <- trimws(x)

  return(x)
}










# Re-encode ---------------------------------------------------------------




#' Re-encode factor levels in a data frame
#'
#' @param data data.frame
#' @param from Source encoding
#' @param to Target encoding
#' @noRd
factor_levels_encoding <- function(data, from = "UTF8", to = "latin1") {
  fact_idx <- vapply(data, is.factor, logical(1))
  data[fact_idx] <- lapply(
    data[fact_idx],
    \(f) { levels(f) <- iconv(levels(f), from, to); f }
  )
  data
}

#' @noRd
character_encoding <- function(data,
                               from = "UTF8",
                               to = "latin1") {
  fact_idx <- vapply(data, is.character, logical(1))
  data[fact_idx] <- lapply(
    data[fact_idx],
    \(f) { iconv(f, from, to) }
  )
  data

  # myFact <-
  #   which(sapply(data, function(x)
  #     inherits(x, "character")) == TRUE)
  # if (length(myFact) > 0) {
  #   for (i in myFact)
  #     data[, i] <-  iconv(data[, i], from , to)
  # }
  # data
}



#' @noRd
names_label_encoding <- function(data,
                                 from = "UTF8",
                                 to = "latin1") {
  nms <-   iconv(names(data), from , to)
  lbl <- get_label(data)
  lbl <-   iconv(lbl, from , to)
  names(lbl) <- nms
  names(data) <- nms
  lbl <- set_label(data, lbl)
  data
}



