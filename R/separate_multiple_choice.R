#' Extract Multiple Responses from Data Frame
#'
#' Processes columns with multiple responses using commas and parentheses as separators
#' and creates binary variables for each unique response.
#'
#' @param data A data frame containing the multiple response columns
#' @param sep Character to split responses (default: ",")
#' @param rm_string Suchstring zu ersaetzen mit dem Seperator
#' @param as_logical output logical or integer (0,1)
#' @return A data frame with binary variables for each unique response
#' @export
#'
#' @importFrom stringr str_split str_remove_all str_trim
#' @importFrom tibble as_tibble
#'
#' @examples
#'
#' dat <-  data.frame(
#' Q1 = c(12345, 3, 35, 134, 12,
#'        13,  14, 12, 123, 123,
#'        13,  14, 12, 123,123),
#' Q2 = c(
#'   "Alm Dudler, Mineral, Cola, Bier, Wein",
#'   " Cola, ",
#'   "Cola,  Wein",
#'   "Alm Dudler,  Cola, Bier",
#'   "Alm Dudler, Mineral",
#'
#'   "Alm Dudler,  Cola",
#'   "Alm Dudler,  Bier,",
#'   "Alm Dudler, Mineral",
#'   "Alm Dudler, Mineral, Cola",
#'   "Alm Dudler, Mineral, Cola",
#'
#'   "Alm Dudler, Cola",
#'   "Alm Dudler,  Bier",
#'   "Alm Dudler, Mineral, ",
#'   "Alm Dudler, Mineral, Cola",
#'   "Alm Dudler, Mineral, Cola"
#' )
#' )
#'
#' extract_multiple_responses(dat[1])
#' extract_multiple_responses(dat[2] )
#'
#'
#' # library(UpSetR)
#' # library(ggplot2)
#'
#' # Daten vorbereiten für UpSetR
#' # DF <- extract_multiple_responses(dat[2] )
#' #upset_data <- as.data.frame(lapply(DF, as.integer))
#' #upset(upset_data, nsets = ncol(upset_data), order.by = "freq")
extract_multiple_responses <- function(data,
                                       sep = ",",
                                       rm_string = NULL,
                                       as_logical = TRUE) {
  if(is.vector(data))
    data <- data.frame(q=data)

  result <- data.frame(row.names = 1:nrow(data))
  all_responses_level <- NULL

  for (col_name in names(data)) {
      col_data <- data[[col_name]]
      if (is.numeric(col_data)) {
        warning("Wenn als Codierung Zahlen verwendet werden geht das nur mit maximal 11 Attributen!")
        col_data <-  make_multi_string(col_data)
        sep <- ","
      }
      else{
        col_data <-  as.character(col_data)
      }

      # Entferne Klammern und teile bei Kommas
      if(!is.null(rm_string))
        clean_text <- gsub(rm_string, sep, col_data)

      responses <- strsplit(col_data, sep)

      # Sauubere die Responses
      responses <- lapply(responses, function(x) {
        x <- trimws(x)
        x <- x[x != ""]
        x <- x[x != " "]
        unique(x)
      })

      # Finde alle unique Responses
      all_responses <- unique(unlist(responses))
      all_responses <- all_responses[!is.na(all_responses)]

      # Erstelle Spalten für jede Response
      for (resp in all_responses) {
        var_name <- paste(col_name,
                          abbreviate(tolower(resp), 5) , sep = ".")
        var_name <- clean_character(var_name,
                        tolower = TRUE,
                        abbreviate = FALSE,
                        ascii = FALSE,
                        unique=TRUE)
        result[[var_name]] <-
          sapply(responses, function(x) {
            if (as_logical)
              ifelse(is.na(x[1]), NA, resp %in% x)
            else
              ifelse(is.na(x[1]), NA, as.integer(resp %in% x))
          })
      }
      all_responses_level <- c(all_responses_level, all_responses )
  }
  names(all_responses_level)<- names(result)
  set_label(result, all_responses_level)
  }


#' make_multi_string(c(12,9,10, 11,123))
#' @noRd
make_multi_string <- function(x) {
    x <- as.character(x)
    x <- gsub("10", "a", x)
    x <- gsub("11", "b", x)
    gsub("(.)\\B", "\\1,", x)
  }



