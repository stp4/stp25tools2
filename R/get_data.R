#' Import a dataset from various formats or from inline text
#'
#' @description
#' Reads data from a file (`.xlsx`, `.csv`, `.sav`) or from a text string directly embedded in an R script.
#'
#' @param file Path to a data file (`.xlsx`, `.sav`, `.csv`) or a character string containing table-like text.
#' @param na.strings Character vector of strings to be interpreted as missing values (default: `c("NA", "na")`).
#' @param dec Decimal separator (default: `.`).
#' @param sep Field separator character (default: `";"` for CSV; whitespace for inline text).
#' @param as_tibble Logical; if `TRUE`, convert output to tibble, otherwise to data.frame.
#'
#' @param sheet Sheet number or name (Excel only, passed to [readxl::read_excel()]).
#' @param range Cell range to read (Excel only).
#' @param skip Number of lines to skip before reading data.
#'
#' @param header Logical; whether the first line of a CSV file contains column names.
#' @param fill Logical; for CSV import, fill rows with unequal length.
#' @param comment.char Character: comment character for CSV files.
#' @param quote Quoting character for CSV files.
#'
#' @param encoding File encoding (SPSS only, passed to [haven::read_sav()]).
#' @param user_na Logical; for SPSS files, if `TRUE`, keep user-defined missing values as labelled objects.
#'
#' @param cleanup.names Logical; if `TRUE`, clean column names via `fix_names()`.
#' @param cleanup.encoding Logical; if `TRUE`, run `names_label_encoding()`, `character_encoding()`, and `factor_levels_encoding()` on the data.
#' @param cleanup.factor Logical; if `TRUE`, clean factor variables.
#' @param from Source encoding (used with `cleanup.encoding`).
#' @param to Target encoding (used with `cleanup.encoding`).
#'
#' @param tabel_expand Logical; if `TRUE`, expand frequency tables to long format via `expand_table()`.
#' @param id.vars Variable indices or names to be treated as ID variables when `tabel_expand = TRUE`.
#' @param value Name of the output value column when `tabel_expand = TRUE`.
#'
#' @param label Integer or character: position or name of a row containing variable labels (0 = no labels).
#' @param ... Additional arguments passed to underlying read functions or to `expand_table()`.
#'
#' @return A data frame or tibble containing the imported dataset.
#' @note
#'
#'  xlsx: readxl::read_excel(file, sheet, skip, range)
#'
#'  csv:   read.table(file, header, sep, quote, dec, na.strings, skip, fill, comment.char)
#'
#'  sav:  haven::read_sav(file, encoding,  user_na)
#'
#'  text: read.text2(file, dec)
#'
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom haven read_sav
#'
#' @examples
#' dat <- get_data("
#' sex treatment control
#' m  2 3
#' f  3 4
#' ",
#'                 tabel_expand = TRUE,
#'                 id.vars = 1)
#'
#' xtabs(~ sex + value, dat)
#'
#' dat <- get_data(
#'   "
#' sex treatment  neg  pos
#' f   KG          3   3
#' f   UG          4   5
#' m   KG          5   4
#' m   UG          4   2
#' ",
#'   tabel_expand = TRUE,
#'   id.vars = 1:2,
#'   value = "befund"
#' )
#'
#' ftable(xtabs(~ sex + treatment + befund, dat))
#'
#' \dontrun{
#' if (file.exists("inst/extdata/dummy.csv")) {
#'   get_data("inst/extdata/dummy.csv", dec = ",", na.strings = "-", skip = 1, label = 1)
#'   get_data("inst/extdata/dummy.xlsx", na.strings = "-")
#'   get_data("inst/extdata/dummy.sav")
#' }
#' }
get_data <- function(
    file = NA_character_,
    na.strings = NULL,
    dec = ".",
    sep = ";",
    as_tibble = TRUE,

    # Excel
    sheet = 1,
    range = NULL,
    skip = 0,

    # CSV
    header = TRUE,
    fill = TRUE,
    comment.char = "",
    quote = "\"",

    # SPSS
    encoding = NULL,
    user_na = FALSE,

    # Cleanup
    cleanup.names = TRUE,
    cleanup.encoding = FALSE,
    cleanup.factor = TRUE,
    from = "UTF8",
    to = "latin1",

    # Table expand
    tabel_expand = FALSE,
    id.vars = 1,
    value = "value",

    # Misc
    label = 0,
    ...
) {


  if(!is.character(file)) {
    # Get the data that was used to fit the model
   return( insight::get_data(file) )
  }


  read_xlsx <- function() {
    if (is.null(na.strings)) {
      data <-
        readxl::read_excel(file,
                           sheet = sheet,
                           skip = skip,
                           range = range)
    }
    else{
      data <-
        readxl::read_excel(
          file,
          sheet = sheet,
          skip = skip,
          range = range,
          na = na.strings
        )
    }
    if (cleanup.names) {
      fix_names(data)
    }
    else {
      data
    }
  }

  read_csv <- function() {
    data <-
    read.table(
      file = file,
      header = header,
      sep = sep,
      quote = quote,
      dec = dec,
      na.strings = na.strings,
      skip = skip,
      check.names = FALSE,
      fill = fill,
      comment.char = comment.char
    )

    if(cleanup.names) {
      fix_names(data)
    }
    else {data}

      # if( skip > 0 & label > 0){
      # label <-
      #   read.table(
      #     file = file,
      #     header = FALSE,
      #     sep = sep,
      #     quote = quote,
      #     skip = label - 1,
      #     nrows = 1
      #   )
      #
      #   if (any(is.na(label))) {
      #     isna <- which(is.na(label))
      #
      #     label[isna] <- names(data)[isna]
      #   }
      #   names(label) <-  names(data)
      #   data <- set_label2(data, label)
      # }
     # data
  }

  read_sav <- function() {
    data <-
      haven::read_sav(file, encoding = encoding, user_na = user_na)
    data <- haven::as_factor(data)

    if (cleanup.names) {
      fix_names(data)
    } else  {
      data
    }
  }

  data <- data.frame(NULL)



  if (length(grep("\n", file)) > 0) {
    # cat("\n\nread-text\n")
    # workaround
    # ich habe nachträglich die tabs eingebaut!
    if( sep == ";" ) sep <- ""
    data <-
      read_text_table(file,
                 na.strings= na.strings,
                 sep = sep,
                 dec = dec)
  }
  else {
    if (file.exists(file)) {
      # cat("\n\nread-file\n")
      file_info <- file.info(file)[c(1, 4, 5)]
      ext <- tolower(tools::file_ext(file))

      data <-
        switch(ext,
               sav = read_sav(),
               xlsx = read_xlsx(),
               csv =  read_csv(),
               stop("Unknown extension '.", ext, "'", call. = FALSE))

      if (cleanup.encoding) {
        data <- names_label_encoding(data)
        data <- character_encoding(data)
        data <- factor_levels_encoding(data)
      }
    }
    else {
      stop("Kein file: ", file, " vorhanden!")
    }
  }


  if (tabel_expand) {
    data <-
      expand_table(data, id.vars = id.vars, value = value, ...)
  }

  if (as_tibble & !tibble::is_tibble(data))
    data <- tibble::tibble(data)
  else if (!as_tibble & tibble::is_tibble(data))
    data <- data.frame(data)
  else NULL

  data
}

# Read Text ---------------------------------------------------------------




#' Read Data Frame from Text String
#'
#' @description
#' Reads a data frame from a character string containing table-like text.
#'
#' @param text_input Character string containing the table data.
#' @param na.strings Character vector of strings to interpret as NA.
#' @param sep Field separator character (default: whitespace).
#' @param dec Decimal point character.
#' @param strings_as_factors Logical; convert character vectors to factors.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' read_text_table("
#' a b c
#' 1 2 3
#' 4 5 6
#' 7 8 9
#' ")
#'
#' #   a b c
#' # 1 1 2 3
#' # 2 4 5 6
#' # 3 7 8 9
read_text_table <- function(
    text_input,
    na.strings = c("NA", "na"),
    sep = "",       # default: whitespace
    dec = ".",
    strings_as_factors = TRUE
) {
  # Replace tabs with spaces if sep is a single space
  if (sep == " ") {
    text_input <- gsub("\t", " ", text_input, fixed = TRUE)
  }

  # Use textConnection safely
  con <- textConnection(text_input)
  on.exit(close(con), add = TRUE)

  # Read the table
  df <- read.table(
    con,
    header = TRUE,
    sep = sep,
    dec = dec,
    na.strings = na.strings,
    stringsAsFactors = strings_as_factors
  )

  df
}



# Expand Data -------------------------------------------------------------



#' Tabel To Expand Data Frame
#'
#' @param data data.frame
#' @param id.vars idetifier
#' @param value name of value
#' @param as.is,dec,na.strings  an  type.convert(...)
#'
#' @return data.frame
#'
#' @examples
#'
#' dat <- expand_table(
#'   data.frame(
#'     sex = c("f", "f", "m", "m"),
#'     treatment = c("KG", "UG", "KG" , "UG"),
#'     neg  = c(3, 4, 5, 4),
#'     pos = c(3, 5, 4, 2)
#'   )
#'   ,
#'   id.vars = 1:2,
#'   value = "befund"
#' )
#'
#' xtabs(~ befund + sex + treatment, dat)
#'
#' @noRd
expand_table <-
  function(data,
           id.vars,
           value = "value",
           na.strings = "NA",
           as.is = FALSE,
           sep = " ",
           dec = ".") {
    if (is.character(data))
      data <-
        read_text_table(data,
                   na.strings= na.strings,
                   sep = sep,
                   dec = dec)  # nur wenn die Funktion dierekt aufgerufen wird moeglich


    if (!is.numeric(id.vars))
      id.vars <- which(names(data) %in% id.vars)

    dataMatrix <- as.matrix(data[, -id.vars])

    if (length(id.vars) == 1) {
      dimnames(dataMatrix)[[1]] <- data[, 1]
      data2 <-
        expand_dft(as.data.frame(as.table(dataMatrix),
                                 stringsAsFactors = TRUE),
                   na.strings,
                   as.is,
                   dec)
      colnames(data2)[2] <- value
    }
    else {
      dimnames(dataMatrix)[[1]] <- apply(data[, id.vars], 1, paste,
                                         collapse = "+")
      data2 <-
        expand_dft(as.data.frame(as.table(dataMatrix),
                                 stringsAsFactors = TRUE),
                   na.strings,
                   as.is,
                   dec)
      colnames(data2)[2] <- value

      data2 <-
        cbind(reshape2_colsplit(data2[, 1], "\\+", names(data)[id.vars]),  data2)
    }

    data2 <-
      as.data.frame(lapply(data2, function(x)
        if (is.character(x))
          factor(x)
        else
          x))
    if (length(id.vars) == 1)  {
      names(data2)[1] <- names(data)[id.vars]
    }

    data2
  }


#' Helper for expand_table
#'
#' http://wiki.stdout.org/rcookbook/Manipulating%20data/Converting%20between%20data%20frames%20and%20contingency%20tables/
#' @noRd
expand_dft <-
  function(x,
           na.strings = "NA",
           as.is = FALSE,
           dec = ".") {
    # Take each row in the source data frame table
    #  and replicate it using the Freq value
    data <- sapply(1:nrow(x),
                   function(i)
                     x[rep(i, each = x$Freq[i]),],
                   simplify = FALSE)

    # Take the above list and rbind it to create a single data
    # Also subset the result to eliminate the Freq column
    data <-
      subset(do.call("rbind", data), select = -Freq)

    # Now apply type.convert to the character coerced factor columns
    # to facilitate data type selection for each column
    for (i in 1:ncol(data)) {
      data[[i]] <-
        type.convert(
          as.character(data[[i]]),
          na.strings = na.strings,
          as.is = as.is,
          dec = dec
        )
    }
    data
  }



#' Split a vector into multiple columns
#'
#' Stolen from reshape2
#'
#' reshape2 ist ein altes packages und womoewglich bald osolet!
#'
#' @param string character vector or factor to split up
#' @param pattern regular expression to split on
#' @param names names for output columns
#' @importFrom stringr str_split_fixed
#' @importFrom plyr alply
#'
#' @examples
#'
#' #'\dontrun{
#'
#' x <- c("a_1", "a_2", "b_2", "c_3")
#' vars <- reshape2_colsplit(x, "_", c("trt", "time"))
#' vars
#' str(vars)
#' }
#'
#' @noRd
reshape2_colsplit<-
  function (string, pattern, names)
  {
    vars <- stringr::str_split_fixed(string, pattern, n = length(names))
    df <- data.frame(plyr::alply(vars, 2, type.convert, as.is = TRUE),
                     stringsAsFactors = FALSE)
    names(df) <- names
    df
  }








#' Helper for expand_table
#'
#' http://wiki.stdout.org/rcookbook/Manipulating%20data/Converting%20between%20data%20frames%20and%20contingency%20tables/
#' @noRd
expand_dft <-
  function(x,
           na.strings = "NA",
           as.is = FALSE,
           dec = ".") {
    # Take each row in the source data frame table
    #  and replicate it using the Freq value
    data <- sapply(1:nrow(x),
                   function(i)
                     x[rep(i, each = x$Freq[i]),],
                   simplify = FALSE)

    # Take the above list and rbind it to create a single data
    # Also subset the result to eliminate the Freq column
    data <-
      subset(do.call("rbind", data), select = -Freq)

    # Now apply type.convert to the character coerced factor columns
    # to facilitate data type selection for each column
    for (i in 1:ncol(data)) {
      data[[i]] <-
        type.convert(
          as.character(data[[i]]),
          na.strings = na.strings,
          as.is = as.is,
          dec = dec
        )
    }
    data
  }











# Bitte einen besseren Vorschlag für die Funktion factor_levels_encoding
#
#
# factor_levels_encoding <- function(data,
#                                    from = "UTF8",
#                                    to = "latin1") {
#   myFact <-
#     which(sapply(data, function(x)
#       inherits(x, "factor")) == TRUE)
#   if (length(myFact) > 0) {
#     for (i in myFact)
#       levels(data[, i]) <-  iconv(levels(data[, i]), from , to)
#   }
#   data
# }

