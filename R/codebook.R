#' Codebook
#' 
#' Speichern des Codebook
#' @name codebook
#' @param file File (xlsx)
#' @export
#'
#' @examples
#'  \dontrun{
#' 
#' DF <- data.frame(
#'   BMI = c(1, 2, 3, 1, 2, 3),
#'   WHtR = gl(2, 3, label = c("Amy", "Bob")),
#'   WHtR_1 = c(9, 7, 6, 8, 6, 9),
#'   bildprof = c(6, 7, 8, 5, 6, 7)
#'   ) |>
#'   Label(
#'         BMI = "Body-Mass-Index",
#'         WHtR =  "Waist-Height-Ratio",
#'         WHtR_1 = "Waist-Height-Ratio"
#'         )
#'  
#'   save_codebook(DF, "codebook.xlsx" )
#'  
#' #' Labels und Factoren ubernehmen
#' DF2 <-   use_codebook(
#'   DF,
#'   file = "codebook.xlsx",
#'   sheet.codebook = 2
#' )
#' 
#' #' Nur die Labels  ubernehmen
#' DF3 <-   use_codebook(
#'   DF,
#'   file = "codebook.xlsx",
#'   sheet.codebook = 2,
#'   value.labels = NULL
#' )
#' 
#' #' Strucktur aus einem anderen data.frame uebernehmen
#' DF1 <- use_codebook(DF, DF2)
#' 
#' #' Daten und Strucktur im Excell-File
#' DF4 <-  use_codebook(file = "codebook.xlsx")
#' #readxl::read_excel("codebook.xlsx", sheet = "codebook")
#' #'
#' }
#'  
save_codebook <- function(x,
                          file = "codebook.xlsx",
                          include.data = FALSE)  {
  cat("\n", paste(getwd(), file, sep = "/"), "\n")
  cdb <- codebook(x)
  
  if (include.data)
    writexl::write_xlsx(list(data = x, codebook = cdb), path = file)
  else
    writexl::write_xlsx(list(codebook = cdb), path = file)
  
  invisible(cdb)
}



#' @rdname codebook
#' @description  codebook: Create a structure of the data as data.frame.
#' @param x  Data.frame
#' @return data.frame
codebook <- function(x) {
  lbl <- get_label(x)
  value_lbl <- sapply(x, function(y) {
    if (is.factor(y))
      paste("factor:", paste(levels(y), collapse = " | "))
    else
      paste(class(y)[1], ":", sep="")
  })
  
  data.frame(names = names(lbl),
             label = lbl,
             value.labels = value_lbl)

}


#' @rdname codebook
#' @description  use_codebook: Restore the structure of the data from the 
#' Excel file created by the Codebook function.
#'
#' @param data data.frame, default is NULL
#' @param codebook data.frame codebook = codebook(DF), default is NULL
#' @param sheet.data,sheet.codebook  file= "demo.xlsx", sheet.data = 1, sheet.codebook = 2,
#' @param names,label,value.labels Variablen NNamen im codebook
#'
#' @return data.frame or tibble
#' @export
use_codebook <-
  function(data = NULL,
           data_codebook = NULL,
           file = "demo.xlsx",
           sheet.data = "data",
           sheet.codebook = "codebook",
           names = "names",
           label = "label",
           value.labels = "value.labels") {
    if (is.null(data)) {
      cat("\n(1)\nUse data from file", file, "\n")
      data <-
        readxl::read_excel(file, sheet = sheet.data)
     # print(head(data))
    } else{
      cat("\n(1)\nI am using the provided data.\n")
      print(deparse(substitute(data)))
      print(head(data))
      if (is.null(data_codebook) & (file == "demo.xlsx")) {
        stop("\nI need the data_codebook either as file location or a  data.frame to copy!\n")
      }
    }
    
    
    if( is.character(data_codebook) ) {
      cat("\nI assume that ", data_codebook, " is the file for the codebook.\n")
      file <- data_codebook
      data_codebook <- NULL
    }
    
    
    if (is.null(data_codebook)) {
      cat("\n(2)\nLabel and levels from file:\n", file, "\n")
      cdbk <- try (readxl::read_excel(file, sheet = sheet.codebook))
      if (inherits(cdbk, "try-error")) {
        cat(
          "\nYou have to tell me the correct sheet of the codebook!\nI am now trying to guess the page.\n\n"
        )
        cdbk <- readxl::read_excel(file, sheet = 1)
      }
      print(head(cdbk))
    }
    else if (is.data.frame(data_codebook)) {
      cdbk <- codebook(data_codebook)
    }
    
    label <- cdbk[[label]]
    names(label) <- cdbk[[names]]
    
    if (!is.null(value.labels)) {
      
      cat("\n(3) I am going to work on the factors (value.labels)\n")
      print(cdbk[[value.labels]])
      cat("????\n")
      
      
      for (i in grep("factor\\: ", cdbk[[value.labels]])) {
        fct <- cdbk[[value.labels]][i]
        fct <- gsub("factor\\: ", "", fct)
        fct <- unlist(stringr::str_split(fct, " \\| "))
        
        old <- data[[cdbk[[names]][i]]]
        if (is.character(old))  {
          cat("\n", cdbk[[names]][i] , ": character -> factor")
          data[[cdbk[[names]][i]]] <- factor(old, fct)
        }
        else if (is.factor(old)) {
          if (identical(levels(old), fct)) {
            cat("\n", cdbk[[names]][i] , ": no change")
          }
          else{
            cat("\n", cdbk[[names]][i] , ": factor -> factor")
            if (nlevels(old) != length(fct)) {
              cat("\n\nFehler!!\n\n Old: ",
                  nlevels(old),
                  "New: ",
                  length(fct),
                  "\n")
              cat("\n Old: \n")
              print(levels(old))
              cat("\n New: \n")
              print(fct)
              cat("\n")
            }
            data[[cdbk[[names]][i]]] <-
              factor(as.numeric(old), seq_len(nlevels(old)), fct)
          }
        }
        else if (is.numeric(old)) {
          cat("\n", cdbk[[names]][i] , ": numeric -> factor")
          data[[cdbk[[names]][i]]] <-
            factor(old, seq_len(nlevels(old)), fct)
        }
      }
      
      for (i in grep("numeric\\:", cdbk[[value.labels]])) {
        if (!is.numeric(data[[cdbk[[names]][i]]])) {
          cat("\n", cdbk[[names]][i] , ": numeric -> factor")
          data[[cdbk[[names]][i]]] <-
            as.numeric(as.character(data[[cdbk[[names]][i]]]))
        }
        cat("\n", cdbk[[names]][i] , ": no change")
      }
      
    }
    cat("\n\n(4) \nI am in the process of label restoration.\n")
    
   # leehre Labels entfernen
    set_label(data, label[-which(is.na(label) | label == "")])
  }


## #
## 
## @rdname codebook
## @description  save_data: Write an Excel or SPSS file.
## 
## @return Invisibly returns the combined data frame that is written to
##         the csv-file.
## 
## @param x data.frame
## 
## @param file file to write to, or just file name (to write to working directory).
## @param sep The field separator string. In some Western European locales, Excel
##            uses a semicolon by default, while in other locales the field
##            separator string in Excel is a comma.
## @param row.names,include.codebook logical include something
## @importFrom utils write.csv2 write.csv
## @importFrom  writexl write_xlsx
## @export
## @examples
## 
## \dontrun{
## dat <-  Label(
##   data.frame(
##   month = rep(1:3, 2),
##   student = rep(c("Amy", "Bob"), each = 3),
##   A = c(19, 27, 16, 28, 10, 29),
##   B = c(6.45, 7.47, 8.76, 5.01, 6.91, 3.47)
##   ),
##   month="Monat", student="Student", A= "Anzahl", B= "B-Score" )
## 
## 
## ## Speichern der Kompletten Strucktur
## save_data(dat, "demo.xlsx")
## }
## 
## save_data <- function(x,
##                       file,
##                       sep = ",",
##                       row.names = FALSE,
##                       include.codebook = TRUE) {
##   if (include.codebook)
##     cdb <- codebook(x)
##   
##   
##   # check if file extension exists
##   has.extension <- (regexpr("\\.[^\\.]*$", file) != -1)
##   if (!has.extension)
##     file <- paste0(file, ".csv")
##   
##   # check fir valid file extention
##   dot.start <- regexpr("\\.[^\\.]*$", file) + 1
##   ext <- tolower(substring(file, dot.start, nchar(file)))
##   
##   
##   # tell user what's going on...
##   cat("Writing file to:\n")
##   cat(normalizePath(
##     path = file,
##     winslash = "/",
##     mustWork = FALSE
##   ))
##   cat("\n\n")
##   if (ext == "csv") {
##     if (include.codebook)
##       cat("\n Codebook geht nur mit .xlsx!\n")
##     
##     # write to excel
##     if (sep == ";")
##       utils::write.csv2(x, file = file, fileEncoding = "UTF-8")
##     else
##       utils::write.csv(x, file = file, fileEncoding = "UTF-8")
##   }
##   else if (ext == "xlsx" | ext == "xls") {
##     if (include.codebook)
##       writexl::write_xlsx(list(data = x, codebook = cdb), path = file)
##     else
##       writexl::write_xlsx(x, path = file)
##     
##   }
##   else{
##     stop("No valid file extention.")
##   }
##   
##   # return data frame
##   invisible(x)
## }
## 
##  
## 




 