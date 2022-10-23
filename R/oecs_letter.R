#'
#' Set up an new screening.
#' @name oecs_evaluate
#' @param adress dataframe with adresses of the childs
#' @param survey_url vector with strings, choosing the scales ("sprachstand","umwelt","who")
#' @param path_letter string path (no filename) to save the files for the letters
#'
#' @return List with 3 dataframes (answers, pending, not assignable)
#' @examples
#' add(1, 1)
#' @export


## Library ================
# CRAN

library(readr)
library(stringr)
library(qrcode)

oecs_letter <- function(adress,survey_url, path_letter="brief/") {
  ## Test Eingangsvariablen   =======================
  # Test adress
  if (!is.data.frame(adress)) {
    stop("adress must be a dataframe")
  }
  if (is.null(adress$firstname)) {
    stop("column firstname is not defined")
  }

if (is.null(adress$name)) {
  stop("column name is not defined")
}
if (is.null(adress$street)) {
  stop("column street is not defined")
}
if (is.null(adress$city)) {
  stop("column city is not defined")
}
if (is.null(adress$token)) {
    stop("column token is not defined")
  }
  # Test survey_url
  if (is.null(survey_url)) {
    stop("survey_url must be defined")
  }

# Pfad für Ablage erstellen

  dir.create(path_letter, showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(path_letter,"qr-codes/"), showWarnings = FALSE, recursive = TRUE)

#  Vorlage einlesen

  letter_head <- readr::read_file(system.file("extdata", "letter_head.html", package="oecs"))
  vorlage <- readr::read_file(system.file("extdata", "letter_content.html", package="oecs"))
  letter_end <- readr::read_file(system.file("extdata", "letter_end.html", package="oecs"))

brief <- NULL
  for (i in 1:dim(adress)[1]) {
    brief_inhalt <- stringr::str_replace_all(vorlage,"--FIRSTNAME--",adress$firstname[i])
    brief_inhalt <- stringr::str_replace_all(brief_inhalt,"--NAME--",adress$name[i])
    brief_inhalt <- stringr::str_replace_all(brief_inhalt,"--STREET--",adress$street[i])
    brief_inhalt <- stringr::str_replace_all(brief_inhalt,"--CITY--",adress$city[i])
    brief_inhalt <- stringr::str_replace_all(brief_inhalt,"--TOKEN--",adress$token[i])

    png(paste0(path_letter,"qr-codes/",adress$token[i],".png"))
    plot(qrcode::qr_code(paste0(survey_url,adress$token[i])))
    dev.off()
    brief <- paste0(brief,brief_inhalt)
  }

brief <- paste0(letter_head,brief,letter_end)
readr::write_file(brief,paste0(path_letter,"/index.html"))

logo <- readr::read_file_raw(system.file("extdata", "letter_logo.png", package="oecs"))
readr::write_file(logo,paste0(path_letter,"letter_logo.png"))

printcss <- readr::read_file(system.file("extdata", "letter_print.css", package="oecs"))
readr::write_file(printcss,paste0(path_letter,"letter_print.css"))
print(paste0("Go to the directory ",getwd(),"/",path_letter," and open index.html"))
  return()
}
