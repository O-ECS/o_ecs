#'
#' Set up an new screening.
#' @name oecs_evaluate
#' @param adress dataframe with adresses of the childs
#' @param survey_id vector with strings, choosing the scales ("sprachstand","umwelt","who")
#' @param save boolean saving files
#' @param path_answers string path and filename of participants
#' @param path_pending string path and filename of important settings (survey id, salt of hash)
#' @param path_not_assignable string path and filename of important settings (survey id, salt of hash)
#'
#' @return List with 3 dataframes (answers, pending, not assignable)
#' @examples
#' add(1, 1)
#' @export


## Library ================
# CRAN
library(RCurl)
library(readr)


# Limer API to LimeSurvey from GitHub
if (!require("limer")) {
  if (!require("devtools")) {
    install.packages("devtools")
    library("devtools")
  }
  install_github("O-ECS/limer")
}
library(limer)



oecs_evaluate <- function(adress,survey_id,save=T, path_answers="antworten.csv", path_pending="ausstehend.csv", path_not_assignable="nichtzuordenbar.csv") {
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
  # Test survey_id
  if (!is.numeric(survey_id)) {
    stop("survey_id must be a integer")
  }


  ## Daten laden und Verknüpfen =====
  data <- limer::get_responses(iSurveyID= survey_id, sCompletionStatus= 'complete', sResponseType='long')
  auswertung <- merge(x = adress, y = data, all.x = TRUE, by.x = "token", by.y = "token")
  auswertung[auswertung=="<NA>"] <- NA

  ##  filtern
  antworten <- auswertung[!is.na(auswertung$submitdate),]
  ausstehend <- auswertung[is.na(auswertung$submitdate),]

  ## Auswertungen ======

  antworten_export <- NULL
  antworten_export$firstname <- antworten$firstname
  antworten_export$name <- antworten$name
  antworten_export$street <- antworten$street
  antworten_export$city <- antworten$city

    ## Scala Sprachstand
  if (!is.null(antworten$Sprachstand1)&!is.null(antworten$Sprachstand2)&!is.null(antworten$Sprachstand3)&!is.null(antworten$Sprachstand4)){
    antworten_export$Sprachstand1 <- ordered(antworten$Sprachstand1, c("nein","ein wenig","ziemlich gut","sehr gut"))
    antworten_export$Sprachstand2 <- ordered(antworten$Sprachstand2, c("nein","ein wenig","ziemlich gut","sehr gut"))
    antworten_export$Sprachstand3 <- ordered(antworten$Sprachstand3, c("nein","ein wenig","ziemlich gut","sehr gut"))
    antworten_export$Sprachstand4 <- ordered(antworten$Sprachstand4, c("es spricht noch kein Deutsch","seit einigen Monaten","seit ungefähr einem Jahr","seit mehreren Jahren"))
    antworten_export$SprachstandSumme <- (as.numeric(antworten_export$Sprachstand1)-1)+(as.numeric(antworten_export$Sprachstand2)-1)+(as.numeric(antworten_export$Sprachstand3)-1)+(as.numeric(antworten_export$Sprachstand4)-1)

  }

  #Ausstehende auslesen =========================
  ausstehend_export <- NULL
  ausstehend_export <- data.frame(ausstehend$token,
                      ausstehend$firstname,
                      ausstehend$name,
                      ausstehend$street,
                      ausstehend$city,
                      ausstehend$SHA256)
  colnames(ausstehend_export) <- c("token","firstname","name","street","city","SHA256")

 ## Speichern ===================

  if(save == TRUE){
    write.csv(antworten_export, file = path_answers, row.names = FALSE)
    write.csv(ausstehend_export, file = path_pending, row.names = FALSE)

  }

  return(list(answers = antworten_export,pending = ausstehend_export))
}
