#'
#' Set up an new screening.
#' @name oecs_create
#' @param adress dataframe with adresses of the childs
#' @param scales vector with strings, choosing the scales ("sprachstand","umwelt","who")
#' @param activate boolean for activateing the survey
#' @param save boolean saving files
#' @param path_participants string path and filename of participants
#' @param path_settings string path and filename of important settings (survey id, salt of hash)
#' @param survey_name string name survey
#'
#' @return Number of the Survey-ID.
#' @examples
#' add(1, 1)
#' @export


## Library ================
# CRAN
library(openssl)
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



oecs_create <- function(adress,scales,activate = TRUE, save = TRUE, path_participants = "teilnehmerliste.csv", path_settings = "einstellungen.csv", survey_name = "Ein guter Start im Kindergarten") {
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

  # Test scales
  if (!is.vector(scales)) {
    stop("scales must be a vector")
  }
  for (i in 1:length(scales)) {
    if (scales[i] == "sprachstand" || scales[i] == "who" || scales[i] == "umwelt")
    {
      } else {
      stop("scales not correct defined")
    }
  }

  ## Umfrage erstellen ==========

  #Umfrage anlegen

  umfragedateiname <- paste0("survey_",scales[1],".lss")

  survey_id <- call_limer(method = "import_survey",
                          params = list(sImportData = base64(read_file(system.file("extdata", umfragedateiname, package="oecs")))[1],
                                        sImportDataType ="lss",
                                        sNewSurveyName = survey_name
                          ))

  #Umfrage Scalen hinzufuegen
  if (length(scales) > 1){
  for (ii in 1:(length(scales)-1)) {
    umfragegruppename <- paste0("group_",scales[ii+1],".lsg")

    call_limer(method = "import_group",
               params = list(iSurveyID = survey_id,
                             sImportData = RCurl::base64(read_file(system.file("extdata", umfragegruppename, package="oecs")))[1],
                             sImportDataType ="lsg"
               ))
  }
  }


  # Benutzer anlegen =================
  #Token aktivieren
  call_limer(method = "activate_tokens",
             params = list(iSurveyID = survey_id
             )
  )

  #Hash erzeugen
  salt <- paste0(do.call(paste0, replicate(5, sample(LETTERS, 1, TRUE), FALSE)), sprintf("%04d", sample(9999, 1, TRUE)), sample(LETTERS, 1, TRUE))
  adress$SHA256 <- sha256(paste0(adress$firstname,adress$name,adress$street,adress$city,salt))

  # Benurtzer in LimeSurvey anlegen und Token generieren
  antwortlimer <- call_limer(method = "add_participants",
                                  params = list(iSurveyID = survey_id,
                                                aParticipantData = data.frame(lastname = as.character(adress$SHA256))
                                  ))
  adress$token <- antwortlimer$token

## weitere Einstellungen =================

  # Umfrage aktivieren
  if(activate == TRUE){
    call_limer(method = "activate_survey",
             params = list(iSurveyID = survey_id
             )
      )
  }

 ## Speichern ===================

  if(save == TRUE){
    write.csv(adress, file = path_participants, row.names = FALSE)
    write.csv(data.frame(survey_id = survey_id,salt=salt), file = path_settings, row.names = FALSE)

  }

  return(list(survey_id = survey_id,salt=salt,adress=adress))
}
