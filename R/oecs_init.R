#'
#' Set up an new screening.
#' @name oecs_init
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


# Limer API to LimeSurvey from GitHub
if (!require("limer")) {
  if (!require("devtools")) {
    install.packages("devtools")
    library("devtools")
  }
  install_github("O-ECS/limer")
}

oecs_init <- function(survey_url,survey_username,survey_password,env_var=FALSE){

  # Load library
  library(limer)

  if(env_var==TRUE){
    survey_url <- Sys.getenv("LIME_API")
    survey_username <- Sys.getenv("LIME_USERNAME")
    survey_password <- Sys.getenv("LIME_PASSWORD")
  }

  # Setup API details
  options(lime_api = survey_url)
  options(lime_username = survey_username)
  options(lime_password = survey_password)

  # Do stuff with LimeSurvey API
  get_session_key()  # Log in

}
