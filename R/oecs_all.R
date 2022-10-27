#'
#' Set up an new screening.
#' @name oecs_all
#' @param file Path to the .xlsx-file. If the file does not exist, it will be created.
#'
#' @return Number of the Survey-ID.
#' @examples
#' add(1, 1)
#' @export


oecs_all <- function(file = "oecs.xlsx"){

  if(file.exists(file)) {
    wb <- openxlsx::loadWorkbook(file = file)

    # Steuerungscode auslesen
    steuerung <- openxlsx::read.xlsx(wb,"Steuerung",colNames = FALSE)[1,1]

    # Anlegen der Umfrage
    if(steuerung==1){
      #Adressdaten auslesen
      adressdaten <- openxlsx::read.xlsx(wb,"Adressen")
      adressdaten <- cbind(adressdaten$firstname,adressdaten$name,adressdaten$additional,adressdaten$street,adressdaten$city,adressdaten$birthday)
      adressdaten <- as.data.frame(adressdaten)
      colnames(adressdaten) <- c("firstname","name","additional","street","city","birthday")


      #Einstellung auslesen
      Einstellungen <- openxlsx::read.xlsx(wb,"Einstellungen")

      oecs::oecs_init(Einstellungen$Werte[7],Einstellungen$Werte[8],Einstellungen$Werte[9])

      Skalen <- NULL
      if(Einstellungen$Werte[2]=="Y"){Skalen <- c(Skalen,"sprachstand") }
      if(Einstellungen$Werte[3]=="Y"){Skalen <- c(Skalen,"who") }
      if(Einstellungen$Werte[4]=="Y"){Skalen <- c(Skalen,"foerderung") }
      if(Einstellungen$Werte[5]=="Y"){Skalen <- c(Skalen,"lebenslage") }
      if(Einstellungen$Werte[6]=="Y"){Skalen <- c(Skalen,"science") }
      Umfragename <- Einstellungen$Werte[1]

      feedback <- oecs::oecs_create(adress = adressdaten, scales = Skalen, save = FALSE, survey_name = Umfragename)

      openxlsx::writeData(wb,"Einstellungen",x = rbind(c("Umfrage-ID",feedback$survey_id,"Umfrage-ID, wird automatisch vergeben"),c("Salt",feedback$salt,"Wert um Hash zu erzeugen")),startCol = 1,startRow = 11,colNames = FALSE)
      openxlsx::writeData(wb,"Adressen",as.data.frame(feedback$adress),startCol = 1,startRow = 1)
      openxlsx::protectWorksheet(wb,"Adressen")

      openxlsx::addStyle(wb, "Einstellungen", style = openxlsx::createStyle(locked = TRUE), rows = 2:10, cols = 2)
      openxlsx::writeData(wb,"Steuerung",x = 2,startCol = 1,startRow = 1,colNames = FALSE)
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

      print("Umfrage wurde angelegt.")

    }

    #Rücklauf und Auswertung
    if(steuerung==2){

      Einstellungen <- openxlsx::read.xlsx(wb,"Einstellungen")
      oecs::oecs_init(Einstellungen$Werte[7],Einstellungen$Werte[8],Einstellungen$Werte[9])

      adressdaten <- openxlsx::read.xlsx(wb,"Adressen")
      adressdaten <- cbind(adressdaten$firstname,adressdaten$name,adressdaten$additional,adressdaten$street,adressdaten$city,adressdaten$birthday,adressdaten$SHA256,adressdaten$token)
      adressdaten <- as.data.frame(adressdaten)
      colnames(adressdaten) <- c("firstname","name","additional","street","city","birthday","SHA256","token")

      responses <- limer::call_limer(method = "get_summary",
                        params = list(iSurveyID =  as.integer(Einstellungen$Werte[10])
                        ))
     if(responses$completed_responses!=0){
      rucklaufdaten <- oecs::oecs_evaluate(adress = adressdaten, survey_id = as.integer(Einstellungen$Werte[10]),save = FALSE)

      if(length(openxlsx::sheets(wb))>5){
        openxlsx::removeWorksheet(wb,"Antworten")
        openxlsx::removeWorksheet(wb,"Ausstehend")
      }

      antworten <- as.data.frame(rucklaufdaten$answers)
      openxlsx::addWorksheet(wb, "Antworten")
      openxlsx::writeData(wb,"Antworten",antworten,startCol = 1,startRow = 1)
      openxlsx::protectWorksheet(wb,"Antworten")

      ausstehend <- as.data.frame(rucklaufdaten$pending)
      openxlsx::addWorksheet(wb, "Ausstehend")
      openxlsx::writeData(wb,"Ausstehend",ausstehend,startCol = 1,startRow = 1)
      openxlsx::protectWorksheet(wb,"Ausstehend")

      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

      print("Abgleich durchgeführt.")


     }else{
       print("Abgleich durchgeführt. Noch keine Antworten!")
     }
    }


  } else {

    # Erstellung Steuerungdatei
    wb <- openxlsx::createWorkbook(
      creator = "oecs",
      title = "Steuerung"
    )

    openxlsx::addWorksheet(wb, "Anleitung")
    openxlsx::writeData(wb,
                        "Anleitung",
                        c("1) Prüfe unter Einstellungen den Namen der Umfrage",
                          "2) Fülle unter Einstellungen aus, welche Fragen im Fragebogen abgefragt werden sollen.",
                          "3) Wähle unter Einstellungen aus, ob die Befragten die Möglichlkeiten haben sollen, ihre Daten für Forschungszwecke zur Verfügung zu stellen.",
                          "4) Gib in in der Arbeitsmappe Adressdaten die Adressen an (ersetze die Beispiele). Die Adressdaten werden nicht ins Internet gestellt sondern bleiben ausschliesslich lokal gespeichert. Der Datenschutz ist somit gewährleistet."),
                        startCol = 1,
                        startRow = 1,
    )

    openxlsx::addWorksheet(wb, "Einstellungen")
    openxlsx::writeData(wb,
              "Einstellungen",
              data.frame(Einstellungen=c("Umfragename",
                                         "Sprachstandserfassung",
                                         "WHO5",
                                         "Foerderung",
                                         "Lebenslage",
                                         "Forschung",
                                         "API URL",
                                         "Username",
                                         "Password"),
                         Werte=c("Ein guter Start im Kindergarten",
                                 "Y",
                                 "N",
                                 "Y",
                                 "N",
                                 "Y",
                                 "https://example.ch/index.php/admin/remotecontrol",
                                 "Benutzer",
                                 "password"),
                         Bemerkungen=c("Hier kann ein Name der Umfrage eingegenen werden. Dieser kann später immer noch im Limesurvey angepasst werden.",
                                       "Y, wenn diese Fragebatterie aufgenommen werden soll, oder N, falls nicht.",
                                       "Y, wenn diese Fragebatterie aufgenommen werden soll, oder N, falls nicht.",
                                       "Y, wenn diese Fragebatterie aufgenommen werden soll, oder N, falls nicht.",
                                       "Y, wenn diese Fragebatterie aufgenommen werden soll, oder N, falls nicht.",
                                       "Y, wenn die Teilnehmenden ihre Daten anonym zu Forschungszwecken freigeben können, falls N, haben sie diese Möglichkeit nicht.",
                                       "URL zur API der LimeSurvey-Installation",
                                       "Benutzername zur LimeSurvey-Installation",
                                       "Passwort zur LimeSurvey-Installation")),
              startCol = 1,
              startRow = 1,
    )
    openxlsx::addStyle(wb, "Einstellungen", style = openxlsx::createStyle(locked = FALSE), rows = 2:10, cols = 2)
    openxlsx::protectWorksheet(
      wb,
      "Einstellungen",
      protect = TRUE,
      password = NULL,
      lockSelectingLockedCells = FALSE,
      lockSelectingUnlockedCells = FALSE,
      lockFormattingCells = FALSE,
      lockFormattingColumns = TRUE,
      lockFormattingRows = TRUE,
      lockInsertingColumns = NULL,
      lockInsertingRows = NULL,
      lockInsertingHyperlinks = NULL,
      lockDeletingColumns = TRUE,
      lockDeletingRows = TRUE,
      lockSorting = TRUE,
      lockAutoFilter = TRUE,
      lockPivotTables = TRUE,
      lockObjects = TRUE,
      lockScenarios = TRUE
    )
    openxlsx::addWorksheet(wb, "Eckwerte")
    openxlsx::writeData(wb,
                        "Eckwerte",
                        data.frame(Variabeln=c("Alter","Stadt/Region","Organisation","Kontakt Name","Kontakt Telefon","Kontakt Email","Grund der Befragung"),Werte=c("","","","","","",""),Bemerkungen=c("Altersspanne der Kinder","Stadt oder Region zur geografischen Verordnung","Name der befragenden Organisation","Vorname und Name der verantwortlichen Person","Telefonnummer der Kontaktperson","Email-Adresse der Kontaktperson","Hintergründe zur Befragung (z.B. Einschätzung vor Schuleintritt, Freiwilligkeit, ob alle oder nur ausgewählte Personen befragt werden etc.")),
                        startCol = 1,
                        startRow = 1,
    )

    openxlsx::addWorksheet(wb, "Adressen")
    openxlsx::writeData(wb,
                        "Adressen",
                        data.frame(firstname=c("Hans","Max"),name=c("Muster","Frisch"),additional=c("",""),street=c("Beispielstrasse 5","Dürenmattstrasse 5"),city=c("8057 Zürich","4057 Basel"),birthday=c("1985-09-14","2009-01-02")),
                        startCol = 1,
                        startRow = 1,
    )

    openxlsx::addWorksheet(wb, "Steuerung",visible = FALSE)
    openxlsx::writeData(wb,
                        "Steuerung",
                        x = 1,
                        startCol = 1,
                        startRow = 1,
                        colNames = FALSE
    )
    openxlsx::protectWorksheet(wb,
                               "Steuerung")
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

    print(".xlsx-Steuerungsdatei angelegt.")
  }





}
