#' Generate tab XXI
#'
#' Yes, generate tab XXI
#'
#' @inheritParams NGERFigAndeler
#' @return list $tabXXI data frame of table data
#' @return list $personsWithMultipleCompl number of persons
#' @export

NGERtabXXI <- function(RegData) {

  N <- dim(RegData)[1]
  mydf <- RegData[ , c("ComplAfterBleed",
                       "ComplAfterBleedAbdom",
                       "ComplAfterBleedVaginal",
                       "ComplAfterBleedIntra",
                       "ComplEquipment",
                       "ComplEquipNet",
                       "ComplEquipInstruments",
                       "ComplEquipSuture",
                       "ComplInfection",
                       "ComplInfSurg",
                       "ComplInfIntra",
                       "ComplInfEndoSalpin",
                       "ComplInfUVI",
                       "ComplInfOther",
                       "ComplOrgan",
                       "ComplOrganIntestinal",
                       "ComplOrganBladder",
                       "ComplOrganUreter",
                       "ComplOrganKar",
                       "ComplOrganOther",
                       "ComplReop",
                       "ComplReopLaparoscopy",
                       "ComplReopHysteroscopy",
                       "ComplReopLaparotomy"
                       )
                   ]

  # how many persons have more than one compl?
  indCompl <- union(which(mydf$ComplAfterBleed==1),
                    which(mydf$ComplEquipment==1))
  indCompl <- union(indCompl, which(mydf$ComplInfection==1))
  indCompl <- union(indCompl, which(mydf$ComplOrgan==1))

  tab <- table(RegData$PatientID[indCompl])
  personsWithMultipleCompl <- length(tab[tab > 1])

  tabXXI <- data.frame(Frekvens=apply(mydf, 2, sum, na.rm = TRUE),
                       Andel=apply(mydf, 2, sum, na.rm = TRUE)/N*100)
  # recode colname
  colnames(tabXXI)[2] <- "Andel (\\%)"

  # recode rownames, also with latex (table) tagging
  row.names(tabXXI)[row.names(tabXXI) == "ComplAfterBleed"] <- "\\textit{Blødning}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplAfterBleedAbdom"] <- "\\quad I abdominal vegg"
  row.names(tabXXI)[row.names(tabXXI) == "ComplAfterBleedVaginal"] <- "\\quad Vaginal"
  row.names(tabXXI)[row.names(tabXXI) == "ComplAfterBleedIntra"] <- "\\quad Intraabdominal "
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipment"] <- "\\textit{Problem med utstyr}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipNet"] <- "\\quad Med nett"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipInstruments"] <- "\\quad Med instumenter"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipSuture"] <- "\\quad Med sutur"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfection"] <- "\\textit{Infeksjon}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfSurg"] <- "\\quad Infeksjon i operasjonssår"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfIntra"] <- "\\quad Intraabdominal"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfEndoSalpin"] <- "\\quad Salpingitt"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfUVI"] <- "\\quad Urinveisinfeksjon"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfOther"] <- "\\quad Annen infeksjon"
  row.names(tabXXI)[row.names(tabXXI) == "ComplOrgan"] <- "\\textit{Organskade}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplOrganIntestinal"] <- "\\quad Tarmskade"
  row.names(tabXXI)[row.names(tabXXI) == "ComplOrganBladder"] <- "\\quad Blæreskade"
  row.names(tabXXI)[row.names(tabXXI) == "ComplOrganUreter"] <- "\\quad Ureterskade"
  row.names(tabXXI)[row.names(tabXXI) == "ComplOrganKar"] <- "\\quad Karskade"
  row.names(tabXXI)[row.names(tabXXI) == "ComplOrganOther"] <- "\\quad Annen organskade"
  row.names(tabXXI)[row.names(tabXXI) == "ComplReop"] <- "\\textit{Reoperasjon}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplReopLaparoscopy"] <- "\\quad til laparoskopi"
  row.names(tabXXI)[row.names(tabXXI) == "ComplReopHysteroscopy"] <- "\\quad til Hysteroskopi?"
  row.names(tabXXI)[row.names(tabXXI) == "ComplReopLaparotomy"] <- "\\quad til laparotomi"


  list(tabXXI=tabXXI, personsWithMultipleCompl=personsWithMultipleCompl)

}
