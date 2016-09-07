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
  mydf <- RegData[ , c("Opf0KomplBlodning",
                       "Opf0KomplBlodningAbdom",
                       "Opf0KomplBlodningVaginal",
                       "Opf0KomplBlodningIntra",
                       "LapAdherProfylakse",
                       "ComplEquipNet",
                       "ComplEquipInstruments",
                       "ComplEquipSuture",
                       "Opf0KomplInfeksjon",
                       "ComplInfSurg",
                       "ComplInfIntra",
                       "ComplInfEndoSalpin",
                       "ComplInfUVI",
                       "ComplInfOther",
                       "Opf0KomplOrgan",
                       "Opf0KomplOrganIntestinal",
                       "Opf0KomplOrganBladder",
                       "Opf0KomplOrganUreter",
                       "Opf0KomplOrganKar",
                       "Opf0KomplOrganOther",
                       "Opf0Reoperasjon",
                       "Opf0ReoperasjonLaparoscopy",
                       "Opf0ReoperasjonHysteroscopy",
                       "Opf0ReoperasjonLaparotomy"
                       )
                   ]

  # how many persons have more than one compl?
  indCompl <- union(which(mydf$Opf0KomplBlodning==1),
                    which(mydf$LapAdherProfylakse==1))
  indCompl <- union(indCompl, which(mydf$Opf0KomplInfeksjon==1))
  indCompl <- union(indCompl, which(mydf$Opf0KomplOrgan==1))

  tab <- table(RegData$PasientID[indCompl])
  personsWithMultipleCompl <- length(tab[tab > 1])

  tabXXI <- data.frame(Frekvens=apply(mydf, 2, sum, na.rm = TRUE),
                       Andel=apply(mydf, 2, sum, na.rm = TRUE)/N*100)
  # recode colname
  colnames(tabXXI)[2] <- "Andel (\\%)"

  # recode rownames, also with latex (table) tagging
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplBlodning"] <- "\\textit{Blødning}"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplBlodningAbdom"] <- "\\quad I abdominal vegg"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplBlodningVaginal"] <- "\\quad Vaginal"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplBlodningIntra"] <- "\\quad Intraabdominal "
  row.names(tabXXI)[row.names(tabXXI) == "LapAdherProfylakse"] <- "\\textit{Problem med utstyr}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipNet"] <- "\\quad Med nett"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipInstruments"] <- "\\quad Med instumenter"
  row.names(tabXXI)[row.names(tabXXI) == "ComplEquipSuture"] <- "\\quad Med sutur"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplInfeksjon"] <- "\\textit{Infeksjon}"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfSurg"] <- "\\quad Infeksjon i operasjonssår"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfIntra"] <- "\\quad Intraabdominal"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfEndoSalpin"] <- "\\quad Salpingitt"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfUVI"] <- "\\quad Urinveisinfeksjon"
  row.names(tabXXI)[row.names(tabXXI) == "ComplInfOther"] <- "\\quad Annen infeksjon"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrgan"] <- "\\textit{Organskade}"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganIntestinal"] <- "\\quad Tarmskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganBladder"] <- "\\quad Blæreskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganUreter"] <- "\\quad Ureterskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganKar"] <- "\\quad Karskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0KomplOrganOther"] <- "\\quad Annen organskade"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0Reoperasjon"] <- "\\textit{Reoperasjon}"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0ReoperasjonLaparoscopy"] <- "\\quad til laparoskopi"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0ReoperasjonHysteroscopy"] <- "\\quad til Hysteroskopi?"
  row.names(tabXXI)[row.names(tabXXI) == "Opf0ReoperasjonLaparotomy"] <- "\\quad til laparotomi"


  list(tabXXI=tabXXI, personsWithMultipleCompl=personsWithMultipleCompl)

}
