#' Generate tab XXI
#'
#' Yes, generate tab XXI
#'
#' @inheritParams FigAndeler
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

  # rename
  colnames(mydf)[which(names(mydf) == "ComplAfterBleed")] <- "\\textit{Blødning}"
  colnames(mydf)[which(names(mydf) == "ComplAfterBleedAbdom")] <- "\\quad I abdominal vegg"
  colnames(mydf)[which(names(mydf) == "ComplAfterBleedVaginal")] <- "\\quad Vaginal"
  colnames(mydf)[which(names(mydf) == "ComplAfterBleedIntra")] <- "\\quad Intraabdominal"
  colnames(mydf)[which(names(mydf) == "ComplEquipment")] <- "\\textit{Problem med utstyr}"
  colnames(mydf)[which(names(mydf) == "ComplEquipNet")] <- "\\quad Med nett"
  colnames(mydf)[which(names(mydf) == "ComplEquipInstruments")] <- "\\quad Med instumenter"
  colnames(mydf)[which(names(mydf) == "ComplEquipSuture")] <- "\\quad Med sutur"
  colnames(mydf)[which(names(mydf) == "ComplInfection")] <- "\\textit{Infeksjon}"
  colnames(mydf)[which(names(mydf) == "ComplInfSurg")] <- "\\quad Infeksjon i operasjonssår"
  colnames(mydf)[which(names(mydf) == "ComplInfIntra")] <- "\\quad Intraabdominal"
  colnames(mydf)[which(names(mydf) == "ComplInfEndoSalpin")] <- "\\quad Salpingitt"
  colnames(mydf)[which(names(mydf) == "ComplInfUVI")] <- "\\quad Urinveisinfeksjon"
  colnames(mydf)[which(names(mydf) == "ComplInfOther")] <- "\\quad Annen infeksjon"
  colnames(mydf)[which(names(mydf) == "ComplOrgan")] <- "\\textit{Organskade}"
  colnames(mydf)[which(names(mydf) == "ComplOrganIntestinal")] <- "\\quad Tarmskade"
  colnames(mydf)[which(names(mydf) == "ComplOrganBladder")] <- "\\quad Blæreskade"
  colnames(mydf)[which(names(mydf) == "ComplOrganUreter")] <- "\\quad Ureterskade"
  colnames(mydf)[which(names(mydf) == "ComplOrganKar")] <- "\\quad Karskade"
  colnames(mydf)[which(names(mydf) == "ComplOrganOther")] <- "\\quad Annen organskade"
  colnames(mydf)[which(names(mydf) == "ComplReop")] <- "\\textit{Reoperasjon}"
  colnames(mydf)[which(names(mydf) == "ComplReopLaparoscopy")] <- "\\quad til laparoskopi"
  colnames(mydf)[which(names(mydf) == "ComplReopHysteroscopy")] <- "\\quad til Hysteroskopi?"
  colnames(mydf)[which(names(mydf) == "ComplReopLaparotomy")] <- "\\quad til laparotomi"

  tabXXI <- data.frame(Frekvens=apply(mydf, 2, sum, na.rm = TRUE),
                       Andel=apply(mydf, 2, sum, na.rm = TRUE)/N)



  list(tabXXI=tabXXI, personsWithMultipleCompl=personsWithMultipleCompl)

}
