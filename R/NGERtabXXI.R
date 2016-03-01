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

  tabXXI <- data.frame(Frekvens=apply(mydf, 2, sum, na.rm = TRUE),
                       Andel=apply(mydf, 2, sum, na.rm = TRUE)/N)



  list(tabXXI=tabXXI, personsWithMultipleCompl=personsWithMultipleCompl)

}
