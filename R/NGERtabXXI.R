#' Generate tab XXI
#'
#' Yes, generate tab XXI
#'
#' @inheritParams FigAndeler
#' @return dataframe tabXXI
#' @export

NGERtabXXI <- function(datoFra = '2014-01-01', datoTil = '2014-04-01') {

  RegData <- NGERHentRegDataXXI(datoFra, datoTil)
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

  tabXXI <- data.frame(Frekvens=apply(mydf, 2, sum, na.rm = TRUE),
                       Andel=apply(mydf, 2, sum, na.rm = TRUE)/N)


  return(tabXXI)

}
