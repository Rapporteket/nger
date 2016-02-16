#' Provide global dataframe for NGER
#'
#' Provides NGER data from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegData <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0('SELECT
  OpDate,
  AVD_RESH,
  BirthDate,
  FollowupSeriousness,
  Education,
  MaritalStatus,
  OpAnesthetic,
  OpASA,
  HypCompleteness,
  PatientNorwegian,
  OpBMICategory,
  Opcat,
  OpType,
  OpEarlierVaginal,
  OpEarlierLaparoscopy,
  OpEarlierPaparotomy,
  OpOpcatOutsideDaytime,
  OpDaySurgery,
  MCEType,
  PatientID,
  BasisRegStatus,
  HovedDato,
  SykehusNavn
  FROM AlleVarNum INNER JOIN ForlopsOversikt ON AlleVarNum.MCEID = ForlopsOversikt.ForlopsID
                  WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
