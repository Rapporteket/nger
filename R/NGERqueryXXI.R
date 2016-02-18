#' Provide dataframe for tab xxi NGER
#'
#' Provides NGER data for tab xxi from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegDataXXI <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0(
'
SELECT
  v.OpDate,
  v.AVD_RESH,
  v.BirthDate,
  v.FollowupSeriousness,
  v.Education,
  v.MaritalStatus,
  v.OpAnesthetic,
  v.OpASA,
  v.HypCompleteness,
  v.PatientNorwegian,
  v.OpBMICategory,
  v.Opcat,
  v.OpType,
  v.OpEarlierVaginal,
  v.OpEarlierLaparoscopy,
  v.OpEarlierPaparotomy,
  v.OpOpcatOutsideDaytime,
  v.OpDaySurgery,
  v.MCEType,
  f.BasisRegStatus,
  v.PatientID,
  f.HovedDato,
  f.SykehusNavn,
  v.ComplExist,
  v.ComplReop,
  v.ComplReopLaparoscopy,
  v.ComplReopHysteroscopy,
  v.ComplReopLaparotomy
  v.ComplInfection,
  v.ComplInfSurg,
  v.ComplInfIntra,
  v.ComplInfEndoSalpin
  v.ComplInfUVI,
  v.ComplInfOther,
  v.ComplAfterBleed,
  v.ComplAfterBleedAbdom,
  v.ComplAfterBleedVaginal,
  v.ComplAfterBleedIntra,
  v.ComplOrgan,
  v.ComplOrganIntestinal,
  v.ComplOrganBladder,
  v.ComplOrganUreter,
  v.ComplOrganKar,
  v.ComplOrganOther,
  v.ComplEquipment,
  v.ComplEquipNet,
  v.ComplEquipInstruments,
  v.ComplEquipSuture
FROM
  AlleVarNum v
INNER JOIN ForlopsOversikt f ON AlleVarNum.MCEID = ForlopsOversikt.ForlopsID
WHERE
  v.MCEType = 1 AND
  f.HovedDato >= \'', datoFra, '\' AND
  f.HovedDato <= \'', datoTil, '\'
'
  )

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
