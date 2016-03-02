#' Provide dataframe for tab vi NGER
#'
#' Provides NGER data for tab vi from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegDataVI <- function(datoFra = '2014-01-01', datoTil = '2099-01-01') {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0(
'
SELECT
  v.OpBMI,
  v.OpParities,
  v.OpPregnancies,
  v.OpOptimeCount,
  v.Opcat,
  v.OpType,
  v.MCEType,
  f.BasisRegStatus,
  v.PatientID,
  f.HovedDato,
  f.SykehusNavn
FROM
  AlleVarNum v
INNER JOIN ForlopsOversikt f ON v.MCEID = f.ForlopsID
WHERE
  f.HovedDato >= \'', datoFra, '\' AND
  f.HovedDato < \'', datoTil, '\'
'
  )

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
