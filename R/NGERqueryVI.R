#' Provide dataframe for tab vi NGER
#'
#' Provides NGER data for tab vi from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegDataVI <- function(reportYear = 2099) {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0(
"
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
  YEAR(f.HovedDato) AS year,
  f.SykehusNavn
FROM
  AlleVarNum v
INNER JOIN ForlopsOversikt f ON v.MCEID = f.ForlopsID
WHERE
  YEAR(f.HovedDato) < ", reportYear + 1
  )

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}