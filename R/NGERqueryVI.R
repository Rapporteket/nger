#' Provide dataframe for tab vi NGER
#'
#' Provides NGER data for tab vi from staging
#'
#' @inheritParams NGERFigAndeler
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
  v.OpKategori,
  v.OpType,
  v.OpMetode,
  f.BasisRegStatus,
  v.PasientID,
  YEAR(f.HovedDato) AS year,
  f.SykehusNavn
FROM
  AlleVarNum v
INNER JOIN ForlopsOversikt f ON v.MCEID = f.ForlopsID
WHERE
  YEAR(f.HovedDato) < ", reportYear + 1, " AND
  YEAR(f.HovedDato) >= ", reportYear - 2
  )

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
