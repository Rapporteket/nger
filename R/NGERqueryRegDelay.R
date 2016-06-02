#' Provide registration delay for NGER
#'
#' Provides delay between event (main date) and registration in days
#'
#' @return RegData data frame
#' @export
#'
NGERHentRegDataRegDelay <- function() {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0(
'select
  year(HovedDato) as yr,
  avg(DATEDIFF(SistLagretDato, HovedDato)) as mean,
  count(*) as N
from
  SkjemaOversikt
where
  SkjemaStatus=1 and SkjemaNavn="Operasjon"
group by
  year(HovedDato);'
  )

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
