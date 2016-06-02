#' Provide registration delay data from NGER
#'
#' Provides delay between event (main date) and registration as a data frame
#'
#' @format Return a data frame with two variables:
#' \describe{
#' \item{year}{the year for the event (from db field \emph{HovedDato})}
#' \item{daysDiff}{the difference in days between event (from db field
#' \emph{HovedDato}) and the last time the registration form was saved
#' (from db field \emph{SistLagretDato})}
#' }
#'
#' @details For the query these conditions apply:
#' \describe{
#' \item{SkjemaStatus = 1}{use Only finished registrations}
#' \item{SkjemaNavn = "Operasjon"}{use only registry form 'Operasjon'}
#' }
#' @return RegDelayData data frame
#' @export


NGERRegDelayData <- function() {

  registryName <- "nger"
  dbType <- "mysql"

  query <- paste0(
'select
  year(HovedDato) as year,
  DATEDIFF(SistLagretDato, HovedDato) as daysDiff
from
  SkjemaOversikt
where
  SkjemaStatus=1 and SkjemaNavn="Operasjon";'
  )

  RegDelayData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegDelayData)
}
