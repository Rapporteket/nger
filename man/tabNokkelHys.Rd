% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NGERtabeller.R
\name{tabNokkelHys}
\alias{tabNokkelHys}
\title{Nøkkeltallstabell for hysteroskopi}
\usage{
tabNokkelHys(
  RegData = RegData,
  datoFra = Sys.Date() - 365,
  datoTil = Sys.Date(),
  reshID = 0,
  velgAvd = 0,
  enhetsUtvalg = 0
)
}
\arguments{
\item{RegData}{NGER-data, dataramme}
}
\value{
Leverer formattert tabell for nøkkeltall
}
\description{
Nøkkeltallstabell for hysteroskopi
}
