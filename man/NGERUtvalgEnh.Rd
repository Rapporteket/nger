% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NGERUtvalgEnh.R
\name{NGERUtvalgEnh}
\alias{NGERUtvalgEnh}
\title{Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.}
\usage{
NGERUtvalgEnh(
  RegData,
  datoFra = "2016-01-01",
  datoTil = "3000-12-31",
  fargepalett = "BlaaOff",
  minald = 0,
  maxald = 110,
  OpMetode = 0,
  AlvorlighetKompl = 0,
  behNivaa = 0,
  enhetsUtvalg = 0,
  velgAvd = 0,
  velgDiag = 0,
  reshID = 0
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{datoFra}{Tidligste dato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste dato i utvalget (vises alltid i figuren).}

\item{fargepalett}{Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)}

\item{minald}{Alder, fra og med (Standardverdi: 0)}

\item{maxald}{Alder, til og med (Standardverdi: 130)}

\item{OpMetode}{1: Laparoskopi
2: Hysteroskopi
3: Begge # 21.okt. 2022: Fjernes som eget valg. Blir med i både laparoskopi og hysteroskopi
4: LCD01 eller LCD04 (total laparoskopisk hysterektomi)
5: LCC11 (laparoskopisk subtotal hysterektomi)
6: LCD11 (laparoskopisk assistert vaginal hysterektomi)
7: Robotassisert inngrep
8: Kolpopeksiene
9: Hysterectomier (alle)}

\item{AlvorlighetKompl}{Alvorlighetsgrad for postoperative komplikasjoner (Flervalg)
Angis som en vektor av tall som tekst, f.eks. c('1','2')
1: Lite alvorlig
2: Middels alvorlig
3: Alvorlig
4: Dødelig}

\item{behNivaa}{Behandlingsvivå: 1-Poliklinisk, 2-Dagkirurgi, 3-Inneliggende}

\item{enhetsUtvalg}{Lag figur for
0: Hele landet
1: Egen enhet mot resten av landet (Standard)
2: Egen enhet}

\item{velgAvd}{Velge hvilke avdelinger som skal vises i figurer med avdelingsvise resultater.
IKKE tatt høyde for sammenlikning mot "resten".}

\item{velgDiag}{0: Alle
1: Ovarialcyster (N83.0, N83.1, N83.2 og D27)
2: Endometriose, livmorvegg (N80.0)
3: Endometriose, unntatt livmorvegg.}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{Hastegrad}{Hastegrad av operasjon - fjernes jan-25
1: Elektiv
2: Akutt
3: Ø-hjelp}

\item{dagkir}{Dagkirurgi? 0-nei, 1-ja - fases ut jan -25}
}
\value{
UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og
tekststreng som angir fargepalett
}
\description{
Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
}
