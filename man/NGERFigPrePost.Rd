% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NGERFigPrePost.R
\name{NGERFigPrePost}
\alias{NGERFigPrePost}
\title{Søylediagram som viser resultat av valgt variabel, målt ved tre tidspunkter}
\usage{
NGERFigPrePost(
  RegData,
  valgtVar = "ScoreGeneral",
  datoFra = "2019-01-01",
  datoTil = Sys.Date(),
  enhetsUtvalg = 0,
  reshID = 0,
  velgAvd = 0,
  minald = 0,
  maxald = 130,
  OpMetode = 99,
  velgDiag = 0,
  AlvorlighetKompl = 0,
  behNivaa = 0,
  Ngrense = 10,
  outfile = "",
  preprosess = 0,
  hentData = 0,
  ...
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel som skal visualiseres}

\item{datoFra}{Tidligste dato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste dato i utvalget (vises alltid i figuren).}

\item{enhetsUtvalg}{Lag figur for
0: Hele landet
1: Egen enhet mot resten av landet (Standard)
2: Egen enhet}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{velgAvd}{Velge hvilke avdelinger som skal vises i figurer med avdelingsvise resultater.
IKKE tatt høyde for sammenlikning mot "resten".}

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

\item{velgDiag}{0: Alle
1: Ovarialcyster (N83.0, N83.1, N83.2 og D27)
2: Endometriose, livmorvegg (N80.0)
3: Endometriose, unntatt livmorvegg.}

\item{AlvorlighetKompl}{Alvorlighetsgrad for postoperative komplikasjoner (Flervalg)
Angis som en vektor av tall som tekst, f.eks. c('1','2')
1: Lite alvorlig
2: Middels alvorlig
3: Alvorlig
4: Dødelig}

\item{behNivaa}{Behandlingsvivå: 1-Poliklinisk, 2-Dagkirurgi, 3-Inneliggende}

\item{outfile}{Navn på fil figuren skrives til. Standard: '' (Figur skrives
til systemets standard utdataenhet (som regel skjerm))}

\item{preprosess}{Preprosesser data
0: Nei (Standard)
1: Ja}

\item{hentData}{Gjør spørring mot database
0: Nei, RegData gis som input til funksjonen (Standard)
1: Ja}
}
\value{
Søylediagram som fordeling av valgt variabel, ved operasjon, samt 1 og 3 år etter.
}
\description{
Funksjon som genererer en figur med som viser endring i en variabels fordeling ved tre ulike tidspunkter.
Man kan velge om det skal vises andeler eller antall
}
\details{
Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
}
