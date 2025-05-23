% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NGERFigAndelTid.R
\name{NGERFigAndelTid}
\alias{NGERFigAndelTid}
\title{Tidstrend (år) av andel for en gitt variabel.}
\usage{
NGERFigAndelTid(
  RegData = 0,
  valgtVar,
  preprosess = 1,
  hentData = 0,
  reshID = 0,
  datoFra = "2015-01-01",
  datoTil = Sys.Date(),
  minald = 0,
  maxald = 130,
  OpMetode = 99,
  AlvorlighetKompl = "",
  velgDiag = 0,
  behNivaa = 0,
  enhetsUtvalg = 0,
  tidsenhet = "Aar",
  outfile = "",
  ...
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel som skal visualiseres}

\item{preprosess}{Preprosesser data
0: Nei (Standard)
1: Ja}

\item{hentData}{Gjør spørring mot database
0: Nei, RegData gis som input til funksjonen (Standard)
1: Ja}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{datoFra}{Tidligste dato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste dato i utvalget (vises alltid i figuren).}

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

\item{velgDiag}{0: Alle
1: Ovarialcyster (N83.0, N83.1, N83.2 og D27)
2: Endometriose, livmorvegg (N80.0)
3: Endometriose, unntatt livmorvegg.}

\item{behNivaa}{Behandlingsvivå: 1-Poliklinisk, 2-Dagkirurgi, 3-Inneliggende}

\item{enhetsUtvalg}{Lag figur for
0: Hele landet
1: Egen enhet mot resten av landet (Standard)
2: Egen enhet}

\item{tidsenhet}{Oppløsning på tidsaksen. Verdier: Aar (standard), Halvaar, Kvartal, Mnd}

\item{outfile}{Navn på fil figuren skrives til. Standard: '' (Figur skrives
til systemets standard utdataenhet (som regel skjerm))}
}
\value{
Figur som viser andel av valgt variabel, utvikling over tid.
}
\description{
Funksjon som genererer en figur med andeler av en variabel for hvert år.
}
\details{
Detaljer:
Argumentet \emph{valgtVar} har følgende valgmuligheter:
   \itemize{
	\item Alder: Pasienter over 70 år
	\item Opf0KomplBlodning: Postop. komplikasjon: Blødning
	\item Opf0KomplUtstyr: Postop. komplikasjon: Problemer med ustyr
	\item Opf0KomplInfeksjon: Postop. komplikasjon: Infeksjon
	\item Opf0KomplOrgan: Postop. komplikasjon: Organskade
	\item Opf0Reoperasjon: Andel reoperasjon som følge av komplikasjon
	\item KomplPostopAlvor: Andel av postoperative komplikasjoner som var alvorlige (3 og 4)
	\item KomplIntra: Komplikasjoner ved operasjon. (kombinerer variablene HysKomplikasjoner og LapKomplikasjoner)
	\item KomplPostop: Andel postoperative komplikasjoner
	\item LapKonvertert: Konvertert til laparoromi?
	\item OpAntibProfylakse: Andel som får antibiotika
	\item OpASA: ASA-grad > II
	\item OpBMI: Pasienter med fedme (BMI>30)
	\item Opf0Status: Pasienter som har fått postoperativ oppfølging
   }
}
