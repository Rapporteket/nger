#' Tidstrend (år) av andel for en gitt variabel.
#'
#' Funksjon som genererer en figur med andeler av en variabel for hvert år.
#'
#' Detaljer:
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'		\item Alder: Pasienter over 70 år
#'		\item Opf0KomplBlodning: Postop. komplikasjon: Blødning
#'		\item LapAdherProfylakse: Postop. komplikasjon: Problemer med ustyr
#'		\item Opf0KomplInfeksjon: Postop. komplikasjon: Infeksjon
#'		\item Opf0KomplOrgan: Postop. komplikasjon: Organskade
#'		\item Opf0Reoperasjon: Andel reoperasjon som følge av komplikasjon
#'		\item Opf0AlvorlighetsGrad: Andel av postoperative komplikasjoner som var alvorlige (3 og 4)
#'		\item KomplIntra: Komplikasjoner ved operasjon. (kombinerer variablene HysKomplikasjoner og LapKomplikasjoner)
#'		\item KomplPostop: Andel postoperative komplikasjoner
#'		\item OpAntibProfylakse: Andel som får antibiotika
#'		\item OpASA: ASA-grad > II
#'		\item OpBMI: Pasienter med fedme (BMI>30)
#'		\item Opf0Status: Pasienter som har fått postoperativ oppfølging
#'    }

#' @inheritParams NGERFigAndeler
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: 'Aar' (standard), 'Mnd'
#'    NB: Når tidsenhet='Mnd', settes datoFra lik X måneder før datoTil.
#' @return Figur med ...
#'
#' @export


NGERFigAndelTid <- function(RegData=0, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31',
                        minald=0, maxald=130, OpMetode=99, Hastegrad='', AlvorlighetKompl='', reshID, outfile='',
                        enhetsUtvalg=1, preprosess=0, hentData=0, tidsenhet='Aar') {


  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERHentRegData(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }


#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$ReshId == reshID),]	#kun egen enhet
	}

RegData$Variabel <- 0


if (valgtVar=='Alder') {
	#Pasienter over 70 år
  	RegData$Variabel[which(RegData$Alder >= 70)] <- 1
  	VarTxt <- 'pasienter >=70år'
	Tittel <- 'Andel pasienter over 70 år'
}

if (valgtVar=='Opf0KomplBlodning') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0KomplBlodning == 1)] <- 1
  	VarTxt <- 'blødninger'
	Tittel <- 'Postop. komplikasjon: Blødning'
}
if (valgtVar=='LapAdherProfylakse') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$LapAdherProfylakse == 1)] <- 1
  	VarTxt <- 'tilfeller av problem med utstyr'
	Tittel <- 'Postop. komplikasjon: Problemer med ustyr'
}
if (valgtVar=='Opf0KomplInfeksjon') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0KomplInfeksjon == 1)] <- 1
	VarTxt <- 'infeksjoner'
	Tittel <- 'Postop. komplikasjon: Infeksjon'
}
if (valgtVar=='Opf0KomplOrgan') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0KomplOrgan == 1)] <- 1
	VarTxt <- 'organskader'
	Tittel <- 'Postop. komplikasjon: Organskade'
}

if (valgtVar=='OpAntibProfylakse') {
	#Andel som får antibiotika
	#Kode 0,1: Nei, Ja (ingen tomme per 22.feb.2016)
	RegData <- RegData[which(RegData$OpAntibProfylakse %in% 0:1), ]
	RegData$Variabel <- RegData[ ,valgtVar]
  	VarTxt <- 'profylakser'
	Tittel <- 'Andel som får antibiotika'
}
if (valgtVar == 'OpASA') {
	#Andel med ASA-grad>2
	RegData <- RegData[which(RegData[,valgtVar] %in% 1:5), ]
	RegData$Variabel[which(RegData[ ,valgtVar] > 2)] <- 1
 	VarTxt <- 'med ASA-grad > II'
	Tittel <- 'ASA-grad > II'
}
if (valgtVar=='OpBMI') {
	#Andel pasienter med fedme (BMI over 30)
	indMed <- which(RegData[ ,valgtVar] >30)
	RegData$Variabel[which(RegData[ ,valgtVar] >30)] <- 1
  	VarTxt <- 'med BMI>30'
	Tittel <- 'Pasienter med fedme'
}

### Komplikasjoner
if (valgtVar=='KomplPostop') {
	# Andel postoperative komplikasjoner
	#Kode 0: Nei, 1:Ja, tomme
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel <- RegData$Opf0Komplikasjoner
  	#RegData$Variabel[which(RegData$Opf0Komplikasjoner==1)] <- 1
  	VarTxt <- 'komplikasjoner'
	Tittel <- 'Komplikasjoner, postoperativt'
}
if (valgtVar=='Opf0AlvorlighetsGrad') {
	#Andel av postoperative komplikasjoner som var alvorlige (3 og 4)
	#Kode 1-Lite alvorlig, 2-Middels alvorlig, 3-Alvorlig, 4-Dødelig
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0AlvorlighetsGrad %in% 3:4)] <- 1
  	VarTxt <- 'alvorlige komplikasjoner'
	Tittel <- 'Alvorlige komplikasjoner (grad 3 og 4)'
}

if (valgtVar=='KomplIntra') {
	# Komplikasjoner ved operasjon. Må kombinere HysKomplikasjoner og LapKomplikasjoner
	#Kode 0: Nei, 1:Ja, tomme
	RegData$KomplIntra <- with(RegData, HysKomplikasjoner + LapKomplikasjoner) #Får mange tomme!!!
  	indMed <- switch(as.character(OpMetode),
					'1' = which(RegData$LapKomplikasjoner %in% 0:1),
					'2' = which(RegData$HysKomplikasjoner %in% 0:1),
					'3' = which(RegData$KomplIntra %in% 0:1),	#Få tomme for dette valget
					'99' = union(which(is.finite(RegData$HysKomplikasjoner)), which(is.finite(RegData$LapKomplikasjoner))))
	RegData <- RegData[indMed, ]
  	indVar <- switch(as.character(OpMetode),
					'1' = which(RegData$LapKomplikasjoner == 1),
					'2' = which(RegData$HysKomplikasjoner == 1),
					'3' = which(RegData$KomplIntra == 1),
					'99' = union(which(RegData$HysKomplikasjoner == 1), which(RegData$LapKomplikasjoner==1)))
	RegData$Variabel[indVar] <- 1
	VarTxt <- 'komplikasjoner'
	Tittel <- 'Komplikasjoner, intraoperativt '
}

if (valgtVar=='Opf0Reoperasjon') {
  #Andel reoperasjon som følge av komplikasjon
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
  RegData$Variabel[which(RegData$Opf0Reoperasjon == 1)] <- 1
  VarTxt <- 'reoperasjoner'
	Tittel <- 'Reoperasjoner som følge av komplikasjon'
}

if (valgtVar=='Opf0Status') {
	#Andel med Opf0Status=1 (av samtlige, også tomme reg.)
	#Kode: tomme, -1,0,1
  #Tar ut hendelser siste 6 uker:
  datoTil <- min(as.POSIXlt(datoTil), as.POSIXlt(Sys.Date() - 8*7))
  RegData$Variabel[RegData$Opf0Status==1] <- 1
  VarTxt <- 'av postoperativ oppfølging'
	Tittel <- 'Pasienter som har fått postoperativ oppfølging'
}


if (valgtVar == 'Utdanning') {
	#PasientSkjema. Andel med Utdanning 4 el 5
	#Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
			#Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
	RegData <- RegData[which(RegData$Utdanning %in% 1:5), ]		#, which(RegData$PasientSkjemaStatus ==1)
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:3)] <- 1
  	VarTxt <- 'uten høyere utdanning'
	Tittel <- 'Andel uten høyere utdanning'
}

#Endrer startdato til å være 12 hele måneder før sluttdato
if (tidsenhet == 'Mnd') {datoFra <- paste0(as.numeric(strftime(datoTil, format="%Y"))-1,'-',
                                     strftime(datoTil, format="%m"),'-','01')}

NGERUtvalg <- NGERUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                            OpMetode=OpMetode, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad)
RegData <- NGERUtvalg$RegData
utvalgTxt <- NGERUtvalg$utvalgTxt

if (dim(RegData)[1] > 0) {datoTil <- max(RegData$InnDato, na.rm=T)}
RegData$TidsEnhet <- as.character(switch(tidsenhet,
                            Aar = strftime(RegData$InnDato, format="%Y"),
                            Mnd = strftime(RegData$InnDato, format="%y.%m")))

if (tidsenhet == 'Mnd') {
  #Må ta høyde for at det kan være en måned uten registreringer
  startAar <- as.numeric(strftime(sort(RegData$InnDato)[1], format = "%y"))
  startMnd <- as.numeric(strftime(sort(RegData$InnDato)[1], format = "%m"))
  mnd <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10','11', '12')
  aar <- c(rep(startAar, 13-startMnd),rep(startAar+1,startMnd))
  Tidtxt <- paste(aar, mnd[c(startMnd:12,1:startMnd)], sep='.')
}
if (tidsenhet == 'Aar') {
  #RegData$TidsEnhet <- strftime(RegData$InnDato, format="%Y")
  #gml: RegData$TidsEnhet <- 1900 + strptime(RegData$InnDato, format="%Y")$year
  Tidtxt <- as.character(min(as.numeric(RegData$TidsEnhet)):max(as.numeric(RegData$TidsEnhet)))
}
RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=Tidtxt)

#Generere hovedgruppe og sammenlikningsgruppe
#Trenger indeksene før genererer tall for figurer med flere variable med ulike utvalg
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$SykehusNavn[indEgen1]) } else {
		shtxt <- 'Hele landet'
			}

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    indRest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$ReshId)==reshID)
      smltxt <- 'Landet forøvrig'
      indRest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }


NHovedRes <- length(indHoved)
NSmlRes <- length(indRest)


#-------------------------Beregning av andel-----------------------------------------
#RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=as.character(Tidtxt))
NTidRest <- tapply(RegData$Variabel[indRest], RegData$TidsEnhet[indRest], length)
NTidHendRest <- tapply(RegData$Variabel[indRest], RegData$TidsEnhet[indRest],sum, na.rm=T)
AndelRest <- NTidHendRest/NTidRest*100
NTidHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'TidsEnhet'], length)
NTidHendHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'TidsEnhet'],sum, na.rm=T)
AndelHoved <- NTidHendHoved/NTidHoved*100
Andeler <- rbind(AndelRest, AndelHoved)


#----------FIGUR------------------------------
#Hvis for få observasjoner..
if (length(indHoved) < 10 | (medSml ==1 & length(indRest)<10)) {
	#-----------Figur---------------------------------------
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
	text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
	if ( outfile != '') {dev.off()}
} else {




#-----------Figur---------------------------------------

#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
farger <- FigTypUt$farger
fargeHoved <- farger[3]
fargeRest <- farger[1]
NutvTxt <- length(utvalgTxt)
hmarg <- 0.04+0.01*NutvTxt
par('fig' = c(0,1,0,1-hmarg))
cexleg <- 1	#Størrelse på legendtekst
cexskala <- switch(tidsenhet, Aar=1, Mnd=0.9)
xskala <- 1:length(Tidtxt)
xaksetxt <- switch(tidsenhet, Aar='Operasjonsår', Mnd='Operasjonsår og -måned')
ymax <- min(119, 1.25*max(Andeler,na.rm=T))

plot(AndelHoved,  font.main=1,  type='o', pch="'", col=fargeHoved, xaxt='n',
		 frame.plot = FALSE,  xaxp=c(1,length(Tidtxt),length(Tidtxt)-1),xlim = c(1,length(Tidtxt)),
		cex=2, lwd=3, xlab=xaksetxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

axis(side=1, at = xskala, labels = Tidtxt, cex.axis=0.9)
title(Tittel, line=1, font.main=1)
text(xskala, AndelHoved, pos=3, NTidHendHoved, cex=0.9, col=fargeHoved)#pos=1,

Ttxt <- paste('(Tall ved punktene angir antall ', VarTxt, ')', sep='')
if (medSml == 1) {
  lines(xskala, AndelRest, col=fargeRest, lwd=3)
  points(xskala, AndelRest, pch="'", cex=2, col=fargeRest)	#}
  text(xskala, AndelRest, pos=3, NTidHendRest, cex=0.9, col=fargeRest)
	legend('topleft', border=NA, c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''),
		paste(smltxt, ' (N=', NSmlRes, ')', sep=''), Ttxt), bty='n', ncol=1, cex=cexleg,
		col=c(fargeHoved, fargeRest, NA), lwd=3)
	} else {
		legend('top', c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''), Ttxt),
		col=c(fargeHoved, NA), lwd=3, bty='n')
	}

#Legge på linjer i plottet. Denne kan nok gjøres mer elegant...
if ((ymax > 10) & (ymax < 40)) {lines(range(xskala),rep(10,2), col=farger[4])}
if (ymax > 20) {lines(range(xskala),rep(20,2), col=farger[4])}
if ((ymax > 30) & (ymax < 40)) {lines(range(xskala),rep(30,2), col=farger[4])}
if (ymax > 40) {lines(range(xskala),rep(40,2), col=farger[4])}
if (ymax > 60) {lines(range(xskala),rep(60,2), col=farger[4])}
if (ymax > 80) {lines(range(xskala),rep(80,2), col=farger[4])}
if (ymax > 100) {lines(range(xskala),rep(100,2), col=farger[4])}
#		axis(2, at=c(0,20,40,60,80,100), pos=0),

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
#------------------------------------------------------------------------------

}	#end else statement
}	#end function



