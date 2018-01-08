#' Tidstrend (år) av andel for en gitt variabel.
#'
#' Funksjon som genererer en figur med andeler av en variabel for hvert år.
#'
#' Detaljer:
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'		\item Alder: Pasienter over 70 år
#'		\item Opf0KomplBlodning: Postop. komplikasjon: Blødning
#'		\item Opf0KomplUtstyr: Postop. komplikasjon: Problemer med ustyr
#'		\item Opf0KomplInfeksjon: Postop. komplikasjon: Infeksjon
#'		\item Opf0KomplOrgan: Postop. komplikasjon: Organskade
#'		\item Opf0Reoperasjon: Andel reoperasjon som følge av komplikasjon
#'		\item Opf0AlvorlighetsGrad: Andel av postoperative komplikasjoner som var alvorlige (3 og 4)
#'		\item KomplIntra: Komplikasjoner ved operasjon. (kombinerer variablene HysKomplikasjoner og LapKomplikasjoner)
#'		\item KomplPostop: Andel postoperative komplikasjoner
#'		\item LapKonvertert: Konvertert til laparoromi?
#'		\item OpAntibProfylakse: Andel som får antibiotika
#'		\item OpASA: ASA-grad > II
#'		\item OpBMI: Pasienter med fedme (BMI>30)
#'		\item Opf0Status: Pasienter som har fått postoperativ oppfølging
#'    }

#' @inheritParams NGERFigAndeler
#' @inheritParams NGERUtvalgEnh
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: 'Aar' (standard), 'Mnd'
#'    NB: Når tidsenhet='Mnd', settes datoFra lik X måneder før datoTil.
#' @return Figur med ...
#'
#' @export


NGERFigAndelTid <- function(RegData=0, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31',
                        minald=0, maxald=130, MCEType=99, Hastegrad='', AlvorlighetKompl='', reshID, outfile='',
                        enhetsUtvalg=1, preprosess=0, hentData=0, tidsenhet='Aar') {


  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

NGERVarSpes <- NGERVarTilrettelegg(RegData, valgtVar=valgtVar, grVar='', figurtype='andelGrVar')
  RegData <- NGERVarSpes$RegData
  flerevar <- NGERVarSpes$flerevar
  subtxt <- NGERVarSpes$subtxt
  grtxt <- NGERVarSpes$grtxt



#Endrer startdato til å være 12 hele måneder før sluttdato
if (tidsenhet == 'Mnd') {datoFra <- paste0(as.numeric(strftime(datoTil, format="%Y"))-1,'-',
                                     strftime(datoTil, format="%m"),'-','01')}

NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                              MCEType=MCEType, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad)
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

if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$ShNavn[indEgen1]) } else {
    medSml <- 0
      smltxt <- 'Landet forøvrig'
  }

ind <- NGERUtvalg$ind
NHovedRes <- length(ind$Hoved)
NSmlRes <- length(ind$Rest)


#-------------------------Beregning av andel-----------------------------------------
#RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=as.character(Tidtxt))
NTidRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)
NTidHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
AndelRest <- NTidHendRest/NTidRest*100
NTidHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length)
NTidHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T)
AndelHoved <- NTidHendHoved/NTidHoved*100
Andeler <- rbind(AndelRest, AndelHoved)


#----------FIGUR------------------------------
#Hvis for få observasjoner..
if (length(ind$Hoved) < 10 | (medSml ==1 & length(ind$Rest)<10)) {
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
		 frame.plot = FALSE,  xlim = c(1,length(Tidtxt)), #xaxp=c(1,length(Tidtxt),length(Tidtxt)-1),
		cex=2, lwd=3, xlab=xaksetxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

axis(side=1, at = xskala, labels = Tidtxt, cex.axis=0.9)
title(tittel, line=1, font.main=1)
text(xskala, AndelHoved, pos=3, NTidHendHoved, cex=0.9, col=fargeHoved)#pos=1,

#Legge på linjer i plottet.
grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")
# if ((ymax > 10) & (ymax < 40)) {lines(range(xskala),rep(10,2), col=farger[4])}
# if (ymax > 20) {lines(range(xskala),rep(20,2), col=farger[4])}
# if ((ymax > 30) & (ymax < 40)) {lines(range(xskala),rep(30,2), col=farger[4])}
# if (ymax > 40) {lines(range(xskala),rep(40,2), col=farger[4])}
# if (ymax > 60) {lines(range(xskala),rep(60,2), col=farger[4])}
# if (ymax > 80) {lines(range(xskala),rep(80,2), col=farger[4])}
# if (ymax > 100) {lines(range(xskala),rep(100,2), col=farger[4])}
#		axis(2, at=c(0,20,40,60,80,100), pos=0),


Ttxt <- paste0('(Tall ved punktene angir antall ', NGERvarSpes$VarTxt, ')')
if (medSml == 1) {
  lines(xskala, AndelRest, col=fargeRest, lwd=3)
  points(xskala, AndelRest, pch="'", cex=2, col=fargeRest)	#}
  text(xskala, AndelRest, pos=3, NTidHendRest, cex=0.9, col=fargeRest)
	legend('topleft', border=NA, c(paste0(shtxt, ' (N=', NHovedRes, ')'),
		paste0(smltxt, ' (N=', NSmlRes, ')'), Ttxt), bty='n', ncol=1, cex=cexleg,
		col=c(fargeHoved, fargeRest, NA), lwd=3)
	} else {
		legend('top', c(paste0(shtxt, ' (N=', NHovedRes, ')'), Ttxt),
		col=c(fargeHoved, NA), lwd=3, bty='n')
	}


#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
#------------------------------------------------------------------------------

}	#end else statement
}	#end function



