#' Søylediagram som viser resultat av valgt variabel, målt ved tre tidspunkter
#'
#' Funksjon som genererer en figur med som viser endring i en variabels fordeling ved tre ulike tidspunkter.
#' Man kan velge om det skal vises andeler eller antall
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NGERFigAndeler
#' @param valgtVar Hvilken variabel som skal visualiseres
#'
#' @return Søylediagram som fordeling av valgt variabel, ved operasjon, samt 1 og 3 år etter.
#'
#' @export
NGERFigPrePost  <- function(RegData, valgtVar='ScoreGeneral', datoFra='2019-01-01', datoTil=Sys.Date(),
                            minald=0, maxald=130, OpMetode=99, velgDiag=0, Ngrense=10,
                            outfile='', preprosess=0, hentData=0,...)
{

  if (hentData == 1) {
    RegData <- NGERRegDataSQL(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert.
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

reshID <- as.numeric(reshID)
RANDvar <- c('ScorePhys',	'ScoreRoleLmtPhy',	'ScoreRoleLmtEmo',
             'ScoreEnergy',	'ScoreEmo', 'ScoreSosial',
             'ScorePain',	'ScoreGeneral')
grtxt <- c('Fysisk funksjon',	'Fysisk \n rollebegrensning',	'Følelsesmessig \n rollebegrensning',
           'Energinivå/vitalitet',	'Mental helse', 'Sosial funksjon',
           'Smerte',	'Generell \n helsetilstand')
valgtVar <- RANDvar[1]

varNr <- match(valgtVar, RANDvar)
PrePostVar <- paste0(c('R0', 'R1'), valgtVar)
RegData$VarPre <- RegData[ ,PrePostVar[1]]
RegData$VarPost <- RegData[ ,PrePostVar[2]]
#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
RegData <- RegData[!(is.na(RegData$VarPre) | is.na(RegData$VarPost)), ]

tittel <- grtxt[varNr]

NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil)
RegData <- NGERUtvalg$RegData
utvalgTxt <- NGERUtvalg$utvalgTxt


#-----------Definisjoner----------------------------
cexgr <- 0.9
retn <- 'V'
txtretn <- 1
grtxt <- ''
subtxt <- ''

#---------------BEREGNINGER --------------------------
utvalg <- c('Hoved','Rest')
GjsnPP <- list(Hoved = 0, Rest =0)

GjsnPre <-tapply(RegData$VarPre, RegData$ShNavn, FUN = 'mean', na.rm=T)
Gjsn1aar <- tapply(RegData$VarPost, RegData$ShNavn, FUN = 'mean', na.rm=T)
Gjsn3aar <- Gjsn1aar
Ngr <- table(RegData$ShNavn)
N <- sum(Ngr)
GjsnPP <- cbind(GjsnPre, Gjsn1aar, Gjsn3aar)

#LEGGE TIL ALLE?

#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg == 1)) {
if (NHoved < 10 ) {
FigTypUt <- rapFigurer::figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(main=tittel)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
	text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
	if ( outfile != '') {dev.off()}
} else {

#-----------Figur---------------------------------------
#Innparametre: subtxt, grtxt, tittel, Andeler

#Plottspesifikke parametre:
FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
NutvTxt <- length(utvalgTxt)
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

farger <- FigTypUt$farger
fargeHoved <- farger[1]
fargeRest <- farger[3]
antGr <- length(Ngr)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 0.9	#Størrelse på legendtekst
cexpt <- 2	#Størrelse på punkter (resten av landet)

if (retn == 'V' ) {
#Vertikale søyler eller linje
	ymax <- min(max(c(GjsnPP),na.rm=T)*1.25, 110)
	pos <- barplot(t(GjsnPP), beside=TRUE, horiz=FALSE, las=txtretn, ylab="Andel pasienter (%)",
		sub=subtxt,	cex.names=cexgr, #names.arg=grtxt,
		col=farger[1:3], border='white', ylim=c(0, ymax))	#
		legend('top', c('perop.', '1 år', '3 år', paste0('N=', N)), bty='n',
			fill=farger[c(1:3,NA)], border=NA, ncol=4, cex=cexleg)
}

if (retn == 'H') {
#Horisontale søyler
#	ymax <- 2*antGr*1.6
	xmax <- min(max(GjsnPP,na.rm=T)*1.25, 100)
pos <- barplot(t(GjsnPP), beside=TRUE, horiz=TRUE, main='', las=1,
		col=farger[1:3], border='white', font.main=1,  xlim=c(0,xmax), #ylim=c(0, ymax),
		cex.names=cexgr, xlab="Skår") #names.arg=grtxt,
		legend('top', c('Før', 'Etter',paste0('N=',N)), bty='n',
			fill=farger[c(1:3,NA)], border=NA, ncol=3, cex=cexleg)
}

title(tittel, font.main=1)	#line=0.5,
#Tekst som angir hvilket utvalg som er gjort
avst <- 0.8
utvpos <- 3+length(tittel)-1	#Startlinje for teksten
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}

}
}
