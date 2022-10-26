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
NGERFigPrePost  <- function(RegData, valgtVar='Antall', datoFra='2012-04-01', datoTil=Sys.Date(),
                            minald=0, maxald=130, OpMetode=99, velgDiag=0, Ngrense=10,
                            outfile='', reshID=0, preprosess=0, hentData=0,...)
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

NGERUtvalg <- NGERUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil)
RegData <- NGERUtvalg$RegData
utvalgTxt <- NGERUtvalg$utvalgTxt


#-----------Definisjoner----------------------------
cexgr <- 0.9
retn <- 'V'
txtretn <- 1
grtxt <- ''
subtxt <- ''

	#RegData <- RegData[RegData$VarPre %in% 0:100, ]
	#RegData <- RegData[RegData$VarPost %in% 0:100, ]
	#RegData$VarPre <- factor(as.numeric(RegData$VarPre), levels=gr, labels = grtxt)
	#RegData$VarPost <- factor(as.numeric(RegData$VarPost), levels=gr, labels = grtxt)
#---------------BEREGNINGER --------------------------


utvalg <- c('Hoved','Rest')
AndelerPP <- list(Hoved = 0, Rest =0)

AntHovedPre <-tapply(RegData$VarPre, RegData$SykehusNavn, FUN = 'mean', na.rm=T)
AntHovedPost <- tapply(RegData$VarPost, RegData$SykehusNavn, FUN = 'mean', na.rm=T)
NHoved <- sum(AntHovedPre)	#length(indHoved)
AndelerPP <- cbind(AntHovedPre, AntHovedPost)/NHoved*100

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
antGr <- length(grtxt)
#Ngr <- matrix(c(AntPre, AntPost), antGr, 2)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 0.9	#Størrelse på legendtekst
cexpt <- 2	#Størrelse på punkter (resten av landet)

if (retn == 'V' ) {
#Vertikale søyler eller linje
	ymax <- min(max(c(AndelerPP$Hoved, AndelerPP$Rest),na.rm=T)*1.25, 110)
	pos <- barplot(t(AndelerPP$Hoved), beside=TRUE, las=txtretn, ylab="Andel pasienter (%)",
		sub=subtxt,	names.arg=grtxt, cex.names=cexgr,
		col=farger[c(2,1)], border='white', ylim=c(0, ymax))	#
		legend('top', c('Før', 'Etter', paste('N=', NHoved , sep='')), bty='n',
			fill=farger[c(2,1,NA)], border=NA, ncol=3, cex=cexleg)
}

if (retn == 'H') {
#Horisontale søyler
	ymax <- 2*antGr*1.6
	xmax <- min(max(c(AndelerPP$Hoved, AndelerPP$Rest),na.rm=T)*1.25, 100)
pos <- barplot(t(AndelerPP), beside=TRUE, horiz=TRUE, main='', las=1,
		col=farger[c(2,1)], border='white', font.main=1,  xlim=c(0,xmax), ylim=c(0.25, 3.3)*antGr,
		names.arg=grtxt, cex.names=cexgr, xlab="Andel pasienter (%)")
	if (medSml == 1) {
		points(as.numeric(t(AndelerPP$Rest)), y=pos+0.1,  col=fargeRest,  cex=cexpt, pch=18) #c("p","b","o"),
		legend('topleft', c(paste(c('Før, N=', 'Etter, N='), NHoved , sep=''),
			paste(smltxt, ' N=', Nrest, sep='')), text.width = c(0.2,0.2,0.21)*xmax,
			bty='n', pch=c(15,15,18), pt.cex=cexpt, #lty=c(NA,NA,NA),
			col=farger[c(2,1,3)], border=farger[c(2,1,3)], ncol=3, cex=cexleg)
		} else {
		legend('top', c('Før', 'Etter',paste('N=',NHoved,sep='')), bty='n',
			fill=farger[c(2,1,NA)], border=NA, ncol=3, cex=cexleg)
		}
}

title(tittel, font.main=1)	#line=0.5,
title(shtxt, font.main=1, line=0.5)
#Tekst som angir hvilket utvalg som er gjort
avst <- 0.8
utvpos <- 3+length(tittel)-1	#Startlinje for teksten
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}

}
}
