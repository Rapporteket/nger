#' Tidstrend (år) av andel for en gitt variabel.
#'
#' Funksjon som genererer en figur med andeler av en variabel for hvert år.
#'
#' Detaljer:
#'
#' @inheritParams FigAndeler
#' @param valgtVar
#'	Alder: Pasienter over 70 år
#'	OpBMI: Pasienter med fedme (BMI>30)
#' @export


FigAndelTid <- function(RegData, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31',
                        minald=0, maxald=130, MCEType=99, reshID, outfile='',
                        enhetsUtvalg=1, preprosess=TRUE, hentData=0) {


  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERHentRegData(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    Data <- NGERPreprosess(RegData=RegData, reshID=reshID)
    RegData <- Data
    rm(Data)
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

if (valgtVar=='OpAntibioticProphylaxis') {
	#Andel som får antibiotika
	#Kode 0,1: Nei, Ja (ingen tomme per 22.feb.2016)
	RegData <- RegData[which(RegData$OpAntibioticProphylaxis %in% 0:1), ]
	RegData$Variabel <- RegData[ ,valgtVar]
  	VarTxt <- 'profylakser'
	Tittel <- 'Andel som får antibiotika'
}

if (valgtVar=='OpBMI') {
	#Pasientskjema. Andel med BMI>30
	indMed <- which(RegData[ ,valgtVar] >30)
	RegData$Variabel[which(RegData[ ,valgtVar] >30)] <- 1
  	VarTxt <- 'med BMI>30'
	Tittel <- 'Pasienter med fedme'
}

if (valgtVar=='KomplinfekDyp3mnd') {
	#3MndSkjema. Andel med KomplinfekDyp3mnd=1
	#Kode 0,1: Nei, Ja +tomme
	RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplinfekDyp3mnd %in% 0:1)), ]
	RegData$Variabel <- RegData[ ,valgtVar]
  	VarTxt <- 'dype infeksjoner'
	Tittel <- 'Pasientrapportert dyp infeksjon, 3 mnd.'
}

if (valgtVar=='Reop') {
	#Andel OpType==2 (1:primær, 2: reop)
  #ComplReop
	#RegData <- RegData[which(RegData$OpType %in% 1:2), ]
	#RegData$Variabel[which(RegData$OpType == 2)] <- 1
  RegData$Variabel[which(RegData$ComplReop == 1)] <- 1
  VarTxt <- 'reoperasjoner'
	Tittel <- 'Reoperasjoner (basert på [ComplReop])'
}
if (valgtVar=='AlvorligeHendelser') {
	#3MndSkjema. Andel med KomplinfekOverfl3mnd=1
	#Kode 0,1: Nei, Ja +tomme
	RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplinfekOverfl3mnd %in% 0:1)), ]
	RegData$Variabel <- RegData[ ,valgtVar]
  	VarTxt <- 'overfladiske infeksjoner'
	Tittel <- 'Overfladisk infeksjon, 3 mnd.'
}

if (valgtVar=='StatusFollowup') {
	#Andel med StatusFollowup=1 (av samtlige, også tomme reg.)
	#Kode: tomme, -1,0,1
  #Tar ut hendelser siste 6 uker:
  datoTil <- min(datoTil, as.POSIXlt(Sys.Date() - 6*7))
  RegData$Variabel[RegData$StatusFollowup==1] <- 1
  VarTxt <- 'av postoperativ oppfølging'
	Tittel <- 'Pasienter som har fått postoperativ oppfølging'
}


if (valgtVar == 'Utdanning') {
	#PasientSkjema. Andel med Utdanning 4 el 5
	#Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
			#Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
	RegData <- RegData[intersect(which(RegData$Utdanning %in% 1:5), which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
  	VarTxt <- 'med høyere utdanning'
	Tittel <- 'Andel høyskole-/universitetsutdannede'
}

NGERUtvalg <- NGERLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                            MCEType=MCEType)
RegData <- NGERUtvalg$RegData
utvalgTxt <- NGERUtvalg$utvalgTxt

RegData$Aar <- 1900 + strptime(RegData$InnDato, format="%Y")$year


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
      smltxt <- 'landet forøvrig'
      indRest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }


NHovedRes <- length(indHoved)
NSmlRes <- length(indRest)


#-------------------------Beregning av andel-----------------------------------------
Aartxt <- min(RegData$Aar):max(RegData$Aar)
RegData$Aar <- factor(RegData$Aar, levels=Aartxt)

NAarRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest], length)
NAarHendRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest],sum, na.rm=T)
AndelRest <- NAarHendRest/NAarRest*100
NAarHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'], length)
NAarHendHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'],sum, na.rm=T)
AndelHoved <- NAarHendHoved/NAarHoved*100
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


ymax <- min(119, 1.25*max(Andeler,na.rm=T))
plot(Aartxt, AndelHoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
		xlim= c(Aartxt[1], max(Aartxt)), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(Aartxt), max(Aartxt),length(Aartxt)-1)
		cex=2, xlab='Operasjonsår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i') 	#Operasjonsår,

#plot(Aartxt, Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
#		#cex=0.8, cex.lab=0.9, cex.axis=0.9,
#		ylab=c(ytxt,'med 95% konfidensintervall'),
#		xlab='Operasjonsår', xaxt='n',
#		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = Aartxt)

title(Tittel, line=1, font.main=1)

#Legge på linjer i plottet. Denne kan nok gjøres mer elegant...
if ((ymax > 10) & (ymax < 40)) {lines(range(Aartxt),rep(10,2), col=farger[4])}
if (ymax > 20) {lines(range(Aartxt),rep(20,2), col=farger[4])}
if ((ymax > 30) & (ymax < 40)) {lines(range(Aartxt),rep(30,2), col=farger[4])}
if (ymax > 40) {lines(range(Aartxt),rep(40,2), col=farger[4])}
if (ymax > 60) {lines(range(Aartxt),rep(60,2), col=farger[4])}
if (ymax > 80) {lines(range(Aartxt),rep(80,2), col=farger[4])}
if (ymax > 100) {lines(range(Aartxt),rep(100,2), col=farger[4])}
#		axis(2, at=c(0,20,40,60,80,100), pos=0),


lines(Aartxt, AndelHoved, col=fargeHoved, lwd=3)
points(Aartxt, AndelHoved, pch="'", cex=2, col=fargeHoved)
text(Aartxt, AndelHoved, pos=1, NAarHendHoved, cex=0.9, col=fargeHoved)

lines(Aartxt, AndelRest, col=fargeRest, lwd=3)
points(Aartxt, AndelRest, pch="'", cex=2, col=fargeRest)	#}

Ttxt <- paste('(Tall ved punktene angir antall ', VarTxt, ')', sep='')
if (medSml == 1) {
	text(Aartxt, AndelRest, pos=3, NAarHendRest, cex=0.9, col=fargeRest)
	legend('topleft', border=NA, c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''),
		paste(smltxt, ' (N=', NSmlRes, ')', sep=''), Ttxt), bty='n', ncol=1, cex=cexleg,
		col=c(fargeHoved, fargeRest, NA), lwd=3)
	} else {
		legend('top', c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''), Ttxt),
		col=c(fargeHoved, NA), lwd=3, bty='n')
	}

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
#------------------------------------------------------------------------------

}	#end else statement
}	#end function



