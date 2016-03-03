#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en grupperingsvariabelen sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#'  Variable funksjonen benytter: Alder (beregnes), ComplExist, ComplReop, ComplAfterBleed, ComplEquipment,
#'  ComplInfection, ComplOrganEducation, FollowupSeriousness
#'  HypComplications, LapComplications, MCEType, OpAntibioticProphylaxis, OpASA, OpBMI, StatusFollowup.
#'  Det benyttes også andre variable til utvalg osv.
#'
#' @inheritParams FigAndelTid
#' @param valgtVar: Velg hvilken variabel du ønsker å se resultat for
#'		Alder: Andel pasienter over 70 år.
#'		ComplAfterBleed: Postop. komplikasjon: Blødning
#'		ComplEquipment: Postop. komplikasjon: Problemer med ustyr
#'		ComplInfection: Postop. komplikasjon: Infeksjon
#'		ComplOrgan: Postop. komplikasjon: Organskade
#'		ComplReop: Reoperasjon som følge av komplikasjon
#'		Education: Pasienter med høyere utdanning
#'		FollowupSeriousness: Alvorlige komplikasjoner
#'		KomplIntra: Komplikasjoner under operasjon (intraoperativt)
#'		KomplPostop: Postoperative komplikasjoner
#'		OpAntibioticProphylaxis: Fått antibiotikaprofylakse
#'		OpASA: ASA-grad > II
#'		OpBMI: Pasienter med fedme (BMI>30)
#'		StatusFollowup: Fått postoperativ pensum
#' @export

FigAndelerGrVar <- function(RegData=0, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31',
                        minald=0, maxald=130, MCEType=99, AlvorlighetKompl=99, reshID, outfile='',
                        enhetsUtvalg=1, preprosess=TRUE, hentData=0) {

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERHentRegData(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- NGERPreprosess(RegData=RegData, reshID=reshID)
  }

cexShNavn <- 0.85

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
smltxt <- 'Hele landet'
if (enhetsUtvalg == 7) {
		smltxt <- as.character(RegData$Region[indEgen1])
		RegData <- RegData[which(RegData$Region == smltxt), ]	#kun egen region
		cexShNavn <- 1
	}

grVar <- 'SykehusNavn'
RegData[ ,grVar] <- factor(RegData[ ,grVar])
Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist


RegData$Variabel <- 0

if (valgtVar == 'Alder') {
#Andel over 70 år
	RegData$Variabel[which(RegData[ ,valgtVar] >= 70)] <- 1
	Tittel <- 'Pasienter over 70 år'
}

if (valgtVar=='OpAntibioticProphylaxis') {
	#Andel som får antibiotika
	#Kode 0,1: Nei, Ja (ingen tomme per 22.feb.2016)
	RegData <- RegData[which(RegData$OpAntibioticProphylaxis %in% 0:1), ]
	RegData$Variabel <- RegData[ ,valgtVar]
	Tittel <- 'Fått antibiotikaprofylakse'
}

if (valgtVar == 'OpASA') {
	RegData <- RegData[which(RegData[,valgtVar] %in% 1:5), ]
	RegData$Variabel[which(RegData[ ,valgtVar] > 2)] <- 1
	Tittel <- 'ASA-grad > II'
}
if (valgtVar == 'OpBMI') {
#BMI > 30
	RegData <- RegData[which(RegData[,valgtVar] >10), ]
	RegData$Variabel[which(RegData[ ,valgtVar] > 30)] <- 1
	Tittel <- 'Pasienter med fedme'
}


### Komplikasjoner
if (valgtVar=='KomplPostop') {
	# Andel postoperative komplikasjoner
	#Kode 0: Nei, 1:Ja, tomme
	RegData <- RegData[intersect(which(RegData$ComplExist %in% 0:1), which(RegData$OppflgRegStatus==2)), ]
	RegData$Variabel <- RegData$ComplExist
	Tittel <- 'Komplikasjoner, postoperativt[ComplExist], uten ukjente'
}

if (valgtVar=='ComplAfterBleed') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$ComplExist %in% 0:1), which(RegData$OppflgRegStatus==2)), ]
	RegData$Variabel[which(RegData$ComplAfterBleed == 1)] <- 1
	Tittel <- 'Postop. komplikasjon: Blødning'
}
if (valgtVar=='ComplEquipment') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$ComplExist %in% 0:1), which(RegData$OppflgRegStatus==2)), ]
	RegData$Variabel[which(RegData$ComplEquipment == 1)] <- 1
	Tittel <- 'Postop. komplikasjon: Problemer med ustyr'
}
if (valgtVar=='ComplInfection') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$ComplExist %in% 0:1), which(RegData$OppflgRegStatus==2)), ]
	RegData$Variabel[which(RegData$ComplInfection == 1)] <- 1
	Tittel <- 'Postop. komplikasjon: Infeksjon'
}
if (valgtVar=='ComplOrgan') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$ComplExist %in% 0:1), which(RegData$OppflgRegStatus==2)), ]
	RegData$Variabel[which(RegData$ComplOrgan == 1)] <- 1
	Tittel <- 'Postop. komplikasjon: Organskade'
}

if (valgtVar=='ComplReop') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$ComplExist %in% 0:1), which(RegData$OppflgRegStatus==2)), ]
	RegData$Variabel[which(RegData$ComplReop == 1)] <- 1
	Tittel <- 'Postop. komplikasjon: Reoperasjon'
}

if (valgtVar=='FollowupSeriousness') {
	#Andel av postoperative komplikasjoner som var alvorlige (3 og 4)
	#Kode 1-Lite alvorlig, 2-Middels alvorlig, 3-Alvorlig, 4-Dødelig
	RegData <- RegData[which(RegData$FollowupSeriousness %in% 1:4), ]
	RegData$Variabel[which(RegData$FollowupSeriousness %in% 3:4)] <- 1
	Tittel <- 'Alvorlige komplikasjoner (grad 3 og 4) av alle komplikasjoner'
}

if (valgtVar=='KomplIntra') {
	# Komplikasjoner ved operasjon. Må kombinere HypComplications og LapComplications
	#Kode 0: Nei, 1:Ja, tomme
	RegData$KomplIntra <- with(RegData, HypComplications + LapComplications) #Får mange tomme!!!
  	indMed <- switch(as.character(MCEType),
					'1' = which(RegData$LapComplications %in% 0:1),
					'2' = which(RegData$HypComplications %in% 0:1),
					'3' = which(RegData$KomplIntra %in% 0:1),	#Få tomme for dette valget
					'99' = union(which(is.finite(RegData$HypComplications)), which(is.finite(RegData$LapComplications))))
	RegData <- RegData[indMed, ]
  	indVar <- switch(as.character(MCEType),
					'1' = which(RegData$LapComplications == 1),
					'2' = which(RegData$HypComplications == 1),
					'3' = which(RegData$KomplIntra == 1),
					'99' = union(which(RegData$HypComplications == 1), which(RegData$LapComplications==1)))
	RegData$Variabel[indVar] <- 1
	Tittel <- 'Komplikasjoner, intraoperativt [HypCompl og LapCompl], uten tomme'
}

if (valgtVar=='StatusFollowup') {
	#Andel med StatusFollowup=1 (av samtlige, også tomme reg.)
	#Kode: tomme, -1,0,1
  #Tar ut hendelser siste 6 uker:
  datoTil <- min(as.POSIXlt(datoTil), as.POSIXlt(Sys.Date() - 6*7))
  RegData$Variabel[RegData$StatusFollowup==1] <- 1
  VarTxt <- 'av postoperativ oppfølging'
	Tittel <- 'Pasienter som har fått postoperativ oppfølging'
}


if (valgtVar == 'Education') {
	#PasientSkjema. Andel med Utdanning 4 el 5
	#Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
			#Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
	RegData <- RegData[which(RegData$Education %in% 1:5), ]		#which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
  	VarTxt <- 'med høyere utdanning'
	Tittel <- 'Andel høyskole-/universitetsutdannede'
}


#Gjør utvalg
NGERUtvalg <- NGERLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                            MCEType=MCEType, AlvorlighetKompl=AlvorlighetKompl)
RegData <- NGERUtvalg$RegData
utvalgTxt <- NGERUtvalg$utvalgTxt


	dummy0 <- -0.001
	N <- dim(RegData)[1]
	Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
	if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
	AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
	AndelerGr <- round(100*Nvar/Ngr,2)

	indGrUt <- as.numeric(which(Ngr < Ngrense))
	if (length(indGrUt)==0) { indGrUt <- 0}
	AndelerGr[indGrUt] <- dummy0
	sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE)
	Ngrtxt <- paste('N=', as.character(Ngr), sep='')	#
	Ngrtxt[indGrUt] <- paste('N<', Ngrense,sep='')	#paste(' (<', Ngrense,')',sep='')	#

	AndelerGrSort <- AndelerGr[sortInd]
	AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
#	GrNavnSort <- paste(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd], sep='')
	GrNavnSort <- names(Ngr)[sortInd]

	andeltxt <- paste(sprintf('%.1f',AndelerGrSort), '%',sep='') 	#round(as.numeric(AndelerGrSort),1)
	if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}


#-----------Figur---------------------------------------
if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	if (dim(RegData)[1]>0) {
	tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
	} else {tekst <- 'Ingen registrerte data for dette utvalget'}
	title(main=Tittel)
	text(0.5, 0.6, tekst, cex=1.2)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
if ( outfile != '') {dev.off()}

} else {

#--------------------------FIGUR---------------------------------------------------
#Innparametre: ...


FigTypUt <- figtype(outfile, height=1*800, fargepalett=NGERUtvalg$fargepalett)
farger <- FigTypUt$farger
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.7)
		#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

xmax <- min(max(AndelerGrSort),100)*1.15
pos <- barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], #main=Tittel,
	xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=0.7)
ybunn <- 0.1
ytopp <- pos[AntGr]+1	#-length(indGrUt)]
lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
	legend=paste(smltxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N,sep='' ),
	bty='o', bg='white', box.col='white')
mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
text(x=0.005*xmax, y=pos, Ngrtxt[sortInd], las=1, cex=cexShNavn, adj=0, col=farger[4], lwd=3)	#c(Nshtxt[sortInd],''),
title(Tittel, line=1, font.main=1, cex.main=1.2)

text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
		las=1, cex=0.8, adj=0, col=farger[1])	#Andeler, hvert sykehus

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
}
}

