#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for grupperingsvariabelen sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#'  Variable funksjonen benytter: Alder (beregnes), Opf0Komplikasjoner, Opf0Reoperasjon, Opf0KomplBlodning, Opf0KomplUtstyr,
#'  Opf0KomplInfeksjon, Opf0KomplOrganUtdanning, Opf0AlvorlighetsGrad
#'  HysKomplikasjoner, LapKomplikasjoner, OpMetode, OpAntibProfylakse, OpASA, OpBMI, Opf0Status.
#'  Det benyttes også andre variable til utvalg osv.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'		\item Alder: Andel pasienter over 70 år.
#'		\item KomplIntra: Komplikasjoner under operasjon (intraoperativt)
#'		\item KomplPostop: Postoperative komplikasjoner
#'		\item OpAntibProfylakse: Fått antibiotikaprofylakse
#'		\item OpASA: ASA-grad > II
#'		\item OpBMI: Pasienter med fedme (BMI>30)
#'		\item Opf0AlvorlighetsGrad: Alvorlige komplikasjoner (grad 3 og 4)
#'		\item Opf0KomplBlodning: Postop. komplikasjon: Blødning
#'		\item Opf0KomplUtstyr: Postop. komplikasjon: Problemer med ustyr
#'		\item Opf0KomplInfeksjon: Postop. komplikasjon: Infeksjon
#'		\item Opf0KomplOrgan: Postop. komplikasjon: Organskade
#'		\item Opf0Reoperasjon: Reoperasjon som følge av komplikasjon
#'		\item Opf0Status: Fått postoperativ oppfølging
#'    \item Tss2Mott: Møtet med gynekologisk avdeling var mindre godt
#'    \item Tss2Behandling: Behandlingens opplegg og innhold passet ikke pasienten
#'    \item Tss2Lytte: Pasientens behandlere lyttet- og forsto ikke det som ble tatt opp
#'    \item Tss2Behandlere: Pasienten hadde ikke tillit til sine behandlere
#'    \item Tss2Enighet: Pasient og behandlere ikke enige om målsetn. for behandlinga
#'    \item Tss2Generelt: Negativ eller svært negativ oppfatning om gyn. avd.
#'		\item Utdanning: Pasienter med høyere utdanning
#'    }
#'
#' @inheritParams NGERFigAndeler
#' @export

NGERFigAndelerGrVar <- function(RegData=0, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31',
                        minald=0, maxald=130, MCEType=99, Hastegrad='', AlvorlighetKompl='', Ngrense=10,
                        reshID, outfile='', enhetsUtvalg=1, preprosess=0, hentData=0) {

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData == 1){
    RegData <- NGERRegDataSQL(datoFra = datoFra, datoTil = datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess==1){
    RegData <- NGERPreprosess(RegData=RegData)
  }

'%i%' <- intersect
  cexShNavn <- 0.85

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
#reshID <- as.numeric(reshID)
#indEgen1 <- match(reshID, RegData$ReshId)
#smltxt <- 'Hele landet'
#if (enhetsUtvalg == 7) {
#		smltxt <- as.character(RegData$Region[indEgen1])
#		RegData <- RegData[which(RegData$Region == smltxt), ]	#kun egen region
#		cexShNavn <- 1
#	}

grVar <- 'ShNavn'
RegData[ ,grVar] <- factor(RegData[ ,grVar])


RegData$Variabel <- 0

if (valgtVar == 'Alder') {
#Andel over 70 år
	RegData$Variabel[which(RegData[ ,valgtVar] >= 70)] <- 1
	tittel <- 'Pasienter over 70 år'
}

if (valgtVar=='OpAntibProfylakse') {
	#Andel som får antibiotika
	#Kode 0,1: Nei, Ja (ingen tomme per 22.feb.2016)
	RegData <- RegData[which(RegData$OpAntibProfylakse %in% 0:1), ]
	RegData$Variabel <- RegData[ ,valgtVar]
	tittel <- 'Fått antibiotikaprofylakse'
}

if (valgtVar == 'OpASA') {
	#Andel med ASA-grad>2
	RegData <- RegData[which(RegData[,valgtVar] %in% 1:5), ]
	RegData$Variabel[which(RegData[ ,valgtVar] > 2)] <- 1
	tittel <- 'ASA-grad > II'
}
if (valgtVar == 'OpBMI') {
#BMI > 30
	RegData <- RegData[which(RegData[,valgtVar] >10), ]
	RegData$Variabel[which(RegData[ ,valgtVar] > 30)] <- 1
	tittel <- 'Pasienter med fedme (BMI > 30)'
}


### Komplikasjoner
if (valgtVar=='KomplPostop') {
	# Andel postoperative komplikasjoner
	#Kode 0: Nei, 1:Ja, tomme
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel <- RegData$Opf0Komplikasjoner
	tittel <- 'Komplikasjoner, postoperativt'
}

if (valgtVar=='Opf0KomplBlodning') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0KomplBlodning == 1)] <- 1
	tittel <- 'Postop. komplikasjon: Blødning'
}
if (valgtVar=='Opf0KomplUtstyr') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0KomplUtstyr == 1)] <- 1
	tittel <- 'Postop. komplikasjon: Problemer med ustyr'
}
if (valgtVar=='Opf0KomplInfeksjon') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0KomplInfeksjon == 1)] <- 1
	tittel <- 'Postop. komplikasjon: Infeksjon'
}
if (valgtVar=='Opf0KomplOrgan') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0KomplOrgan == 1)] <- 1
	tittel <- 'Postop. komplikasjon: Organskade'
}

if (valgtVar=='Opf0Reoperasjon') {
	#Kode 0: Nei, 1:Ja
	RegData <- RegData[intersect(which(RegData$Opf0Komplikasjoner %in% 0:1), which(RegData$Opf0Status == 1)), ]
	RegData$Variabel[which(RegData$Opf0Reoperasjon == 1)] <- 1
	tittel <- 'Postop. komplikasjon: Reoperasjon'
}

if (valgtVar=='Opf0AlvorlighetsGrad') {
	#Andel av postoperative komplikasjoner som var alvorlige (3 og 4)
	#Kode 1-Lite alvorlig, 2-Middels alvorlig, 3-Alvorlig, 4-Dødelig
	RegData <- RegData[which(RegData$Opf0Komplikasjoner %in% 0:1) %i% which(RegData$Opf0Status == 1), ]
	#RegData <- RegData[which(RegData$Opf0AlvorlighetsGrad %in% 1:4), ]
	RegData$Variabel[which(RegData$Opf0AlvorlighetsGrad %in% 3:4)] <- 1
	tittel <- 'Alvorlige komplikasjoner (grad 3 og 4)'
}
if (valgtVar == 'Tss2Mott') {
  #0:Mindre godt, 1:Ingen mening, 2:Ganske godt, 3:Svært godt
  RegData <- RegData[which(RegData$Tss2Status == 1), ]
  #tittel <- 'Hvordan ble du møtt på gynekologisk avdeling?'
  tittel <- 'Møtet med gynekologisk avdeling var mindre godt'
  RegData$Variabel[which(RegData$Tss2Mott == 0)] <- 1
}
if (valgtVar == 'Tss2Behandling') {
  #0:Passet ikke, 1:Verken eller, 2:Ganske bra, 3:Svært bra
  RegData <- RegData[which(RegData$Tss2Status == 1), ]
  tittel <- 'Behandlingens opplegg og innhold passet ikke pasienten'
  RegData$Variabel[which(RegData$Tss2Behandling == 0)] <- 1
}
if (valgtVar == 'Tss2Lytte') {
  #0:Nei, 1:Ja, til en viss grad, 2:Ja, i ganske stor grad, 3:Ja, i svært stor grad
  RegData <- RegData[which(RegData$Tss2Status == 1), ]
  tittel <- 'Pasientens behandlere lyttet- og forsto ikke det som ble tatt opp'
  RegData$Variabel[which(RegData$Tss2Lytte == 0)] <- 1
}
if (valgtVar == 'Tss2Behandlere') {
  #0:Nei, 1:Ja, til en viss grad, 2:Ja, i ganske stor grad, 3:Ja, i svært stor grad
  RegData <- RegData[which(RegData$Tss2Status == 1), ]
  tittel <- 'Pasienten hadde ikke tillit til sine behandlere'
  RegData$Variabel[which(RegData$Tss2Behandlere == 0)] <- 1
}
if (valgtVar == 'Tss2Enighet') {
  #0:Nei, 1:Ja, til en viss grad, 2:Ja, i ganske stor grad, 3:Ja, i svært stor grad
  RegData <- RegData[which(RegData$Tss2Status == 1), ]
  tittel <- 'Pasient og behandlere ikke enige om målsetn. for behandlinga'
  RegData$Variabel[which(RegData$Tss2Enighet == 0)] <- 1
}
if (valgtVar == 'Tss2Generelt') {
  #0:Svært negativ, 1:Negativ, 2:Nøytral, 3:Positiv, 4:Svært positiv
  RegData <- RegData[which(RegData$Tss2Status == 1), ]
  tittel <- 'Negativ eller svært negativ oppfatning om gyn. avd.'
  RegData$Variabel[which(RegData$Tss2Generelt == 0)] <- 1
}

if (valgtVar=='KomplIntra') {
	# Komplikasjoner ved operasjon. Må kombinere HysKomplikasjoner og LapKomplikasjoner
	#Kode 0: Nei, 1:Ja, tomme
	RegData$KomplIntra <- with(RegData, HysKomplikasjoner + LapKomplikasjoner) #Får mange tomme!!!
	if (MCEType %in% c(1,4:6)) {indMed <- which(RegData$LapKomplikasjoner %in% 0:1)
	                            indVar <- which(RegData$LapKomplikasjoner == 1)
	                   } else {
	  indMed <- switch(as.character(MCEType),
					'2' = which(RegData$HysKomplikasjoner %in% 0:1),
					'3' = which(RegData$KomplIntra %in% 0:1),	#Få tomme for dette valget
					'99' = union(which(is.finite(RegData$HysKomplikasjoner)), which(is.finite(RegData$LapKomplikasjoner))))
  	indVar <- switch(as.character(MCEType),
					#'1' = which(RegData$LapKomplikasjoner == 1),
					'2' = which(RegData$HysKomplikasjoner == 1),
					'3' = which(RegData$KomplIntra %in% 1:2),
					'99' = union(which(RegData$HysKomplikasjoner == 1), which(RegData$LapKomplikasjoner==1)))
	                   }
  	RegData$Variabel[indVar] <- 1
  	RegData <- RegData[indMed, ]
  	tittel <- 'Komplikasjoner, intraoperativt'
}

if (valgtVar=='Opf0Status') {
	#Andel med Opf0Status=1 (av samtlige, også tomme reg.)
	#Kode: tomme, -1,0,1
  #Tar ut hendelser siste 8 uker:
  datoTil <- min(as.POSIXlt(datoTil), as.POSIXlt(Sys.Date() - 8*7))
  RegData$Variabel[RegData$Opf0Status==1] <- 1
  VarTxt <- 'av postoperativ oppfølging'
	tittel <- 'Pasienter som har fått oppfølging etter 6-8 uker'
}
#Lag figur for ett års oppfølging


if (valgtVar == 'Utdanning') {
	#PasientSkjema. Andel med Utdanning 4 el 5
	#Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
			#Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
	RegData <- RegData[which(RegData$Utdanning %in% 1:5), ]		#which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:3)] <- 1
  	VarTxt <- 'uten høyere utdanning'
	tittel <- 'Andel uten høyere utdanning'
}


#Gjør utvalg
NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                         MCEType=MCEType, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad)
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
	Ngrtxt <- as.character(Ngr)
	Ngrtxt[indGrUt] <- paste0('<', Ngrense)	#paste(' (<', Ngrense,')',sep='')	#

	AndelerGrSort <- AndelerGr[sortInd]
	AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
	GrNavnSort <- paste0(names(Ngr)[sortInd], ' (',Ngrtxt[sortInd], ')')
#	GrNavnSort <- names(Ngr)[sortInd]

	andeltxt <- paste0(sprintf('%.1f',AndelerGrSort), '%') 	#round(as.numeric(AndelerGrSort),1)
	if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}


#-----------Figur---------------------------------------
if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	if (dim(RegData)[1]>0) {
	tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
	} else {tekst <- 'Ingen registrerte data for dette utvalget'}
	title(main=tittel)
	text(0.5, 0.6, tekst, cex=1.2)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
if ( outfile != '') {dev.off()}

} else {

#--------------------------FIGUR---------------------------------------------------
#Innparametre: ...


FigTypUt <- figtype(outfile, fargepalett=NGERUtvalg$fargepalett)	#height=3*800,
farger <- FigTypUt$farger
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.75)
		#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

xmax <- min(max(AndelerGrSort),100)*1.15
pos <- barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], #main=tittel,
	xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=0.7)
ybunn <- 0.1
ytopp <- pos[AntGr]+1	#-length(indGrUt)]
lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
	legend=paste0(NGERUtvalg$hovedgrTxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
	bty='o', bg='white', box.col='white')
mtext(at=max(pos)+0.35*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)
mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
#text(x=0.005*xmax, y=pos, Ngrtxt[sortInd], las=1, cex=cexShNavn, adj=0, col=farger[4], lwd=3)	#Legge til N
title(tittel, line=1, font.main=1, cex.main=1.2)

text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
		las=1, cex=0.8, adj=0, col=farger[1])	#Andeler, hvert sykehus

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
}
}

