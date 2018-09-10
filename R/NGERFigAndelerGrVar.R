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
#'    \item RegForsinkelse: Mer enn XX (1mnd?) fra operasjon til ferdigstilt registrering
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
#' @inheritParams NGERUtvalgEnh
#' @export

NGERFigAndelerGrVar <- function(RegData=0, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31', velgAvd='',
                                minald=0, maxald=130, MCEType=99, Hastegrad='', AlvorlighetKompl='', Ngrense=10,
                                reshID, outfile='', enhetsUtvalg=0, velgDiag=0, preprosess=1, hentData=0) {

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

  NGERVarSpes <- NGERVarTilrettelegg(RegData, valgtVar=valgtVar, grVar='', figurtype='andelGrVar')
  RegData <- NGERVarSpes$RegData
  flerevar <- NGERVarSpes$flerevar
  subtxt <- NGERVarSpes$subtxt
  grtxt <- NGERVarSpes$grtxt

  grVar <- 'ShNavn'
  #RegData[ ,grVar] <- factor(RegData[ ,grVar])


  NGERUtvalg <- NGERUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                              MCEType=MCEType, AlvorlighetKompl=AlvorlighetKompl, Hastegrad=Hastegrad,
                              velgAvd=velgAvd, velgDiag=velgDiag)
  RegData <- NGERUtvalg$RegData
  utvalgTxt <- NGERUtvalg$utvalgTxt


  dummy0 <- NA #-0.001
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
    title(main=NGERVarSpes$tittel)
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

    xmax <- min(max(AndelerGrSort, na.rm = T),100)*1.15
    pos <- barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], #main=tittel,
                   xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=0.7)
    ybunn <- 0.1
    ytopp <- pos[AntGr]+1	#-length(indGrUt)]
    lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
    legend('top', xjust=1, cex=1, lwd=2, col=farger[2],
           legend=paste0(NGERUtvalg$hovedgrTxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
           bty='o', bg='white', box.col='white')
    mtext(at=max(pos)+0.35*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)
    mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
    #text(x=0.005*xmax, y=pos, Ngrtxt[sortInd], las=1, cex=cexShNavn, adj=0, col=farger[4], lwd=3)	#Legge til N
    title(NGERVarSpes$tittel, line=1, font.main=1, cex.main=1.2)

    text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
         las=1, cex=0.8, adj=0, col=farger[1])	#Andeler, hvert sykehus

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #----------------------------------------------------------------------------------
  }
}

