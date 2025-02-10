rm(list=ls())

dato <- '2017-11-30'
#NGERBasis <- read.table(paste0('A:/NGER/allevarnum', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #,
#NGERForlop <- read.table(paste0('A:/NGER/forlopsoversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
#NGERData <- merge(NGERBasis, NGERForlop, by = "ForlopsID", suffixes = c('','YY'),all.x = TRUE, all.y=FALSE)
#RegData <- NGERData
#save(RegData, file=paste0('A:/NGER/NGER', dato, '.Rdata'))
load(paste0('A:/NGER/NGER', dato, '.Rdata'))

library(nger)
setwd("C:/ResultattjenesteGIT/nger/")
#-----------------------------------Datauttrekk filtrert på LapDiagnose ------------------------------
#anomysierte, filtrerte data fra OUS, Bergen og Tønsberg fra 2016 til i dag
#med diagnosekode N80.xx
# Oslo universitetssykehus Ullevål: 700399
# Bergen universitetssykehus Haukeland: 102954
# Sykehuset i Vestfold Tønsberg: 110734
reshID <- c(110734, 102954, 700399)  # 110734 (Tønsberg)  	#Må sendes med til funksjon
datoFra <- '2016-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2017-12-31'

RegData <- NGERPreprosess(RegData) ##Her konverteres til bare store bokstaver i prosedyre- og diagnosekoder
Utvalg <- NGERUtvalgEnh(RegData, datoFra = datoFra, datoTil = datoTil)
RegData <- Utvalg$RegData
RegData$Aar <- as.character(strftime(RegData$InnDato, format="%Y"))
#table(RegData$Aar)
RegData <- RegData[which(RegData$ReshId %in% reshID),] #Bare Ullevål, Haukeland og Tønsberg
#table(RegData$ShNavn)

DiagVar <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3', 'HysDiagnose1','HysDiagnose2', 'HysDiagnose3')

ind <- NULL
for (var in DiagVar) {
  ind <- union(ind, grep('N80',RegData[ ,var]))
}
#759, 248, 71
RegDataN80 <- RegData[ind, c('ShNavn', "OpDato", 'Aar', DiagVar,  "LapProsedyre1", "LapProsedyre2", "LapProsedyre3")]
table(RegData$Aar, RegData$ShNavn)
write.table(RegDataN80, file = "A:/NGER/RegDataN80.csv", row.names= FALSE, sep = ';') #, fileEncoding = 'UTF-8') #



#---------- Lage variabel for Ovarialcyster og Endometriose
RegData <- NGERPreprosess(RegData) #Her konverteres til bare store bokstaver i prosedyre- og diagnosekoder

DiagVar <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3', 'HysDiagnose1','HysDiagnose2', 'HysDiagnose3')
#Ovarialcyster: Diagnosekoder N83.0, N83.1, N83.2 og D27
koder <- c('N830', 'N831', 'N832', 'D27')
indOvar <- NULL
for (var in DiagVar) {
  indOvar <- union(indOvar, grep(paste(koder, collapse = "|"), RegData[ ,var]))
  #(Se også på pmatch, carmatch
}
ind <- which(RegData$ReshId == 110734)
table(RegData$Aar[intersect(indOvar,ind)])

#Endometriose i livmorveggen)  (N80.0)
indEndL <- NULL
for (var in DiagVar) {
  indEndL <- union(indEndL, grep('N800', RegData[ ,var]))
}
table(RegData$Aar[intersect(indEndL,ind)])

#Endometriose (N80.1-N80.9.)
koder <- paste0('N80', 1:9)
indEnd <- NULL
for (var in DiagVar) {
  indEnd <- union(indEnd, grep(paste(koder, collapse = "|"), RegData[ ,var]))
}
table(RegData$Aar[intersect(indEnd,ind)])















