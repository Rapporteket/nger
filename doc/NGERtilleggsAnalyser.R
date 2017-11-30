rm(list=ls())

dato <- '2017-11-30'
#NGERBasis <- read.table(paste0('A:/NGER/AlleVarNum', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #,
#NGERForlop <- read.table(paste0('A:/NGER/ForlopsOversikt', dato, '.csv'), sep=';', header=T, fileEncoding = 'UTF-8')
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

RegData <- NGERPreprosess(RegData)
Utvalg <- NGERUtvalgEnh(RegData, datoFra = datoFra, datoTil = datoTil)
RegData <- Utvalg$RegData
RegData$Aar <- as.character(strftime(RegData$InnDato, format="%Y"))
#table(RegData$Aar)
RegData <- RegData[which(RegData$ReshId %in% reshID),] #Bare Ullevål, Haukeland og Tønsberg
#table(RegData$ShNavn)

DiagVar <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3')
ind <- NULL
#var <- DiagVar[2]
for (var in DiagVar) {
  RegData[ ,var] <- toupper(RegData[ ,var])
  ind <- union(ind, grep('N80',RegData[ ,var]))
}
#759, 248, 265
RegDataN80 <- RegData[ind, c('ShNavn', "OpDato", 'Aar', DiagVar,  "LapProsedyre1", "LapProsedyre2", "LapProsedyre3")]
table(RegData$Aar, RegData$ShNavn)
write.table(RegDataN80, file = "A:/NGER/RegDataN80.csv", row.names= FALSE, sep = ';') #, fileEncoding = 'UTF-8') #


