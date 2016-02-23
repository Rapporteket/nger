rm(list=ls())
# admdata <- read.table('C:/SVN/jasper/nger/data/ForlopsOversikt2015-10-09 09-49-55.txt', sep=';', header=T)
# RegData <- read.table('C:/SVN/jasper/nger/data/AlleVarNum2015-10-09 09-49-49.txt', sep=';', header=T)
RegData <- read.table('C:/SVN/jasper/nger/data/AlleVarNum2016-02-04 10-35-25.txt', sep=';', header=T)
admdata <- read.table('C:/SVN/jasper/nger/data/ForlopsOversikt2016-02-04 10-35-42.txt', sep=';', header=T)

RegData <- merge(RegData, admdata, by.x = 'MCEID', by.y = 'ForlopsID')


reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2014-01-01' 	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2099-12-31'
enhetsUtvalg <- 1 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet, (3-egen enhet mot egen region)
libkat <- 'C:/SVN/jasper/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
valgtVar <- 'Alder'
#Må velge... Alder, Education, Alvorlighetsgrad, MaritalStatus, OpAnesthetic, OpASA, HypCompleteness, PatientNorwegian, OpBMICategory,
#             Opcat, OpType
outfile <- ""
hentData<-0
preprosess=T
MCEType<-99


if (outfile=='') {
  x11() #figure new window
}
nger::FigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
           minald=minald, maxald=maxald, outfile=outfile, reshID=reshID, enhetsUtvalg=enhetsUtvalg,
           MCEType=MCEType, hentData=hentData, preprosess=preprosess)



tmp <- NGERAntallRegPrAvd(RegData = RegData)

OpInd <- NGEROpInd(RegData=RegData, datoFra='2000-04-01', datoTil='2050-12-31', minald=0, maxald=130, outfile='',
                   reshID=reshID, enhetsUtvalg=1, MCEType=99, preprosess=TRUE, hentData=F)


## For Are:
## tabXXI <- data.frame(Frekvens=apply(mydf, 2, sum, na.rm = TRUE), Andel=apply(mydf, 2, sum, na.rm = TRUE)/N)








# sort(table(admdata$SykehusNavn))
# sort(table(RegData$AVD_RESH))


# Finn alle operasjonsindikatorer:
# AlleOpInd <- unique(c(unique(toupper(as.character(RegData$OpInd1))),
#                       unique(toupper(as.character(RegData$OpInd2))),
#                         unique(toupper(as.character(RegData$OpInd3)))))
#
# aux <- apply(apply(RegData[, c('OpInd1', 'OpInd2', 'OpInd3')], 2, function(x){
#   return(as.character(toupper(x)))}), 2, factor, levels = sort(AlleOpInd))
# Tabell1 <- table(aux)
# Tabell1 <- sort(Tabell1[-which(names(Tabell1)=='')], decreasing = TRUE)[1:20]
#
# # Lag faktorer med ALLE nivåene inkludert
# tmp1<- factor(toupper(as.character(RegData$OpInd1)), levels = sort(AlleOpInd))
# tmp2<- factor(toupper(as.character(RegData$OpInd2)), levels = sort(AlleOpInd))
# tmp3<- factor(toupper(as.character(RegData$OpInd3)), levels = sort(AlleOpInd))
#
# # Kombiner tabellene
# Tabell <- table(tmp1) + table(tmp2) + table(tmp3)
#
# # Ikke tell med de tomme feltene
# if (names(Tabell)[1]=='') {Tabell <- Tabell[2:length(Tabell)]}
#
# # Plukk ut de 20 hyppigst forekommende operasjonsindikatorene
# Tabell <- sort(Tabell, decreasing = TRUE)[1:20]
#
#
#
#
# reshID <- 110734 # 110734 (Tønsberg)    #Må sendes med til funksjon
# # reshID <- 601227
# minald <- 0	#alder, fra og med
# maxald <- 130	#alder, til og med
# datoFra <- as.Date('2000-01-01') 	 # min og max dato i utvalget vises alltid i figuren.
# datoTil <- as.Date('2015-12-31')
# minCom=1
# maxCom=4
# enhetsUtvalg <- 1 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet, (3-egen enhet mot egen region)
# libkat <- 'C:/SVN/jasper/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
# valgtVar <- 'Alder'
# #Må velge... Alder, Education, Alvorlighetsgrad, MaritalStatus, OpAnesthetic, OpASA, HypCompleteness, PatientNorwegian, OpBMICategory,
# #             Opcat, OpType
# outfile <- "" #paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper
# minASA=1; maxASA=5
# minBMI=0; maxBMI=100; hentData=0
#
#
#
#
# tmp<-table(RegData$PatientID)
# MultippelIDer <- as.numeric(names(tmp[tmp>1]))
#
# regdata <- RegData
#
# regdata <- regdata[-which(regdata$PatientID %in% MultippelIDer),]
# RegData2 <- rbind(regdata, RegData[match(MultippelIDer, RegData$PatientID),])
#
# RegData3 <- rbind(RegData[-which(RegData$PatientID %in% MultippelIDer),], RegData[match(MultippelIDer, RegData$PatientID),])
#
# RegData <- RegData[match(unique(RegData$PatientID), RegData$PatientID),]
#
#
# RegData$PatientID[RegData$PatientID %in% MultippelIDer]
# RegData$Alder[RegData$PatientID %in% MultippelIDer]
# RegData[RegData$PatientID %in% MultippelIDer, c('PatientID', 'Alder')]
# RegData[RegData$PatientID %in% MultippelIDer, c('PatientID', 'Alder', 'Education')]
# RegData[match(MultippelIDer, RegData$PatientID), c('PatientID', 'Alder', 'Education')]
#
# cbind(RegData[RegData$PatientID %in% MultippelIDer, c('PatientID', 'Alder', 'Education')][1:50,],
#       RegData[match(MultippelIDer, RegData$PatientID), c('PatientID', 'Alder', 'Education')][1:50,])
#


