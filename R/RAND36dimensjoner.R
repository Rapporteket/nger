#' Beregning og test av tidligere beregnede dimensjoner i RAND36
#'
#'
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return RAND36
#'
#' @export


NGERFigGjsnGrVar <- function(RegData) {


RegData <- RegData[which(RegData$R0Status ==1),]
library(plyr)

#Definerer variabelnavn
 RAND36 <- data.frame('r1'= RegData$R0Spm1,
 'Endring' = RegData$R0Spm2,
 'r3a' = RegData$R0Spm3a,
 'r3b' = RegData$R0Spm3b,
 'r3c' = RegData$R0Spm3c,
 'r3d' = RegData$R0Spm3d,
 'r3e' = RegData$R0Spm3e,
 'r3f' = RegData$R0Spm3f,
 'r3g' = RegData$R0Spm3g,
 'r3h' = RegData$R0Spm3h,
 'r3i' = RegData$R0Spm3i,
 'r3j' = RegData$R0Spm3j,
 'r4a' = RegData$R0Spm4a,
 'r4b' = RegData$R0Spm4b,
 'r4c' = RegData$R0Spm4c,
 'r4d' = RegData$R0Spm4d,
 'r5a' = RegData$R0Spm5a,
 'r5b' = RegData$R0Spm5b,
 'r5c' = RegData$R0Spm5c,
 'r6' = RegData$R0Spm6,
 'r7' = RegData$R0Spm7,
 'r8' = RegData$R0Spm8,
 'r9a' = RegData$R0Spm9a,
 'r9b' = RegData$R0Spm9b,
 'r9c' = RegData$R0Spm9c,
 'r9d' = RegData$R0Spm9d,
 'r9e' = RegData$R0Spm9e,
 'r9f' = RegData$R0Spm9f,
 'r9g' = RegData$R0Spm9g,
 'r9h' = RegData$R0Spm9h,
 'r9i' = RegData$R0Spm9i,
 'r10' = RegData$R0Spm10,
 'r11a' = RegData$R0Spm11a,
 'r11b' = RegData$R0Spm11b,
 'r11c' = RegData$R0Spm11c,
 'r11d' = RegData$R0Spm11d)

 RANDvar <- c('R0Spm1', 'R0Spm2', 'R0Spm3a', 'R0Spm3b', 'R0Spm3c', 'R0Spm3d', 'R0Spm3e', 'R0Spm3f', 'R0Spm3g',
              'R0Spm3h', 'R0Spm3i', 'R0Spm3j', 'R0Spm4a', 'R0Spm4b', 'R0Spm4c', 'R0Spm4d', 'R0Spm5a', 'R0Spm5b',
              'R0Spm5c', 'R0Spm6', 'R0Spm7', 'R0Spm8', 'R0Spm9a', 'R0Spm9b', 'R0Spm9c', 'R0Spm9d', 'R0Spm9e',
              'R0Spm9f', 'R0Spm9g', 'R0Spm9h', 'R0Spm9i', 'R0Spm10', 'R0Spm11a', 'R0Spm11b', 'R0Spm11c', 'R0Spm11d')


# Kod om til skårer
 ny2 <- c(0,100)
 ny3 <- c(0,50,100)
 ny5 <- c(0,25,50,75,100)
 ny6 <- c(0,20,40,60,80,100)

 svar5rev <- c('r1', 'Endring', 'r6', 'r8', 'r11b',  'r11d')
 for (var in svar5rev) { RAND36[,var] <-  mapvalues(RAND36[,var], from = 1:5, to = rev(ny5))}

 svar3 <- c('r3a','r3b','r3c','r3d','r3e','r3f','r3g','r3h','r3i','r3j')
 for (var in svar3) { RAND36[,var] <-  mapvalues(RAND36[,var], from = 1:3, to = ny3)}

 svar2 <- c('r4a','r4b','r4c','r4d','r5a','r5b','r5c')
 for (var in svar2) { RAND36[,var] <-  mapvalues(RAND36[,var], from = 1:2, to = ny2)}

 svar6rev <- c('r7','r9a','r9d','r9e','r9h')
 for (var in svar6rev) { RAND36[,var] <-  mapvalues(RAND36[,var], from = 1:6, to = rev(ny6))}

 svar6 <- c('r9b','r9c','r9f','r9g','r9i')
 for (var in svar6) { RAND36[,var] <-  mapvalues(RAND36[,var], from = 1:6, to = ny6)}

 svar5 <- c('r10', 'r11a', 'r11c')
 for (var in svar5) { RAND36[,var] <-  mapvalues(RAND36[,var], from = 1:5, to = ny5)}

 #test <- sort(c(svar2,svar3,svar5,svar5rev,svar6,svar6rev))

# Beregn skårer for dimensjonene der hvor minst halvparten av enkeltspørsmål er besvart.
 #For NGER er alle svar obligatoriske slik at trenger ikke sjekke hvor mange som er besvart.

 RAND36$Fysisk = rowMeans(RAND36[,c('r3a','r3b','r3c','r3d','r3e','r3f','r3g','r3h','r3i','r3j')]) #3 svaralt
 RAND36$FysRolle = rowMeans(RAND36[,c('r4a','r4b','r4c','r4d')]) #2 svaralt
 RAND36$Smerte = rowMeans(RAND36[,c('r7','r8')]) #6 og 5 svaralt
 RAND36$Generelt = rowMeans(RAND36[,c('r1','r11a','r11b','r11c','r11d')]) #5svaralt
 RAND36$Vitalitet = rowMeans(RAND36[,c('r9a','r9e','r9g','r9i')]) #6 svaralt
 RAND36$Sosialt = rowMeans(RAND36[,c('r6','r10')]) #5 svaralt
 RAND36$MentalRolle = rowMeans(RAND36[,c('r5a','r5b','r5c')]) #2 svaralt
 RAND36$Mentalt = rowMeans(RAND36[,c('r9b','r9c','r9d','r9f','r9h')]) #6svaralt.


(test <- colSums(floor(RAND36[ ,c('Fysisk','FysRolle','Smerte',"Generelt",
                           "Vitalitet","Sosialt", "MentalRolle","Mentalt")])
                - RegData[ ,c('R0ScorePhys', 'R0ScoreRoleLmtPhy', 'R0ScorePain',	'R0ScoreGeneral',
                              'R0ScoreEnergy','R0ScoreSosial','R0ScoreRoleLmtEmo',   'R0ScoreEmo')], na.rm = T))
Smerte <- sum(floor(RAND36$Smerte)-RegData$R0ScorePain, na.rm=T)
Sosialt <- sum(floor(RAND36$Sosialt)-RegData$R0ScoreSosial, na.rm=T)
MentalRolle <- sum(floor(RAND36$MentalRolle)-RegData$R0ScoreRoleLmtEmo, na.rm=T)
Mental <- sum(floor(RAND36$Mentalt)-RegData$R0ScoreEmo, na.rm=T)
sum(round(RAND36$MentalRolle), na.rm = T)
sum(RegData$R0ScoreRoleLmtEmo, na.rm = T)

#ind <- which(RAND36$Fysisk>-1)
#c('Fysisk','FysRolle','Smerte',"Generelt","Vitalitet","Sosialt", "MentalRolle","Mentalt")
skaarer <- c('R0ScorePhys', 'R0ScoreRoleLmtPhy', 'R0ScorePain',	'R0ScoreGeneral',
             'R0ScoreEnergy','R0ScoreSosial','R0ScoreRoleLmtEmo',   'R0ScoreEmo')
for (var in skaarer) {table(RegData[,var])}


testRAND36 <- data.frame(RegData[ ,c('PasientID',	'OpDato',	'ForlopsID',	'AvdRESH',RANDvar,
                         'R0ScorePhys', 'R0ScoreRoleLmtPhy', 'R0ScorePain',	'R0ScoreGeneral',
                         'R0ScoreEnergy','R0ScoreSosial','R0ScoreRoleLmtEmo',   'R0ScoreEmo')],
                         RAND36)

write.table(testRAND36, file = "testRAND36.csv", row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')
write.table(data.frame(RAND36$MentalRolle, RegData$R0ScoreRoleLmtEmo), file = "testRAND36.csv", row.names= FALSE, sep = ';', fileEncoding = 'UTF-8')

#return(invisible(RAND36))
}
