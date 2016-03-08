#' Generate tab VI
#'
#' Yes, generate tab VI
#'
#' @inheritParams FigAndeler
#' @return list $tabVI data frame of table data
#' @export

NGERtabVI <- function(RegData) {

  # make dummy column for all MCEs
  n <- dim(RegData)[1]
  RegData$dummy <- rep("\\textbf{Alle BMI} ($kg/m^2$)", n)
  myTab <- xtabs(OpBMI ~ dummy + year,
                 aggregate(OpBMI~dummy+year,RegData,mean))
  myTab <- rbind(myTab,
                 xtabs(OpBMI ~ MCEType + year,
                       aggregate(OpBMI~MCEType+year,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle fødsler} (antall)"
  myTab <- rbind(myTab,
                 xtabs(OpParities ~ dummy + year,
                       aggregate(OpParities~dummy+year,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpParities ~ MCEType + year,
                       aggregate(OpParities~MCEType+year,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle graviditeter} (antall)"
  myTab <- rbind(myTab,
                 xtabs(OpPregnancies ~ dummy + year,
                       aggregate(OpPregnancies~dummy+year,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpPregnancies ~ MCEType + year,
                       aggregate(OpPregnancies~MCEType+year,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle knivtider} (minutt)"
  myTab <- rbind(myTab,
                 xtabs(OpOptimeCount ~ dummy + year,
                       aggregate(OpOptimeCount~dummy+year,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpOptimeCount ~ MCEType + year,
                       aggregate(OpOptimeCount~MCEType+year,RegData,mean)))

  # move rownames to its own column do allow duplicate names
  # MCEType 1=laparo, 2=hysteroskopi, 3=begge
  pe <- rownames(myTab)
  pe[which(pe==1)] <- "\\quad Laparoskopi"
  pe[which(pe==2)] <- "\\quad Hysteroskopi"
  pe[which(pe==3)] <- "\\quad Begge"

  mydf <- data.frame(Pasientegenskap=pe, myTab)
  list(tabVI=mydf)

}
