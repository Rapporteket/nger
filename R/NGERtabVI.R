#' Generate tab VI
#'
#' Yes, generate tab VI
#'
#' @inheritParams NGERFigAndeler
#' @return list $tabVI data frame of table data
#' @export

NGERtabVI <- function(RegData) {

  # make dummy column for all MCEs
  n <- dim(RegData)[1]
  RegData$dummy <- rep("\\textbf{Alle BMI} ($kg/m^2$)", n)
  myTab <- xtabs(OpBMI ~ dummy + OpAar,
                 aggregate(OpBMI~dummy+OpAar,RegData,mean))
  myTab <- rbind(myTab,
                 xtabs(OpBMI ~ MCEType + OpAar,
                       aggregate(OpBMI~MCEType+OpAar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle fødsler} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpParities ~ dummy + OpAar,
                       aggregate(OpParities~dummy+OpAar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpParities ~ MCEType + OpAar,
                       aggregate(OpParities~MCEType+OpAar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle graviditeter} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpPregnancies ~ dummy + OpAar,
                       aggregate(OpPregnancies~dummy+OpAar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpPregnancies ~ MCEType + OpAar,
                       aggregate(OpPregnancies~MCEType+OpAar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle knivtider} (\\textit{minutt})"
  myTab <- rbind(myTab,
                 xtabs(OpOptimeCount ~ dummy + OpAar,
                       aggregate(OpOptimeCount~dummy+OpAar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpOptimeCount ~ MCEType + OpAar,
                       aggregate(OpOptimeCount~MCEType+OpAar,RegData,mean)))

  # move rownames to its own column do allow duplicate names
  # MCEType 1=laparo, 2=hysteroskopi, 3=begge
  pe <- rownames(myTab)
  pe[which(pe==1)] <- "\\quad Laparoskopi"
  pe[which(pe==2)] <- "\\quad Hysteroskopi"
  pe[which(pe==3)] <- "\\quad Begge"

  mydf <- data.frame(Pasientegenskap=pe, myTab, check.names = FALSE)
  list(tabVI=mydf)

}
