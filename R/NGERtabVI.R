#' Generate tab VI
#'
#' Yes, generate tab VI
#'
#' @inheritParams FigAndeler
#' @return list $tabVI data frame of table data
#' @export

NGERtabVI <- function(RegData) {

  # recode MCEType
#   ind <- which(RegData$MCEType==1)
#   RegData$MCEType[ind] <- "\\quad Laparoskopi"
#   ind <- which(RegData$MCEType==2)
#   RegData$MCEType[ind] <- "\\quad Hysteroskopi"
#   ind <- which(RegData$MCEType==3)
#   RegData$MCEType[ind] <- "\\quad Begge"

  # make dummy column for all MCEs
  n <- dim(RegData)[1]
  RegData$dummy <- rep("\\textbf{Alle BMI}", n)
  myTab <- xtabs(OpBMI ~ dummy + year,
                 aggregate(OpBMI~dummy+year,RegData,mean))
  myTab <- rbind(myTab,
                 xtabs(OpBMI ~ MCEType + year,
                       aggregate(OpBMI~MCEType+year,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle fødsler}"
  myTab <- rbind(myTab,
                 xtabs(OpParities ~ dummy + year,
                       aggregate(OpParities~dummy+year,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpParities ~ MCEType + year,
                       aggregate(OpParities~MCEType+year,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle graviditeter}"
  myTab <- rbind(myTab,
                 xtabs(OpPregnancies ~ dummy + year,
                       aggregate(OpPregnancies~dummy+year,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpPregnancies ~ MCEType + year,
                       aggregate(OpPregnancies~MCEType+year,RegData,mean)))

  list(tabVI=myTab)

}
