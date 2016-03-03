#' Generate tab VI
#'
#' Yes, generate tab VI
#'
#' @inheritParams FigAndeler
#' @return list $tabVI data frame of table data
#' @export

NGERtabVI <- function(RegData) {

  # recode MCEType
  ind <- which(RegData$MCEType==1)
  RegData$MCEType[ind] <- "Laparoskopi"
  ind <- which(RegData$MCEType==2)
  RegData$MCEType[ind] <- "Hysteroskopi"
  ind <- which(RegData$MCEType==3)
  RegData$MCEType[ind] <- "Begge"

  myTab <- xtabs(OpBMI ~ MCEType + year,
                 aggregate(OpBMI~MCEType+year,RegData,mean))
  myTab <- rbind(myTab,
                 xtabs(OpParities ~ MCEType + year,
                       aggregate(OpParities~MCEType+year,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(Pregnancies ~ MCEType + year,
                       aggregate(Pregnancies~MCEType+year,RegData,mean)))

  list(tabVI=myTab)

}
