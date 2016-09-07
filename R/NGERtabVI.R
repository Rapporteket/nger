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
                 xtabs(OpBMI ~ OpMetode + OpAar,
                       aggregate(OpBMI~OpMetode+OpAar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle fødsler} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpPariteter ~ dummy + OpAar,
                       aggregate(OpPariteter~dummy+OpAar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpPariteter ~ OpMetode + OpAar,
                       aggregate(OpPariteter~OpMetode+OpAar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle graviditeter} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpGraviditeter ~ dummy + OpAar,
                       aggregate(OpGraviditeter~dummy+OpAar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpGraviditeter ~ OpMetode + OpAar,
                       aggregate(OpGraviditeter~OpMetode+OpAar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle knivtider} (\\textit{minutt})"
  myTab <- rbind(myTab,
                 xtabs(OpTid ~ dummy + OpAar,
                       aggregate(OpTid~dummy+OpAar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpTid ~ OpMetode + OpAar,
                       aggregate(OpTid~OpMetode+OpAar,RegData,mean)))

  # move rownames to its own column do allow duplicate names
  # OpMetode 1=laparo, 2=hysteroskopi, 3=begge
  pe <- rownames(myTab)
  pe[which(pe==1)] <- "\\quad Laparoskopi"
  pe[which(pe==2)] <- "\\quad Hysteroskopi"
  pe[which(pe==3)] <- "\\quad Begge"

  mydf <- data.frame(Pasientegenskap=pe, myTab, check.names = FALSE)
#  list(tabVI=mydf)
return(invisible(mydf))
}
