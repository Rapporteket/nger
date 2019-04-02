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
  myTab <- xtabs(OpBMI ~ dummy + Aar,
                 aggregate(OpBMI~dummy+Aar,RegData,mean))
  myTab <- rbind(myTab,
                 xtabs(OpBMI ~ OpMetode + Aar,
                       aggregate(OpBMI~OpMetode+Aar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle fødsler} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpPariteter ~ dummy + Aar,
                       aggregate(OpPariteter~dummy+Aar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpPariteter ~ OpMetode + Aar,
                       aggregate(OpPariteter~OpMetode+Aar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle graviditeter} (\\textit{antall})"
  myTab <- rbind(myTab,
                 xtabs(OpGraviditeter ~ dummy + Aar,
                       aggregate(OpGraviditeter~dummy+Aar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpGraviditeter ~ OpMetode + Aar,
                       aggregate(OpGraviditeter~OpMetode+Aar,RegData,mean)))
  RegData$dummy <- "\\textbf{Alle knivtider} (\\textit{minutt})"
  myTab <- rbind(myTab,
                 xtabs(OpTid ~ dummy + Aar,
                       aggregate(OpTid~dummy+Aar,RegData,mean)))
  myTab <- rbind(myTab,
                 xtabs(OpTid ~ OpMetode + Aar,
                       aggregate(OpTid~OpMetode+Aar,RegData,mean)))

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
