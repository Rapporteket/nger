#' Generate tab VI
#'
#' Yes, generate tab VI
#'
#' @inheritParams FigAndeler
#' @return list $tabVI data frame of table data
#' @export

NGERtabVI <- function(RegData) {

  tabVI <- plyr::ddply(.RegData, .(year),
                       summarize, mean=round(mean(OpBMI), 2))

  list(tabVI=tabVI)

}
