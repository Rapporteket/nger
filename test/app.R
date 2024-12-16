#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  #uiOutput("moreControls"),

  uiOutput("velgReshReg"),

  textOutput("resultat")
)


server <- function(input, output) {

  RegData <- NGERRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
   RegData <- NGERPreprosess(RegData)

#Definere utvalgsinnhold
sykehusNavn <- sort(unique(RegData$ShNavn), index.return=T)
sykehusValgUts <- unique(RegData$ReshId)[sykehusNavn$ix]
names(sykehusValgUts) <- sykehusNavn$x #c('Alle',sykehusNavn$x)
sykehusValg <- c(0,sykehusValgUts)
names(sykehusValg) <- c('Ikke valgt',sykehusNavn$x)

output$velgReshReg <- renderUI({
  selectInput(inputId = 'velgReshReg', label='Velg sykehus',
              selected = 0,
              choices = sykehusValg)
  })

output$resultat <- renderText({
  paste0('Du har valgt:', input$velgReshReg)})

  output$moreControls <- renderUI({
    tagList(
      sliderInput("n", "N", 1, 1000, 500),
      textInput("label", "Label")
    )
  })
}
shinyApp(ui, server)
