#Resultattjeneste for NGER
library(nger)
# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- (context %in% c("DEV", "TEST", "QA", "PRODUCTION")) #rapbase::isRapContext()
regTitle = ifelse(paaServer,'NORSK GYNEKOLOGISK ENDOSKOPIREGISTER',
                  'NORSK GYNEKOLOGISK ENDOSKOPIREGISTER med FIKTIVE data')

ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
  id = 'hovedark',

  # lag logo og tittel som en del av navbar
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  # sett inn tittel også i browser-vindu
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",


  #-------Registeradministrasjon----------
  tabPanel(p("Registeradministrasjon", title='Registeradministrasjonens side for registreringer og resultater'),
           value = "Registeradministrasjon",
           h3('Siden er bare synlig for SC-bruker', align = 'center'),

           tabPanel(
               h4("Eksport av krypterte data"),
               sidebarPanel(
                 rapbase::exportUCInput("ngerExport")
               ),
               mainPanel(
                 rapbase::exportGuideUI("ngerExportGuide")
               )
             ) #Eksport-tab
  ) #tab SC

) #ui-del




#----- Define server logic required to draw a histogram-------
server <- function(input, output, session) {

    #-- Div serveroppstart----

  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 105460)
  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  brukernavn <- reactive({ifelse(paaServer, rapbase::getUserName(session), 'inkognito')})

  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle, '<br> ReshID: ', reshID) )}

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })

  if (rolle=='SC') {

    #----------- Eksport ----------------
    registryName <- "nger"
    ## brukerkontroller
    rapbase::exportUCServer("ngerExport", registryName)
    ## veileding
    rapbase::exportGuideServer("ngerExportGuide", registryName)
  }

} #server
# Run the application
shinyApp(ui = ui, server = server)

