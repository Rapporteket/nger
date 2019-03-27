#Resultattjeneste for NGER
library(nger)
library(shiny)
library(knitr)
library(lubridate)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(kableExtra)
#library(zoo)

startDatoStandard <- '2018-01-01' #Sys.Date()-364
reshID <- 110734
if (!exists('RegData')) {
data('NGERtulledata', package = 'nger')
SkjemaOversikt <- plyr::rename(SkjemaOversikt, replace=c('AvdRESH'='reshID'))
SkjemaOversikt <- plyr::rename(SkjemaOversikt, replace=c('AvdRESH'='reshID'))
}


# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

regTitle = 'NORSK GYNEKOLOGISK ENDOSKOPIREGISTER med FIKTIVE data'



ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen

      # title = 'NORSK GYNEKOLOGISK ENDOSKOPIREGISTER med FIKTIVE data'

            # lag logo og tittel som en del av navbar
            title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle),
            # sett inn tittle ogsÃ¥ i browser-vindu
            windowTitle = regTitle,
            # velg css (forelÃ¸pig den eneste bortsett fra "naken" utgave)
            #theme = "rap/bootstrap.css",

      tabPanel("Startside",
               #fluidRow(
               #column(width=5,
               br(),
               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color

               sidebarPanel(width = 3,
                            br(),
                            #h2('Nedlastbare dokumenter med samling av resultater'),
                            h3("Månedsrapport"), #),
                            downloadButton(outputId = 'mndRapp.pdf', label='Last ned MÅNEDSRAPPORT', class = "butt"),
                            br(),
                            br(),
                            br()
               ),
               mainPanel(width = 8,
                         h2('Velkommen til Rapporteket - Norsk Gynekologisk Endoskopiregister!', align='center'),
                         br(),
                         h3('Dere står fritt til å endre alle tekster som dere ønsker...'),
                         br(),
                         h4('Du er nå inne på Rapporteket for NGER. Rapporteket er registerets resultattjeneste.
                            Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
                            På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
                            på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                            Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
                         h4('Du kan se på resultater for eget sykehus, nasjonale tall og eget sykehus sett opp
                              mot landet for øvrig. Resultatene som vises er
                              basert på operasjonsdato. Alle figurer og
                            tabeller kan lastes ned.'),
                         br(),
                         h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
                         h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret. Også her kan man gjøre filtreringer.'),
                         h4(tags$b('Fordelinger '), 'viser på fordelinger (figur/tabell) av ulike variable.
                              Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
                         h4(tags$b('Sykehusvise resultater '), 'viser gjennomsnittsverdier per sykehus.
                            Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt eller median.
                            Man kan også velge å filtrere data.'),
                         br(),
                         br(),
                         br(),
                         h4('Oversikt over registerets kvalitetsindikatorer og resultater med offentlig tilgjengelige tall
                            finner du på www.kvalitetsregistre.no:', #helpText
                                  a("NGER", href="https://www.kvalitetsregistre.no/registers/547/resultater"),
                                  target="_blank", align='center'),
                         br(),
                         br(),
                         h4('Antall registreringer ved eget sykehus:'),
                         uiOutput("tabEgneReg"),
                         br(),
                         h3('Legge inn andre figurer/tabeller her?')
                             )
      ), #tab
      #-----Registreringsoversikter------------
      tabPanel("Registreringsoversikter",

               sidebarPanel(width=3,
                            h3('Utvalg'),
                            conditionalPanel(condition = "input.ark == 'Antall registrerte operasjoner'",
                                             dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                       value = Sys.Date(), max = Sys.Date() )
                            ),
                            conditionalPanel(
                              condition = "input.ark == 'Antall registrerte operasjoner'",
                              selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                                          choices = rev(c('År'= 'Aar', 'Måned'='Mnd')))),
                            conditionalPanel(
                              condition = "input.ark == 'Antall registrerte skjema'",
                              dateRangeInput(inputId = 'datovalgReg', start = startDatoStandard, end = Sys.Date(),
                                             label = "Tidsperiode", separator="t.o.m.", language="nb"),
                              selectInput(inputId = 'skjemastatus', label='Velg skjemastatus',
                                          choices = c("Ferdigstilt"=1,
                                                      "Kladd"=0,
                                                      "Ikke opprettet"=-1)
                              )

                            )
               ),

               mainPanel(
                 tabsetPanel(id='ark',
                             tabPanel('Antall registrerte operasjoner',
                                      uiOutput("undertittelReg"),
                                      p("Velg tidsperiode ved å velge sluttdato/tidsenhet i menyen til venstre"),
                                      br(),
                                      fluidRow(tableOutput("tabAntOpphShMnd12"),
                                      downloadButton(outputId = 'lastNed_tabAntOpph', label='Last ned')
                                      )
                             ),
                             tabPanel('Antall registrerte skjema',
                                      #uiOutput("undertittelReg"),
                                      p("Velg tidsperiode i menyen til venstre"),
                                      br(),
                                      fluidRow(tableOutput("tabAntSkjema"),
                                               downloadButton(outputId = 'lastNed_tabAntSkjema', label='Last ned')
                                      )
                             )

                 )
               )

      ), #tab Registreringsoversikter

      #--------Fordelinger-----------
      tabPanel("Fordelinger",
               sidebarPanel(width = 3,
                            selectInput(
                                  inputId = "valgtVar", label="Velg variabel (flere kommer)",
                                  choices = c('Alder' = 'Alder',
                                              'Komplikasjoner, postoperativt' = 'KomplPostopType'
                                  )
                            ),
                            dateRangeInput(inputId = 'datovalg', start = startDatoStandard, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            sliderInput(inputId="alder", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                        choices = c("Egen mot resten av landet"=1,
                                                    "Hele landet"=0,
                                                    "Egen enhet"=2)
                            )

                            # selectInput(inputId = 'AIS', label='AIS-grad',
                            #             multiple = T, #selected=0,
                            #             choices = #valgAIS
                            #                   c("Alle"=0,
                            #                     "A"=1,
                            #                     "B"=2,
                            #                     "C"=3,
                            #                     "D"=4,
                            #                     "E"=5)
                            # ),
                            # selectInput(inputId = 'traume', label='Traume',
                            #             choices = c("Alle"=' ', #'ikke'
                            #                         "Traume"='ja',
                            #                         "Ikke traume"='nei')
                            # )

                            #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                            #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 br(),
                                 em('(Høyreklikk på figuren for å laste den ned)'),
                                 br(),
                                 br(),
                                 plotOutput('fordelinger')),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelFord"),
                                 br(),
                                 tableOutput('fordelingTab'),
                                 downloadButton(outputId = 'lastNed_tabFord', label='Last ned')
                           )
                     ))
      ), #tab Fordelinger


#------------Sykehusvise resultater------------
      tabPanel("Sykehusvise resultater",
               sidebarPanel(width = 3,
                            selectInput(
                                  inputId = "valgtVarGjsnGrVar", label="Velg variabel",
                                  choices = c('Alder' = 'Alder',
                                              'Lengde på rehab.opphold' = 'DagerRehab'
                                             )
                            ),
                            dateRangeInput(inputId = 'datovalgGjsnGrVar', start = startDatoStandard, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            sliderInput(inputId="alderGjsnGrVar", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                                                               choices = c("Gjennomsnitt"='gjsn', "Median"='med'))

               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 br(),
                                 em('(Høyreklikk på figuren for å laste den ned)'),
                                 br(),
                                 br()
                             #    plotOutput('gjsnGrVar')
                             ),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelGjsnGrVar"),
                                 br()
                           #      tableOutput('gjsnGrVarTab'),
                            #     downloadButton(outputId = 'lastNed_tabGjsnGrVar', label='Last ned') # , class = "butt"))
                           )
                     )
               )

) #GjsnGrVar



) #ui-del




#----- Define server logic required to draw a histogram-------
server <- function(input, output) {

 #NB: Skal bare forholde oss til oppfølgingsskjema som er tilknyttet et gyldig Hovedskjema

      context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
      if (context == "TEST" | context == "QA" | context == "PRODUCTION") {

                  registryName <- "nger"
                  dbType <- "mysql"

                 query <- paste0('SELECT  ...')

                  #RegData <- NGERRegDataSQL(valgtVar = valgtVar) #datoFra = datoFra, datoTil = datoTil)
                  HovedSkjema <- NGERRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
                  Livskvalitet <- rapbase::LoadRegData(registryName, qLivs, dbType)
                  } #hente data på server

  #----------Hente data og evt. parametre som er statistke i appen----------
     # if (!exists('RegData')){
     #        #Tulledata:
     #        data('NGERfiktiveData', package = 'nger')
     #  }

      RegData <- NGERPreprosess(RegData)

      # AlleTab <- list(HovedSkjema=HovedSkjema,
      #                 LivskvalitetH=LivskvalitetH,
      #                 AktivitetH = AktivitetH)

#-------Samlerapporter--------------------
      # funksjon for å kjøre Rnw-filer (render file funksjon)
      contentFile <- function(file, srcFil, tmpFile) {
            src <- normalizePath(system.file(srcFil, package="nger"))

            # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, tmpFile, overwrite = TRUE)

            texfil <- knitr::knit(tmpFile, encoding = 'UTF-8')
            tools::texi2pdf(texfil, clean = TRUE)

            gc() #Opprydning gc-"garbage collection"
            file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
      }



      output$mndRapp.pdf <- downloadHandler(
            filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')},
            content = function(file){
                  contentFile(file, srcFil="NGERmndRapp.Rnw", tmpFile="tmpNGERmndRapp.Rnw")
            })


#--------------Startside------------------------------

      #output$lenkeNorScir <- renderUI({tagList("www.norscir.no", www.norscir.no)})

     output$tabEgneReg <- renderTable({
       xtable::xtable(tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12, reshID = reshID))},
            rownames=T,
            digits = 0
      )

 #----------Tabeller, registreringsoversikter ----------------------

            output$undertittelReg <- renderUI({
                  br()
                  t1 <- 'Tabellen viser operasjoner '
                  h4(HTML(switch(input$tidsenhetReg, #undertittel <-
                         Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, '<br />'),
                         Aar = paste0(t1, 'siste 5 år før ', input$sluttDatoReg, '<br />'))
                  ))})
      observe({
            tabAntOpphShMndAar <- switch(input$tidsenhetReg,
                                         Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])
                                         Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))
            #Aar=xtable::xtable(tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg)), digits=0)

                  output$tabAntOpphShMnd12 <- renderTable(tabAntOpphShMndAar, rownames = T, digits=0, spacing="xs")
      output$lastNed_tabAntOpph <- downloadHandler(
            filename = function(){paste0('tabAntOpph.csv')},
            content = function(file, filename){write.csv2(tabAntOpphShMndAar, file, row.names = T, na = '')
            })



      #RegData som har tilknyttede skjema av ulik type
      AntSkjemaAvHver <- tabAntSkjema(SkjemaOversikt=SkjemaOversikt, datoFra = input$datovalgReg[1], datoTil=input$datovalgReg[2],
                                      skjemastatus=as.numeric(input$skjemastatus))
      #tabAntSkjema(SkjemaOversikt=SkjemaOversikt)
            output$tabAntSkjema <- renderTable(
              AntSkjemaAvHver
                  ,rownames = T, digits=0, spacing="xs" )

            output$lastNed_tabAntSkjema <- downloadHandler(
                  filename = function(){'tabAntSkjema.csv'},
                  content = function(file, filename){write.csv2(AntSkjemaAvHver, file, row.names = T, na = '')
                  })
      #       #Andel (prosent) av registreringsskjemaene som har oppfølgingsskjema.
      #       output$tabAndelTilknyttedeHovedSkjema <- renderTable(
      #             tabTilknHovedSkjema$Andeler
      #             ,rownames = T, digits=0, spacing="xs" )
      #
      #        output$lastNed_tabOppfHovedPst <- downloadHandler(
      #              filename = function(){'tabOppfHovedPst.csv'},
      #              content = function(file, filename){write.csv2(tabTilknHovedSkjema$Andeler, file, row.names = T, na = '')
      #              })
       })


#---------Fordelinger------------
            observe({   #Fordelingsfigurer og tabeller

            output$fordelinger <- renderPlot({
                  NGERFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                               reshID = reshID,
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )

            #RegData må hentes ut fra valgtVar
            UtDataFord <- NGERFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                       reshID = reshID,
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                       enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            tabFord <- lagTabavFig(UtDataFraFig = UtDataFord) #lagTabavFigAndeler
            output$tittelFord <- renderUI({
                  tagList(
                        h3(UtDataFord$tittel),
                        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
                  )}) #, align='center'
            output$fordelingTab <- renderTable(
                  tabFord, rownames = T)

            output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
                  antKol <- ncol(tabFord)
                  kableExtra::kable(tabFord, format = 'html'
                                    , full_width=F
                                    , digits = c(0,1,0,1)[1:antKol]
                  ) %>%
                        add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
                        column_spec(column = 1, width_min = '7em') %>%
                        column_spec(column = 2:(ncol(tabFord)+1), width = '7em') %>%
                        row_spec(0, bold = T)
            }

            output$lastNed_tabFord <- downloadHandler(
                  filename = function(){paste0(input$valgtVar, '_fordeling.csv')},
                  content = function(file, filename){write.csv2(tabFord, file, row.names = F, na = '')
                  })
      }) #observe Fordeling

      # observe({ #Sykehusvise gjennomsnitt, figur og tabell
      #       output$gjsnGrVar <- renderPlot(
      #             NGERFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar,
      #                            datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2],
      #                            minald=as.numeric(input$alderGjsnGrVar[1]), maxald=as.numeric(input$alderGjsnGrVar[2]),
      #                            valgtMaal = input$sentralmaal
      #             ),
      #             width = 800, height = 600)
      #       UtDataGjsnGrVar <- NGERFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar,
      #                                         datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2],
      #                                         minald=as.numeric(input$alderGjsnGrVar[1]), maxald=as.numeric(input$alderGjsnGrVar[2]),
      #                                         valgtMaal = input$sentralmaal)
      #       tabGjsnGrVar <- lagTabavFigGjsnGrVar(UtDataFraFig = UtDataGjsnGrVar)
      #
      #       output$tittelGjsnGrVar <- renderUI({
      #             tagList(
      #                   h3(UtDataGjsnGrVar$tittel),
      #                   h5(HTML(paste0(UtDataGjsnGrVar$utvalgTxt, '<br />')))
      #             )}) #, align='center'
      #
      #       output$gjsnGrVarTab <- function() {
      #             antKol <- ncol(tabGjsnGrVar)
      #             kableExtra::kable(tabGjsnGrVar, format = 'html'
      #                               #, full_width=T
      #                               , digits = c(0,1) #,0,1)[1:antKol]
      #             ) %>%
      #                   column_spec(column = 1, width_min = '5em') %>%
      #                   column_spec(column = 2:(antKol+1), width = '4em') %>%
      #                   row_spec(0, bold = T)
      #       }
      #       output$lastNed_tabGjsnGrVar <- downloadHandler(
      #             filename = function(){
      #                   paste0(input$valgtVar, '_tabGjsnSh .csv')
      #             },
      #             content = function(file, filename){
      #                   write.csv2(tabGjsnGrVar, file, row.names = T, na = '')
      #             })
      #
      #       output$titteltabGjsnGrVar <- renderUI({
      #             tagList(
      #                   h3(tabGjsnGrVar$tittel),
      #                   h5(HTML(paste0(tabGjsnGrVar$utvalgTxt, '<br />')))
      #             )}) #, align='center'
      # }) #observe gjsnGrVar
} #server
# Run the application
shinyApp(ui = ui, server = server)

