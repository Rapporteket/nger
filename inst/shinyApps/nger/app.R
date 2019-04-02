#Resultattjeneste for NGER
library(nger)
library(shiny)
library(knitr)
library(lubridate)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(kableExtra)
#library(zoo)

startDato <- '2018-01-01' #Sys.Date()-364
idag <- Sys.Date()

# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

regTitle = 'NORSK GYNEKOLOGISK ENDOSKOPIREGISTER med FIKTIVE data'

#Definere utvalgsinnhold
 enhetsUtvalg <- c("Egen mot resten av landet"=1,
                        "Hele landet"=0,
                        "Egen enhet"=2)
 diag <- 0:3
 names(diag) <- c('Alle', 'Godartede ovarialcyster', 'Endometriose, livmorvegg', 'Endo u livmorvegg')

 opMetode <- c('Alle'=0,
               'Laparoskopi'=1,
               'Hysteroskopi'=2,
               'Begge'=3,
               'Tot. lap. hysterektomi (LCD01/LCD04)'=4,
               'Lap. subtotal hysterektomi (LCC11)'=5,
               'Lap. ass. vag. hysterektomi (LCD11)'=6)

 alvorKompl <- c("Alle"=0,
                  "Lite alvorlig"=1,
                  "Middels alvorlig"=2,
                  "Alvorlig"=3,
                  "Dødelig"=4)
 hastegrad <- c('Alle'=0,
                'Elektiv'=1,
                'Akutt'=2,
                'Ø-hjelp'=3)

ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen

      # title = 'NORSK GYNEKOLOGISK ENDOSKOPIREGISTER med FIKTIVE data'

            # lag logo og tittel som en del av navbar
            title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle),
            # sett inn tittle ogsÃ¥ i browser-vindu
            windowTitle = regTitle,
            # velg css (forelÃ¸pig den eneste bortsett fra "naken" utgave)
            #theme = "rap/bootstrap.css",




#------------Startside--------------------------
      tabPanel("Startside",
               #fluidRow(
               #column(width=5,
               br(),
               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
               h2('Velkommen til Rapporteket - Norsk Gynekologisk Endoskopiregister!', align='center'),
               br(),

               sidebarPanel(width = 3,
               #             br(),
                            #h2('Nedlastbare dokumenter med samling av resultater'),
                            h3("Månedsrapport"), #),
                            downloadButton(outputId = 'mndRapp.pdf', label='Last ned MÅNEDSRAPPORT', class = "butt"),
                            br(),
                            br(),
                            br()
               ),
               mainPanel(width = 8,
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
                         h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret.'),
                         h4(tags$b('Fordelinger '), 'viser på fordelinger (figur/tabell) av ulike variable.
                              Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
                         h4(tags$b('Andeler: per sykehus og over tid'), ' viser andeler(prosent) en per sykehus og utvikling over tid.
                            Man kan velge hvilken variabel man vil se på, om man vil filtrere data og velge tidsskala.'),
                         h4(tags$b('Gjennomsnitt: per sykehus og over tid'), ' viser gjennomsnittsverdier per sykehus og utvikling over tid.
                            Man kan velge hvilken variabel man vil se på, om man vil se gjennomsnitt eller median og velge tidsskala.
                            Man kan også velge å filtrere data.'),
                         h4('Årsaken til at resultater deles inn i gjennomsnitt og andeler er
                            at dette er programmeringsmessig effektivt. Gi gjerne innspill til registerledelsen om ønsket organisering
                            av innholdet på Rapporteket-NGER.'),
                         br(),
                         br(),
                         h4('Antall registreringer ved eget sykehus siste år:'),
                         uiOutput("tabEgneReg"),
                         br(),
                         br(),
                         h4('Oversikt over registerets kvalitetsindikatorer og resultater med offentlig tilgjengelige tall
                            finner du på www.kvalitetsregistre.no:', #helpText
                                  a("NGER", href="https://www.kvalitetsregistre.no/registers/547/resultater"),
                                  target="_blank", align='center'),
                         br()
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
                              dateRangeInput(inputId = 'datovalgReg', start = startDato, end = Sys.Date(),
                                             label = "Tidsperiode", separator="t.o.m.", language="nb"),
                              selectInput(inputId = 'skjemastatus', label='Velg skjemastatus',
                                          choices = c("Ferdigstilt"=1,
                                                      "Kladd"=0,
                                                      "Åpen"=-1)
                              )

                            )
               ),

               mainPanel(
                 tabsetPanel(id='ark',
                             tabPanel('Antall registrerte operasjoner',
                                      uiOutput("undertittelReg"),
                                      p("Velg tidsperiode ved å velge sluttdato/tidsenhet i menyen til venstre"),
                                      br(),
                                      fluidRow(
                                        tableOutput("tabAntOpphShMnd12"),
                                        downloadButton(outputId = 'lastNed_tabAntOpph', label='Last ned')
                                      )
                             ),
                             tabPanel('Antall registrerte skjema',
                                      h4("Tabellen viser antall registrerte skjema for valgt tidsperiode"),
                                      p("Velg tidsperiode i menyen til venstre"),
                                      br(),
                                      fluidRow(
                                        tableOutput("tabAntSkjema"),
                                        downloadButton(outputId = 'lastNed_tabAntSkjema', label='Last ned')
                                      )
                             )

                 )
               )

      ), #tab Registreringsoversikter

      #--------Fordelinger-----------
      tabPanel("Fordelinger",
               sidebarPanel(width = 3,
                            h3('Utvalg'),
                            selectInput(
                                  inputId = "valgtVar", label="Velg variabel (flere kommer)",
                                  choices = c('Alder' = 'Alder',
                                              'Komplikasjoner, postoperativt' = 'KomplPostopType'
                                  )
                            ),
                            dateRangeInput(inputId = 'datovalg', start = startDato, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            sliderInput(inputId="alder", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                        choices = enhetsUtvalg
                            ),
                            selectInput(inputId = 'opMetode', label='Operasjonstype',
                                        choices = opMetode
                            ),
                             selectInput(inputId = 'velgDiag', label='Diagnose',
                                         choices = diag
                             ),
                            selectInput(inputId = 'hastegrad', label='Hastegrad',
                                        choices = hastegrad
                            ),
                            selectInput(inputId = 'alvorlighetKompl',
                                        label='Alvorlighetsgrad, postoperative komplikasjoner',
                                        multiple = T, #selected=0,
                                        choices = alvorKompl
                            ),
                            h5('Velge eget sykehus (bare admin)')

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


#----------Andeler-----------------------------
tabPanel(p("Andeler: per sykehus og tid", title='Alder, Gjennomføringsgrad,...'),
         h2("Sykehusvise andeler og utvikling over tid for valgt variabel", align='center'),
         h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
            til venstre. Man kan også gjøre ulike filtreringer.", align='center'),
         br(),
         sidebarPanel(
           width=3,
           h3('Utvalg'),


           selectInput(
             inputId = "valgtVarAndel", label="Velg variabel",
             choices = c('Alder over 70 år' = 'Alder',
                         'Antibiotika' = 'OpAntibProfylakse',
                         'ASA-grad > II' = 'OpASA',
                         'Fedme (BMI>30)' = 'OpBMI',
                         'Pasienter med høyere utdanning' = 'Utdanning',
                         'Postop. komplikasjon: Blødning' = 'Opf0KomplBlodning',
                         'Postop. komplikasjon: Problemer med ustyr' = 'Opf0KomplUtstyr',
                         'Postop. komplikasjon: Infeksjon' = 'Opf0KomplInfeksjon',
                         'Postop. komplikasjon: Organskade' = 'Opf0KomplOrgan',
                         'Postop. komplikasjon: Reoperasjon' = 'Opf0Reoperasjon',
                         'Postop. komplikasjon: andel moderate/alvorlige (grad 2-4)' = 'Opf0AlvorlighetsGrad',
                         'Postop. komplikasjon: Alle' = 'KomplPostop',
                         'Komplikasjoner under operasjon' = 'KomplIntra',
                         'Konvertert til laparoromi?' = 'LapKonvertert',
                         'Postoperativ oppfølging' = 'Opf0Status',
                         'Registreringsforsinkelse' = 'RegForsinkelse',
                         'TSS2: Møtet med gyn. avd. var svært godt' = 'Tss2Mott',
                         'TSS2: Behandlingsopplegg/-innhold passet svært bra' = 'Tss2Behandling',
                         'TSS2: Behandlerne lyttet- og forsto i svært stor grad' = 'Tss2Lytte',
                         'TSS2: Pasienten hadde svært stor tillit til sine behandlere' = 'Tss2Behandlere',
                         'TSS2: Pasient og behandlere svært enige om målsetn. for behandlinga' = 'Tss2Enighet',
                         'TSS2: Svært positiv oppfatning om gyn. avd.' = 'Tss2Generelt'
                         )
           ),
           dateRangeInput(inputId = 'datovalgAndel', start = startDato, end = idag,
                          label = "Tidsperiode", separator="t.o.m.", language="nb"),
           sliderInput(inputId="alderAndel", label = "Alder", min = 0,
                       max = 110, value = c(0, 110)),
           br(),
           p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
           selectInput(inputId = 'enhetsUtvalgAndel', label='Egen enhet og/eller landet',
                       choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)),
           selectInput(inputId = "tidsenhetAndel", label="Velg tidsenhet",
                       choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                       'Kvartal'='Kvartal', 'Måned'='Mnd')))
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("Figurer",
               #column(10,
               h3(em("Utvikling over tid")),
               plotOutput("andelTid", height = 'auto'),
               br(),
               h3(em("Sykehusvise resultater")),
               plotOutput("andelerGrVar", height='auto')
             ),
             tabPanel("Tabeller",
                      uiOutput("tittelAndel"),
                      br(),
                      #fluidRow(
                      column(width = 3,
                             h3("Sykehusvise resultater"),
                             tableOutput("andelerGrVarTab"),
                             downloadButton(outputId = 'lastNed_tabAndelGrVar', label='Last ned tabell')),
                      column(width = 1),
                      column(width = 5,
                             h3("Utvikling over tid"),
                             tableOutput("andelTidTab"),
                             downloadButton(outputId = 'lastNed_tabAndelTid', label='Last ned tabell'))
                      #DT::DTOutput("andelerGrVarTab")
             ))
         ) #mainPanel

         ), #tab

#------------ Gjennomsnittresultater------------
      tabPanel("Gjennomsnitt: per sykehus og over tid",
               h2('Sykehusvise gjennomsnitt/median og utvikling over tid for valgt variabel', align='center'),
               h5('Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen til venstre.
                  (Man kan også gjøre ulike filtreringer.)', align='center'),
               br(),
               sidebarPanel(width = 3,
                            h3('Utvalg'),
                            selectInput(
                                  inputId = "valgtVarGjsn", label="Velg variabel (flere valg kommer)",
                                  choices = c('Alder' = 'Alder',
                                              'Bla, bla' = 'DagerRehab'
                                             )
                            ),
                            dateRangeInput(inputId = 'datovalgGjsn', start = startDato, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            sliderInput(inputId="alderGjsn", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                                                               choices = c("Gjennomsnitt"='gjsn', "Median"='med')),
                            br(),
                            p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
                            selectInput(inputId = 'enhetsUtvalgGjsn', label='Egen enhet og/eller landet',
                                        choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
                            ),
                            selectInput(inputId = "tidsenhetGjsn", label="Velg tidsenhet",
                                        choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                                        'Kvartal'='Kvartal', 'Måned'='Mnd'))
                            )


               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figurer',
                                 br(),
                                 em('(Høyreklikk på figuren for å laste den ned)'),
                                 br(),
                                 br(),
                                 h3(em("Utvikling over tid")),
                                 plotOutput("gjsnTid", height = 'auto'),
                                 br(),
                                 h3(em("Sykehusvise resultater")),
                                 plotOutput("gjsnGrVar", height='auto')

                             ),
                           tabPanel(
                                 'Tabeller',
                                 uiOutput("tittelGjsn"),
                                 br(),
                                 column(width = 3,
                                        h3("Sykehusvise resultater"),
                                        tableOutput('gjsnGrVarTab'),
                                 downloadButton(outputId = 'lastNed_gjsnGrVarTab', label='Last ned')), # , class = "butt"))
                                 column(width = 1),
                                 column(width = 5,
                                        h3("Utvikling over tid"),
                                        tableOutput("gjsnTidTab"),
                                        downloadButton(outputId = 'lastNed_gjsnTidTab', label='Hvorfor virker den ikke???)'))
                                 )
                     )
               )
) #GjsnGrVar/Tid



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
      reshID <- 8 #110734
 #     if (!exists('RegData')) {
        data('NGERtulledata', package = 'nger')
        reshID <- 8
        #SkjemaOversikt <- plyr::rename(SkjemaOversikt, replace=c('SykehusNavn'='ShNavn'))
#      }

      RegData <- NGERPreprosess(RegData)
      SkjemaOversikt <- NGERPreprosess(RegData = SkjemaOversikt)

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
      tabAntOpphShMnd(RegData=RegData, reshID = 8)
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
        output$tabAntSkjema <- renderTable(AntSkjemaAvHver
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
                               OpMetode = as.numeric(input$opMetode),
                               Hastegrad = as.numeric(input$hastegrad),
                               velgDiag = as.numeric(input$velgDiag),
                               AlvorlighetKompl = as.numeric(input$alvorlighetKompl),
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


#--------------Andeler-----------------------------------
      output$andelerGrVar <- renderPlot({
        NGERFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                           datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                           minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]))
      }, height = 800, width=700 #height = function() {session$clientData$output_andelerGrVarFig_width} #})
      )

      output$andelTid <- renderPlot({

        NGERFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                       reshID=reshID,
                       datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                       minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                       tidsenhet = input$tidsenhetAndel,
                       enhetsUtvalg = input$enhetsUtvalgAndel)
      }, height = 300, width = 1000
      )

      observe({
        #AndelTid
        AndelerTid <- NGERFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                     reshID=reshID,
                                     datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                     minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                     tidsenhet = input$tidsenhetAndel,
                                     enhetsUtvalg = input$enhetsUtvalgAndel) #,lagFig=0)
        tabAndelTid <- lagTabavFig(UtDataFraFig = AndelerTid, figurtype = 'andelTid')


        output$andelTidTab <- function() {
          antKol <- ncol(tabAndelTid)
          kableExtra::kable(tabAndelTid, format = 'html'
                            , full_width=F
                            , digits = c(0,1,0,1)[1:antKol]
          ) %>%
            add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
            column_spec(column = 1, width_min = '7em') %>%
            column_spec(column = 2:(antKol+1), width = '7em') %>%
            row_spec(0, bold = T)
        }
        output$lastNed_tabAndelTid <- downloadHandler(
          filename = function(){
            paste0(input$valgtVar, '_andelTid.csv')
          },
          content = function(file, filename){
            write.csv2(tabAndelTid, file, row.names = T, na = '')
          })


        #AndelGrVar
        AndelerShus <- NGERFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                          datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                          minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2])) #, lagFig = 0))
        tabAndelerShus <- cbind(Antall=AndelerShus$Ngr,
                                Andeler = AndelerShus$AggVerdier$Hoved)

        output$andelerGrVarTab <- function() {
          antKol <- ncol(tabAndelerShus)
          kableExtra::kable(tabAndelerShus, format = 'html'
                            #, full_width=T
                            , digits = c(0,1) #,0,1)[1:antKol]
          ) %>%
            column_spec(column = 1, width_min = '5em') %>%
            column_spec(column = 2:(antKol+1), width = '4em') %>%
            row_spec(0, bold = T)
        }
        output$lastNed_tabAndelGrVar <- downloadHandler(
          filename = function(){
            paste0(input$valgtVar, '_andelGrVar.csv')
          },
          content = function(file, filename){
            write.csv2(tabAndelerShus, file, row.names = T, na = '')
          })

        output$tittelAndel <- renderUI({
          tagList(
            h3(AndelerShus$tittel),
            h5(HTML(paste0(AndelerShus$utvalgTxt, '<br />')))
          )}) #, align='center'
      }) #observe




#---------Gjennomsnitt------------
      observe({ #Sykehusvise gjennomsnitt, figur og tabell
            output$gjsnGrVar <- renderPlot(
                  NGERFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                 datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                 minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                 valgtMaal = input$sentralmaal
                  ),
                  width = 700, height = 800)
            UtDataGjsnGrVar <- NGERFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                              datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                              minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                              valgtMaal = input$sentralmaal)
            output$tittelGjsn <- renderUI({
                  tagList(
                        h3(UtDataGjsnGrVar$tittel),
                        h5(HTML(paste0(UtDataGjsnGrVar$utvalgTxt, '<br />')))
                  )}) #, align='center'

            tabGjsnGrVar <- cbind(Antall = UtDataGjsnGrVar$Ngr$Hoved,
                                  Sentralmål = UtDataGjsnGrVar$AggVerdier$Hoved)
            colnames(tabGjsnGrVar)[2] <- ifelse(input$sentralmaal == 'Med', 'Median', 'Gjennomsnitt')

            output$gjsnGrVarTab <- function() {
              kableExtra::kable(tabGjsnGrVar, format = 'html'
                                , full_width=F
                                , digits = c(0,1) #,1,1)[1:antKol]
              ) %>%
                column_spec(column = 1, width_min = '7em') %>%
                column_spec(column = 2:3, width = '7em') %>%
                row_spec(0, bold = T)
            }

            output$lastNed_gjsnGrVarTab <- downloadHandler(
                  filename = function(){
                        paste0(input$valgtVarGjsn, '_tabGjsnSh .csv')
                  },
                  content = function(file, filename){
                        write.csv2(tabGjsnGrVar, file, row.names = T, na = '')
                  })

            output$titteltabGjsnGrVar <- renderUI({
                  tagList(
                        h3(tabGjsnGrVar$tittel),
                        h5(HTML(paste0(tabGjsnGrVar$utvalgTxt, '<br />')))
                  )}) #, align='center'



            #------gjsnTid

            output$gjsnTid <- renderPlot(
              NGERFigGjsnTid(RegData=RegData, reshID=reshID, preprosess = 0, valgtVar=input$valgtVarGjsn,
                               datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                               minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                               valgtMaal = input$sentralmaal, enhetsUtvalg =  as.numeric(input$enhetsUtvalgGjsn),
                            tidsenhet = input$tidsenhetGjsn
              ),
              width = 1000, height = 300)
            UtDataGjsnTid <- NGERFigGjsnTid(RegData=RegData, reshID=reshID, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                                datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                                minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                                valgtMaal = input$sentralmaal, enhetsUtvalg =  as.numeric(input$enhetsUtvalgGjsn),
                                            tidsenhet = input$tidsenhetGjsn)

            tabGjsnTid <- t(UtDataGjsnTid$AggVerdier)
            grtxt <-UtDataGjsnTid$grtxt
            if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
              grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
            rownames(tabGjsnTid) <- grtxt

            antKol <- ncol(tabGjsnTid)
            navnKol <- colnames(tabGjsnTid)
            if (antKol==6) {colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])}

            output$gjsnTidTab <- function() {
              kableExtra::kable(tabGjsnTid, format = 'html'
                                , full_width=F
                                , digits = 1 #c(0,1,1,1)[1:antKol]
              ) %>%
                add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
                #add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
                column_spec(column = 1, width_min = '7em') %>%
                column_spec(column = 2:(antKol+1), width = '7em') %>%
                row_spec(0, bold = T)
            }

            output$lastNed_GjsnTidTab <- downloadHandler(
              filename = function(){
                paste0(input$valgtVarGjsn, '_tabGjsnTid .csv')
              },
              content = function(file, filename){
                write.csv2(tabGjsnTid, file, row.names = T, na = '')
              })


                }) #observe gjsnGrVar
} #server
# Run the application
shinyApp(ui = ui, server = server)

