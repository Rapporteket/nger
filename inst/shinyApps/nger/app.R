#Resultattjeneste for NGER
library(nger)
library(kableExtra)
library(knitr)
library(lubridate)
library(plyr)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(rapbase)
library(rapFigurer)
library(shiny)
library(shinyjs)
#library(zoo)

idag <- Sys.Date()
startDato <- startDato <- paste0(as.numeric(format(idag-100, "%Y")), '-01-01') #'2019-01-01' #Sys.Date()-364
sluttDato <- idag
# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- (context %in% c("DEV", "TEST", "QA", "PRODUCTION")) #rapbase::isRapContext()
regTitle = ifelse(paaServer,'NORSK GYNEKOLOGISK ENDOSKOPIREGISTER',
							'NORSK GYNEKOLOGISK ENDOSKOPIREGISTER med FIKTIVE data')


#----------Hente data og evt. parametre som er statiske i appen----------
if (paaServer) {
  RegData <- NGERRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
  qSkjemaOversikt <- 'SELECT * FROM SkjemaOversikt'
  SkjemaOversikt <- rapbase::LoadRegData(registryName='nger',
                                         query=qSkjemaOversikt, dbType='mysql')
}

tulledata <- 0
if (!exists('RegData')) {
  data('NGERtulledata', package = 'nger')
  #SkjemaOversikt <- plyr::rename(SkjemaOversikt, replace=c('SykehusNavn'='ShNavn'))
  tulledata <- 1 #Må få med denne i tulledatafila..
  }

RegData <- NGERPreprosess(RegData)
SkjemaOversikt <- NGERPreprosess(RegData = SkjemaOversikt)


#-----Definere utvalgsinnhold
#Definere utvalgsinnhold
#sykehusNavn <- sort(c('',unique(RegData$ShNavn)), index.return=T)
#sykehusValg <- c(0,unique(RegData$ReshId))[sykehusNavn$ix]
sykehusNavn <- sort(unique(RegData$ShNavn), index.return=T)
sykehusValg <- unique(RegData$ReshId)[sykehusNavn$ix]
sykehusValg <- c(0,sykehusValg)
names(sykehusValg) <- c(' ',sykehusNavn$x)

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

 alvorKompl <- c(#"Alle"=0,
                  "Lite alvorlig"=1,
                  "Middels alvorlig"=2,
                  "Alvorlig"=3,
                  "Dødelig"=4)
 hastegrad <- c('Alle'=0,
                'Elektiv'=1,
                'Akutt'=2,
                'Ø-hjelp'=3)
 dagkir <- c('Alle'=9,
             'Dagkirurgi'=1,
             'Ikke dagkirurgi'=0)

 tidsenheter <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                      'Kvartal'='Kvartal', 'Måned'='Mnd'))

ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen

            # lag logo og tittel som en del av navbar
            #title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle),
            title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                        regTitle),
            # sett inn tittel også i browser-vindu
            windowTitle = regTitle,
            theme = "rap/bootstrap.css",



#------------Startside--------------------------
      tabPanel("Startside",
               shinyjs::useShinyjs(),
               #fluidRow(
               #column(width=5,
               br(),
               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
               h2('Velkommen til Rapporteket - Norsk Gynekologisk Endoskopiregister!', align='center'),
               br(),

               sidebarPanel(width = 3,
                           h3('Nedlastbare dokumenter med samling av resultater'),
                            h3("Månedsrapport"), #),
                            downloadButton(outputId = 'mndRapp.pdf', label='Last ned MÅNEDSRAPPORT', class = "butt"),
                            br(),
                            br(),
               h3('Samledokument'),
               helpText('Samledokumentet er ei samling av utvalgte tabeller og figurer basert på
                        operasjoner i valgt tidsrom.'),
               dateRangeInput(inputId = 'datovalgSamleDok', start = startDato, end = Sys.Date(),
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),

                downloadButton(outputId = 'samleDok.pdf', label='Last ned samledokument', class = "butt"),
                             br(),
               br(),
               br(),
               helpText('Det tar noen sekunder å generere en samlerapport/månedsrapport.
                        I mellomtida får du ikke sett på andre resultater'),
               helpText(tags$b('Ønsker du månedsrapporten eller samlerapporten tilsendt regelmessig på e-post,
                        kan du bestille dette under fanen "Abonnement."'))
               ),
               mainPanel(width = 8,
                         shinyalert::useShinyalert(),
                         tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
                         rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                             organization = uiOutput("appOrgName")
                                             , addUserInfo = TRUE
                                             ),
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
                         h4('I feltet til venstre på hver side kan man velge hvilken variabel man ønsker å se
                            resultater for. Der kan man også gjøre ulike filtreringer/utvalg av data.'),
                         h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret.'),
                         h4(tags$b('Kvalitetsindikatorer '), 'viser på fordelinger (figur/tabell) av ulike variable.'),
                         h4(tags$b('Fordelinger '), 'viser på fordelinger (figur/tabell) av ulike variable.'),
                         h4(tags$b('Andeler: per sykehus og over tid'), ' viser andeler(prosent) en per sykehus og utvikling over tid.
                            Man kan velge hvilken tidsskala man vi se på.'),
                         h4(tags$b('Gjennomsnitt: per sykehus og over tid'), ' viser gjennomsnittsverdier per sykehus og utvikling over tid.
                            Man kan velge om man vil se gjennomsnitt eller median.'),
                         h4('Gi gjerne innspill og tilbakemeldinger til registerledelsen vedrørende organisering
                            av og innhold på Rapporteket-NGER.'),
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
                            conditionalPanel(condition = "input.ark == 'Antall operasjoner'",
                                             dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                       value = Sys.Date(), max = Sys.Date() ),
                                             selectInput(inputId = 'opMetodeReg', label='Operasjonstype',
                                                         choices = opMetode
                                             ),
                                             selectInput(inputId = 'velgDiagReg', label='Diagnose',
                                                         choices = diag
                                             )

                            ),
                            conditionalPanel(
                              condition = "input.ark == 'Antall operasjoner'"  ,
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
                            ),

                            br(),
                            br(),
                            br(),
                            br(),
                            h3('Last ned egne data '),
                            #uiOutput("test"),
                            dateRangeInput(inputId = 'datovalgRegKtr', start = startDato, end = idag,
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            selectInput(inputId = 'velgReshReg', label='Velg sykehus',
                                        selected = 0,
                                        choices = sykehusValg),
                            downloadButton(outputId = 'lastNed_dataTilRegKtr', label='Last ned fødselsnummer og operasjonsdato'),
                            br(),
                            br(),
                            downloadButton(outputId = 'lastNed_dataDump', label='Last ned datadump')
               ),

               mainPanel(
                 tabsetPanel(id='ark',
                             tabPanel('Antall operasjoner',
                                      h2("Antall opphold per avdeling"),
                                      uiOutput("undertittelReg"),
                                      p("Velg tidsperiode ved å velge sluttdato/tidsenhet i menyen til venstre"),
                                      br(),
                                      fluidRow(
                                        tableOutput("tabAntOpphSh")
                                        ,downloadButton(outputId = 'lastNed_tabAntOpph', label='Last ned')
                                      )
                             ),
                              # output$tabAntOpphSh <- renderTable({
                             #   switch(input$tidsenhetReg,
                             #          Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])
                             #          Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))
                             # }, rownames = T, digits=0, spacing="xs"

                             tabPanel('Antall registrerte skjema',
                                      h4("Tabellen viser antall registrerte skjema for valgt tidsperiode"),
                                      p("Velg tidsperiode i menyen til venstre"),
                                      br(),
                                      fluidRow(
                                        tableOutput("tabAntSkjema")
                                        #,downloadButton(outputId = 'lastNed_tabAntSkjema', label='Last ned')
                                      )
                             )

                 )
               )

      ), #tab Registreringsoversikter


#-----Kvalitetsindikatorer------------
tabPanel(p("Kvalitetsindikatorer", title = 'Prosessindikatorer, RAND36, TSS2'),
h3('Registerets kvalitetsindikatorer', align='center'),
         sidebarPanel(width=3,
                      h3('Utvalg'),
                      selectInput(
                        inputId = "valgtVarKval", label="Velg variabel",
                        choices = c('Prosessindikatorer' = 'kvalInd',
                                    'TSS2, oppfølging' = 'TSS0',
                                    'RAND36, oppfølging' = 'RAND0'
                        )
                      ),
                      dateRangeInput(inputId = 'datovalgKval', start = startDato, end = Sys.Date(),
                                     label = "Tidsperiode", separator="t.o.m.", language="nb"),
                      sliderInput(inputId="alderKval", label = "Alder", min = 0,
                                  max = 110, value = c(0, 110)
                      ),
                      selectInput(inputId = 'enhetsUtvalgKval', label='Egen enhet og/eller landet',
                                  choices = enhetsUtvalg
                      ),
                      selectInput(inputId = 'opMetodeKval', label='Operasjonstype',
                                  choices = opMetode
                      ),
                      selectInput(inputId = 'velgDiagKval', label='Diagnose',
                                  choices = diag
                      ),
                      selectInput(inputId = 'hastegradKval', label='Hastegrad',
                                  choices = hastegrad
                      ),
                      selectInput(inputId = 'dagkirKval', label='Dagkirurgi',
                                  choices = dagkir
                      ),
                      selectInput(inputId = 'alvorlighetKomplKval',
                                  label='Alvorlighetsgrad, postoperative komplikasjoner',
                                  multiple = T, #selected=0,
                                  choices = alvorKompl
                      ),
                      selectInput(inputId = 'velgReshKval', label='Velg eget Sykehus',
                                  #selected = 0,
                                  choices = sykehusValg)
         ),
         mainPanel(
           tabsetPanel(
             tabPanel(
               'Figur',
               br(),
               em('(Høyreklikk på figuren for å laste den ned)'),
               br(),
               br(),
               plotOutput('kvalInd')),
             tabPanel(
               'Tabell',
               uiOutput("tittelKvalInd"),
               br(),
               tableOutput('kvalIndTab'),
               downloadButton(outputId = 'lastNed_tabKvalInd', label='Last ned')
             )
           ))

), #tab Kvalitetsindikatorer

#--------------Tabelloversikter-------------
tabPanel(p("Tabelloversikter", title = 'Instrumentbruk, komplikasjoner'),
         h3('Tabelloversikter', align='center'),
         sidebarPanel(width=3,
                      h3('Utvalg'),
                      dateRangeInput(inputId = 'datovalgTab', start = startDato, end = Sys.Date(),
                                     label = "Tidsperiode", separator="t.o.m.", language="nb"),
                      conditionalPanel(
                        condition = "input.tab == 'Pasientegenskaper'"  ,
                       selectInput(inputId = "tidsenhetTab", label="Velg tidsenhet",
                                  choices = tidsenheter))
         ),
         mainPanel(
           tabsetPanel(id='tab',
             tabPanel('Instrumentbruk, Lap',
                      br(),
                      h4('Tabellen viser antall ganger i den valgte tidsperioden ulike instrumenter er benyttet ved laparoskopi. '),
                      br(),
                      tableOutput('tabInstrBruk'),
                      downloadButton(outputId = 'lastNed_tabInstrBruk', label='Last ned tabell')
             ),
             tabPanel('Komplikasjoner, Lap.',
                      br(),
                      uiOutput("tittelLapKompl"),
                      br(),
                      tableOutput('LapKompl'),
                      downloadButton(outputId = 'lastNed_tabLapKompl', label='Last ned tabell')
             )
           ))
), #Tab tabelloversikter


#--------Fordelinger-----------
tabPanel(p("Fordelinger", title= 'Alder, anestesi, ASA, BMI, diagnoser, komplikasjoner, prosessvariable, prosedyrer,
           RAND36, TSS2, utdanning'),
         #-----
         sidebarPanel(width = 3,
                      h3('Utvalg'),
                      selectInput(
                        inputId = "valgtVar", label="Velg variabel",
                        choices = c('Alder' = 'Alder',
                                         'Alvorlighetsgrad, postop. kompl.' = 'Opf0AlvorlighetsGrad',
                                         'Anestesitype' = 'OpAnestesi',
                                         'ASA-grad' = 'OpASA',
                                         'BMI-kategori' = 'OpBMI',
                                         'Dagkirurgiske inngrep' = 'OpDagkirurgi',
                                         'Diagnoser, hyppigste' = 'Diagnoser',
                                         'Gjennomføringsgrad av hysteroskopi' = 'HysGjforingsGrad',
                                         'Hastegrad av operasjon' = 'OpKategori',
                                         'Hjelpeinnstikk, antall' = 'LapNumHjelpeinnstikk',
                                         'Hysteroskopi intrapoerative komplikasjoner' = 'HysKomplikasjoner',
                                         'Infeksjoner, type' = 'Opf0KomplInfeksjon',
                                         'Komplikasjoner, postoperativt' = 'KomplPostopType',
                                         'Laparaskopisk ekstrautstyr' = 'LapEkstrautstyr',
                                         'Laparaskopisk tilgang, teknikk og metode' = 'LapTeknikk',
                                         'Laparoskopiske intraabdominale komplikasjoner' = 'LapIntraabdominell',
                                         'Laparoskopiske intrapoerative komplikasjoner' = 'LapKomplikasjoner',
                                         'Norskkunnskaper' = 'Norsktalende',
                                         'Opfølgingsmetode' = 'Opf0metode',
                                         'Operasjon i legens vakttid' = 'OpIVaktTid',
                                         'Operasjonsmetode' = 'OpMetode',
                                         'Operasjonstid (minutter)' = 'OpTid',
                                         'Primæroperasjon eller reoperasjon' = 'OpType',
                                         'Prosedyrer, hyppigste' = 'Prosedyrer',
                                         'Postoperative komplikasjoner vs. utdanning' = 'KomplPostUtd',
                                         'RAND36 Fysisk funksjon' = 'R0ScorePhys',
                                         'RAND36 Begrenses av fysisk helse' = 'R0ScoreRoleLmtPhy',
                                         'RAND36 Følelsesmessig rollebegrensning' = 'R0ScoreRoleLmtEmo',
                                         'RAND36 Energinivå/vitalitet' = 'R0ScoreEnergy',
                                         'RAND36 Mental helse' = 'R0ScoreEmo',
                                         'RAND36 Sosial funksjon' = 'R0ScoreSosial',
                                         'RAND36 Smerte' = 'R0ScorePain',
                                         'RAND36 Generell helsetilstand' = 'R0ScoreGeneral',
                                         'Registreringsforsinkelse' =  'RegForsinkelse',
                                         'Reoperasjoner som følge av komplikasjon vs. utdanning' = 'KomplReopUtd',
                                         'Sivilstatus' = 'SivilStatus',
                                         'Tidligere vaginale inngrep' = 'OpTidlVagInngrep',
                                         'Tidligere laparoskopi' = 'OpTidlLapsko',
                                         'Tidligere laparatomi' = 'OpTidlLaparotomi',
                                         'TSS2, sp.1 Mottak på avdelinga' = 'Tss2Mott',
                                         'TSS2, sp.2 Behandlingsopplegg' = 'Tss2Behandling',
                                         'TSS2, sp.3 Lyttet behandleren' = 'Tss2Lytte',
                                         'TSS2, sp.4 Tillit til behandleren' = 'Tss2Behandlere',
                                         'TSS2, sp.5 Enighet om målsetning' = 'Tss2Enighet',
                                         'TSS2, sp.6 Generell oppfatning av avdelinga' = 'Tss2Generelt',
                                         'Utdanning' = 'Utdanning'
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
                      selectInput(inputId = 'velgResh', label='Velg eget Sykehus',
                                  selected = 0,
                                  choices = sykehusValg)
         ),
         #--------
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
tabPanel(p("Andeler: per sykehus og tid", title='Alder, antibiotika, ASA, fedme, gjennomføringsgrad, komplikasjoner,
           konvertering, oppfølging, registreringsforsinkelse, komplikasjoner, TSS2, utdanning'),
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
                         'Dagkirurgiske inngrep' = 'OpDagkirurgi',
                         'Fedme (BMI>30)' = 'OpBMI',
                         'Operasjonstid (minutter)' = 'OpTid',
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
           selectInput(inputId = 'opMetodeAndel', label='Operasjonstype',
                       choices = opMetode
           ),
           selectInput(inputId = 'velgDiagAndel', label='Diagnose',
                       choices = diag
           ),
           selectInput(inputId = 'hastegradAndel', label='Hastegrad',
                       choices = hastegrad
           ),
           selectInput(inputId = 'alvorlighetKomplAndel',
                       label='Alvorlighetsgrad, postoperative komplikasjoner',
                       multiple = T, #selected=0,
                       choices = alvorKompl
           ),
           br(),
           p(em('Følgende utvalg gjelder bare figuren/tabellen som viser utvikling over tid')),
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
               br(),
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
      tabPanel(p("Gjennomsnitt: per sykehus og over tid",title='Alder, operasjonstid, registreringsforsinkelse, RAND36, TSS2sumskår'),
               h2('Sykehusvise gjennomsnitt/median og utvikling over tid for valgt variabel', align='center'),
               h5('Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen til venstre.
                  (Man kan også gjøre ulike filtreringer.)', align='center'),
               br(),
               sidebarPanel(width = 3,
                            h3('Utvalg'),
                            #8 hoveddimensjoner av Rand, TSS2spm + sumskår
                            selectInput(
                                  inputId = "valgtVarGjsn", label="Velg variabel (flere valg kommer)",
                                  choices = c('Alder' = 'Alder',
                                              'Operasjonstid (minutter)' = 'OpTid',
                                              'Registreringsforsinkelse' = 'RegForsinkelse',
                                              'RAND36 Fysisk funksjon' = 'R0ScorePhys',
                                              'RAND36 Begrenses av fysisk helse' = 'R0ScoreRoleLmtPhy',
                                              'RAND36 Følelsesmessig rollebegrensning' = 'R0ScoreRoleLmtEmo',
                                              'RAND36 Energinivå/vitalitet' = 'R0ScoreEnergy',
                                              'RAND36 Mental helse' = 'R0ScoreEmo',
                                              'RAND36 Sosial funksjon' = 'R0ScoreSosial',
                                              'RAND36 Smerte' = 'R0ScorePain',
                                              'RAND36 Generell helsetilstand' = 'R0ScoreGeneral',
                                              'TSS2, sumskår' = 'Tss2Sumskaar'
                                             )
                            ),
                            dateRangeInput(inputId = 'datovalgGjsn', start = startDato, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            sliderInput(inputId="alderGjsn", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                                                               choices = c("Gjennomsnitt"='gjsn', "Median"='med')),
                            selectInput(inputId = 'opMetodeGjsn', label='Operasjonstype',
                                        choices = opMetode
                            ),
                            selectInput(inputId = 'velgDiagGjsn', label='Diagnose',
                                        choices = diag
                            ),
                            selectInput(inputId = 'hastegradGjsn', label='Hastegrad',
                                        choices = hastegrad
                            ),
                            selectInput(inputId = 'alvorlighetKomplGjsn',
                                        label='Alvorlighetsgrad, postoperative komplikasjoner',
                                        multiple = T, #selected=0,
                                        choices = alvorKompl
                            ),
                            br(),
                            p(em('Følgende utvalg gjelder bare figuren/tabellen som viser utvikling over tid')),
                            selectInput(inputId = 'enhetsUtvalgGjsn', label='Egen enhet og/eller landet',
                                        choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
                            ),
                            selectInput(inputId = "tidsenhetGjsn", label="Velg tidsenhet",
                                        choices = tidsenheter
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
                                        downloadButton(outputId = 'lastNed_gjsnTidTab', label='Last ned tabell'))
                                 )
                     )
                )
), #GjsnGrVar/Tid

#----------Abonnement-----------------
tabPanel(p("Abonnement",
           title='Bestill automatisk utsending av rapporter på e-post'),
         sidebarLayout(
           sidebarPanel(width = 3,
                        selectInput("subscriptionRep", "Rapport:",
                                    c("Månedsrapport", "Samlerapport")),
                        selectInput("subscriptionFreq", "Frekvens:",
                                    list(Årlig="Årlig-year",
                                          Kvartalsvis="Kvartalsvis-quarter",
                                          Månedlig="Månedlig-month",
                                          Ukentlig="Ukentlig-week",
                                          Daglig="Daglig-DSTday"),
                                    selected = "Månedlig-month"),
                        #selectInput("subscriptionFileFormat", "Format:",
                        #            c("html", "pdf")),
                        actionButton("subscribe", "Bestill!")
           ),
           mainPanel(
             uiOutput("subscriptionContent")
           )
         )
) #tab abonnement

#--------slutt tab'er----------

) #ui-del




#----- Define server logic required to draw a histogram-------
server <- function(input, output, session) {

  raplog::appLogger(session, msg = 'Starter Rapporteket-NGER')
  #system.file('NGERmndRapp.Rnw', package='nger')

    #hospitalName <-getHospitalName(rapbase::getUserReshId(session))
    #reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)),
                         #      ifelse(tulledata==1, 8, 105460))})
    reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 105460)
    #rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
    rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
    brukernavn <- reactive({ifelse(paaServer, rapbase::getUserName(session), 'inkognito')})


    #output$reshID <- renderText({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 105460)}) #evt renderUI

     observe({
       #vise <- rolle =='SC'
       #shinyjs::toggle(id = 'velgResh', condition = vise)
       #shinyjs::toggle(id = 'velgReshReg', condition = vise)
       #shinyjs::toggle(id = 'velgReshKval', condition = vise)
        if (rolle != 'SC') { #
       shinyjs::hide(id = 'velgResh')
       shinyjs::hide(id = 'velgReshReg')
       shinyjs::hide(id = 'velgReshKval')
     #hideTab(inputId = "tabs_andeler", target = "Figur, sykehusvisning")
       }
    })
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

  #--------------Startside------------------------------
  #-------Samlerapporter--------------------

  # filename function for re-use - i dette tilfellet vil det funke fint å hardkode det samme..
  downloadFilename <- function(fileBaseName, type='') {
    paste0(fileBaseName, as.character(as.integer(as.POSIXct(Sys.time()))), '.pdf')
    }

        # contentFile <- function(file, srcFil, tmpFil, package,
      #                           reshID=0, datoFra=startDato, datoTil=Sys.Date()) {
      #       src <- normalizePath(system.file(srcFil, package="nger"))
      #       #dev.off()
      #
      #       # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
      #       owd <- setwd(tempdir())
      #       on.exit(setwd(owd))
      #       file.copy(src, tmpFil, overwrite = TRUE)
      #
      #       #texfil <- knitr::knit(tmpFil, encoding = 'UTF-8')
      #       #tools::texi2pdf(texfil, clean = TRUE)
      #       knitr::knit2pdf(tmpFil)
      #
      #       gc() #Opprydning gc-"garbage collection"
      #       file.rename(stringr::str_replace(texfil,'tex','pdf'), file)
      # }
       output$mndRapp.pdf <- downloadHandler(
            filename = function(){ downloadFilename('NGERmaanedsrapport')},
            content = function(file){
              henteSamlerapporter(file, rnwFil="NGERmndRapp.Rnw",
                                  reshID = reshID)
            })

      output$samleDok.pdf <- downloadHandler(
        filename = function(){ downloadFilename('Samledokument')},
        content = function(file){
          henteSamlerapporter(file, rnwFil="NGERSamleRapp.Rnw",
                      reshID = reshID,
                      datoFra = input$datovalgSamleDok[1],
                      datoTil = input$datovalgSamleDok[2])
        }
      )

      #output$lenkeNorScir <- renderUI({tagList("www.norscir.no", www.norscir.no)})

     output$tabEgneReg <- renderTable({
       xtable::xtable(tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg,
                                      antMnd=12, reshID = reshID))},
            rownames=T,
            digits = 0
      )
      #tabAntOpphShMnd(RegData=RegData, reshID = 8)
 #----------Registreringsoversikter ----------------------

            output$undertittelReg <- renderUI({
              t1 <- 'Tabellen viser operasjoner '
              tagList(
                    br(),
                  h4(HTML(switch(input$tidsenhetReg,
                         Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, '<br />'),
                         Aar = paste0(t1, 'siste 5 år før ', input$sluttDatoReg, '<br />'))),

                    if(as.numeric(input$opMetodeReg)!=0){
                      HTML(paste0('Operasjonsmetode: ',
                        names(opMetode[opMetode==as.numeric(input$opMetodeReg)]), '<br />'))},
                    if(as.numeric(input$velgDiagReg)!=0){
                      paste0('Diagnose: ', names(diag[diag==as.numeric(input$velgDiagReg)]))}) #, '<br />'
                  )
                  #names(diag[diag==as.numeric(2)])
                  # h4(HTML(paste0(names(opMetode[opMetode==as.numeric(input$opMetodeReg)]), '<br />'),
                   #       names(velgDiag[velgDiag==as.numeric(input$velgDiag)]), '<br />'))
                  })
      observe({
        tabAntOpphShMndAar <- switch(input$tidsenhetReg,
                                     Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12,
                                                         OpMetode=as.numeric(input$opMetodeReg),
                                                         velgDiag=as.numeric(input$velgDiagReg)), #input$datovalgTab[2])
                                     Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg,
                                                          OpMetode=as.numeric(input$opMetodeReg),
                                                          velgDiag=as.numeric(input$velgDiagReg)))
        #utvalg <- tabAntOpphShMndAarDUM[[2]]
        #Aar=xtable::xtable(tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg)), digits=0)
        output$tabAntOpphSh <- renderTable(tabAntOpphShMndAar, rownames = T, digits=0, spacing="xs")
        output$lastNed_tabAntOpph <- downloadHandler(
          filename = function(){paste0('tabAntOpph.csv')},
          content = function(file, filename){write.csv2(tabAntOpphShMndAar, file, row.names = T, na = '')
          })



        #RegData som har tilknyttede skjema av ulik type
        AntSkjemaAvHver <- tabAntSkjema(SkjemaOversikt=SkjemaOversikt,
                                        datoFra = input$datovalgReg[1],
                                        datoTil=input$datovalgReg[2],
                                        skjemastatus=as.numeric(input$skjemastatus))
        output$tabAntSkjema <- renderTable(AntSkjemaAvHver
                                           ,rownames = T, digits=0, spacing="xs" )
        output$lastNed_tabAntSkjema <- downloadHandler(
          filename = function(){'tabAntSkjema.csv'},
          content = function(file, filename){write.csv2(AntSkjemaAvHver, file, row.names = T, na = '')
          })
      })

      # Hente oversikt over hvilke registrereinger som er gjort (opdato og fødselsdato)
      RegOversikt <- RegData[ , c('Fodselsdato', 'OpDato', 'ReshId', 'ShNavn', 'BasisRegStatus')]
      observe({
        RegOversikt <- dplyr::filter(RegOversikt,
                                     as.Date(OpDato) >= input$datovalgRegKtr[1],
                                     as.Date(OpDato) <= input$datovalgRegKtr[2])
        if (rolle == 'SC') {
          valgtResh <- as.numeric(input$velgReshReg)
          ind <- if (valgtResh == 0) {1:dim(RegOversikt)[1]
          } else {which(as.numeric(RegOversikt$ReshId) %in% as.numeric(valgtResh))}
          tabDataRegKtr <- RegOversikt[ind,]
         }  else {
           tabDataRegKtr <- RegOversikt[which(RegOversikt$ReshId == reshID), ]}
        #tabDataRegKtr <-RegOversikt[which(RegOversikt$ReshId == reshID), ]
         output$lastNed_dataTilRegKtr <- downloadHandler(
           filename = function(){'dataTilKtr.csv'},
           content = function(file, filename){write.csv2(tabDataRegKtr, file, row.names = F, na = '')})
       })

      # Egen datadump
      variablePRM <- c("R0Metode", "R0ScoreEmo", "R0ScoreEnergy", "R0ScoreGeneral", "R0ScorePain",
                       "R0ScorePhys", "R0ScoreRoleLmtEmo", "R0ScoreRoleLmtPhy", "R0ScoreSosial",
                       "R0Spm2", "R0Status", "Tss2Behandlere", "Tss2Behandling", "Tss2Enighet",
                       "Tss2Generelt", "Tss2Lytte", "Tss2Mott", "Tss2Status", "Tss2Type")
      observe({
        DataDump <- dplyr::filter(RegData,
                                     as.Date(OpDato) >= input$datovalgRegKtr[1],
                                     as.Date(OpDato) <= input$datovalgRegKtr[2])


        if (rolle =='SC') {
        valgtResh <- as.numeric(input$velgReshReg)
        ind <- if (valgtResh == 0) {1:dim(DataDump)[1]
         } else {which(as.numeric(DataDump$ReshId) %in% as.numeric(valgtResh))}
        tabDataDump <- DataDump[ind,]
        #output$test <- renderText(valgtResh)
        } else {
          tabDataDump <-
            DataDump[which(DataDump$ReshId == reshID), -which(names(DataDump) %in% variablePRM)]
          #output$test <- renderText(dim(tabDataDump)[1])
          } #Tar bort PROM/PREM til egen avdeling
        #tabDataDump <- DataDump[which(DataDump$ReshId == reshID), -which(names(DataDump) %in% variablePRM)]

output$lastNed_dataDump <- downloadHandler(
        filename = function(){'dataDumpNGER.csv'},
        content = function(file, filename){write.csv2(tabDataDump, file, row.names = F, na = '')})
      })
      #---------Kvalitetsindikatorer------------
      observe({   #KvalInd
        output$kvalInd <- renderPlot({
          NGERFigKvalInd(RegData=RegData, valgtVar=input$valgtVarKval, preprosess = 0,
                         datoFra=input$datovalgKval[1], datoTil=input$datovalgKval[2],
                         reshID = reshID,
                         minald=as.numeric(input$alderKval[1]), maxald=as.numeric(input$alderKval[2]),
                         OpMetode = as.numeric(input$opMetodeKval),
                         Hastegrad = as.numeric(input$hastegradKval),
                         dagkir = as.numeric(input$dagkirKval),
                         velgDiag = as.numeric(input$velgDiagKval),
                        # AlvorlighetKompl = as.numeric(input$alvorlighetKompl),
                         enhetsUtvalg=as.numeric(input$enhetsUtvalgKval),
                         velgAvd=input$velgReshKval,
                        session = session)
        }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
        )
        #RegData må hentes ut fra valgtVar
        UtDataKvalInd <- NGERFigKvalInd(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarKval,
                                     datoFra=input$datovalgKval[1], datoTil=input$datovalgKval[2],
                                     reshID = reshID,
                                     minald=as.numeric(input$alderKval[1]), maxald=as.numeric(input$alderKval[2]),
                                     OpMetode = as.numeric(input$opMetodeKval),
                                     Hastegrad = as.numeric(input$hastegradKval),
                                     velgDiag = as.numeric(input$velgDiagKval),
                                     enhetsUtvalg=as.numeric(input$enhetsUtvalgKval),
                                     velgAvd=input$velgReshKval,
                                     session = session)
        # testData <- NGERFigKvalInd(RegData=RegData, preprosess = 0, valgtVar='kvalInd',
        #                            datoFra='2019-01-01', datoTil=Sys.Date(),
        #                            reshID = 110734,
        #                            enhetsUtvalg=1)
        # tabKvalInd <- lagTabavFig(UtDataFraFig = testData)
        tabKvalInd <- lagTabavFig(UtDataFraFig = UtDataKvalInd) #lagTabavFigAndeler

        output$tittelKvalInd <- renderUI({
          tagList(
            h3(UtDataKvalInd$tittel),
            h5(HTML(paste0(UtDataKvalInd$utvalgTxt, '<br />')))
          )}) #, align='center'

        #output$kvalIndTab <- renderTable(tabKvalInd, rownames = T)

        output$kvalIndTab <- function() {
          antKol <- ncol(tabKvalInd)
          kableExtra::kable(tabKvalInd, format = 'html'
                            , full_width=F
                            , digits = c(0,1,0,1)[1:antKol]
          ) %>%
            add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
            column_spec(column = 1, width_min = '7em') %>%
            column_spec(column = 2:(ncol(tabKvalInd)+1), width = '7em') %>%
            row_spec(0, bold = T)
        }

        output$lastNed_tabKvalInd <- downloadHandler(
          filename = function(){paste0(input$valgtVarKval, '_kvalInd.csv')},
          content = function(file, filename){write.csv2(tabKvalInd, file, row.names = T, na = '')
          })
      }) #observe Kvalitetsind


      #----------Tabelloversikter ----------------------
      observe({
        tabInstrumentbruk <- instrumentbruk(RegData = RegData,
                                            datoFra = input$datovalgTab[1], datoTil = input$datovalgTab[2])
         output$tabInstrBruk <- renderTable(tabInstrumentbruk, rownames = T, digits=0, spacing="xs")
         output$lastNed_tabInstrBruk <- downloadHandler(
           filename = function(){paste0('tabInstrumentbruk.csv')},
           content = function(file, filename){write.csv2(tabInstrumentbruk, file, row.names = T, na = '')})
         LapKomplData <- tabKomplLap(RegData=RegData, reshID=reshID,
                                  datoFra = input$datovalgTab[1], datoTil = input$datovalgTab[2])
         output$tittelLapKompl <- renderUI(tagList(
           h4('Hyppighet (%) av laparoskopiske komplikasjoner. '),
           h4(paste0('Totalt ble det utført ', LapKomplData$AntLap, ' laparaskopier i tidsperioden.'))))
         output$LapKompl <- renderTable(LapKomplData$AndelLapKomplTab, rownames = T, digits=1, spacing="xs") #,caption = tabtxtLapKompl)
         output$lastNed_tabLapKompl <-  downloadHandler(
           filename = function(){paste0('tabLapKompl.csv')},
           content = function(file, filename){write.csv2(LapKomplData$AntLap, file, row.names = T, na = '')})
          #,caption = tabtxtLapKompl)

      })



      #---------Fordelinger------------
            observe({   #Fordelingsfigurer og tabeller

            output$fordelinger <- renderPlot({
                  NGERFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                               reshID = reshID,
                               minald=as.numeric(input$alder[1]),
                               maxald=as.numeric(input$alder[2]),
                               OpMetode = as.numeric(input$opMetode),
                               Hastegrad = as.numeric(input$hastegrad),
                               velgDiag = as.numeric(input$velgDiag),
                               AlvorlighetKompl = as.numeric(input$alvorlighetKompl),
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                               velgAvd=input$velgResh,
                               session = session)
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )
            #RegData må hentes ut fra valgtVar
            UtDataFord <- NGERFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                       reshID = reshID,
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                       OpMetode = as.numeric(input$opMetode),
                                       Hastegrad = as.numeric(input$hastegrad),
                                       velgDiag = as.numeric(input$velgDiag),
                                       AlvorlighetKompl = as.numeric(input$alvorlighetKompl),
                                       enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                       velgAvd=input$velgResh,
                                       session = session)
            tabFord <- lagTabavFig(UtDataFraFig = UtDataFord) #lagTabavFigAndeler
            output$tittelFord <- renderUI({
                  tagList(
                        h3(UtDataFord$tittel),
                        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
                  )}) #, align='center'
            #output$fordelingTab <- renderTable(tabFord, rownames = T)

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
                  content = function(file, filename){write.csv2(tabFord, file, row.names = T, na = '')
                  })
      }) #observe Fordeling


#--------------Andeler-----------------------------------
      output$andelerGrVar <- renderPlot({
        NGERFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                            datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                            minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                            OpMetode = as.numeric(input$opMetodeAndel),
                            Hastegrad = as.numeric(input$hastegradAndel),
                            velgDiag = as.numeric(input$velgDiagAndel),
                            AlvorlighetKompl = as.numeric(input$alvorlighetKomplAndel),
                            session=session)
      }, height = 800, width=700 #height = function() {session$clientData$output_andelerGrVarFig_width} #})
      )

      output$andelTid <- renderPlot({

        NGERFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                       reshID= reshID,
                       datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                       minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                       OpMetode = as.numeric(input$opMetodeAndel),
                       Hastegrad = as.numeric(input$hastegradAndel),
                       velgDiag = as.numeric(input$velgDiagAndel),
                       AlvorlighetKompl = as.numeric(input$alvorlighetKomplAndel),
                       tidsenhet = input$tidsenhetAndel,
                       enhetsUtvalg = input$enhetsUtvalgAndel,
                       session=session)
      }, height = 300, width = 1000
      )

      observe({
        #AndelTid
        AndelerTid <- NGERFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                     reshID= reshID,
                                     datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                     minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                     OpMetode = as.numeric(input$opMetodeAndel),
                                     Hastegrad = as.numeric(input$hastegradAndel),
                                     velgDiag = as.numeric(input$velgDiagAndel),
                                     AlvorlighetKompl = as.numeric(input$alvorlighetKomplAndel),
                                     tidsenhet = input$tidsenhetAndel,
                                     enhetsUtvalg = input$enhetsUtvalgAndel,
                                     session=session) #,lagFig=0)
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
                                          minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                          OpMetode = as.numeric(input$opMetodeAndel),
                                          Hastegrad = as.numeric(input$hastegradAndel),
                                          velgDiag = as.numeric(input$velgDiagAndel),
                                          AlvorlighetKompl = as.numeric(input$alvorlighetKomplAndel),
                                          session=session) #, lagFig = 0))
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
                                 valgtMaal = input$sentralmaal,
                                 OpMetode = as.numeric(input$opMetodeGjsn),
                                 Hastegrad = as.numeric(input$hastegradGjsn),
                                 velgDiag = as.numeric(input$velgDiagGjsn),
                                 AlvorlighetKompl = as.numeric(input$alvorlighetKomplGjsn),
                                 session = session
            ),
                  width = 700, height = 800)
            UtDataGjsnGrVar <- NGERFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                              datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                              minald=as.numeric(input$alderGjsn[1]),
                                              maxald=as.numeric(input$alderGjsn[2]),
                                              valgtMaal = input$sentralmaal,
                                              OpMetode = as.numeric(input$opMetodeGjsn),
                                              Hastegrad = as.numeric(input$hastegradGjsn),
                                              velgDiag = as.numeric(input$velgDiagGjsn),
                                              AlvorlighetKompl = as.numeric(input$alvorlighetKomplGjsn),
                                              session = session)
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
              NGERFigGjsnTid(RegData=RegData, reshID= reshID, preprosess = 0, valgtVar=input$valgtVarGjsn,
                               datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                               minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                               valgtMaal = input$sentralmaal, enhetsUtvalg =  as.numeric(input$enhetsUtvalgGjsn),
                             OpMetode = as.numeric(input$opMetodeGjsn),
                             Hastegrad = as.numeric(input$hastegradGjsn),
                             velgDiag = as.numeric(input$velgDiagGjsn),
                             AlvorlighetKompl = as.numeric(input$alvorlighetKomplGjsn),
                            tidsenhet = input$tidsenhetGjsn,
                            session = session
              ),
              width = 1000, height = 300)
            UtDataGjsnTid <- NGERFigGjsnTid(RegData=RegData, reshID= reshID, preprosess = 0,
                                            valgtVar=input$valgtVarGjsn,
                                                datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                                minald=as.numeric(input$alderGjsn[1]),
                                            maxald=as.numeric(input$alderGjsn[2]),
                                                valgtMaal = input$sentralmaal,
                                            enhetsUtvalg =  as.numeric(input$enhetsUtvalgGjsn),
                                            OpMetode = as.numeric(input$opMetodeGjsn),
                                            Hastegrad = as.numeric(input$hastegradGjsn),
                                            velgDiag = as.numeric(input$velgDiagGjsn),
                                            AlvorlighetKompl = as.numeric(input$alvorlighetKomplGjsn),
                                            tidsenhet = input$tidsenhetGjsn, lagFigur = 0,
                                            session = session)

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

            output$lastNed_gjsnTidTab <- downloadHandler(
              filename = function(){
                paste0(input$valgtVarGjsn, '_tabGjsnTid .csv')
              },
              content = function(file, filename){
                write.csv2(tabGjsnTid, file, row.names = T, na = '')
              })

                }) #observe gjsnGrVar


      #------------------ Abonnement ----------------------------------------------
      ## reaktive verdier for å holde rede på endringer som skjer mens
      ## applikasjonen kjører
      rv <- reactiveValues(
        subscriptionTab = rapbase::makeUserSubscriptionTab(session))
      ## lag tabell over gjeldende status for abonnement
      output$activeSubscriptions <- DT::renderDataTable(
        rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
        rownames = FALSE, options = list(dom = 't')
      )

      ## lag side som viser status for abonnement, også når det ikke finnes noen
      output$subscriptionContent <- renderUI({
        fullName <- rapbase::getUserFullName(session)
        if (length(rv$subscriptionTab) == 0) {
          p(paste("Ingen aktive abonnement for", fullName))
        } else {
          tagList(
            p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                    rapbase::getUserEmail(session), ":")),
            DT::dataTableOutput("activeSubscriptions")
          )
        }
      })

            ## nye abonnement
      observeEvent (input$subscribe, { #MÅ HA
        #package <- "intensiv"
        owner <- rapbase::getUserName(session)
        interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
        intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
        organization <- rapbase::getUserReshId(session)
        runDayOfYear <- rapbase::makeRunDayOfYearSequence(
          interval = interval
        )
        email <- rapbase::getUserEmail(session)
        if (input$subscriptionRep == "Månedsrapport") {
          synopsis <- "NGER/Rapporteket: Månedsrapport"
          rnwFil <- "NGERmndRapp.Rnw" #Navn på fila
        }
        if (input$subscriptionRep == "Samlerapport") {
          synopsis <- "NGER/Rapporteket: Samlerapport"
          rnwFil <- "NGERSamleRapp.Rnw" #Navn på fila
        }

        fun <- "abonnementNGER"  #"henteSamlerapporter"
        paramNames <- c('rnwFil', 'brukernavn', "reshID", "datoFra", 'datoTil')
        paramValues <- c(rnwFil, brukernavn(), reshID, startDato, as.character(idag)) #input$subscriptionFileFormat)

        test <- abonnementNGER(rnwFil="NGERSamleRapp.Rnw", brukernavn='tullebukk',
                               reshID=105460, datoFra = '2019-03-01')

        rapbase::createAutoReport(synopsis = synopsis, package = 'nger',
                                  fun = fun, paramNames = paramNames,
                                  paramValues = paramValues, owner = owner,
                                  email = email, organization = organization,
                                  runDayOfYear = runDayOfYear, interval = interval,
                                  intervalName = intervalName)
        rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
      })

      ## slett eksisterende abonnement
      observeEvent(input$del_button, {
        selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
        rapbase::deleteAutoReport(selectedRepId)
        rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
      })

} #server
# Run the application
shinyApp(ui = ui, server = server)

