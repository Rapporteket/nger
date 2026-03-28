
#sship::dec("c://Users/ast046/Downloads/nger15b2d454c.sql.gz__20260121_104135.tar.gz",
#           keyfile = "c://Users/ast046/.ssh/id_rsa",
#           target_dir = "c://Users/ast046/Downloads/."
#           )


Sys.setlocale(locale = 'nb_NO.UTF-8')
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="C:/Users/ast046/repo/rapporteket/rygg/dev/config")
Sys.setenv(MYSQL_DB_DATA="nger")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

source("dev/sysSetenv.R")
rmarkdown::render(
  "inst/NGERmndRapp.Rmd",
  params = list(reshId = 100412),
  output_format = "pdf_document"
)

devtools::install(".", upgrade = FALSE)
source("dev/sysSetenv.R")
nger::kjor_NGERapp(browser = TRUE)

