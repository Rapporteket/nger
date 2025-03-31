
#sship::dec("c://Users/ast046/Downloads/nger17bc480df.sql.gz__20250304_095058.tar.gz",
#           keyfile = "p://.ssh/id_rsa",
#           target_dir = "c://Users/ast046/Downloads/."
#           )


Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="C:/Users/ast046/repo/rapporteket/rygg/dev/config")
Sys.setenv(MYSQL_DB_DATA="nger")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor
nger::kjor_NGERapp()

