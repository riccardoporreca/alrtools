# Author: Adam L. Rich
# Date:   March 6, 2014
# Description:
#
#   Output details about the settings for this session
#


session <- list()



# Get the command line args
session$commandArgs <- commandArgs()



# Check to see if environment variables exist
#   R_ENVIRON
#   R_HOME
#   R_HISTFILE
session$CURL_CA_BUNDLE                <- Sys.getenv('CURL_CA_BUNDLE', unset = NA)
session$EDITOR                        <- Sys.getenv('EDITOR', unset = NA)
session$GFORTRAN_STDOUT_UNIT          <- Sys.getenv('GFORTRAN_STDOUT_UNIT', unset = NA)
session$MAKE                          <- Sys.getenv('MAKE', unset = NA)
session$PAGER                         <- Sys.getenv('PAGER', unset = NA)
session$R_ARCH                        <- Sys.getenv('R_ARCH', unset = NA)
session$R_BATCH                       <- Sys.getenv('R_BATCH', unset = NA)
session$R_BZIPCMD                     <- Sys.getenv('R_BZIPCMD', unset = NA)
session$R_BROWSER                     <- Sys.getenv('R_BROWSER', unset = NA)
session$R_DEFAULT_PACKAGES            <- Sys.getenv('R_DEFAULT_PACKAGES', unset = NA)
session$R_DOC_DIR                     <- Sys.getenv('R_DOC_DIR', unset = NA)
session$R_ENVIRON                     <- Sys.getenv('R_ENVIRON', unset = NA)
session$R_ENVIRON_USER                <- Sys.getenv('R_ENVIRON_USER', unset = NA)
session$R_GZIPCMD                     <- Sys.getenv('R_GZIPCMD', unset = NA)
session$R_HISTFILE                    <- Sys.getenv('R_HISTFILE', unset = NA)
session$R_HOME                        <- Sys.getenv('R_HOME', unset = NA)
session$R_INCLUDE_DIR                 <- Sys.getenv('R_INCLUDE_DIR', unset = NA)
session$R_LIBS                        <- Sys.getenv('R_LIBS', unset = NA)
session$R_LIBS_SITE                   <- Sys.getenv('R_LIBS_SITE', unset = NA)
session$R_LIBS_USER                   <- Sys.getenv('R_LIBS_USER', unset = NA)
session$R_MAKEVARS_SITE               <- Sys.getenv('R_MAKEVARS_SITE', unset = NA)
session$R_MAKEVARS_USER               <- Sys.getenv('R_MAKEVARS_USER', unset = NA)
session$R_OSTYPE                      <- Sys.getenv('R_OSTYPE', unset = NA)
session$R_PAPERSIZE                   <- Sys.getenv('R_PAPERSIZE', unset = NA)
session$R_PAPERSIZE_USER              <- Sys.getenv('R_PAPERSIZE_USER', unset = NA)
session$R_PDFVIEWER                   <- Sys.getenv('R_PDFVIEWER', unset = NA)
session$R_PLATFORM                    <- Sys.getenv('R_PLATFORM', unset = NA)
session$R_PROFILE                     <- Sys.getenv('R_PROFILE', unset = NA)
session$R_RD4PDF                      <- Sys.getenv('R_RD4PDF', unset = NA)
session$R_SESSION_TIME_LIMIT_CPU      <- Sys.getenv('R_SESSION_TIME_LIMIT_CPU', unset = NA)
session$R_SESSION_TIME_LIMIT_ELAPSED  <- Sys.getenv('R_SESSION_TIME_LIMIT_ELAPSED', unset = NA)
session$R_SHARE_DIR                   <- Sys.getenv('R_SHARE_DIR', unset = NA)
session$R_TESTS                       <- Sys.getenv('R_TESTS', unset = NA)
session$R_TEXI2DVICMD                 <- Sys.getenv('R_TEXI2DVICMD', unset = NA)
session$R_TRANSLATIONS                <- Sys.getenv('R_TRANSLATIONS', unset = NA)
session$R_UNZIPCMD                    <- Sys.getenv('R_UNZIPCMD', unset = NA)
session$R_USER                        <- Sys.getenv('R_USER', unset = NA)
session$R_ZIPCMD                      <- Sys.getenv('R_ZIPCMD', unset = NA)
session$SED                           <- Sys.getenv('SED', unset = NA)






# Check existence of command line switches that start up cares about
#   --no-environ
#   --no-site-file
#   --no-init-file
#   --no-restore-data
#   --no-restore-history
#
#   --no-restore
#       --no-restore-data --no-restore-history
#
#   --vanilla
#       --no-save --no-restore --no-site-file
#       --no-init-file --no-environ
#
session$switches <- list(
  no.environ = any(c('--no-environ', '--vanilla') %in% session$commandArgs),
  no.site.file = any(c('--no-site-file', '--vanilla') %in% session$commandArgs),
  no.init.file = any(c('--no-init-file', '--vanilla') %in% session$commandArgs),
  no.restore.data = any(c('--no-restore-data', '--no-restore', '--vanilla') %in% session$commandArgs),
  no.restore.history = any(c('--no-restore-history', '--no-restore', '--vanilla') %in% session$commandArgs)
)



# Get the current directory at startup
session$pwd <- getwd()



# Collect paths of files to load
session$files <- list(
  R_ENVIRON.config = NA,
  R_ENVIRON.site = NA,
  R_ENVIRON.user = NA,
  R_PROFILE.site = NA,
  R_PROFILE.user = NA,
  R_PROFILE.base = NA,
  R_DATA = NA
)



# R_ENVIRON installation path
#   Always $R_HOME/etc/Renviron
session$files[[1]] <- paste(session$R_HOME, '/etc/Renviron', sep = '')



# R_ENVIRON site path
#   If no.environ, skip
#   If R_ENVIRON is set, use that
#   Otherwise, use $R_HOME/etc/Renviron.site
if (!session$switches$no.environ)
  session$files[[2]] <- ifelse(
    !is.na(session$R_ENVIRON),
    session$R_ENVIRON,
    paste(session$R_HOME, '/etc/Renviron.site', sep = '')
  )



# R_ENVIRON user path
#   If no.environ, skip
#   If $PWD/.Renviron exists, use that
#   If $R_USER/.Renviron exists, use that
if (!session$switches$no.environ)
  session$files[[3]] <- ifelse(
    file.exists(paste(session$pwd, '/.Renviron', sep = '')),
    paste(session$pwd, '/.Renviron', sep = ''),
    paste(session$R_USER, '/.Renviron', sep = '')
  )



# R_PROFILE site path
#   If no.site.file, skip
#   If R_PROFILE is set, use that
#   Otherwise, use $R_HOME/etc/Rprofile.site
if (!session$switches$no.site.file)
  session$files[[4]] <- ifelse(
    !is.na(session$R_PROFILE),
    session$R_PROFILE,
    paste(session$R_HOME, '/etc/Rprofile.site', sep = '')
  )



# R_RPOFILE user path
#   If no.init.file, skip
#   If $PWD/.Rprofile exists, use that
#   If $R_USER/.Rprofile exists, use that
if (!session$switches$no.init.file)
  session$files[[5]] <- ifelse(
    file.exists(paste(session$pwd, '/.Rprofile', sep = '')),
    paste(session$pwd, '/.Rprofile', sep = ''),
    paste(session$R_USER, '/.Rprofile', sep = '')
  )



# .RData path
#   If no.restore.data, skip
#   If $PWD/.RData exists, use that
#   If $R_USER/.RData exists, use that
if (!session$switches$no.restore.data)
  session$files[[6]] <- ifelse(
    file.exists(paste(session$pwd, '/.RData', sep = '')),
    paste(session$pwd, '/.RData', sep = ''),
    paste(session$R_USER, '/.RData', sep = '')
  )



# If files exist, copy their contents
#   use base::load(RData, env) to load RData file to environment



# [April 8, 2014 ALR]
.libPaths()



