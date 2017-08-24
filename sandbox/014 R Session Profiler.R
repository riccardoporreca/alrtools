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
session$R_ENVIRON       <- Sys.getenv('R_ENVIRON', unset = NA)
session$R_HOME          <- Sys.getenv('R_HOME', unset = NA)
session$R_HISTFILE      <- Sys.getenv('R_HISTFILE', unset = NA)
session$R_ENVIRON_USER  <- Sys.getenv('R_ENVIRON_USER', unset = NA)
session$R_USER          <- Sys.getenv('R_USER', unset = NA)
session$R_ARCH          <- Sys.getenv('R_ARCH', unset = NA)
session$R_PROFILE       <- Sys.getenv('R_PROFILE', unset = NA)



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



