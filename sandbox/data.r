data <- function (
          ..., 
          list = character(0L), 
          package = NULL, 
          lib.loc = NULL, 
          verbose = getOption("verbose"), 
          envir = .GlobalEnv) {

  fileExt <- function(x) {
    # only stuff like '.a.bz2' matches
    db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
    
    ans <- sub(".*\\.", "", x)
    ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
      x[db])
    ans
  }

  
  names <- c(as.character(substitute(list(...))[-1L]), list)

  if (!is.null(package)) {
      if (!is.character(package)) 
          stop("'package' must be a character string or NULL")
      if (any(package %in% "base")) 
          warning("datasets have been moved from package 'base' to package 'datasets'")
      if (any(package %in% "stats")) 
          warning("datasets have been moved from package 'stats' to package 'datasets'")
      package[package %in% c("base", "stats")] <- "datasets"
  }

  paths <- .find.package(package, lib.loc, verbose = verbose)

  if (is.null(lib.loc)) 
      paths <- c(.path.package(package, TRUE), if (!length(package)) getwd(), 
          paths)
  paths <- unique(paths[file.exists(paths)])
  paths <- paths[file_test("-d", file.path(paths, "data"))]
  dataExts <- tools:::.make_file_exts("data")
  if (length(names) == 0L) {
      db <- matrix(character(0L), nrow = 0L, ncol = 4L)
      for (path in paths) {
          entries <- NULL
          packageName <- if (file_test("-f", file.path(path, 
              "DESCRIPTION"))) 
              basename(path)
          else "."
          if (file_test("-f", INDEX <- file.path(path, "Meta", 
              "data.rds"))) {
              entries <- .readRDS(INDEX)
          }
          else {
              dataDir <- file.path(path, "data")
              entries <- tools::list_files_with_type(dataDir, 
                "data")
              if (length(entries)) {
                entries <- unique(tools::file_path_sans_ext(basename(entries)))
                entries <- cbind(entries, "")
              }
          }
          if (NROW(entries)) {
              if (is.matrix(entries) && ncol(entries) == 2L) 
                db <- rbind(db, cbind(packageName, dirname(path), 
                  entries))
              else warning(gettextf("data index for package '%s' is invalid and will be ignored", 
                packageName), domain = NA, call. = FALSE)
          }
      }
      colnames(db) <- c("Package", "LibPath", "Item", "Title")
      footer <- if (missing(package)) 
          paste("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
              "\n", "to list the data sets in all *available* packages.", 
              sep = "")
      else NULL
      y <- list(title = "Data sets", header = NULL, results = db, 
          footer = footer)
      class(y) <- "packageIQR"
      return(y)
  }
  paths <- file.path(paths, "data")
  for (name in names) {
      found <- FALSE
      for (p in paths) {
          if (file_test("-f", file.path(p, "Rdata.rds"))) {
              rds <- .readRDS(file.path(p, "Rdata.rds"))
              if (name %in% names(rds)) {
                found <- TRUE
                if (verbose) 
                  message(sprintf("name=%s:\t found in Rdata.rdb", 
                    name), domain = NA)
                thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
                thispkg <- sub("_.*$", "", thispkg)
                thispkg <- paste("package:", thispkg, sep = "")
                objs <- rds[[name]]
                lazyLoad(file.path(p, "Rdata"), envir = envir, 
                  filter = function(x) x %in% objs)
                break
              }
          }
          if (file_test("-f", file.path(p, "Rdata.zip"))) {
              if (file_test("-f", fp <- file.path(p, "filelist"))) 
                files <- file.path(p, scan(fp, what = "", quiet = TRUE))
              else {
                warning(gettextf("file 'filelist' is missing for directory '%s'", 
                  p), domain = NA)
                next
              }
          }
          else {
              files <- list.files(p, full.names = TRUE)
          }
          files <- files[grep(name, files, fixed = TRUE)]
          if (length(files) > 1L) {
              o <- match(fileExt(files), dataExts, nomatch = 100L)
              paths0 <- dirname(files)
              paths0 <- factor(paths0, levels = unique(paths0))
              files <- files[order(paths0, o)]
          }
          if (length(files)) {
              for (file in files) {
                if (verbose) 
                  message("name=", name, ":\t file= ...", .Platform$file.sep, 
                    basename(file), "::\t", appendLF = FALSE, 
                    domain = NA)
                ext <- fileExt(file)
                if (basename(file) != paste(name, ".", ext, 
                  sep = "")) 
                  found <- FALSE
                else {
                  found <- TRUE
                  Rdatadir <- file.path(tempdir(), "Rdata")
                  dir.create(Rdatadir, showWarnings = FALSE)
                  zfile <- zip.file.extract(file, "Rdata.zip", 
                    dir = Rdatadir)
                  if (zfile != file) 
                    on.exit(unlink(zfile))
                  switch(ext, R = , r = {
                    library("utils")
                    sys.source(zfile, chdir = TRUE, envir = envir)
                  }, RData = , rdata = , rda = load(zfile, 
                    envir = envir), TXT = , txt = , tab = , 
                    tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
                    txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
                      header = TRUE, as.is = FALSE), envir = envir), 
                    CSV = , csv = , csv.gz = , csv.bz2 = , 
                    csv.xz = assign(name, read.table(zfile, 
                      header = TRUE, sep = ";", as.is = FALSE), 
                      envir = envir), found <- FALSE)
                }
                if (found) 
                  break
              }
              if (verbose) 
                message(if (!found) 
                  "*NOT* ", "found", domain = NA)
          }
          if (found) 
              break
      }
      if (!found) 
          warning(gettextf("data set '%s' not found", name), 
              domain = NA)
  }
  invisible(names)
}
