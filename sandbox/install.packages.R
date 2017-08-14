# Author: Adam L. Rich
# Date:   November 26, 2012
# Description:
# 
#   Annotation of the install.packages function
#   
# 

install.packages <- function (
    pkgs, 
    lib, 
    repos = getOption("repos"), 
    contriburl = contrib.url(repos, type), 
    method, 
    available = NULL, 
    destdir = NULL, 
    dependencies = NA, 
    type = getOption("pkgType"), 
    configure.args = getOption("configure.args"), 
    configure.vars = getOption("configure.vars"), 
    clean = FALSE, 
    Ncpus = getOption("Ncpus"), 
    libs_only = FALSE, 
    INSTALL_opts, 
    ...
) {

    # By default, clean is FALSE
    if (is.logical(clean) && clean) 
        clean <- "--clean"
        
    # By default, is set to 
    #
    #   c("Depends", "Imports", "LinkingTo")
    #
    if (is.logical(dependencies) && is.na(dependencies)) 
        dependencies <- if (!missing(lib) && length(lib) > 1L) 
            FALSE
        else c("Depends", "Imports", "LinkingTo")
    
    # Empty character vector returned on Windows
    getConfigureArgs <- function(pkg) {
        if (.Platform$OS.type == "windows") 
            return(character())
        pkg <- gsub("_\\.(zip|tar\\.gz)", "", gsub(.standard_regexps()$valid_package_version, 
            "", basename(pkg)))
        if (length(pkgs) == 1L && length(configure.args) && length(names(configure.args)) == 
            0L) 
            return(paste("--configure-args=", shQuote(paste(configure.args, 
                collapse = " ")), sep = ""))
        if (length(configure.args) && length(names(configure.args)) && 
            pkg %in% names(configure.args)) 
            config <- paste("--configure-args=", shQuote(paste(configure.args[[pkg]], 
                collapse = " ")), sep = "")
        else config <- character()
        config
    }
    
    # Empty character vector returned on Windows
    getConfigureVars <- function(pkg) {
        if (.Platform$OS.type == "windows") 
            return(character())
        pkg <- gsub("_\\.(zip|tar\\.gz)", "", gsub(.standard_regexps()$valid_package_version, 
            "", basename(pkg)))
        if (length(pkgs) == 1L && length(configure.vars) && length(names(configure.vars)) == 
            0L) 
            return(paste("--configure-vars=", shQuote(paste(configure.vars, 
                collapse = " ")), sep = ""))
        if (length(configure.vars) && length(names(configure.vars)) && 
            pkg %in% names(configure.vars)) 
            config <- paste("--configure-vars=", shQuote(paste(configure.vars[[pkg]], 
                collapse = " ")), sep = "")
        else config <- character()
        config
    }
    
    # If no packages are specified, show a selection box
    if (missing(pkgs) || !length(pkgs)) {
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA" || (capabilities("tcltk") && capabilities("X11") && 
            suppressWarnings(tcltk:::.TkUp))) {
        }
        else stop("no packages were specified")
        if (is.null(available)) 
            available <- available.packages(contriburl = contriburl, 
                method = method)
        if (NROW(available)) {
            pkgs <- select.list(sort(unique(rownames(available))), 
                multiple = TRUE, title = "Packages", graphics = TRUE)
        }
        if (!length(pkgs)) 
            stop("no packages were specified")
    }
    

    # Find a place to install the packages
    if (missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1L]
        if (length(.libPaths()) > 1L) 
            message(gettextf("Installing package(s) into %s\n(as %s is unspecified)", 
                sQuote(lib), sQuote("lib")), domain = NA)
    }
    ok <- file.info(lib)$isdir & (file.access(lib, 2) == 0)
    if (length(lib) > 1 && any(!ok)) 
        stop(sprintf(ngettext(sum(!ok), "'lib' element '%s' is not a writable directory", 
            "'lib' elements '%s' are not writable directories"), 
            paste(lib[!ok], collapse = ", ")), domain = NA)
    if (length(lib) == 1L && .Platform$OS.type == "windows") {
        ok <- file.info(lib)$isdir %in% TRUE
        if (ok) {
            fn <- file.path(lib, paste("_test_dir", Sys.getpid(), 
                sep = "_"))
            unlink(fn, recursive = TRUE)
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) 
                ok <- FALSE
            else unlink(fn, recursive = TRUE)
        }
    }
    if (length(lib) == 1L && !ok) {
        warning(gettextf("'lib = \"%s\"' is not writable", lib), 
            domain = NA, immediate. = TRUE)
        userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), 
            .Platform$path.sep))[1L]
        if (interactive() && !file.exists(userdir)) {
            msg <- gettext("Would you like to create a personal library\n'%s'\nto install packages into?")
            if (.Platform$OS.type == "windows") {
                ans <- winDialog("yesno", sprintf(msg, userdir))
                if (ans != "YES") 
                  stop("unable to install packages")
            }
            else {
                ans <- readline(paste(sprintf(msg, userdir), 
                  " (y/n) "))
                if (substr(ans, 1L, 1L) == "n") 
                  stop("unable to install packages")
            }
            if (!dir.create(userdir, recursive = TRUE)) 
                stop("unable to create ", sQuote(userdir))
            lib <- userdir
            .libPaths(c(userdir, .libPaths()))
        }
        else stop("unable to install packages")
    }
    
    
    if (length(pkgs) == 1L && missing(repos) && missing(contriburl)) {
        if ((type == "source" && length(grep("\\.tar.gz$", pkgs))) || 
            (type %in% c("win.binary", "win64.binary") && length(grep("\\.zip$", 
                pkgs))) || (substr(type, 1L, 10L) == "mac.binary" && 
            length(grep("\\.tgz$", pkgs)))) {
            repos <- NULL
            message("inferring 'repos = NULL' from the file name")
        }
    }
    if (.Platform$OS.type == "windows") {
        if (type == "mac.binary") 
            stop("cannot install MacOS X binary packages on Windows")
        if (type %in% c("win.binary", "win64.binary")) {
            .install.winbinary(pkgs = pkgs, lib = lib, contriburl = contriburl, 
                method = method, available = available, destdir = destdir, 
                dependencies = dependencies, libs_only = libs_only, 
                ...)
            return(invisible())
        }
        have_spaces <- grep(" ", pkgs)
        if (length(have_spaces)) {
            p <- pkgs[have_spaces]
            dirs <- shortPathName(dirname(p))
            pkgs[have_spaces] <- file.path(dirs, basename(p))
        }
        pkgs <- gsub("\\\\", "/", pkgs)
    }
    else {
        if (substr(type, 1L, 10L) == "mac.binary") {
            if (!length(grep("darwin", R.version$platform))) 
                stop("cannot install MacOS X binary packages on this plaform")
            .install.macbinary(pkgs = pkgs, lib = lib, contriburl = contriburl, 
                method = method, available = available, destdir = destdir, 
                dependencies = dependencies, ...)
            return(invisible())
        }
        if (type %in% c("win.binary", "win64.binary")) 
            stop("cannot install Windows binary packages on this plaform")
        if (!file.exists(file.path(R.home("bin"), "INSTALL"))) 
            stop("This version of R is not set up to install source packages\nIf it was installed from an RPM, you may need the R-devel RPM")
    }
    
    
    libpath <- .libPaths()
    libpath <- libpath[!libpath %in% .Library]
    if (length(libpath)) 
        libpath <- paste(libpath, collapse = .Platform$path.sep)
    cmd0 <- paste(file.path(R.home("bin"), "R"), "CMD INSTALL")
    if (length(libpath)) 
        if (.Platform$OS.type == "windows") {
            oldrlibs <- Sys.getenv("R_LIBS")
            Sys.setenv(R_LIBS = libpath)
            on.exit(Sys.setenv(R_LIBS = oldrlibs))
        }
        else cmd0 <- paste(paste("R_LIBS", shQuote(libpath), 
            sep = "="), cmd0)
    if (is.character(clean)) 
        cmd0 <- paste(cmd0, clean)
    if (libs_only) 
        cmd0 <- paste(cmd0, "--libs-only")
    if (!missing(INSTALL_opts)) 
        cmd0 <- paste(cmd0, paste(INSTALL_opts, collapse = " "))
    if (is.null(repos) & missing(contriburl)) {
        update <- cbind(path.expand(pkgs), lib)
        for (i in seq_len(nrow(update))) {
            cmd <- paste(cmd0, "-l", shQuote(update[i, 2L]), 
                getConfigureArgs(update[i, 1L]), getConfigureVars(update[i, 
                  1L]), shQuote(update[i, 1L]))
            if (system(cmd) > 0L) 
                warning(gettextf("installation of package '%s' had non-zero exit status", 
                  update[i, 1L]), domain = NA)
        }
        return(invisible())
    }
    tmpd <- destdir
    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if (is.null(destdir) && nonlocalcran) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd)) 
            stop(gettextf("unable to create temporary directory '%s'", 
                tmpd), domain = NA)
    }
    if (is.null(available)) 
        available <- available.packages(contriburl = contriburl, 
            method = method)
    pkgs <- getDependencies(pkgs, dependencies, available, lib)
    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available, 
        contriburl = contriburl, method = method, type = "source", 
        ...)
    if (length(foundpkgs)) {
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        found <- pkgs %in% foundpkgs[, 1L]
        files <- foundpkgs[match(pkgs[found], foundpkgs[, 1L]), 
            2L]
        update <- cbind(update[found, , drop = FALSE], file = files)
        if (nrow(update) > 1L) {
            upkgs <- unique(pkgs <- update[, 1L])
            DL <- .make_dependency_list(upkgs, available)
            p0 <- .find_install_order(upkgs, DL)
            update <- update[sort.list(match(pkgs, p0)), ]
        }
        if (is.null(Ncpus)) 
            Ncpus <- 1L
        if (Ncpus > 1L && nrow(update) > 1L) {
            cmd0 <- paste(cmd0, "--pkglock")
            tmpd <- file.path(tempdir(), "make_packages")
            if (!file.exists(tmpd) && !dir.create(tmpd)) 
                stop(gettextf("unable to create temporary directory '%s'", 
                  tmpd), domain = NA)
            mfile <- file.path(tmpd, "Makefile")
            conn <- file(mfile, "wt")
            cat("all: ", paste(paste(update[, 1L], ".ts", sep = ""), 
                collapse = " "), "\n", sep = "", file = conn)
            for (i in seq_len(nrow(update))) {
                pkg <- update[i, 1L]
                cmd <- paste(cmd0, "-l", shQuote(update[i, 2L]), 
                  getConfigureArgs(update[i, 3L]), getConfigureVars(update[i, 
                    3L]), update[i, 3L], ">", paste(pkg, ".out", 
                    sep = ""), "2>&1")
                deps <- DL[[pkg]]
                deps <- deps[deps %in% pkgs]
                deps <- if (length(deps)) 
                  paste(paste(deps, ".ts", sep = ""), collapse = " ")
                else ""
                cat(paste(pkg, ".ts: ", deps, sep = ""), paste("\t@echo installing package", 
                  pkg), paste("\t@", cmd, " && touch ", pkg, 
                  ".ts", sep = ""), paste("\t@cat ", pkg, ".out", 
                  sep = ""), "", sep = "\n", file = conn)
            }
            close(conn)
            cwd <- setwd(tmpd)
            on.exit(setwd(cwd))
            make <- Sys.getenv("MAKE")
            if (!nzchar(make)) 
                make <- "make"
            status <- system(paste(make, "-k -j", Ncpus))
            if (status > 0L) {
                pkgs <- update[, 1L]
                tss <- sub("\\.ts$", "", dir(".", pattern = "\\.ts$"))
                failed <- pkgs[!pkgs %in% tss]
                for (pkg in failed) system(paste("cat ", pkg, 
                  ".out", sep = ""))
                warning(gettextf("installation of one of more packages failed,\n  probably %s", 
                  paste(sQuote(failed), collapse = ", ")), domain = NA)
            }
            setwd(cwd)
            on.exit()
            unlink(tmpd, recursive = TRUE)
        }
        else {
            for (i in seq_len(nrow(update))) {
                cmd <- paste(cmd0, "-l", shQuote(update[i, 2L]), 
                  getConfigureArgs(update[i, 3L]), getConfigureVars(update[i, 
                    3L]), update[i, 3L])
                status <- system(cmd)
                if (status > 0L) 
                  warning(gettextf("installation of package '%s' had non-zero exit status", 
                    update[i, 1L]), domain = NA)
            }
        }
        if (!is.null(tmpd) && is.null(destdir)) 
            cat("\n", gettextf("The downloaded packages are in\n\t%s", 
                sQuote(normalizePath(tmpd))), "\n", sep = "")
        libs_used <- unique(update[, 2L])
        if (.Platform$OS.type == "unix" && .Library %in% libs_used) 
            link.html.help(verbose = TRUE)
    }
    else if (!is.null(tmpd) && is.null(destdir)) 
        unlink(tmpd, TRUE)
    invisible()
}
<environment: namespace:utils>
