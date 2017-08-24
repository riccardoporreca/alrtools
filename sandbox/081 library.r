# library()

function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
    logical.return = FALSE, warn.conflicts = TRUE, quietly = FALSE, 
    keep.source = getOption("keep.source.pkgs"), verbose = getOption("verbose")) 
{
    paste0 <- function(...) paste(..., sep = "")
    testRversion <- function(pkgInfo, pkgname, pkgpath) {
        if (is.null(built <- pkgInfo$Built)) 
            stop(gettextf("package '%s' has not been installed properly\n", 
                pkgname), call. = FALSE, domain = NA)
        R_version_built_under <- as.numeric_version(built$R)
        if (R_version_built_under < "2.10.0") 
            stop(gettextf("package '%s' was built before R 2.10.0: please re-install it", 
                pkgname), call. = FALSE, domain = NA)
        current <- getRversion()
        if (length(Rdeps <- pkgInfo$Rdepends2)) {
            for (dep in Rdeps) if (length(dep) > 1L) {
                target <- as.numeric_version(dep$version)
                res <- eval(parse(text = paste("current", dep$op, 
                  "target")))
                if (!res) 
                  stop(gettextf("This is R %s, package '%s' needs %s %s", 
                    current, pkgname, dep$op, target), call. = FALSE, 
                    domain = NA)
            }
        }
        if (R_version_built_under > current) 
            warning(gettextf("package '%s' was built under R version %s", 
                pkgname, as.character(built$R)), call. = FALSE, 
                domain = NA)
        platform <- built$Platform
        r_arch <- .Platform$r_arch
        if (.Platform$OS.type == "unix") {
            if (!nzchar(r_arch) && length(grep("\\w", platform)) && 
                !testPlatformEquivalence(platform, R.version$platform)) 
                stop(gettextf("package '%s' was built for %s", 
                  pkgname, platform), call. = FALSE, domain = NA)
        }
        else {
            if (nzchar(platform) && !grepl("mingw", platform)) 
                stop(gettextf("package '%s' was built for %s", 
                  pkgname, platform), call. = FALSE, domain = NA)
        }
        if (nzchar(r_arch) && file.exists(file.path(pkgpath, 
            "libs")) && !file.exists(file.path(pkgpath, "libs", 
            r_arch))) 
            stop(gettextf("package '%s' is not installed for 'arch=%s'", 
                pkgname, r_arch), call. = FALSE, domain = NA)
    }
    checkLicense <- function(pkg, pkgInfo, pkgPath) {
        L <- tools:::analyze_license(pkgInfo$DESCRIPTION["License"])
        if (!L$is_empty && !L$is_verified) {
            site_file <- path.expand(file.path(R.home("etc"), 
                "licensed.site"))
            if (file.exists(site_file) && pkg %in% readLines(site_file)) 
                return()
            personal_file <- path.expand("~/.R/licensed")
            if (file.exists(personal_file)) {
                agreed <- readLines(personal_file)
                if (pkg %in% agreed) 
                  return()
            }
            else agreed <- character()
            if (!interactive()) 
                stop(gettextf("package '%s' has a license that you need to accept in an interactive session", 
                  pkg), domain = NA)
            lfiles <- file.path(pkgpath, c("LICENSE", "LICENCE"))
            lfiles <- lfiles[file.exists(lfiles)]
            if (length(lfiles)) {
                message(gettextf("Package '%s' has a license that you need to accept after viewing", 
                  pkg), domain = NA)
                readline("press RETURN to view license")
                encoding <- pkgInfo$DESCRIPTION["Encoding"]
                if (is.na(encoding)) 
                  encoding <- ""
                if (encoding == "latin1") 
                  encoding <- "cp1252"
                file.show(lfiles[1L], encoding = encoding)
            }
            else {
                message(gettextf("Package '%s' has a license that you need to accept:\naccording to the DESCRIPTION file it is", 
                  pkg), domain = NA)
                message(pkgInfo$DESCRIPTION["License"])
            }
            choice <- menu(c("accept", "decline"), title = paste("License for", 
                sQuote(pkg)))
            if (choice != 1) 
                stop(gettextf("License for package '%s' not accepted", 
                  package), domain = NA, call. = FALSE)
            dir.create(dirname(personal_file), showWarnings = FALSE)
            writeLines(c(agreed, pkg), personal_file)
        }
    }
    checkNoGenerics <- function(env, pkg) {
        nenv <- env
        ns <- .Internal(getRegisteredNamespace(as.name(pkg)))
        if (!is.null(ns)) 
            nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE)) 
            TRUE
        else {
            length(objects(env, pattern = "^\\.__[MT]", all.names = TRUE)) == 
                0L
        }
    }
    checkConflicts <- function(package, pkgname, pkgpath, nogenerics, 
        env) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value", 
            ".Random.seed", ".First.lib", ".Last.lib", ".packageName", 
            ".noGenerics", ".required", ".no_S3_generics", ".Depends", 
            ".requireCachedGenerics")
        sp <- search()
        lib.pos <- match(pkgname, sp)
        ob <- objects(lib.pos, all.names = TRUE)
        if (!nogenerics) {
            these <- objects(lib.pos, all.names = TRUE)
            these <- these[substr(these, 1L, 6L) == ".__T__"]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != package]
            ob <- ob[!(ob %in% gen)]
        }
        fst <- TRUE
        ipos <- seq_along(sp)[-c(lib.pos, match(c("Autoloads", 
            "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(objects(i, all.names = TRUE), ob, 
                nomatch = 0L)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if (length(Classobjs)) 
                  same <- same[-Classobjs]
                same.isFn <- function(where) vapply(same, exists, 
                  NA, where = where, mode = "function", inherits = FALSE)
                same <- same[same.isFn(i) == same.isFn(lib.pos)]
                if (length(same)) 
                  same <- same[vapply(same, function(.) !identical(get(., 
                    i), get(., lib.pos)), NA)]
                if (length(same)) {
                  if (fst) {
                    fst <- FALSE
                    packageStartupMessage(gettextf("\nAttaching package: '%s'\n", 
                      package), domain = NA)
                  }
                  objs <- strwrap(paste(same, collapse = ", "), 
                    indent = 4, exdent = 4)
                  msg <- sprintf("The following object(s) are masked %s '%s':\n\n%s\n", 
                    if (i < lib.pos) 
                      "_by_"
                    else "from", sp[i], paste(objs, collapse = "\n"))
                  packageStartupMessage(msg)
                }
            }
        }
    }
    runUserHook <- function(pkgname, pkgpath) {
        hook <- getHook(packageEvent(pkgname, "attach"))
        for (fun in hook) try(fun(pkgname, pkgpath))
    }
    bindTranslations <- function(pkgname, pkgpath) {
        popath <- file.path(pkgpath, "po")
        if (!file.exists(popath)) 
            return()
        bindtextdomain(pkgname, popath)
        bindtextdomain(paste("R", pkgname, sep = "-"), popath)
    }
    if (verbose && quietly) 
        message("'verbose' and 'quietly' are both true; being verbose then ..")
    if (!missing(package)) {
        if (is.null(lib.loc)) 
            lib.loc <- .libPaths()
        lib.loc <- lib.loc[file.info(lib.loc)$isdir %in% TRUE]
        if (!character.only) 
            package <- as.character(substitute(package))
        if (length(package) != 1L) 
            stop("'package' must be of length 1")
        if (is.na(package) || (package == "")) 
            stop("invalid package name")
        pkgname <- paste("package", package, sep = ":")
        newpackage <- is.na(match(pkgname, search()))
        if (newpackage) {
            pkgpath <- find.package(package, lib.loc, quiet = TRUE, 
                verbose = verbose)
            if (length(pkgpath) == 0L) {
                txt <- if (length(lib.loc)) 
                  gettextf("there is no package called '%s'", 
                    package)
                else gettext("no library trees found in 'lib.loc'")
                if (logical.return) {
                  warning(txt, domain = NA)
                  return(FALSE)
                }
                else stop(txt, domain = NA)
            }
            which.lib.loc <- normalizePath(dirname(pkgpath), 
                "/", TRUE)
            pfile <- system.file("Meta", "package.rds", package = package, 
                lib.loc = which.lib.loc)
            if (!nzchar(pfile)) 
                stop(gettextf("'%s' is not a valid installed package", 
                  package), domain = NA)
            pkgInfo <- readRDS(pfile)
            testRversion(pkgInfo, package, pkgpath)
            if (!package %in% c("datasets", "grDevices", "graphics", 
                "methods", "splines", "stats", "stats4", "tcltk", 
                "tools", "utils") && isTRUE(getOption("checkPackageLicense", 
                FALSE))) 
                checkLicense(package, pkgInfo, pkgpath)
            if (is.character(pos)) {
                npos <- match(pos, search())
                if (is.na(npos)) {
                  warning(gettextf("'%s' not found on search path, using pos = 2", 
                    pos), domain = NA)
                  pos <- 2
                }
                else pos <- npos
            }
            .getRequiredPackages2(pkgInfo, quietly = quietly)
            deps <- unique(names(pkgInfo$Depends))
            if (packageHasNamespace(package, which.lib.loc)) {
                tt <- try({
                  ns <- loadNamespace(package, c(which.lib.loc, 
                    lib.loc), keep.source = keep.source)
                  dataPath <- file.path(which.lib.loc, package, 
                    "data")
                  env <- attachNamespace(ns, pos = pos, dataPath = dataPath, 
                    deps)
                })
                if (inherits(tt, "try-error")) 
                  if (logical.return) 
                    return(FALSE)
                  else stop(gettextf("package/namespace load failed for '%s'", 
                    package), call. = FALSE, domain = NA)
                else {
                  on.exit(detach(pos = pos))
                  nogenerics <- !.isMethodsDispatchOn() || checkNoGenerics(env, 
                    package)
                  if (warn.conflicts && !exists(".conflicts.OK", 
                    envir = env, inherits = FALSE)) 
                    checkConflicts(package, pkgname, pkgpath, 
                      nogenerics, ns)
                  runUserHook(package, pkgpath)
                  on.exit()
                  if (logical.return) 
                    return(TRUE)
                  else return(invisible(.packages()))
                }
            }
            codeFile <- file.path(which.lib.loc, package, "R", 
                package)
            loadenv <- new.env(hash = TRUE, parent = .GlobalEnv)
            assign(".packageName", package, envir = loadenv)
            if (length(deps)) 
                assign(".Depends", deps, envir = loadenv)
            if (file.exists(codeFile)) {
                res <- try(sys.source(codeFile, loadenv, keep.source = keep.source))
                if (inherits(res, "try-error")) 
                  stop(gettextf("unable to load R code in package '%s'", 
                    package), call. = FALSE, domain = NA)
            }
            else if (verbose) 
                warning(gettextf("package '%s' contains no R code", 
                  package), domain = NA)
            dbbase <- file.path(which.lib.loc, package, "data", 
                "Rdata")
            if (file.exists(paste0(dbbase, ".rdb"))) 
                lazyLoad(dbbase, loadenv)
            dbbase <- file.path(which.lib.loc, package, "R", 
                "sysdata")
            if (file.exists(paste0(dbbase, ".rdb"))) 
                lazyLoad(dbbase, loadenv)
            env <- attach(NULL, pos = pos, name = pkgname)
            on.exit(do.call("detach", list(name = pkgname)))
            attr(env, "path") <- file.path(which.lib.loc, package)
            .Internal(lib.fixup(loadenv, env))
            bindTranslations(package, pkgpath)
            if (exists(".First.lib", mode = "function", envir = env, 
                inherits = FALSE)) {
                firstlib <- get(".First.lib", mode = "function", 
                  envir = env, inherits = FALSE)
                tt <- try(firstlib(which.lib.loc, package))
                if (inherits(tt, "try-error")) 
                  if (logical.return) 
                    return(FALSE)
                  else stop(gettextf(".First.lib failed for '%s'", 
                    package), domain = NA)
            }
            if (!is.null(firstlib <- getOption(".First.lib")[[package]])) {
                tt <- try(firstlib(which.lib.loc, package))
                if (inherits(tt, "try-error")) 
                  if (logical.return) 
                    return(FALSE)
                  else stop(gettextf(".First.lib failed for '%s'", 
                    package), domain = NA)
            }
            if (.isMethodsDispatchOn()) {
                nogenerics <- checkNoGenerics(env, package)
                doCache <- !nogenerics || methods:::.hasS4MetaData(env)
            }
            else {
                nogenerics <- TRUE
                doCache <- FALSE
            }
            if (warn.conflicts && !exists(".conflicts.OK", envir = env, 
                inherits = FALSE)) 
                checkConflicts(package, pkgname, pkgpath, nogenerics, 
                  env)
            if (doCache) 
                methods::cacheMetaData(env, TRUE, searchWhere = .GlobalEnv)
            runUserHook(package, pkgpath)
            on.exit()
        }
        if (verbose && !newpackage) 
            warning(gettextf("package '%s' already present in search()", 
                package), domain = NA)
    }
    else if (!missing(help)) {
        if (!character.only) 
            help <- as.character(substitute(help))
        pkgName <- help[1L]
        pkgPath <- find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"), 
            file.path(pkgPath, "INDEX"))
        if (file.exists(vignetteIndexRDS <- file.path(pkgPath, 
            "Meta", "vignette.rds"))) 
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector("list", 3L)
        readDocFile <- function(f) {
            if (basename(f) %in% "package.rds") {
                txt <- readRDS(f)$DESCRIPTION
                if ("Encoding" %in% names(txt)) {
                  to <- if (Sys.getlocale("LC_CTYPE") == "C") 
                    "ASCII//TRANSLIT"
                  else ""
                  tmp <- try(iconv(txt, from = txt["Encoding"], 
                    to = to))
                  if (!inherits(tmp, "try-error")) 
                    txt <- tmp
                  else warning("'DESCRIPTION' has 'Encoding' field and re-encoding is not possible", 
                    call. = FALSE)
                }
                nm <- paste0(names(txt), ":")
                formatDL(nm, txt, indent = max(nchar(nm, "w")) + 
                  3)
            }
            else if (basename(f) %in% "vignette.rds") {
                txt <- readRDS(f)
                if (is.data.frame(txt) && nrow(txt)) 
                  cbind(basename(gsub("\\.[[:alpha:]]+$", "", 
                    txt$File)), paste(txt$Title, paste0(rep.int("(source", 
                    NROW(txt)), ifelse(txt$PDF != "", ", pdf", 
                    ""), ")")))
                else NULL
            }
            else readLines(f)
        }
        for (i in which(file.exists(docFiles))) pkgInfo[[i]] <- readDocFile(docFiles[i])
        y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
        class(y) <- "packageInfo"
        return(y)
    }
    else {
        if (is.null(lib.loc)) 
            lib.loc <- .libPaths()
        db <- matrix(character(), nrow = 0L, ncol = 3L)
        nopkgs <- character()
        for (lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for (i in sort(a)) {
                file <- system.file("Meta", "package.rds", package = i, 
                  lib.loc = lib)
                title <- if (file != "") {
                  txt <- readRDS(file)
                  if (is.list(txt)) 
                    txt <- txt$DESCRIPTION
                  if ("Encoding" %in% names(txt)) {
                    to <- if (Sys.getlocale("LC_CTYPE") == "C") 
                      "ASCII//TRANSLIT"
                    else ""
                    tmp <- try(iconv(txt, txt["Encoding"], to, 
                      "?"))
                    if (!inherits(tmp, "try-error")) 
                      txt <- tmp
                    else warning("'DESCRIPTION' has 'Encoding' field and re-encoding is not possible", 
                      call. = FALSE)
                  }
                  txt["Title"]
                }
                else NA
                if (is.na(title)) 
                  title <- " ** No title available ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if (length(a) == 0L) 
                nopkgs <- c(nopkgs, lib)
        }
        dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
        if (length(nopkgs) && !missing(lib.loc)) {
            pkglist <- paste(sQuote(nopkgs), collapse = ", ")
            msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages", 
                "libraries %s contain no packages"), pkglist)
            warning(msg, domain = NA)
        }
        y <- list(header = NULL, results = db, footer = NULL)
        class(y) <- "libraryIQR"
        return(y)
    }
    if (logical.return) 
        TRUE
    else invisible(.packages())
}
