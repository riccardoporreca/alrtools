texi2dvi <- function (file, pdf = FALSE, clean = FALSE, quiet = TRUE, texi2dvi = getOption("texi2dvi"), 
    texinputs = NULL, index = TRUE) 
{
    do_cleanup <- function(clean) if (clean) {
        out_file <- paste(basename(file_path_sans_ext(file)), 
            if (pdf) 
                "pdf"
            else "dvi", sep = ".")
        files <- list.files(all.files = TRUE) %w/o% c(".", "..", 
            out_file)
        file.remove(files[file_test("-nt", files, ".timestamp")])
    }
    if (is.null(texi2dvi) || !nzchar(texi2dvi)) 
        texi2dvi <- Sys.which("texi2dvi")
    envSep <- .Platform$path.sep
    texinputs0 <- texinputs
    Rtexmf <- file.path(R.home("share"), "texmf")
    Rtexinputs <- file.path(Rtexmf, "tex", "latex")
    texinputs <- paste(c(texinputs0, Rtexinputs, ""), collapse = envSep)
    if (.Platform$OS.type == "windows") 
        texinputs <- gsub("\\", "/", texinputs, fixed = TRUE)
    Rbibinputs <- file.path(Rtexmf, "bibtex", "bib")
    bibinputs <- paste(c(texinputs0, Rbibinputs, ""), collapse = envSep)
    Rbstinputs <- file.path(Rtexmf, "bibtex", "bst")
    bstinputs <- paste(c(texinputs0, Rbstinputs, ""), collapse = envSep)
    otexinputs <- Sys.getenv("TEXINPUTS", unset = NA)
    if (is.na(otexinputs)) {
        on.exit(Sys.unsetenv("TEXINPUTS"))
        otexinputs <- "."
    }
    else on.exit(Sys.setenv(TEXINPUTS = otexinputs))
    Sys.setenv(TEXINPUTS = paste(otexinputs, texinputs, sep = envSep))
    obibinputs <- Sys.getenv("BIBINPUTS", unset = NA)
    if (is.na(obibinputs)) {
        on.exit(Sys.unsetenv("BIBINPUTS"), add = TRUE)
        obibinputs <- "."
    }
    else on.exit(Sys.setenv(BIBINPUTS = obibinputs, add = TRUE))
    Sys.setenv(BIBINPUTS = paste(obibinputs, bibinputs, sep = envSep))
    obstinputs <- Sys.getenv("BSTINPUTS", unset = NA)
    if (is.na(obstinputs)) {
        on.exit(Sys.unsetenv("BSTINPUTS"), add = TRUE)
        obstinputs <- "."
    }
    else on.exit(Sys.setenv(BSTINPUTS = obstinputs), add = TRUE)
    Sys.setenv(BSTINPUTS = paste(obstinputs, bstinputs, sep = envSep))
    if (clean) {
        file.create(".timestamp")
        on.exit(file.remove(".timestamp"))
        Sys.sleep(0.1)
    }
    if (index && nzchar(texi2dvi) && .Platform$OS.type != "windows") {
        Sys.setenv(TEXINDY = "false")
        on.exit(Sys.unsetenv("TEXINDY"), add = TRUE)
        opt_pdf <- if (pdf) 
            "--pdf"
        else ""
        opt_quiet <- if (quiet) 
            "--quiet"
        else ""
        opt_extra <- ""
        out <- .shell_with_capture(paste(shQuote(texi2dvi), "--help"))
        if (length(grep("--no-line-error", out$stdout))) 
            opt_extra <- "--no-line-error"
        out <- .shell_with_capture(paste(shQuote(texi2dvi), opt_pdf, 
            opt_quiet, opt_extra, shQuote(file)))
        errors <- character()
        log <- paste(file_path_sans_ext(file), "log", sep = ".")
        if (file_test("-f", log)) {
            lines <- .get_LaTeX_errors_from_log_file(log)
            if (length(lines)) 
                errors <- paste("LaTeX errors:", paste(lines, 
                  collapse = "\n"), sep = "\n")
        }
        log <- paste(file_path_sans_ext(file), "blg", sep = ".")
        if (file_test("-f", log)) {
            lines <- .get_BibTeX_errors_from_blg_file(log)
            if (length(lines)) 
                errors <- paste("BibTeX errors:", paste(lines, 
                  collapse = "\n"), sep = "\n")
        }
        msg <- ""
        if (out$status) {
            msg <- gettextf("Running 'texi2dvi' on '%s' failed.", 
                file)
            if (length(errors)) 
                msg <- paste(msg, errors, sep = "\n")
            else if (length(out$stderr)) 
                msg <- paste(msg, "Messages:", paste(out$stderr, 
                  collapse = "\n"), sep = "\n")
            if (!quiet) 
                msg <- paste(msg, "Output:", paste(out$stdout, 
                  collapse = "\n"), sep = "\n")
        }
        do_cleanup(clean)
        if (nzchar(msg)) 
            stop(msg, domain = NA)
        else if (!quiet) 
            message(paste(paste(out$stderr, collapse = "\n"), 
                paste(out$stdout, collapse = "\n"), sep = "\n"))
    }
    else if (index && nzchar(texi2dvi)) {
        extra <- ""
        ver <- system(paste(shQuote(texi2dvi), "--version"), 
            intern = TRUE)
        if (length(grep("MiKTeX", ver[1L]))) {
            texinputs <- c(texinputs0, Rtexinputs, Rbstinputs)
            texinputs <- gsub("\\", "/", texinputs, fixed = TRUE)
            paths <- paste("-I", shQuote(texinputs))
            extra <- paste(extra, paste(paths, collapse = " "))
        }
        base <- basename(file_path_sans_ext(file))
        system(paste(shQuote(texi2dvi), if (quiet) 
            "--quiet"
        else "", if (pdf) 
            "--pdf"
        else "", shQuote(file), extra), intern = TRUE, ignore.stderr = TRUE)
        msg <- ""
        logfile <- paste(base, "log", sep = ".")
        if (file_test("-f", logfile)) {
            lines <- .get_LaTeX_errors_from_log_file(logfile)
            if (length(lines)) 
                msg <- paste(msg, "LaTeX errors:", paste(lines, 
                  collapse = "\n"), sep = "\n")
        }
        logfile <- paste(base, "blg", sep = ".")
        if (file_test("-f", logfile)) {
            lines <- .get_BibTeX_errors_from_blg_file(logfile)
            if (length(lines)) 
                msg <- paste(msg, "BibTeX errors:", paste(lines, 
                  collapse = "\n"), sep = "\n")
        }
        do_cleanup(clean)
        if (nzchar(msg)) {
            msg <- paste(gettextf("running 'texi2dvi' on '%s' failed", 
                file), msg, "", sep = "\n")
            stop(msg, call. = FALSE, domain = NA)
        }
    }
    else {
        texfile <- shQuote(file)
        base <- basename(file_path_sans_ext(file))
        idxfile <- paste(base, ".idx", sep = "")
        latex <- if (pdf) 
            Sys.getenv("PDFLATEX", "pdflatex")
        else Sys.getenv("LATEX", "latex")
        bibtex <- Sys.getenv("BIBTEX", "bibtex")
        makeindex <- Sys.getenv("MAKEINDEX", "makeindex")
        if (system(paste(shQuote(latex), "-interaction=nonstopmode", 
            texfile))) 
            stop(gettextf("unable to run '%s' on '%s'", latex, 
                file), domain = NA)
        nmiss <- length(grep("^LaTeX Warning:.*Citation.*undefined", 
            readLines(paste(base, ".log", sep = ""))))
        for (iter in 1L:10L) {
            if (nmiss) 
                system(paste(shQuote(bibtex), shQuote(base)))
            nmiss_prev <- nmiss
            if (index && file.exists(idxfile)) {
                if (system(paste(shQuote(makeindex), shQuote(idxfile)))) 
                  stop(gettextf("unable to run '%s' on '%s'", 
                    makeindex, idxfile), domain = NA)
            }
            if (system(paste(shQuote(latex), "-interaction=nonstopmode", 
                texfile))) 
                stop(gettextf("unable to run %s on '%s'", latex, 
                  file), domain = NA)
            Log <- readLines(paste(base, ".log", sep = ""))
            nmiss <- length(grep("^LaTeX Warning:.*Citation.*undefined", 
                Log))
            if (nmiss == nmiss_prev && !length(grep("Rerun to get", 
                Log))) 
                break
        }
        do_cleanup(clean)
    }
    invisible(NULL)
}
