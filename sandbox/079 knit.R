# Author: Yihui Xie
# Notes:  Adam L. Rich
# Date:   July 2, 2013
# Description:
#
#   knit function from the knitr package
#

knit <- function (
          input,                              # Path of the input file
          output = NULL,                      # Path of the output file
          tangle = FALSE,                     # Tangle R code from file?
          text = NULL,                        # A characer vector to knit (in lieu of an input file)
          quiet = FALSE,                      # Show progress and info?
          envir = parent.frame(),             # Environment to evaluate in
          encoding = getOption("encoding")    # Encoding of the input file
) {

    # Determine whether we should use the "input" or "text"
    #   To use input, 
    #   It cannot be missing and
    #   must be a character, file, or connection object
    #
    in.file = !missing(input) && (is.character(input) || prod(inherits(input, 
        c("file", "connection"), TRUE)))
    oconc = knit_concord$get()
    on.exit(knit_concord$set(oconc), add = TRUE)
    if (!missing(input)) 
        input2 = input
    if (in.file && !is.character(input)) 
        input = summary(input)$description
    if (child_mode()) {
        setwd(opts_knit$get("output.dir"))
        if (in.file && !is_abs_path(input)) {
            input = str_c(opts_knit$get("child.path"), input)
            input = file.path(input_dir(), input)
        }
    }
    else {
        oenvir = .knitEnv$knit_global
        .knitEnv$knit_global = envir
        on.exit({
            .knitEnv$knit_global = oenvir
        }, add = TRUE)
        opts_knit$set(output.dir = getwd())
        knit_log$restore()
        on.exit(chunk_counter(reset = TRUE), add = TRUE)
        oopts = options(useFancyQuotes = FALSE, digits = 4L, 
            warn = 1L, width = getOption("KNITR_WIDTH", 75L), 
            device = function(width = 7, height = 7, ...) pdf(NULL, 
                width, height, ...))
        on.exit(options(oopts), add = TRUE)
        optc = opts_chunk$get()
        on.exit(opts_chunk$restore(optc), add = TRUE)
        ocode = knit_code$get()
        on.exit(knit_code$restore(ocode), add = TRUE)
        if (tangle) 
            knit_code$restore()
        optk = opts_knit$get()
        on.exit(opts_knit$set(optk), add = TRUE)
        opts_knit$set(tangle = tangle, encoding = encoding, progress = opts_knit$get("progress") && 
            getOption("KNITR_PROGRESS", TRUE) && !quiet)
    }
    ext = "unknown"
    if (in.file) {
        input.dir = .knitEnv$input.dir
        on.exit({
            .knitEnv$input.dir = input.dir
        }, add = TRUE)
        .knitEnv$input.dir = dirname(input)
        ext = tolower(file_ext(input))
        if (is.null(output) && !child_mode()) 
            output = basename(auto_out_name(input, ext))
        options(tikzMetricsDictionary = tikz_dict(input))
        knit_concord$set(infile = input, outfile = output)
    }
    encoding = correct_encode(encoding)
    text = if (is.null(text)) {
        readLines(if (is.character(input2)) {
            con = file(input2, encoding = encoding)
            on.exit(close(con), add = TRUE)
            con
        }
        else input2, warn = FALSE)
    }
    else split_lines(text)
    if (!length(text)) 
        return()
    text = native_encode(text)
    apat = all_patterns
    opat = knit_patterns$get()
    on.exit(knit_patterns$restore(opat), add = TRUE)
    if (length(opat) == 0 || all(sapply(opat, is.null))) {
        if (is.null(pattern <- detect_pattern(text, ext))) {
            if (is.null(output)) 
                return(paste(text, collapse = "\n"))
            else {
                cat(text, sep = "\n", file = output)
                return(output)
            }
        }
        if (!(pattern %in% names(apat))) 
            stop("a pattern list cannot be automatically found for the file extension '", 
                ext, "' in built-in pattern lists; ", "see ?knit_patterns on how to set up customized patterns")
        set_pattern(pattern)
        if (pattern == "rnw" && is_sweave(text)) 
            remind_sweave(if (in.file) 
                input)
        opts_knit$set(out.format = switch(pattern, rnw = "latex", 
            tex = "latex", html = "html", md = "markdown", rst = "rst", 
            brew = "brew"))
    }
    if (is.null(out_format())) 
        auto_format(ext)
    if (identical(knit_hooks$get(names(.default.hooks)), .default.hooks) && 
        !child_mode()) {
        switch(out_format(), latex = render_latex(), sweave = render_sweave(), 
            listings = render_listings(), html = render_html(), 
            jekyll = render_jekyll(), markdown = render_markdown(), 
            rst = render_rst())
        on.exit(knit_hooks$set(.default.hooks), add = TRUE)
    }
    progress = opts_knit$get("progress")
    if (in.file && !quiet) 
        message(ifelse(progress, "\n\n", ""), "processing file: ", 
            input)
    res = process_file(text, output)
    res = paste(knit_hooks$get("document")(res), collapse = "\n")
    if (!is.null(output)) 
        writeLines(if (encoding == "") 
            res
        else native_encode(res, to = encoding), con = output, 
            useBytes = encoding != "")
    if (!child_mode()) {
        dep_list$restore()
        .knitEnv$labels = NULL
    }
    if (in.file && is.character(output) && file.exists(output)) {
        concord_gen(input, output)
        if (!quiet) 
            message("output file: ", output, ifelse(progress, 
                "\n", ""))
    }
    output %n% res
}

