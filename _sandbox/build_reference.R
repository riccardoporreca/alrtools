


# pkg <- pkgdown:::section_init(
#     pkg = '.', depth = 1L, override = list())

devtools::document('.')
pkg <- as_pkgdown('.')
topics <- purrr::transpose(pkg$topics)
data <- pkgdown:::data_reference_topic(
    topic, pkg, examples = FALSE,
    run_dont_run = FALSE, mathjax = TRUE)



# purrr::map(
#     topics,
#     pkgdown:::build_reference_topic,
#     pkg = pkg,
#     lazy = TRUE,
#     examples = FALSE,
#     run_dont_run = FALSE,
#     mathjax = TRUE)
#
#
# pkgdown:::build_reference_topic

topic <- topics[[1]]

fs::path(pkg$src_path, "man")
fs::path(pkg$dst_path, "reference")

in_path <- fs::path(pkg$src_path, "man", topic$file_in)
out_path <- fs::path(pkg$dst_path, "reference", topic$file_out)


data <- pkgdown:::data_reference_topic(
    topic, pkg, examples = FALSE,
    run_dont_run = FALSE, mathjax = TRUE)


# # pkgdown:::data_reference_topic
# tag_names <- purrr::map_chr(topic$rd, ~class(.)[[1]])
# tag_names
#
# tags <- split(topic$rd, tag_names)
#
# out <- list()
# out$name <- flatten_text(tags$tag_name[[1]][[1]])
# out$title <- extract_title(tags$tag_title)
# out$pagetitle <- paste0(out$title, " — ", out$name)
# out$source <- github_source_links(pkg$github_url, topic$source)
# out$filename <- topic$file_in
# out$aliases <- purrr::map_chr(tags$tag_alias %||% list(),
#                               flatten_text)
# out$author <- purrr::map_chr(tags$tag_author %||% list(),
#                              flatten_text)
# out$keywords <- purrr::map_chr(tags$tag_keyword %||% list(),
#                                flatten_text)
# out$description <- as_data(tags$tag_description[[1]])
# out$opengraph <- list(description = strip_html_tags(out$description$contents))
# out$usage <- as_data(tags$tag_usage[[1]])
# out$arguments <- as_data(tags$tag_arguments[[1]])
# if (length(out$arguments)) {
#     out$has_args <- TRUE
# }
# out$examples <- as_data(tags$tag_examples[[1]], env = new.env(parent = globalenv()),
#                         topic = tools::file_path_sans_ext(topic$file_in), examples = examples,
#                         run_dont_run = run_dont_run)
# section_tags <- c("tag_details", "tag_references", "tag_source",
#                   "tag_format", "tag_note", "tag_seealso", "tag_section",
#                   "tag_value")
# sections <- topic$rd[tag_names %in% section_tags]
# out$sections <- sections %>% purrr::map(as_data) %>% purrr::map(add_slug)
# out










render_page(
    pkg = pkg,
    name = "reference-topic",
    data = data,
    path = fs:::path("reference", topic$file_out))





# render_page

require(rlang)

path <- '.'
devtools::document(path)
pkg <- as_pkgdown(path)
topics <- purrr::transpose(pkg$topics)
topic <- topics[[1]]

data <- pkgdown:::data_reference_topic(
    topic, pkg, examples = FALSE,
    run_dont_run = FALSE, mathjax = TRUE)


tag_names <-
    sapply(topic$rd, function(x){class(x)[1]})

tag_names <- purrr::map_chr(topic$rd, ~class(.)[[1]])
tags <- split(topic$rd, tag_names)
out <- list()
out$name <- flatten_text(tags$tag_name[[1]][[1]])
out$title <- extract_title(tags$tag_title)
out$pagetitle <- paste0(out$title, " — ", out$name)
out$source <- github_source_links(pkg$github_url, topic$source)
out$filename <- topic$file_in
out$aliases <- purrr::map_chr(tags$tag_alias %||% list(),
                              flatten_text)
out$author <- purrr::map_chr(tags$tag_author %||% list(),
                             flatten_text)
out$keywords <- purrr::map_chr(tags$tag_keyword %||% list(),
                               flatten_text)
out$description <- as_data(tags$tag_description[[1]])
out$opengraph <- list(description = strip_html_tags(out$description$contents))
out$usage <- as_data(tags$tag_usage[[1]])
out$arguments <- as_data(tags$tag_arguments[[1]])
if (length(out$arguments)) {
    out$has_args <- TRUE
}
out$examples <- as_data(tags$tag_examples[[1]], env = new.env(parent = globalenv()),
                        topic = tools::file_path_sans_ext(topic$file_in), examples = examples,
                        run_dont_run = run_dont_run)
section_tags <- c("tag_details", "tag_references", "tag_source",
                  "tag_format", "tag_note", "tag_seealso", "tag_section",
                  "tag_value")
sections <- topic$rd[tag_names %in% section_tags]
out$sections <- sections %>% purrr::map(as_data) %>% purrr::map(add_slug)
out




whisker::whisker.render(
    template = readLines('./data-raw/content-reference-topic.md'),
    data = data)







topics <- pkgdown:::package_topics('.')
topic <- topics[1, ]




pkgdown:::package_rd

pkgdown:::rd_file('man/cnumeric.Rd')


rd <- pkgdown:::set_classes(tools::parse_Rd('man/cnumeric.Rd'))



rd <- pkgdown:::package_rd(path)
scoped_package_context(package, topic_index = character(),
                       src_path = path)
scoped_file_context()
aliases <- purrr::map(rd, extract_tag, "tag_alias")
names <- purrr::map_chr(rd, extract_tag, "tag_name")
titles <- purrr::map_chr(rd, extract_title)
concepts <- purrr::map(rd, extract_tag, "tag_concept")
internal <- purrr::map_lgl(rd, is_internal)
source <- purrr::map(rd, extract_source)
file_in <- names(rd)
file_out <- gsub("\\.Rd$", ".html", file_in)
funs <- purrr::map(rd, topic_funs)
tibble::tibble(name = names, file_in = file_in, file_out = file_out,
               alias = aliases, funs = funs, title = titles, rd = rd,
               source = source, concepts = concepts, internal = internal)




rd <- tools::parse_Rd('man/cnumeric.Rd')
rd %>% unclass


