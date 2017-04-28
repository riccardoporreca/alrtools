template_file <- './data-raw/test-template.txt'
test_template <- readChar(template_file, file.info(template_file)$size)

# Save to R/Sysdata.rda
devtools::use_data(test_template, internal = TRUE)
