template_file <- './data-raw/test-template.txt'
test_template <- readChar(template_file, file.info(template_file)$size)

# Save to R/Sysdata.rda
usethis::use_data(test_template, internal = TRUE, overwrite = TRUE)



## Define Euler's number
e <- exp(1)
usethis::use_data(e, internal = FALSE)
