template_file <- './data-raw/test-template.txt'
test_template <- readChar(template_file, file.info(template_file)$size)

# Save to R/Sysdata.rda
<<<<<<< HEAD
devtools::use_data(test_template, internal = TRUE, overwrite = TRUE)
=======
devtools::use_data(test_template, internal = TRUE)
>>>>>>> 21ce52281f6ac8841f32e5a7163888ce2023c506
