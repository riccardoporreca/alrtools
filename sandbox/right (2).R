right <- function (string_input, no) 
{
    substr(string_input, nchar(string_input) + 1 - no, nchar(string_input))
}
