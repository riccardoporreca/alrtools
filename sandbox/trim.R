trim <- function (x, method = "") 
{
    switch(method, Leading = sub("^ +", "", x), Trailing = sub(" +$", 
        "", x), gsub("(^ +)|( +$)", "", x))
}
