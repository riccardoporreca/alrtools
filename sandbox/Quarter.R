Quarter <- function (d) 
{
    rep(1:4, each = 3)[Month(d)]
}
