x <- sample(10) < 4


mtcars1 <- mtcars[sample(1:nrow(mtcars), nrow(mtcars)), 
                  sample(1:ncol(mtcars), ncol(mtcars))]





copy.table(mtcars[sample(1:nrow(mtcars), nrow(mtcars)), 
                  sample(1:ncol(mtcars), ncol(mtcars))], row.names = T)




a <- sort(sample(1:nrow(mtcars), 2, replace = T))
mtcars
a
mtcars[a[1]:a[2], ]


# Get a list of all distributions in R
f1 <- apropos('^q')
f2 <- apropos('^p')
f3 <- apropos('^d')
f4 <- apropos('^r')

fs1 <- gsub('^q', '', f1)
fs2 <- gsub('^p', '', f2)
fs3 <- gsub('^d', '', f3)
fs4 <- gsub('^r', '', f4)

fall <- c(fs1, fs2, fs3, fs4)
tfall <- table(fall)



intersect(intersect(fs1, fs2), intersect(fs3, fs4))
