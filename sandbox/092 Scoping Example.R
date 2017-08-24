

# An example of scoping

if(exists('a')) {rm(a)}
a

with(new.env(),{
  a <<- 'Global'
  a <- 'First With'
  print(a)
  
  with(new.env(),{
    print(a)
    a <<- 'Second With'
  })
  
  print(a)
  
}) 

a