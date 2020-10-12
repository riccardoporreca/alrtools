# List all objects in the package

library(alrtools)
objs <- ls('package:alrtools')



# Open the test pages for each
for (o in objs) {
  eval(call('open_test', o))
}


