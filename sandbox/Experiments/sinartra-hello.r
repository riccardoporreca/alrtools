library(sinartra)
library(tools)

# Create router and define some basic routes
router <- Router$clone()
router$set_base_url("/custom/myapp")

router$get("/", function(path, query = c(), ...) {
  name <- query["name"]
  if (is.null(name)) {
    "Hello!"    
  } else {
    paste("Hello ", name, "!", sep = "")
  }
})

# Hook up to R's html server (this process is not well documented)
route <- function(path, query, ...){
  router$route(path, query)
}

# Start html server
port <- tools:::httpdPort
if (port == 0L) {
  port <- startDynamicHelp()    
}
env <- (tools:::.httpd.handlers.env)
env[["myapp"]] <- route

# Open in browser
browseURL(paste("http://localhost:", port, "/custom/myapp/", sep = ""))
browserURL(paste("http://localhost:", port, "/custom/myapp/?name=hadley", 
  sep = ""))