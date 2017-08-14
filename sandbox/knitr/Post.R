# Author: Adam L. Rich
# Date:   July 18, 2013
# Description:
#
#   A test of using knitr to post to a wordpress blog
#



library(knitr)
library(XMLRPC)



setwd('P:/home/projects/development/R/knitr')




options(WordpressLogin = c(adamleerich = 'ezekiel9'),
        WordpressURL = 'http://rtestalr.wordpress.com/xmlrpc.php')



newPost <- function (
  content, 
  publish = TRUE, 
  blogid = 0, 
  login = getOption("WordpressLogin", stop("need a login and password")), 
  .server = getOption("WordpressURL", stop("need a WordpressURL"))
) {
  
  if(publish) {
    content$post_status <- 'publish'
  } else {
    content$post_status <- 'draft'
  }
  
  ans <- xml.rpc(
    url = .server, 
    method = 'wp.newPost', 
    .args = list(
      blogid = as.character(blogid),
      username = names(login), 
      password = as.character(login),
      content = content
    )
  )
  
  structure(ans, class = "WordpressPostId")
}


input = 'Sample.Rmd'
title = 'Another Post from knitr'
shortcode = FALSE
encoding = getOption("encoding")
publish = TRUE



knit2wp <- function(
  input, 
  title = "A post from knitr", 
  ..., 
  shortcode = FALSE, 
  encoding = getOption("encoding"), 
  publish = TRUE
) {
  
  out = knit(input, encoding = encoding)
  on.exit(unlink(out))
  con = file(out, encoding = encoding)
  on.exit(close(con), add = TRUE)
  post = knitr:::native_encode(readLines(con, warn = FALSE))
  post = paste(post, collapse = "\n")
  post = markdown::markdownToHTML(text = post, fragment.only = TRUE)
  if (shortcode) {
    post = gsub("<pre><code class=\"([[:alpha:]]+)\">", 
                   "[code light=\"true\" language=\"\\1\"]\n", post)
    post = gsub("<pre><code( class=\"no-highlight\"|)>", 
                  "[code light=\"true\"]\n", post)
    post = gsub("</code></pre>", "[/code]", post)
  }
  post = knitr:::native_encode(post, "UTF-8")
  title = knitr:::native_encode(title, "UTF-8")

  
  newPost(
    content = list(
      post_title = title,
      post_content = post, 
      ...
    ), 
    publish = publish
  )
  
}



spin(hair = 'Sample.R', knit = TRUE, report = TRUE)
knit2wp('sample.Rmd', title = 'From knitr', shortcode = TRUE)



