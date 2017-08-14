# Author Adam L. Rich
# Date   July 18, 2013
# Description
#
#   A test of using knitr to post to a wordpress blog
#

library(XMLRPC)
setwd('P/home/projects/development/R/knitr')
source('W/SL/Actuarial/R-Software/Utilities/Dev/Enhanced.R')



# TeachR
options(
  WordpressLogin = c(adamleerich = 'ezekiel9'),
  WordpressURL = 'http//adamleerich.wordpress.com/xmlrpc.php'
)



# rtestalr
options(
  WordpressLogin = c(adamleerich = 'ezekiel9'),
  WordpressURL = 'http//rtestalr.wordpress.com/xmlrpc.php'
)



# xml.rpc <- function(
#   url, 
#   method, 
#   ..., 
#   .args = list(...), 
#   .opts = list(), 
#   .defaultOpts = list(
#     httpheader = c(`Content-Type` = "text/xml"), 
#     followlocation = TRUE, 
#     useragent = useragent), 
#   .convert = TRUE, 
#   .curl = getCurlHandle(), 
#   useragent = "R-XMLRPC"
# ) 
#
# xml.rpc
#   url           the url of the XML-RPC server
#   method        the XML-RPC method name
#   ...           argument values
#   .args         list of argument values
#   .opts         options passed to postForm
#   .defaultOpts  RCurl options
#   .convert      convert to an R object?
#                 or a function to convert
#   .curl         CURLHandle for recycling connections
#   useragent     name to give the remote procedure
#
#
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'mt.supportedMethods'
)



# demo.sayHello
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'demo.sayHello'
)




# demo.addTwoNumbers
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'demo.addTwoNumbers',
  .args = list(100.21, -54.39)
)



# wp.getUsersBlogs
#   If you use the XMLRPC URL to a blog, 
#   you only get the info for that blog
#   If you send it as below, you get
#   all blogs for a user
xml.rpc(
  url = 'http//wordpress.com/xmlrpc.php', 
  method = 'wp.getUsersBlogs', 
  .args = list(
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1]
  )
)



# wp.getPosts
#   blogid is required
#   The value passed does not matter
posts <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getPosts', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1]
  )
)
posts[1]$value$post_id



# wp.getPost
#   postid must be an existing post
post <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getPost', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    postid = posts[1]$value$post_id
  )
)




# post.text <- "A sample document Sepal.Length   Sepal.Width    Petal.Length   Petal.Width   Min.   4.30   Min.   2.00   Min.   1.00   Min.   0.1    1st Qu.5.10   1st Qu.2.80   1st Qu.1.60   1st Qu.0.3    Median 5.80   Median 3.00   Median 4.35   Median 1.3    Mean   5.84   Mean   3.06   Mean   3.76   Mean   1.2    3rd Qu.6.40   3rd Qu.3.30   3rd Qu.5.10   3rd Qu.1.8    Max.   7.90   Max.   4.40   Max.   6.90   Max.   2.5          Species    setosa    50    versicolor50    virginica 50"
# post.text <- gsub('\t', ' ', post.text)
# post.text <- gsub('&', '&amp;', post.text)
# post.text <- gsub('>', '&gt;', post.text)
# post.text <- gsub('<', '&lt;', post.text)



post.text <- posts[6]$value$post_content



# wp.newPost
newid <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.newPost', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    content = list(
      post_title = 'Post #20 (draft)',
      post_content = post.text,
      post_status = 'draft'
    )
  )
)



# wp.editPost
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.editPost', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    postid = newid,
    content = list(
      post_title = 'Post #9 (published)',
      post_content = 'The content of the post',
      post_status = 'publish'
    )
  )
)



# wp.deletePost
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.deletePost', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    post_id = newid
  )
)



# wp.getTaxonomies
taxonomies <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getTaxonomies', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1]
  )
)



# wp.getTaxonomy
taxonomy <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getTaxonomy', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    taxonomy = 'category'
  )
)



# wp.getTerms
terms <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getTerms', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    taxonomy = 'category'
  )
)



# wp.newTerm
newterm.id <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.newTerm', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    content = list(
      name = 'fake',
      taxonomy = 'category'
    )
  )
)



# wp.getTerm
term <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getTerm', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    taxonomy = 'category',
    term_id = terms[1]$value$term_id
  )
)



# wp.editTerm
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.editTerm', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    termid = term$term_id,
    content = list(
      name = 'faker',
      taxonomy = 'category'
    )
  )
)



# wp.deleteTerm
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.deleteTerm', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    taxonomy = 'category',
    termid = 30299
  )
)



# wp.getUsers
users <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getUsers', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1]
  )
)




# wp.getUser
user <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getUser', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    userid = users[1]$value$user_id
  )
)



# wp.getProfile
profile <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.getProfile', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1]
  )
)



# wp.editProfile
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.editProfile', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    content = list(
      # first_name = 'Adam',
      # last_name = 'Rich',
      # url = 'http//adamleerich.com',
      # display_name = 'Adam L. Rich',
      nickname = 'adamleerich',
      nicename = 'adamleerich',
      bio = 'An actuary in Hartford'
    )
  )
)



# wp.getPage
# wp.getPages
# wp.newPage
newid <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'wp.newPage', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    content = list(
      title = 'A New Page',
      description = 'The content of the post',
      post_status = 'draft'
    )
  )
)



# wp.deletePage
# wp.editPage
# wp.getPageList
# wp.getAuthors
# wp.getCategories
# wp.getTags
# wp.newCategory
# wp.deleteCategory
# wp.suggestCategories
# wp.uploadFile
# wp.getCommentCount
# wp.getPostStatusList
# wp.getPageStatusList
# wp.getPageTemplates
# wp.getOptions
# wp.setOptions
# wp.getComment
# wp.getComments
# wp.deleteComment
# wp.editComment
# wp.newComment
# wp.getCommentStatusList
# wp.getMediaItem
# wp.getMediaLibrary
# wp.getPostFormats
# wp.getPostType
# wp.getPostTypes
# wp.getRevisions
# wp.restoreRevision



# blogger.getUsersBlogs
# blogger.getUserInfo
# blogger.getPost
# blogger.getRecentPosts
# blogger.newPost
# blogger.editPost
# blogger.deletePost



# metaWeblog.newPost
#
#   publish will default to FALSE
#   description is not necessary
#   date_created_gmt has problems
#   type will default to 'post'
#
newid <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'metaWeblog.newPost', 
  .args = list(
    blogid = 1,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1],
    content = list(
      title = 'Post #6',
      description = 'The content of the post'
    ),
    publish = TRUE
  )
)



# metaWeblog.editPost
# metaWeblog.getPost
# metaWeblog.getRecentPosts
# metaWeblog.getCategories
# metaWeblog.newMediaObject
# metaWeblog.deletePost
# metaWeblog.getUsersBlogs



# mt.getCategoryList
categories <- xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'mt.getCategoryList', 
  .args = list(
    blogid = 0,
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1]
  )
)



# mt.getRecentPostTitles
# mt.getPostCategories
# mt.setPostCategories



# mt.supportedMethods
#   no arguments
xml.rpc(
  url = getOption('WordpressURL')[1], 
  method = 'mt.supportedMethods', 
  .args = list(
    username = names(getOption('WordpressLogin'))[1], 
    password = unname(getOption('WordpressLogin'))[1]
  )
)



# mt.supportedTextFilters
# mt.getTrackbackPings
# mt.publishPost
# pingback.ping
# pingback.extensions.getPingbacks
# wpStats.get_key
# wpStats.check_key
# wpStats.get_blog_id
# wpStats.get_site_id
# wpStats.update_bloginfo
# wpStats.update_postinfo
# wpStats.ping_blog
# wpStats.flush_posts
# wpcom.get_user_blogids
# wpcom.getFeatures
# wpcom.addApplicationPassword
# wpcom.blackberryUploadFile
# wpcom.blackberryGetUploadingFileKeys
# wpcom.getUsersSubs
# wpcom.set_mobile_push_notification_settings
# wpcom.get_mobile_push_notification_settings
# wpcom.mobile_push_register_token
# wpcom.mobile_push_unregister_token
# wpcom.mobile_push_set_blogs_list
# wpcom.mobile_push_win_phone_get_last_notification
