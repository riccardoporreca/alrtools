install_git <- function(
                url, 
                subdir = NULL, 
                branch = NULL, 
                credentials = NULL, 
                args = character(0), ...) {
  
  if (!missing(args)) 
    warning("`args` is deprecated", call. = FALSE)
  
  
  remotes <- lapply(url, git_remote, subdir = subdir, branch = branch, 
                    credentials = credentials)
  install_remotes(remotes, ...)
}



git_remote <- function (url, subdir = NULL, branch = NULL, credentials = NULL) {
  remote("git", url = url, subdir = subdir, branch = branch, 
         credentials = credentials)
}


remote <- function (type, ...) {
  structure(list(...), class = c(paste0(type, "_remote"), "remote"))
}





