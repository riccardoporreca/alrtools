compilePdf <- function(file, completedAction = "")
{
   # get the path info
   pathInfo = .Call("rs_pathInfo", file)
   
   # check for spaces in path (sweave chokes on these)
   if ( length(grep(" ", pathInfo$name)) > 0 )
   {
     stop(paste("Invalid filename: '", pathInfo$name,
                "' (TeX does not understand paths with spaces)",
                sep=""))
   }
     
   # determine the directory name of the passed file and setwd to it
   # (but restore to the current wd on exit)
   currentDir <- getwd()
   setwd(pathInfo$directory)
   
   # on exit restore working dir 
   on.exit(setwd(currentDir))
   
   # set the filename for the compile (will be changed if we Sweave)
   fileName <- pathInfo$name

   # check extension to see if we need to Sweave
   ext <- tolower(pathInfo$extension)
   if (ext == ".rnw" || ext == ".snw" || ext == ".nw")
   {
     .Call("rs_callSweave", R.home("bin"), fileName)
     fileName = paste(pathInfo$stem, ".tex", sep="")

     # validate the TeX file before proceeding (will be invalid if
     # Sweave aborted due to an R syntax or processing error)
     if ( !.Call("rs_validateTexFile", fileName) )
        return ()
   }

   # run texi2dvi
   cat("\n")
   cat("Running texi2dvi...")
   if (getRversion() >= "2.12")
   {
      # workaround for: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=577741
      # set index = FALSE on unix to force use of pdflatex rather than
      # texi2dvi (the bug is that texi2dvi doesn't escape tildes)
      index = !identical(.Platform$pkgType, "source")
      tools:::texi2dvi(file=fileName, pdf=TRUE, index=index)
   }
   else
   {
      tools:::texi2dvi(file=fileName, pdf=TRUE)
   }
   cat("completed\n\n")
 
   # check for completed action
   if (completedAction == "view")
     .Call("rs_viewPdf", pathInfo$path)
   else if (completedAction == "publish")
     .Call("rs_publishPdf", pathInfo$path)
}
