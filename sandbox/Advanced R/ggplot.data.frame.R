# Author: Adam L. Rich
# Date:   June 26, 2015
# Description:
#
#   Annotated function
#   ggplot.data.frame*
#


ggplot2:::ggplot.data.frame <- function(
  data, 
  mapping = aes(), 
  ..., 
  environment = globalenv()
) {
  
  # Check to make sure that we have some sort of mapping
  if (!missing(mapping) && !inherits(mapping, "uneval")) 
    stop("Mapping should be created with aes or aes_string")
  
  
  # p is a structure of type gg and ggplot
  #   data          a data.frame
  #   layers        a list
  #   scales        ???
  #   mapping       ???
  #   theme         a list
  #   coordinates   ???
  #   facet         ???
  #   plot_env      an environment
  
  p <- structure(list(
    data = data, 
    layers = list(), 
    scales = Scales$new(), 
    mapping = mapping, 
    theme = list(), 
    coordinates = coord_cartesian(), 
    facet = facet_null(), 
    plot_env = environment
  ), class = c("gg", "ggplot"))
  
  
  p$labels <- make_labels(mapping)
  set_last_plot(p)
  p
  
  
  
}





ggplot2:::plot.ggplot <- function (
  x, 
  newpage = is.null(vp), 
  vp = NULL, 
  ...
) {
  
  set_last_plot(x)
  if (newpage) 
    grid.newpage()
  data <- ggplot_build(x)
  gtable <- ggplot_gtable(data)
  if (is.null(vp)) {
    grid.draw(gtable)
  }
  else {
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }
  invisible(data)
  
}



