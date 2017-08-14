# Author: Adam L. Rich
# Date:   August 25, 2011
# Description:
#
#   A function to list all objects in the search path
#

all.attached.objects <- function(){
  
  for (e in (lapply(search(), as.environment))) {
    # for each environment, find out what the objects are
    cs <- unlist(eapply(e, class));
    ns <- names(cs);
    
    print(paste(environmentName(e),
                'length:',
                length(cs)));
    
    if (length(cs) > 0) {    
      df_temp <- data.frame(package = environmentName(e),
                            name    = ns,
                            class   = cs);
      if (exists('d.f')) {
        d.f <- rbind(d.f, df_temp);
      } else {
        d.f <- df_temp;
      }; # end if
	
    }; # end if
  }; # end for

  d.f;

}; # end function

d.f <- all.attached.objects();

length(unlist(d.f[d.f$package == 'R_GlobalEnv',]));
