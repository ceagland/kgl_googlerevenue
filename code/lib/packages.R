# Libraries ------

loadpkgs <- c(
              #For Primary Use / Data Cleaning
              "data.table",    #Faster for large data sets (+fread)
              "tidyverse",     #Readable verbs for working with data (dplyr) and includes many other useful packages
              "dtplyr",        #data.table back-end for dplyr - not sure whether this does anything
              "sjmisc",        #Some useful data munging functions
              "zoo",           #Time series (using mostly for na.locf function to "fill down" NAs (or up))
              "lubridate",     #Working with dates - actually don't use this that much. Generally base R is enough
              
              #Modeling
              "caret",         #For more automated classification/regression/cross validation/data prep (also, fnctn for checking linear combos)
              #"mlr",           #Not using this yet, but it's a very popular machine learning package 
              "mgcv",          #GAM
              "glmnet",        #lasso Regression - Poisson
              "xgboost",       #for building xgboost models
              #"stringr",       #Working with strings - now loads with tidyverse as intended
              "rebus",         #Plain language regular expressions

              #Reading/Writing data (openxlsx is also very useful for automated reporting)  
              "fst",           #Very fast read/write of large datasets (data.table and data.frame, or vectors)
              "openxlsx",      #For working with xlsx files, both reading and creating automated reports in excel (openxlsx has no java requirement, unlike similar excel packages)
              
              #Programming tools
              "profvis",       #for profiling code (why is this taking so long to run, code!?)
              "assertthat",   #assert functions that improve on base R options
              "parallel",     #For multicore processing
              
              #Visualization
              "scales",        #Here, primarily for formatting axes in graphics, but also formats numbers for any text output
              "DataExplorer",   #Great package for visualizing many variables at once with single functions (plot_histogram, for ex)
              "RColorBrewer",  #Colors for ggplot
              "gridExtra",     #Mostly use this for putting multiple plots in a grid
              "extrafont",     #For working with fonts (loading package registers fonts, too)

              #Reporting
              "knitr",         #For R Markdown
              "kableExtra",    #Nicer looking markdown tables (addition to kable - load both)
              "formattable")   #Table Formatting 


Sys.setenv(TZ = "Etc/GMT+5")

notinstalled <- loadednow <- alreadyloaded <- c()

for (i in loadpkgs) {
  if(!paste0("package:",i) %in% search()) {
    hold <- tryCatch(suppressPackageStartupMessages(library(i, character.only = TRUE)), error = function(e) e)
    if(inherits(hold, "error")) {
      notinstalled <- c(notinstalled,i) #don't care about growing a vector this small
    } else {
      loadednow <- c(loadednow,i)
    }
  } else {
    alreadyloaded <- c(alreadyloaded, i)
  }
}

if (length(loadednow) > 0) {
  message("The following ", length(loadednow), " packages have been loaded, in this order:")
  writeLines(c(paste(loadednow, collapse = "\n"),"\n"))
}

if (length(alreadyloaded) > 0) {
  message("These packages were not loaded because they were already found in the environment:")
  writeLines(c(paste(alreadyloaded, collapse = "\n"),"\n"))
}

if (length(notinstalled) > 0) {
  notinstalledmsg <- paste(c("The following packages could not be found:\n ",
                             paste0(notinstalled,"\n"),"\n\rInstall missing packages?"),
                    collapse = "")
  
  if(winDialog(type = c("yesno"), notinstalledmsg)=="YES"){       
    install.packages(notinstalled)
    sapply(notinstalled, library, character.only = TRUE)
  }
}

torm <- c("i","alreadyloaded","loadednow","notinstalled","loadpkgs")
rm(list = torm)
