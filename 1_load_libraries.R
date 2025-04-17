# 1_load_libraries

# libraries
libs <- c(
  "sp",
  "spdep",
  "sf",
  "giscoR",
  "tmap",
  "classInt",
  "ggpubr",
  "scales",
  "rmapshaper",
  "RColorBrewer",
  "modelsummary",
  "tinytable",
  "table1",
  "tidyverse",
  "INLA" 
  )

installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)

#install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13") # retired from cran
library(maptools)  

#install.packages("rgeos", repos = "https://packagemanager.posit.co/cran/2023-07-18")  # retired from cran
library(rgeos)

# set options
theme_set(theme_bw())

# custom functions

## turn string in date as words
extract.date.string = function(x.date, abbr.month = F, elements="dmy", sep = " "){
  x.date = lubridate::ymd(x.date)
  if(elements == "dmy"){
    date.string =  paste0(lubridate::day(x.date), sep, lubridate::month(x.date, label=T, abbr = abbr.month), sep, lubridate::year(x.date))
  }else if(elements == "my"){
    date.string =  paste0(lubridate::month(x.date, label=T, abbr = abbr.month), sep, lubridate::year(x.date))
  }else if(elements == "dm"){
    date.string =  paste0(lubridate::day(x.date), sep, lubridate::month(x.date, label=T, abbr = abbr.month))
  }else if(elements=="m"){
    date.string = lubridate::month(x.date, label=T, abbr = abbr.month)
  }
  
  return(date.string)
} 

## print all rows
print.all = function(x){x %>% print(n=nrow(x))}

## INLA transform marginals - https://becarioprecario.bitbucket.io/inla-gitbook/ch-spatial.html#sec:lattice 
tmarg <- function(marginals) {
  post.means <- parallel::mclapply(marginals, function (marg) {
    # Transform post. marginals
    aux <- inla.tmarginal(exp, marg)
    # Compute posterior mean
    inla.emarginal(function(x) x, aux)
  })
  
  return(as.vector(unlist(post.means)))
}


