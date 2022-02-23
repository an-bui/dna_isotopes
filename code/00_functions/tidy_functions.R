# Tidy Functions
# Ana Miller-ter Kuile
# September 30, 2021

# this is a set of functions that will be useful
# for the DNA isotope project. 

# Load packages -----------------------------------------------------------

package.list <- c("tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Isotope correction function ---------------------------------------------

isotope_correction <- function(d15_plant,  
                               d13_plant,
                               d15_marine, 
                               d13_marine,
                               d15_consumer, 
                               d13_consumer) {
  a <- (d13_consumer - d13_marine)/(d13_plant - d13_marine)
  
  delta <- 3.4
  
  d15_base <- (d15_plant*a + d15_marine*(1-a))/delta
  
  d15_corrected <- 1 + (d15_consumer - d15_base)/delta
  
  return(d15_corrected)
}
