### MAIN -----------------------------------------------------------------------

#Packages
library(tidyverse) #Tidy data
library(tidytext) #Text mining (unnest tokens)
library(stringr) #Text manipulation
library(dplyr) #Data manipulation
library(readxl) #Importing .xml's
library(janitor) #Cleaning data frames
library(textdata) #Access the AFINN sentiment dictionary
library(patchwork) #Free faceting of figures
library(openxlsx) #Writing excel sheets

### Functions ------------------------------------------------------------------
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  
  if (length(u[tab == max(tab)]) > 1) {NA}
  else {u[tab == max(tab)]}
  
}

### Global Variables -----------------------------------------------------------
speaker = c("global", "cleitophon", "glaucon", "polemarchus", "socrates", "thrasymacus")

### IMPORTING DATA -------------------------------------------------------------
source("Scripts/importing.R")

### LENGTHS --------------------------------------------------------------------
source("Scripts/turn_conseq.R")  # Sentences per Turn
source("Scripts/turn_lengths.R") # Words per Turn

### SENTIMENTS -----------------------------------------------------------------
source("Scripts/sentiments.R")   # Sentiments per Turn

### ANALYSES -------------------------------------------------------------------
source("Scripts/analyses.R") # Combo Charts