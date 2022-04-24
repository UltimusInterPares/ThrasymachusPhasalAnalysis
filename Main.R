### MAIN -----------------------------------------------------------------------

#Packages
library(tidyverse) #Tidy data
library(tidytext) #Text mining (unnest tokens)
library(stringr) #Text manipulation
library(dplyr) #Data manipulation
library(readxl) #Importing .xml's
library(janitor) #Cleaning data frames
library(textdata) #Access the AFINN sentiment dictionary

### IMPORTING DATA -------------------------------------------------------------
source("Scripts/importing.R")

### LENGTHS --------------------------------------------------------------------
source("Scripts/lengths.R")

### SENTIMENTS -----------------------------------------------------------------
source("Scripts/sentiments.R")
