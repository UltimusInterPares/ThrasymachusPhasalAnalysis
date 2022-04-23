### MAIN -----------------------------------------------------------------------

#Packages
library(tidyverse) #Tidy data
library(readxl) #Importing .xml's
library(janitor) #Cleaning data frames

# Import the Raw Data Set

unsorted_dialogue <- read_excel("SocVsThrasEx.xlsx", skip = 1) %>%
  as_tibble() %>%
  clean_names()

sorted_dialogue <- tibble(start_page = c(NA),
                          end_page = c(NA),
                          start_line = c(NA),
                          end_line = c(NA),
                          speaker = c(NA),
                          inquition = c(NA),
                          sentence = c(NA))

find_turns <- function(input = unsorted_dialogue[1:10,], output = sorted_dialogue) {
  print(input$speaker)
  
  # Placeholder for Finding Turns
  dialogue_placeholder <- tibble(start_page = c(NA),
                                 end_page = c(NA),
                                 start_line = c(NA),
                                 end_line = c(NA),
                                 speaker = c(NA),
                                 inquition = c(NA),
                                 sentence = c(NA))
  
  # Start active row
  ROW_a <- 1
  ROW_b <- 1
  
  # Capture Pages
  start_page <- input$start_page[ROW_a]
  end_page <- input$end_page[ROW_b]
  
  # Capture Lines
  start_line <- input$start_line[ROW_a]
  end_line <- input$end_line[ROW_b]
  
  # Loop: See if names are the same
  for (i in c(1:nrow(input))) {
    if (is.na(input$speaker[i+1])) {
      print("NA")
    }
    else if (input$speaker[i+1] == input$speaker[i]) {
      print("SAME")
      print(input[i,])
      print(dialogue_placeholder[i,])
      dialogue_placeholder[i,] <- input[i,] # doesn't work for some reason
    }
    else if (input$speaker[i+1] != input$speaker[i]) {
      print("DIFF")
    }
  }
  
  
}
