### IMPORTING DATA -------------------------------------------------------------
# Import the Raw Data Set
nested_dialogue <- read_excel("SocVsThrasEx.xlsx", sheet = "Typology ") %>%
  as_tibble() %>%
  clean_names()

# Prepare for data analysis: one-token-per-row format
# Include the column "index" for easier re-organization down the line
unnested_dialogue <- nested_dialogue %>%
  unnest_tokens(word, text)
unnested_dialogue <- unnested_dialogue %>%
  add_column(index = c(1:nrow(unnested_dialogue)),
             .before = "start_page")

# Color blind-safe palette to be used later
cbPalette <- c("#999999", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#0072B2", "#D55E00", "#CC79A7")
