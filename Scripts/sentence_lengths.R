### SENTENCE LENGTHS -----------------------------------------------------------
# Sentence Length
# count() is essentially group_by(a, b) %>% summarise(n = n())
# summarise truncates and sorts
# add_count() swaps summarise for mutate, which does neither
# add_count() %>% distinct effectively truncates without resorting
# also adding an index


# Length by Sentence by Words
republic_sentence_length <- unnested_dialogue %>%
  add_count(turn, sentence, sort = FALSE) %>%
  distinct(speaker, turn, sentence, n)
republic_sentence_length <- republic_sentence_length  %>%
  add_column(index = c(1:nrow(republic_sentence_length)),
             .before = "speaker")
names(republic_sentence_length) <- c("index",
                                     "speaker",
                                     "turn",
                                     "sentence",
                                     "length (words)") %>%
  make_clean_names()

# Sentence Data by Speaker + Global - - - - - - - - - - - - - - - - - - - - - -
# mean, median, mode, and standard deviation of sentence lengths by speaker

# Gather turn Data
cleitophon_length <- republic_sentence_length %>%
  filter(str_detect(speaker, "Cleitophon") == T)
glaucon_length <- republic_sentence_length %>%
  filter(str_detect(speaker, "Glaucon") == T)
polemarchus_length <- republic_sentence_length %>%
  filter(str_detect(speaker, "Polemarchus") == T)
socrates_length <- republic_sentence_length %>%
  filter(str_detect(speaker, "Socrates") == T)
thrasymachus_length <- republic_sentence_length %>%
  filter(str_detect(speaker, "Thrasymachus") == T)

# Generate table from data
los <- tibble(
  global = c(
    mean(republic_sentence_length$length_words),
    median(republic_sentence_length$length_words),
    mode(republic_sentence_length$length_words),
    sd(republic_sentence_length$length_words)
  ),
  cleitophon = c(
    mean(cleitophon_length$length_words),
    median(cleitophon_length$length_words),
    mode(cleitophon_length$length_words),
    sd(cleitophon_length$length_words)
  ),
  glaucon = c(
    mean(glaucon_length$length_words),
    median(glaucon_length$length_words),
    mode(glaucon_length$length_words),
    sd(glaucon_length$length_words)
  ),
  polemarchus = c(
    mean(polemarchus_length$length_words),
    median(polemarchus_length$length_words),
    mode(polemarchus_length$length_words),
    sd(polemarchus_length$length_words)
  ),
  socrates = c(
    mean(socrates_length$length_words),
    median(socrates_length$length_words),
    mode(socrates_length$length_words),
    sd(socrates_length$length_words)
  ),
  thrasymachus = c(
    mean(thrasymachus_length$length_words),
    median(thrasymachus_length$length_words),
    mode(thrasymachus_length$length_words),
    sd(thrasymachus_length$length_words)
  )
) # Length of Turns

row.names(los) <- c("mean", "median", "mode", "s.d.")

rm(cleitophon_length,
   glaucon_length,
   polemarchus_length,
   socrates_length,
   thrasymachus_length)