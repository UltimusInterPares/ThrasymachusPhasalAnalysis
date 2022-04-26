### TURN LENGTHS ---------------------------------------------------------------
# Turn and Sentence Length
# count() is essentially group_by(a, b) %>% summarise(n = n())
# summarise truncates and sorts
# add_count() swaps summarise for mutate, which does neither
# add_count() %>% distinct effectively truncates without resorting
# also adding an index

# Length of Turn by Words
republic_turn_length <- unnested_dialogue %>%
  count(reorder(turn, index), speaker, sort = FALSE)
republic_turn_length <- republic_turn_length  %>%
  add_column(index = c(1:nrow(republic_turn_length)),
             .before = "reorder(turn, index)")

names(republic_turn_length) <- c("index", "turn", "speaker", "length (words)") %>%
  make_clean_names()

republic_turn_length <- republic_turn_length %>% #reorder to match sentence length
  relocate("speaker", .before = "turn")

# Turn Data by Speaker + Global - - - - - - - - - - - - - - - - - - - - - -
# mean, median, mode, and standard deviation of sentence lengths by speaker

# Gather turn Data
cleitophon_length <- republic_turn_length %>%
  filter(str_detect(speaker, "Cleitophon") == T)
glaucon_length <- republic_turn_length %>%
  filter(str_detect(speaker, "Glaucon") == T)
polemarchus_length <- republic_turn_length %>%
  filter(str_detect(speaker, "Polemarchus") == T)
socrates_length <- republic_turn_length %>%
  filter(str_detect(speaker, "Socrates") == T)
thrasymachus_length <- republic_turn_length %>%
  filter(str_detect(speaker, "Thrasymachus") == T)

lot <- tibble(
  speaker = c("global", "cleitophon", "glaucon", "polemarchus", "socrates", "thrasymacus"),
  mean = c(mean(republic_turn_length$length_words),
           mean(cleitophon_length$length_words),
           mean(glaucon_length$length_words),
           mean(polemarchus_length$length_words),
           mean(socrates_length$length_words),
           mean(thrasymachus_length$length_words)),
  median = c(median(republic_turn_length$length_words),
             median(cleitophon_length$length_words),
             median(glaucon_length$length_words),
             median(polemarchus_length$length_words),
             median(socrates_length$length_words),
             median(thrasymachus_length$length_words)),
  mode = c(find_mode(republic_turn_length$length_words),
           find_mode(cleitophon_length$length_words),
           find_mode(glaucon_length$length_words),
           find_mode(polemarchus_length$length_words),
           find_mode(socrates_length$length_words),
           find_mode(thrasymachus_length$length_words)),
  sd = c(sd(republic_turn_length$length_words),
         sd(cleitophon_length$length_words),
         sd(glaucon_length$length_words),
         sd(polemarchus_length$length_words),
         sd(socrates_length$length_words),
         sd(thrasymachus_length$length_words))
)

rm(cleitophon_length,
   glaucon_length,
   polemarchus_length,
   socrates_length,
   thrasymachus_length)

# Global Sentence Length Bar Plot - - - - - - - - - - - - - - - - - - - - - - -
# makes a stacked bar plot of turn lengths
# stacks of sentences by length
# Really kills two birds with one stone, but it's a bit informationally dense
# changed republic_sentence_length to " "_turn_" " to clean it up
# changed color and _color_ to fill and _fill_
turn_length <- republic_turn_length %>%
  ggplot(mapping = aes(x=reorder(turn, index), y=length_words, fill = speaker)) +
  scale_fill_manual(values=cbPalette) +
  geom_bar(stat = "identity") + # prevents geom_bar() from reorganizing
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn") +
  ylab("Length")

# Trying to get a bell curve of sentence lengths
# ggplot(count(republic_sentence_length), mapping = aes(x = length_words, y = n)) +
#   stat_function(fun = dnorm,
#                 args = list(mean = mean(republic_sentence_length$length_words),
#                             sd = sd(republic_sentence_length$length_words)))