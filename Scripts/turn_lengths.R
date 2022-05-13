### TURN LENGTHS ---------------------------------------------------------------
# Turn Length
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
  sd = c(sd(republic_turn_length$length_words),
         sd(cleitophon_length$length_words),
         sd(glaucon_length$length_words),
         sd(polemarchus_length$length_words),
         sd(socrates_length$length_words),
         sd(thrasymachus_length$length_words))
)

### Statistical Tests ----------------------------------------------------------
length_tests <- tibble(speaker = speaker[2:6])

length_t <- c(t.test(x = cleitophon_length$length_words, mu = mean(republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value,
              t.test(x = glaucon_length$length_words, mu = mean(republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value,
              t.test(x = polemarchus_length$length_words, mu = mean(republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value,
              t.test(x = socrates_length$length_words, mu = mean(republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value,
              t.test(x = thrasymachus_length$length_words, mu = mean(republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value)

length_f <- c(var.test(x = cleitophon_length$length_words, y = (republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value,
              var.test(x = glaucon_length$length_words, y = (republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value,
              var.test(x = polemarchus_length$length_words, y = (republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value,
              var.test(x = socrates_length$length_words, y = (republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value,
              var.test(x = thrasymachus_length$length_words, y = (republic_turn_length$length_words), alternative = "two.sided", conf.level = 0.95)$p.value)

lot <- lot %>%
  add_column(p_mean = c(NA, length_t), .after = "mean") %>%
  add_column(p_sd = c(NA, length_f), .after = "sd")

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
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=length_words, fill = speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=length_words), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  xlab("Turn (Index)") +
  ylab("Words")

p1_length <- republic_turn_length[c(1:150),] %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=length_words, fill = speaker), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  xlab("Turn") +
  ylab("Length (Words)")

# # t-test
# t.test(x = cleitophon_conseq$n,
#        mu = mean(republic_turn_conseq$n),
#        alternative = "two.sided",
#        conf.level = 0.95)
# 
# # F-test
# var.test(x = cleitophon_conseq$n,
#          y = (republic_turn_conseq$n),
#          alternative = "two.sided",
#          conf.level = 0.95)

# rm(cleitophon_conseq,
#    glaucon_conseq,
#    polemarchus_conseq,
#    socrates_conseq,
#    thrasymachus_conseq)

# sen_sentiment_bar <- republic_afinn %>%
#   filter(str_detect(speaker, "(Socrates|Thrasymachus)") == T) %>%
#   ggplot(aes(index, value, fill = speaker)) +
#   scale_fill_manual(values=cbPalette[c(4,5)]) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   geom_smooth(se = FALSE, show.legend = FALSE) +
#   facet_wrap(~speaker, nrow = 2, scales = "fixed") +
#   theme(axis.text.x=element_text(angle=90)) +
#   xlab("Word") +
#   ylab("Sentiment")

