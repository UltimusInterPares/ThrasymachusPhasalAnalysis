### CONSECUTIVE SENTENCES ------------------------------------------------------
# Consecutive Sentences by Turn
republic_turn_conseq <- nested_dialogue %>% 
  add_count(turn) %>%
  distinct(speaker, turn, n)

republic_turn_conseq <- republic_turn_conseq %>%
  add_column(index = c(1:nrow(republic_turn_conseq)),
             .before = "speaker")

# Gather turn Data
cleitophon_conseq <- republic_turn_conseq %>%
  filter(str_detect(speaker, "Cleitophon") == T)
glaucon_conseq <- republic_turn_conseq %>%
  filter(str_detect(speaker, "Glaucon") == T)
polemarchus_conseq <- republic_turn_conseq %>%
  filter(str_detect(speaker, "Polemarchus") == T)
socrates_conseq <- republic_turn_conseq %>%
  filter(str_detect(speaker, "Socrates") == T)
thrasymachus_conseq <- republic_turn_conseq %>%
  filter(str_detect(speaker, "Thrasymachus") == T)

# Generate table from data
dot <- tibble(
  speaker = c("global", "cleitophon", "glaucon", "polemarchus", "socrates", "thrasymacus"),
  mean = c(mean(republic_turn_conseq$n),
                mean(cleitophon_conseq$n),
                mean(glaucon_conseq$n),
                mean(polemarchus_conseq$n),
                mean(socrates_conseq$n),
                mean(thrasymachus_conseq$n)),
  median = c(median(republic_turn_conseq$n),
             median(cleitophon_conseq$n),
             median(glaucon_conseq$n),
             median(polemarchus_conseq$n),
             median(socrates_conseq$n),
             median(thrasymachus_conseq$n)),
  mode = c(find_mode(republic_turn_conseq$n),
           find_mode(cleitophon_conseq$n),
           find_mode(glaucon_conseq$n),
           find_mode(polemarchus_conseq$n),
           find_mode(socrates_conseq$n),
           find_mode(thrasymachus_conseq$n)),
  sd = c(sd(republic_turn_conseq$n),
         sd(cleitophon_conseq$n),
         sd(glaucon_conseq$n),
         sd(polemarchus_conseq$n),
         sd(socrates_conseq$n),
         sd(thrasymachus_conseq$n))
)

rm(cleitophon_conseq,
   glaucon_conseq,
   polemarchus_conseq,
   socrates_conseq,
   thrasymachus_conseq)

# Graph of Turns
turn_conseq <- republic_turn_conseq %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=n, fill=speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=n), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn (Index)") +
  ylab("Sentences")


# T Tests ----------------------------------------------------------------------
# Quick p-values of means
for (i in c(2:6)) {
  val <- t.test(c(dot$mean[1], dot$mean[i]))$p.value
  paste(dot$speaker[i], val, sep = ": ") %>%
    print()
}

# Quick p-values of standard deviations
for (i in c(2:6)) {
  val <- t.test(c(dot$sd[1], dot$sd[i]))$p.value
  paste(dot$speaker[i], val, sep = ": ") %>%
    print()
}


# P values for each individual turn.
conseq_p <- c()
for (i in c(1:nrow(republic_turn_conseq))) {
  conseq_p[i] <- t.test(c(dot$mean[1], republic_turn_conseq$n[i]))$p.value
}

republic_turn_conseq_p <- republic_turn_conseq %>%
  add_column(p_val = conseq_p, .after = "n")


