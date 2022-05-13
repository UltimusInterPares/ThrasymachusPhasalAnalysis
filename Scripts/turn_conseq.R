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
  speaker = speaker,
  mean = c(mean(republic_turn_conseq$n),
                mean(cleitophon_conseq$n),
                mean(glaucon_conseq$n),
                mean(polemarchus_conseq$n),
                mean(socrates_conseq$n),
                mean(thrasymachus_conseq$n)),
  sd = c(sd(republic_turn_conseq$n),
         sd(cleitophon_conseq$n),
         sd(glaucon_conseq$n),
         sd(polemarchus_conseq$n),
         sd(socrates_conseq$n),
         sd(thrasymachus_conseq$n))
)

### Statistical Tests ----------------------------------------------------------
density_tests <- tibble(speaker = speaker[2:6])

density_t <- c(t.test(x = cleitophon_conseq$n, mu = mean(republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value,
               t.test(x = glaucon_conseq$n, mu = mean(republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value,
               t.test(x = polemarchus_conseq$n, mu = mean(republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value,
               t.test(x = socrates_conseq$n, mu = mean(republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value,
               t.test(x = thrasymachus_conseq$n, mu = mean(republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value)

density_f <- c(var.test(x = cleitophon_conseq$n, y = (republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value,
               var.test(x = glaucon_conseq$n, y = (republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value,
               var.test(x = polemarchus_conseq$n, y = (republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value,
               var.test(x = socrates_conseq$n, y = (republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value,
               var.test(x = thrasymachus_conseq$n, y = (republic_turn_conseq$n), alternative = "two.sided", conf.level = 0.95)$p.value)

dot <- dot %>%
  add_column(p_mean = c(NA, density_t), .after = "mean") %>%
  add_column(p_sd = c(NA, density_f), .after = "sd")

# density_tests <- density_tests %>% 
#   add_column(p_t = density_t) %>%
#   add_column(p_F = density_f)

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

# Graph of Turns
turn_conseq <- republic_turn_conseq %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=n, fill=speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=n), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  xlab("Index (Turn)") +
  ylab("Density (Sentences)")
