### SENTIMENTS -----------------------------------------------------------------

# Export to excel to get avg per turn
#unnested_dialogue%>%
#  inner_join(get_sentiments("afinn")) %>%
#  group_by(turn) %>%
#  write.xlsx("republic_afinn.xlsx")

# Create the base tibble
republic_afinn <- unnested_dialogue %>%
  inner_join(get_sentiments("afinn")) %>%
  distinct(speaker, turn)

# Add index
republic_afinn <- republic_afinn %>%
  add_column(index = c(1:nrow(republic_afinn)),
             .before = "speaker")

# Get sentiment
 afinn <- read_excel("republic_afinn.xlsx", 
                     sheet = "avg")

# Add to tibble
republic_afinn <- republic_afinn %>%
  add_column(afinn)

# Drop excess
republic_afinn <- republic_afinn %>%
  select(index, speaker, turn, avg_sen)

# Rename that ugly final column
# republic_afinn <- republic_afinn %>%
#    rename(avg_sen = avg_sen.avge_sen)
# Doesn't work??? and names() says it's just avg_sen???

### Mean Median Mode and SD
# Gather turn Data
cleitophon_sen <- republic_afinn %>%
  filter(str_detect(speaker, "Cleitophon") == T)
glaucon_sen <- republic_afinn %>%
  filter(str_detect(speaker, "Glaucon") == T)
polemarchus_sen <- republic_afinn %>%
  filter(str_detect(speaker, "Polemarchus") == T)
socrates_sen <- republic_afinn %>%
  filter(str_detect(speaker, "Socrates") == T)
thrasymachus_sen <- republic_afinn %>%
  filter(str_detect(speaker, "Thrasymachus") == T)

sot <- tibble(
  speaker = c("global", "cleitophon", "glaucon", "polemarchus", "socrates", "thrasymacus"),
  mean = c(mean(republic_afinn$avg_sen),
           mean(cleitophon_sen$avg_sen),
           mean(glaucon_sen$avg_sen),
           mean(polemarchus_sen$avg_sen),
           mean(socrates_sen$avg_sen),
           mean(thrasymachus_sen$avg_sen)),
  sd = c(sd(republic_afinn$avg_sen),
         sd(cleitophon_sen$avg_sen),
         sd(glaucon_sen$avg_sen),
         sd(polemarchus_sen$avg_sen),
         sd(socrates_sen$avg_sen),
         sd(thrasymachus_sen$avg_sen))
)

### Statistical Tests ----------------------------------------------------------
sentiment_t <- c(#t.test(x = cleitophon_sen$avg_sen, mu = mean(republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value,
               t.test(x = glaucon_sen$avg_sen, mu = mean(republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value,
               t.test(x = polemarchus_sen$avg_sen, mu = mean(republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value,
               t.test(x = socrates_sen$avg_sen, mu = mean(republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value,
               t.test(x = thrasymachus_sen$avg_sen, mu = mean(republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value)

sentiment_f <- c(#var.test(x = cleitophon_sen$avg_sen, y = (republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value,
               var.test(x = glaucon_sen$avg_sen, y = (republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value,
               var.test(x = polemarchus_sen$avg_sen, y = (republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value,
               var.test(x = socrates_sen$avg_sen, y = (republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value,
               var.test(x = thrasymachus_sen$avg_sen, y = (republic_afinn$avg_sen), alternative = "two.sided", conf.level = 0.95)$p.value)

sot <- sot %>%
  add_column(p_mean = c(NA, NA, sentiment_t), .after = "mean") %>%
  add_column(p_sd = c(NA, NA, sentiment_f), .after = "sd")

rm(cleitophon_sen,
   glaucon_sen,
   polemarchus_sen,
   socrates_sen,
   thrasymachus_sen,
   sentiment_t,
   sentiment_f)

### CHARTS ---------------------------------------------------------------------
turn_sen <- republic_afinn %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=avg_sen, fill = speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=avg_sen), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  xlab("Index (Turn)") +
  ylab("Avg Sentiment")

### WHAT AFINN LOOSES ----------------------------------------------------------
afinn_loss <- select(republic_turn_conseq, -n) %>%
  filter((turn %in% republic_afinn$turn) == FALSE)

# Calculate Population Variance from Sample (Statology)
n <- length(republic_afinn$avg_sen)
pop_var <- var(republic_afinn$avg_sen) * (n-1)/n
afinn_var <- var(republic_afinn$avg_sen)

