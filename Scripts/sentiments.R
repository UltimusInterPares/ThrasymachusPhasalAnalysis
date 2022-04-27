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
  median = c(median(republic_afinn$avg_sen),
             median(cleitophon_sen$avg_sen),
             median(glaucon_sen$avg_sen),
             median(polemarchus_sen$avg_sen),
             median(socrates_sen$avg_sen),
             median(thrasymachus_sen$avg_sen)),
  mode = c(find_mode(republic_afinn$avg_sen),
           find_mode(cleitophon_sen$avg_sen),
           find_mode(glaucon_sen$avg_sen),
           find_mode(polemarchus_sen$avg_sen),
           find_mode(socrates_sen$avg_sen),
           find_mode(thrasymachus_sen$avg_sen)),
  sd = c(sd(republic_afinn$avg_sen),
         sd(cleitophon_sen$avg_sen),
         sd(glaucon_sen$avg_sen),
         sd(polemarchus_sen$avg_sen),
         sd(socrates_sen$avg_sen),
         sd(thrasymachus_sen$avg_sen))
)

rm(cleitophon_sen,
   glaucon_sen,
   polemarchus_sen,
   socrates_sen,
   thrasymachus_sen)

# T Tests ----------------------------------------------------------------------
# Quick p-values of means
for (i in c(2:6)) {
  val <- t.test(c(sot$mean[1], sot$mean[i]))$p.value
  paste(sot$speaker[i], val, sep = ": ") %>%
    print()
}

# # Quick p-values of standard deviations
# for (i in c(2:6)) {
#   val <- t.test(c(sot$sd[1], sot$sd[i]))$p.value
#   paste(sot$speaker[i], val, sep = ": ") %>%
#     print()
# }

### CHARTS ---------------------------------------------------------------------
turn_sen <- republic_afinn %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=avg_sen, fill = speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=avg_sen), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Key") +
  ylab("Avg Sentiment")

republic_afinn %>%
  ggplot(mapping = aes(x=index, y=avg_sen)) +
  geom_line()