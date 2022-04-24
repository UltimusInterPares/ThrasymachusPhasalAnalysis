### LENGTHS --------------------------------------------------------------------
# Turn and Sentence Length
# count() is essentially group_by(a, b) %>% summarise(n = n())
# summarise truncates and sorts
# add_count() swaps summarise for mutate, which does neither
# add_count() %>% distinct effectively truncates without resorting
# also adding an index
republic_length <- unnested_dialogue %>%
  add_count(turn, sentence, sort = FALSE) %>%
  distinct(speaker, turn, sentence, n)
republic_length <- republic_length  %>%
  add_column(index = c(1:nrow(republic_length)),
             .before = "speaker")

# makes a stacked bar plot of turn lengths
# stacks of sentences by length
turn_length <- republic_length %>%
  ggplot(mapping = aes(x=reorder(turn, index), y=n, color = speaker)) +
  scale_color_manual(values=cbPalette) +
  geom_bar(stat = "identity") + # prevents geom_bar() from reorganizing
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn") +
  ylab("Length")