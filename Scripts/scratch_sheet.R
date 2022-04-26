republic_turn_conseq %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=n, fill=speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=n), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn (Index)") +
  ylab("Sentences")


# DIRTY DATA THRASY DOESN'T TAKE THREE TURNS IN A ROW
republic_turn_conseq[c(225:250),] %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=n, fill=speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=n), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn (Index)") +
  ylab("Sentences")
