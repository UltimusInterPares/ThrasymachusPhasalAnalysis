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

# Uh oh somebody can't figure out what to do if a number doesn't repeat
# Add in NA for non-existent modes?
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  
  if (length(u[tab == max(tab)]) > 1) {NA}
  else {u[tab == max(tab)]}
  
}

# Oh damn we're correlating
cor(republic_turn_conseq$n, republic_turn_length$length_words) # Unreasonably high correlation coefficient?

ggplot() +
  geom_smooth(data = republic_turn_conseq, mapping = aes(x=index, y=n), se = F, method = "loess", color = "red") +
  geom_smooth(data = republic_turn_length, mapping = aes(x=index, y=length_words), se = F, method = "loess", color = "blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn") +
  ylab("Length/Density")


# Trying to get avg sentiment per sentence

# Disecting turn length to find a point where I can hijack it and make it
# an average sentiment count

unnested_dialogue %>%
  count(reorder(turn, index), speaker, sort = FALSE)

# It's tallying values, now we just need to swap it to either a sum or an avg
unnested_dialogue %>%
  inner_join(get_sentiments("afinn")) %>%
  add_count(value, turn, sort = F)

# Counts but also reorders
unnested_dialogue%>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(turn) %>%
  summarise_at(vars(value), list(name = mean))


unnested_dialogue%>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(turn) %>%
  write.xlsx("republic_afinn.xlsx")

unnested_dialogue %>%
  distinct(speaker, turn)

distinct(unnested_dialogue, speaker, turn)


# Phase 1 Data
# First 150 turns (Lengths)
temp_length <- republic_turn_length[c(1:150),] %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=reorder(turn,index), y=length_words, fill = speaker), stat = "identity") +
  theme(axis.text.x = element_blank()) +
  xlab("Turn") +
  ylab("Length (Words)")

temp_conseq <- republic_turn_conseq[c(1:150),] %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=reorder(turn,index), y=n, fill=speaker), stat = "identity") +
  theme(axis.text.x = element_blank()) +
  xlab("Turn") +
  ylab("Density (Sentences)")

temp_index <- republic_size[c(1:150),] %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=reorder(turn,index), y=size_index, fill =speaker), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn") +
  ylab(TeX("Size $\\left(\\frac{L+D}{2}\\right)$"))

print(temp_length / temp_conseq / temp_index)

# F-Test for Standard Deviations
for (i in c(2:6)) {
  val <- var.test(c(dot$sd[1], dot$sd[i]), alternative = "greater")$p.value
  paste(dot$speaker[i], val, sep = ": ") %>%
    print()
}

for (i in c(2:6)) {
  val <- t.test(c(dot$sd[1], dot$sd[i]))$p.value
  paste(dot$speaker[i], val, sep = ": ") %>%
    print()
}

t.test(x = cleitophon_conseq$n,
       mu = mean(republic_turn_conseq$n),
       alternative = "two.sided",
       conf.level = 0.95)

var.test(x = cleitophon_conseq$n,
         y = (republic_turn_conseq$n),
         alternative = "two.sided",
         conf.level = 0.95)


republic_turn_conseq %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=n, fill=speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=n), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(0.85, 0.815),
        legend.background = element_rect(fill = "white", color = "black")) +
  xlab("Turn (Index)") +
  ylab("Sentences")

republic_afinn %>%
  ggplot() +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=avg_sen, fill = speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=avg_sen), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Key") +
  ylab("Avg Sentiment")


# Finding out what AFINN cuts


filter(republic_turn_conseq, str_detect(turn, republic_afinn$turn) == F)
filter(republic_turn_conseq, str_detect(turn, paste0(c(as.character(republic_afinn$turn[1]), "$"), collapse = "")) == T)



placeholder <- c("a", "b", "c")
placeholder2 <- c("c",)
filter(placeholder, str_detect(turn, placeholder2) == F) %>% print()
rm(placeholder, placeholder2)

republic_afinn$turn[i] 