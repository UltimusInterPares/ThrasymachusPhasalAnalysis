### RIDICULOUSLY GOOD CORRELATION COEFFICIENT BTWN DENSITY & LENGTH WHO KNEW ---
cor(republic_turn_conseq$n, republic_turn_length$length_words)

### SIZE INDEX -----------------------------------------------------------------
size_index <- c()

for (i in c(1:nrow(republic_turn_conseq))) {
  size_index[i] <-
    mean(c(republic_turn_conseq$n[i],
           republic_turn_length$length_words[i])) %>% print()

}


### COMBO TABLE ----------------------------------------------------------------
republic_size <- republic_turn_length %>%
  add_column(density = republic_turn_conseq$n, .after="length_words") %>%
  add_column(size_index = size_index, .after="density")

# Size Index Lines
republic_size_line <- ggplot(data = republic_size) +
  geom_smooth(mapping = aes(x=index, y=density), se = F, method = "loess", color = cbPalette[3]) +
  geom_smooth(mapping = aes(x=index, y=length_words), se = F, method = "loess", color = cbPalette[4]) +
  geom_smooth(mapping = aes(x=index, y=size_index), se = F, method = "loess", color = cbPalette[5], linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn") +
  ylab("Length/Density")

# Size Index Bar
size_index <-  ggplot(data = republic_size) +
  scale_fill_manual(values=cbPalette) +
  geom_bar(mapping = aes(x=index, y=size_index, fill =speaker), stat = "identity") +
  geom_smooth(mapping = aes(x=index, y=size_index), se = F, method = "loess", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn") +
  ylab("Size")
  
  
  
  ylab("Length/Density")