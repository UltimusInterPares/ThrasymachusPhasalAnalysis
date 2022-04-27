### ZOOMING IN ON TURNING POINTS
size_100 <- ggplot(data = republic_size[c(100:150),]) +
  scale_fill_manual(values=cbPalette[c(2,4,5)]) +
  geom_bar(mapping = aes(x=index, y=size_index, fill =speaker), stat = "identity") +
  geom_smooth(mapping = aes(x=index, y=size_index), se = F, method = "loess", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn") +
  ylab("Size")

# T40 : S73 (100 to 149)
# Corresponds to Afinn 54 - 94
# 342d7 - 348e5
# Glaucon jumps in 347e6 (g4)
sen_size_100 <- ggplot(data = republic_afinn[c(54:95),]) +
  scale_fill_manual(values=cbPalette[c(2,4,5)]) +
  geom_bar(mapping = aes(x=index, y=avg_sen, fill = speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=avg_sen), se = F, method = "loess") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Key") +
  ylab("Avg Sentiment")

# Indices:
# Glaucon's turn ends: G8 (348b6)
# size_100 index 132
# afinn only has G1 and G2
# afinn_size_100 indices 9 and 78




### FIRST TURNING POINTS -------------------------------------------------------
size_100_turn <- ggplot(data = republic_size[c(100:150),]) +
  scale_fill_manual(values=cbPalette[c(2,4,5)]) +
  geom_bar(mapping = aes(x=index, y=size_index, fill =speaker), stat = "identity") +
  geom_smooth(mapping = aes(x=index, y=size_index), se = F, method = "loess", linetype = "dashed") +
  geom_vline(xintercept = 120) +
  geom_vline(xintercept = 132) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Turn") +
  ylab("Size")

sen_size_100_turn <- ggplot(data = republic_afinn[c(54:95),]) +
  scale_fill_manual(values=cbPalette[c(2,4,5)]) +
  geom_bar(mapping = aes(x=index, y=avg_sen, fill = speaker), stat = "identity") + # prevents geom_bar() from reorganizing
  geom_smooth(mapping = aes(x=index, y=avg_sen), se = F, method = "loess") +
  geom_vline(xintercept = 78) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Key") +
  ylab("Avg Sentiment")


