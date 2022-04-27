### SENTIMENTS -----------------------------------------------------------------

# Generate AFINN Sentiment Analysis
# Re-index to maintain order later on
republic_afinn <- unnested_dialogue %>%
  inner_join(get_sentiments("afinn"))
republic_afinn$index <- c(1:nrow(republic_afinn))

sentence_sentiment <- republic_afinn %>%
  ggplot(mapping = aes(x=index, y=value, fill=speaker)) +
  scale_fill_manual(values=cbPalette) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Sentence") +
  ylab("Sentiment") +
  theme_classic()

# Global Sentiment
global_sentiment <- republic_afinn %>%
  ggplot(mapping = aes(x=reorder(turn,index), y=value, fill=speaker)) +
  scale_fill_manual(values=cbPalette) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept=mean(value)), color="blue", size=.5) +
  geom_hline(aes(yintercept=0), size=.5) +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Turn") +
  ylab("Sentiment")  

turn_sentiment_box <- republic_afinn %>%
  filter(str_detect(speaker, "(Socrates|Thrasymachus)") == T) %>%
  ggplot(aes(reorder(turn, index), value, color = speaker)) +
  scale_color_manual(values=cbPalette[c(4,5)]) +
  geom_boxplot(show.legend = FALSE) +
  geom_smooth(aes(y=value), se = FALSE, show.legend = FALSE) +
  facet_wrap(~speaker, nrow = 2, scales = "fixed") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Turn") +
  ylab("Sentiment")

turn_sentiment_bar <- republic_afinn %>%
  filter(str_detect(speaker, "(Socrates|Thrasymachus)") == T) %>%
  ggplot(aes(reorder(turn, index), value, fill = speaker)) +
  scale_fill_manual(values=cbPalette[c(4,5)]) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_smooth(se = FALSE, show.legend = FALSE) +
  facet_wrap(~speaker, nrow = 2, scales = "fixed") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Turn") +
  ylab("Sentiment")

sen_sentiment_bar <- republic_afinn %>%
  filter(str_detect(speaker, "(Socrates|Thrasymachus)") == T) %>%
  ggplot(aes(index, value, fill = speaker)) +
  scale_fill_manual(values=cbPalette[c(4,5)]) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_smooth(se = FALSE, show.legend = FALSE) +
  facet_wrap(~speaker, nrow = 2, scales = "fixed") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Word") +
  ylab("Sentiment")

### Seperate Sentiments by Turn ------------------------------------------------

# These may become helpful in the future, but right now there doesn't seem to be
# any reason for separating Socrates and Thrasymachus out like this, given that
# the analysis is supposed to be contrastive.

# Thrasymachus' turns
thrasymachus_turns <- republic_afinn %>%
  filter(str_detect(speaker, "Thrasymachus") == T)

# Sentiment for Thrasymachus
thrasymachus_sentiment <- thrasymachus_turns %>%
  ggplot(mapping = aes(x=reorder(turn,index), y=value, color=speaker)) +
  scale_color_manual(values=cbPalette[5]) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept=mean(value)), color="blue", size=.5) +
  geom_hline(aes(yintercept=0), size=.5) +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Turn") +
  ylab("Sentiment")

# Socrates' turns
socrates_turns <- republic_afinn %>%
  filter(str_detect(speaker, "Socrates") == T)

# Sentiment for Socrates
socrates_sentiment <- socrates_turns %>%
  ggplot(mapping = aes(x=reorder(turn,index), y=value, color=speaker)) +
  scale_color_manual(values=cbPalette[4]) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept=mean(value)), color="blue", size=.5) +
  geom_hline(aes(yintercept=0), size=.5) +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Turn") +
  ylab("Sentiment")