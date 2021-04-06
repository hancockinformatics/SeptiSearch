
library(ggwordcloud)
library(RColorBrewer)
library(tidyverse)


mytext <- read_tsv("data/septisearch_words.txt", col_names = "molecule") %>%
  group_by(molecule) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n >= 15) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(70, 30)))


all_nums <- unique(mytext$n) %>% as.factor()

set.seed(4); colours_expanded <-
  colorRampPalette(brewer.pal(8, "Dark2"), bias = 2)(length(all_nums)) %>%
  sample(x = ., size = length(all_nums), replace = FALSE) %>%
  set_names(levels(all_nums))

ggplot(mytext, aes(label = molecule, size = n, angle = angle, color = as.factor(n))) +
  geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE) +
  scale_size_area(max_size = 14) +
  scale_color_manual(name = "n", values = colours_expanded) +
  theme_minimal()

# Recommend saving the image using the GUI from the plot pane, as the words
# are shifted around depending on the output image size. May need to adjust the
# SVG manually with for e.g. Inkscape.
