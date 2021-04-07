
# Load packages -----------------------------------------------------------

library(ggwordcloud)
library(RColorBrewer)
library(tidyverse)


# Load the current app data -----------------------------------------------

current_data <-
  list.files("data", "fulldata_[0-9]{8}\\.txt", full.names = TRUE) %>%
  dplyr::last()

full_data <- read_tsv(current_data, col_types = cols()) %>%
  filter(!is.na(Molecule))


# Find most common genes/molecules ----------------------------------------

mytext <- full_data %>%
  group_by(Molecule) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n >= 15) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(70, 30)))


# Retrieve the number of unique molecules ---------------------------------

all_nums <- unique(mytext$n) %>% as.factor()


# Create the colours to be used in the wordcloud --------------------------

set.seed(4); colours_expanded <-
  colorRampPalette(brewer.pal(8, "Dark2"), bias = 2)(length(all_nums)) %>%
  sample(x = ., size = length(all_nums), replace = FALSE) %>%
  set_names(levels(all_nums))


# Create the wordcloud ----------------------------------------------------

ggplot(mytext, aes(label = Molecule, size = n, angle = angle, color = as.factor(n))) +
  geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE) +
  scale_size_area(max_size = 14) +
  scale_color_manual(name = "n", values = colours_expanded) +
  theme_minimal()

# Recommend saving the image using the GUI from the RStudio plot pane, as the
# words are shifted around depending on the output image size. May need to
# adjust the SVG manually with a vector editor, for example Inkscape.
