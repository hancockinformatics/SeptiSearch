
# Load packages -----------------------------------------------------------

library(ggwordcloud)
library(pals)
library(tidyverse)


# Load the current app data -----------------------------------------------

current_data <-
  list.files("data", "fulldata_[0-9]{8}\\.txt", full.names = TRUE) %>%
  dplyr::last()

full_data <- read_tsv(current_data, col_types = cols()) %>%
  filter(!is.na(Molecule))


# Find most common genes/molecules ----------------------------------------

# Recommended trying different filter thresholds here to get the number of
# levels (and therefore colours) down to 22 or less, so we can use the
# `pals::kelly()` colour palette.
top_molecules <- full_data %>%
  count(Molecule) %>%
  arrange(desc(n)) %>%
  filter(n >= 18) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(80, 20)))


# Retrieve the number of unique molecules ---------------------------------

molecule_freqs <- unique(top_molecules$n) %>% as.factor()

if (length(molecule_freqs) > 22) {
  stop("Too many levels; Increase the filter above to achieve 22 levels.")
}


# Create the colours to be used in the wordcloud --------------------------

colours_expanded <- kelly(n = length(molecule_freqs)) %>%
  set_names(levels(molecule_freqs))


# Create the wordcloud ----------------------------------------------------

ggplot(
  top_molecules,
  aes(label = Molecule, size = n, angle = angle, color = as.factor(n))
) +
  geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE) +
  scale_size_area(max_size = 14) +
  scale_color_manual(name = "n", values = colours_expanded) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

ggsave("www/wordcloud.svg", units = "in", width = 13, height = 6)
