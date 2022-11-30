# Load packages -----------------------------------------------------------

library(ggwordcloud)
library(pals)
library(tidyverse)


# Load the current app data -----------------------------------------------

current_data <-
  list.files("data", "appdata_[0-9]{8}\\.txt", full.names = TRUE) %>%
  dplyr::last()

full_data <- read_tsv(current_data, col_types = cols()) %>%
  filter(!is.na(Molecule))


# Find most common genes/molecules ----------------------------------------

# Determine the optimal cutoff, based on having 22 unique entries to match the
# number of colours in the `pals::kelly()` palette
found_cutoff <- seq(1, 25) %>% map(
  function(x) {
    temp_df <- full_data %>%
      count(Molecule) %>%
      arrange(desc(n)) %>%
      filter(n >= x)

    if (length(unique(temp_df$n)) == 22) {
      return(x)
    } else {
      return(NULL)
    }
  }
) %>%
  discard(~is.null(.x)) %>%
  as.numeric()

top_molecules <- full_data %>%
  count(Molecule) %>%
  arrange(desc(n)) %>%
  filter(n >= found_cutoff) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(80, 20)))


# Retrieve the number of unique molecules ---------------------------------

molecule_freqs <- unique(top_molecules$n) %>% as.factor()


# Create the colours to be used in the word cloud -------------------------

colours_expanded <- kelly(n = length(molecule_freqs)) %>%
  set_names(levels(molecule_freqs))


# Create the word cloud ---------------------------------------------------

wordcloud <- ggplot(
  top_molecules,
  aes(label = Molecule, size = n, angle = angle, color = as.factor(n))
) +
  geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE) +
  scale_size_area(max_size = 14) +
  scale_color_manual(name = "n", values = colours_expanded) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

ggsave(
  plot = wordcloud,
  filename = "www/wordcloud.svg",
  units = "in",
  width = 13,
  height = 6
)
