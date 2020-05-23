library(tidyverse)
library(lubridate)
library(scales)
library(readxl)

theme_set(theme_bw())

# Supp data from https://www.nature.com/articles/nprot.2017.149
"https://static-content.springer.com/esm/art%3A10.1038%2Fnprot.2017.149/MediaObjects/41596_2018_BFnprot2017149_MOESM17_ESM.xlsx" %>%
  download.file(here::here("lectures/data/scRNATimeline.xlsx"))

pdf(here::here("lectures/figures/scRNATimeline.pdf"), width = 12, height = 6)
here::here("lectures/data/scRNATimeline.xlsx") %>%
  read_excel() %>%
  mutate(
    Technique = str_replace_all(Technique, "Tang.*", "Tang et al")
  ) %>%
  group_by(Technique) %>% 
  dplyr::filter(!all(is.na(Isolation))) %>%
  mutate(Isolation = zoo::na.locf(Isolation)) %>% 
  ungroup() %>%
  mutate(Isolation = fct_inorder(Isolation)) %>% 
  ggplot(aes(as.Date(Date), `Reported Cells Total`, colour = Isolation)) + 
  geom_point() + 
  geom_text_repel(aes(label = Technique), show.legend = FALSE) + 
  scale_y_log10(breaks = 10^(1:6), label = comma) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 year", labels = lubridate::year) + 
  labs(x = "Publication Date", colour = "Isolation Strategy") +
  theme(
    legend.position = c(0,1),
    legend.justification = c(0,1),
    legend.background = element_rect(colour = "grey50")
  )
dev.off()
