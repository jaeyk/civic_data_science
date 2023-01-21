# install pkgs 

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, 
               glue, 
               here, 
               gtrendsR)

# load gtrends 

fiv_trends <- gtrends(
  keyword = "fourth industrial revolution",
  time = "2016-1-1 2023-1-1",
  low_search_volume = T)

# key list elements  
count <- fiv_trends$interest_over_time # y
date <- fiv_trends$interest_by_dma # x
country <- fiv_trends$interest_by_country

df <- data.frame(
  hits = parse_number(country$hits),
  locations = country$location
)

write_csv(df, here("outputs", "gtrends.csv"))

# visualize 
df %>%
  filter(hits >= 1) %>%
  mutate(highlight = if_else(str_detect(locations, "United States|South Korea"), 1, 0)) %>%
  mutate(highlight = factor(highlight)) %>%
  ggplot(aes(x = fct_reorder(locations, hits), y = hits,
             fill = highlight)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("black", "red")) +
  theme_light() +
  labs(y = "구글 트렌드에 따른 4차 산업혁명 검색빈도",
       x = "") +
  theme(text = element_text(size = 20))

ggsave(here("outputs", "gtrends.png"), height = 10)

# comparison


subset(df, locations == "South Korea")$hit/subset(df, locations == "United States")$hit