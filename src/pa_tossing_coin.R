
# install pkgs 

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, 
               glue, 
               here, 
               DeclareDesign)

# coin flipping 

flip_coin <- function(x) {

 out <- mean(rbinom(x, 1, .5))

 return(out)
}

# rerun 

nums <- 1:100
outcomes <- map_dbl(nums, ~flip_coin(.))
  
df <- data.frame(nums, 
                 outcomes)

df %>%
  ggplot(aes(x = nums, y = outcomes)) +
    geom_point() +
    labs(x = "동전을 던짓 횟수",
         y = "윗면이 나올 확률 (%)") +
    theme(text = element_text(size = 40)) +
    theme_light()
  
ggsave(here("outputs", "coin_flipping.png"), height = 5, width = 5)