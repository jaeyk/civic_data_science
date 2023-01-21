# install pkgs 

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, 
               glue, 
               here, 
               DeclareDesign)

# big data and the illusion 
# reference: https://declaredesign.org/blog/with-great-power-comes-great-responsibility.html

N <- 100 

big_data_design <- 
  
  # model
  declare_model(
    
    N = N, 
    u = rnorm(N), 
    e = rnorm(N, sd = 4), 
    Z = rbinom(N, size = 1, prob = pnorm(u)), 
    potential_outcomes(Y ~ u + e)
    
  ) + 
  
  # outcomes 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +

  # null effect
  declare_inquiry(ATE = 0) +
  
  # diff in means for the estimator 
  declare_estimator(Y ~ Z, 
                    model = difference_in_means, 
                    inquiry = "ATE")

# diagnosis 
diags <- declare_diagnosands(bias = mean(estimate - estimand),
                             power = mean(p.value <= 0.05))

designs <- redesign(big_data_design, 
                    N = seq(100, 10000, by = 500))

diagnosis <- diagnose_design(designs, diagnosands = diags)

# simulations 
sims <- get_simulations(diagnosis)
sims_power <- get_diagnosands(diagnosis)

write_csv(sims, here("outputs", "sims.csv"))
write_csv(sims_power, here("outputs", "sims_power.csv"))

sims_power %>%
  ggplot(aes(x = N, y = power)) +
  geom_point(alpha = 0.7) + 
  geom_line() +
  theme_light() +
  geom_hline(yintercept = 0.5, col = "red", linetype = "dashed") +
  geom_text(data = data.frame(
    
    N = 6000, 
    power = 0.55, 
    label = "참인 검정력 (true power) = 0.5"
    
  ), aes(label = label), 
  size = 10) +
  theme(text = element_text(size = 20)) +
  labs(x = "표본 크기", 
       y = "검정력 (power)")

ggsave(here("outputs", "power.png"))
