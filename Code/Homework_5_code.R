library(tidyverse)
library(here)
library(broom)
library(MASS)
library(kableExtra)

kablize <- function(tab, digits = 3) {
  kable(tab,digits = digits, booktabs = T) %>% 
    kableExtra::kable_styling(latex_options = c("hold_position", "striped"), position = "center")
}

cereal_dat <- read.csv(here("DataRaw/Cereal2.csv"))

cereal_dat <- cereal_dat %>%
  filter(FamMem == 3) %>% 
  dplyr::select(C1, Sex, Cond, Wt1) %>% 
  mutate(Sex = if_else(Sex == 0, "M", "F"),
         Cond = if_else(Cond == 0, "Ctl", "Trt"))

mean(cereal_dat$C1)

var(cereal_dat$C1)
#Model 1
pois_mod <- glm(C1 ~ Cond + Wt1 + Sex, data = cereal_dat, family = "poisson")

pois_res <- broom::tidy(pois_mod, exp = T, conf.int = T)%>% 
  mutate(conf_int = paste("(", round(conf.low, 3), ", ", round(conf.high, 3), ")", sep = ""))

#Model 2
quasipois_mod <- glm(C1 ~ Cond + Wt1 + Sex, data = cereal_dat, family = "quasipoisson")

quasipois_res <- broom::tidy(quasipois_mod, exp = T, conf.int = T) %>% 
  mutate(conf_int = paste("(", round(conf.low, 3), ", ", round(conf.high, 3), ")", sep = ""))

#Model 3
nb_mod <- glm.nb(C1 ~ Cond + Wt1 + Sex, data = cereal_dat)

nb_res <- broom::tidy(nb_mod, exp = T, conf.int = T)%>% 
  mutate(conf_int = paste("(", round(conf.low, 3), ", ", round(conf.high, 3), ")", sep = ""))

pois_res %>% kablize()

quasipois_res %>% kablize()

nb_res %>% kablize()

QIC(pois_mod)
