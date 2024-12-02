library(tidyverse)
library(readr)
library(readxl)
library(janitor)

load("input/tmort5F.RData")

pob <- read_xlsx("input/0_Pob_Mitad_1950_2070.xlsx") %>%
  clean_names() %>%
  select(-renglon)

names(pob) <- c("year", "edo", "cve_geo", "age", "sex", "pop")

pobF <- pob %>%
  filter(sex == "Mujeres")

nacs <- read_xlsx("input/4_Tasas_Especificas_Fecundidad_proyecciones.xlsx") %>%
  clean_names() %>%
  select(-renglon) %>%
  filter(cve_geo == 0) %>% 
  select(-entidad, -cve_geo) %>% 
  mutate(id = paste(ano, gpo_edad, sep = "_"))

names(nacs) <- c("year", "age5", "fx", "Bx", "id")

bins <- seq(15, 50, by = 5)

labels <- paste(bins[-length(bins)],
                bins[-1] - 1,
                sep = "-"
)

pobF5 <- pobF %>%
  filter(age >= 15, age < 50) %>%
  mutate(age5 = cut(age, breaks = bins, labels = labels, right = F)) %>%
  group_by(year, edo, cve_geo, age5) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  filter(cve_geo == 0) %>% 
  select(year, age5, pop) %>% 
  mutate(id = paste(year, age5, sep = "_"))

tmort5F <- tmort5F %>% 
  mutate(id = paste(year, age5, sep = "_")) %>% 
  filter(as.numeric(sub("-.*", "", age5)) >= 15 & as.numeric(sub("-.*", "", age5)) < 50)

base_repr <- full_join(
  full_join(
  pobF5,
  nacs,
  by = "id"
),
tmort5F, 
by = "id") %>%
  select(year, age5, pop, Bx, Lx = Lx5) %>% 
  mutate(BxF = Bx * (1 / 2.05),
         fxF = BxF / pop,
         fxFLxF = fxF * Lx)

reprod_meas <- base_repr %>% 
  group_by(year) %>% 
  summarise(GRR = 5 * sum(fxF),
            NRR = sum(fxFLxF),
            TGF = 2.05 * GRR,
            pAm = NRR * 2.05 / TGF)

reprod_meas %>% 
  ggplot() +
  geom_line(aes(x = year, y = GRR)) +
  geom_line(aes(x = year, y = NRR), linetype = "dashed") +
  geom_line(aes(x = year, y = TGF), linetype = "dashed") +
  scale_y_continuous(n.breaks = 10) +
  labs(y = "")


reprod_meas %>% 
  ggplot() +
  geom_line(aes(x = year, y = pAm))
