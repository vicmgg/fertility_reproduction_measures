library(tidyverse)
library(readr)
library(readxl)
library(janitor)

pob <- read_xlsx("input/0_Pob_Mitad_1950_2070.xlsx") %>%
  clean_names() %>%
  select(-renglon)

names(pob) <- c("year", "edo", "cve_geo", "age", "sex", "pop")

pobF <- pob %>%
  filter(sex == "Mujeres")

nacs <- read_xlsx("input/4_Tasas_Especificas_Fecundidad_proyecciones.xlsx") %>%
  clean_names() %>%
  select(-renglon) %>%
  mutate(id = paste(ano, cve_geo, gpo_edad, sep = "_"))

names(nacs) <- c("year", "edo", "cve_geo", "age_5", "fx", "Bx", "id")

# tbn ----

pob_tot <- pob %>%
  group_by(year, edo, cve_geo) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  mutate(id = paste(year, cve_geo, sep = "_"))

nacs_tot <- nacs %>%
  group_by(year, edo, cve_geo) %>%
  summarise(B = sum(Bx), .groups = "drop") %>%
  mutate(id = paste(year, cve_geo, sep = "_"))

cbr <- full_join(
  pob_tot,
  nacs_tot,
  by = "id"
) %>%
  select(year = year.x, edo = edo.x, edo_id = cve_geo.x, pop, B) %>%
  mutate(cbr = 1000 * B / pop)

cbr %>% 
  filter(edo_id == 0) %>% 
  ggplot(aes(x = year, y = cbr)) +
  geom_line(color = "blue") +
  theme_light() +
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20)

cbr %>% 
  filter(cbr == max(cbr))

cbr %>% 
  filter(year == 1970) %>% 
  filter(cbr == min(cbr))

cbr %>% 
  ggplot() +
  geom_line(aes(x = year, y = cbr, group = edo), color = "gray") +
  geom_line(data = cbr %>% 
              filter(edo_id == 7), 
            aes(x = year, y = cbr), color = "darkred") +
  geom_line(data = cbr %>% 
              filter(edo_id == 3), 
            aes(x = year, y = cbr), color = "darkgreen") +
  geom_line(data = cbr %>% 
              filter(edo_id == 9), 
            aes(x = year, y = cbr), color = "blue") +
  geom_line(data = cbr %>% 
              filter(edo_id == 0), 
            aes(x = year, y = cbr), color = "black") +
  theme_light() +
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20)

# agrupacion por edades quinquenales ----
bins <- seq(15, 50, by = 5)

labels <- paste(bins[-length(bins)],
  bins[-1] - 1,
  sep = "-"
)

pob_5 <- pob %>%
  filter(age >= 15, age < 50) %>%
  mutate(age_5 = cut(age, breaks = bins, labels = labels, right = F)) %>%
  group_by(year, edo, cve_geo, age_5) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  mutate(id = paste(year, cve_geo, age_5, sep = "_"))

base_nacs0 <- full_join(
  pob_5,
  nacs,
  by = "id"
) %>%
  select(year = year.x, edo = edo.x, edo_id = cve_geo.x, age_5 = age_5.x, pop, Bx) %>% 
  mutate(fx0 = Bx / pop) %>% 
  group_by(year, edo) %>% 
  mutate(cx = pop / sum(pop)) %>% 
  ungroup() %>% 
  group_by(age_5) %>% 
  mutate(cxs = mean(cx)) %>% 
  ungroup() %>% 
  mutate(fxcx = fx0 * cx,
         fxcxs = fx0 * cxs)

nacs_estan <- base_nacs0  %>% 
  group_by(year, edo, edo_id) %>% 
  summarise(cbr = 1000 * sum(fxcx),
            ascbr = 1000 * sum(fxcxs), .groups = "drop")

nacs_estan %>% 
  filter(ascbr == max(ascbr))

nacs_estan %>% 
  filter(year == 1970) %>% 
  filter(ascbr == min(ascbr))

nacs_estan %>% 
  ggplot() +
  geom_line(aes(x = year, y = ascbr, group = edo), color = "gray") +
  geom_line(data = nacs_estan %>% 
              filter(edo_id == 20), 
            aes(x = year, y = ascbr), color = "darkred") +
  geom_line(data = nacs_estan %>% 
              filter(edo_id == 19), 
            aes(x = year, y = ascbr), color = "darkgreen") +
  geom_line(data = nacs_estan %>% 
              filter(edo_id == 9), 
            aes(x = year, y = ascbr), color = "blue") +
  geom_line(data = nacs_estan %>% 
              filter(edo_id == 0), 
            aes(x = year, y = ascbr), color = "black") +
  theme_light() +
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20)

# GFR ----
pobF_5 <- pobF %>%
  filter(age >= 15, age < 50) %>%
  mutate(age_5 = cut(age, breaks = bins, labels = labels, right = F)) %>%
  group_by(year, edo, cve_geo, age_5) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  mutate(id = paste(year, cve_geo, age_5, sep = "_"))

base_nacs <- full_join(
  pobF_5,
  nacs,
  by = "id"
) %>%
  select(year = year.x, edo = edo.x, edo_id = cve_geo.x, age_5 = age_5.x, pop, Bx)

GFR <- base_nacs %>% 
  group_by(year, edo, edo_id) %>% 
  summarise(GFR = 1000 * sum(Bx) / sum(pop), .groups = "drop")

GFR %>% 
  filter(year == 1970) %>% 
  filter(GFR == min(GFR))

GFR %>% 
  ggplot() +
  geom_line(aes(x = year, y = GFR, group = edo), color = "gray") +
  geom_line(data = GFR %>% 
              filter(edo_id == 12), 
            aes(x = year, y = GFR), color = "darkred") +
  geom_line(data = GFR %>% 
              filter(edo_id == 2), 
            aes(x = year, y = GFR), color = "darkgreen") +
  geom_line(data = GFR %>% 
              filter(edo_id == 9), 
            aes(x = year, y = GFR), color = "blue") +
  geom_line(data = GFR %>% 
              filter(edo_id == 0), 
            aes(x = year, y = GFR), color = "black") +
  theme_light() +
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20)

tef <- base_nacs %>% 
  mutate(fx = Bx / pop)

tef %>% 
  filter(year == 2000) %>% 
  ggplot() +
  geom_point(aes(x = age_5, y = fx, color = edo), show.legend = T) +
  theme_light() +
  scale_color_viridis_d()

tef %>% 
  filter(age_5 == "15-19") %>% 
  ggplot() +
  geom_line(aes(x = year, y = fx, color = edo, group = edo)) +
  scale_color_viridis_d(option = "H")

tef %>% 
  filter(year == 2019) %>% 
  filter(fx == min(fx))

TGF <- tef %>% 
  group_by(year, edo, edo_id) %>% 
  summarise(TGF = 5 * sum(fx), .groups = "drop")

TGF %>% 
  ggplot() +
  geom_line(aes(x = year, y = TGF, group = edo), color = "gray") +
  geom_line(data = TGF %>% 
              filter(edo_id == 12), 
            aes(x = year, y = TGF), color = "darkred") +
  geom_line(data = TGF %>% 
              filter(edo_id == 2), 
            aes(x = year, y = TGF), color = "darkgreen") +
  geom_line(data = TGF %>% 
              filter(edo_id == 9), 
            aes(x = year, y = TGF), color = "blue") +
  geom_line(data = TGF %>% 
              filter(edo_id == 0), 
            aes(x = year, y = TGF), color = "black") +
  theme_light() +
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20) +
  geom_hline(yintercept = 2.1, linetype = "dashed")
