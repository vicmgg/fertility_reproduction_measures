library(tidyverse)
library(readr)
library(readxl)
library(janitor)

df <- read_xlsx("input/paridades_cpv2020.xlsx", skip = 3) %>% 
  row_to_names(1)

names(df)[c(1:3)] <- c("edo_id", "edo", "age")
  
df <- df %>% 
  select(-edo_id, -Total) %>% 
  filter(edo != "Total", age != "Total") %>% 
  gather(key = "parid", value = "pi", -edo, -age) %>% 
  filter(parid != "No especificado") %>% 
  mutate(age = as.numeric(str_extract(age, "\\d{1,3}")),
         parid = as.numeric(str_extract(parid, "\\d{1,3}"))) %>% 
  type_convert()

ppr_c <- df %>% 
  filter(age == 75) %>% 
  group_by(edo, parid) %>% 
  summarise(pi = sum(pi, na.rm = T), .groups = "drop") %>% 
  group_by(edo) %>% 
  mutate(pi = rev(cumsum(rev(pi)))) %>% 
  replace_na(list(pi = 0)) %>% 
  na.omit() %>% 
  group_by(edo) %>% 
  mutate(ppr = lead(pi, default = 0) / pi,
         ppr0 = pi / pi[parid == 0]) %>% 
  mutate(ppr0 = ifelse(ppr0 == 1, 0, ppr0)) %>% 
  ungroup()

ppr_c %>% 
  group_by(edo) %>% 
  summarise(TFR_C = sum(ppr0)) %>% 
  arrange(TFR_C)

