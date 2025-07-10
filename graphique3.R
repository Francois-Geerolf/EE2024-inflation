rm(list = ls())
setwd("~/Dropbox/ofce/2023-04-15-economie-europeenne/")
source("../../code/R-markdown/init_eurostat.R")
library(tidyverse)
library(eurostat)
library(countrycode)
library(gt)
library(gtExtras)
load_data("eurostat/geo.RData")
load_data("eurostat/na_item.RData")
load_data("eurostat/coicop.RData")
load_data("eurostat/nasq_10_nf_tr.RData")

load_data("eurostat/geo_fr.Rdata")
geo_fr <- geo %>%
  setNames(c("geo", "Geo_fr")) %>%
  mutate(Geo_fr = ifelse(geo == "DE", "Allemagne", Geo_fr),
         Geo_fr = ifelse(geo == "EA19", "Zone Euro", Geo_fr))
load_data("eurostat/geo.Rdata")
load_data("eurostat/na_item_fr.Rdata")


graphique3 <- nasq_10_nf_tr %>%
  filter(geo %in% c("FR", "DE", "IT", "ES", "EA19"),
         na_item %in% c("B2A3G", "B1G"),
         direct == "PAID",
         unit == "CP_MNAC",
         s_adj == "SCA",
         sector == "S11") %>%
  select(geo, time, values, na_item) %>%
  spread(na_item, values) %>%
  quarter_to_date %>%
  filter(date >= as.Date("2017-01-01")) %>%
  arrange(date) %>%
  transmute(date, geo, profit_share = B2A3G/B1G)

geo_colors <- tribble(~ geo, ~ Geo, ~ color,
                      "FR", "France", "#ED2939",
                      "DE", "Allemagne", "#000000",
                      "EA19", "Zone Euro", "#003399",
                      "ES", "Espagne", "#FFC400",
                      "IT", "Italie", "#009246")


graphique3 %>%
  left_join(geo_colors, by = "geo") %>%
  ggplot + theme_minimal() + xlab("") + ylab("Excédent d'exploitation et revenu mixte (% du PIB)") +
  geom_line(aes(x = date, y = profit_share, color = Geo, linetype = Geo), size = 1) +
  scale_color_manual(values = c("#000000", "#FFC400", "#ED2939", "#009246", "#003399")) +
  scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "dashed", "solid")) +
  scale_x_date(breaks = as.Date(paste0(seq(1940, 2030, 1), "-01-01")),
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(0, 100, 1),
                     labels = percent_format(a = 1)) +
theme(legend.title = element_blank(),
      legend.position = c(0.45, 0.93),
      legend.direction = "horizontal")

write.csv(graphique3, "graphique3.csv")
ggsave("graphique3.pdf", width = 7, height = 4, device = cairo_pdf)



