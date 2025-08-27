

rm(list = ls())
library(tidyverse)
library(eurostat)
library(countrycode)
library(gt)
library(gtExtras)
load("~/iCloud/website/data/fred/energy.RData")


graphique4 <- energy %>%
  filter(variable %in% c("GASREGW", "MICH")) %>%
  filter(date >= as.Date("1990-08-20")) %>%
  select(date, variable, value) %>%
  arrange(date)


graphique4 %>%
  mutate(Variable = case_when(variable == "GASREGW" ~ "Prix du pétrole à la pompe ($/gallon)",
                              variable == "MICH" ~ "Université du Michigan: Anticipations d'inflation (%)")) %>%
  ggplot() + geom_line(aes(x = date, y = value, color = Variable, linetype = Variable), size = 1) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(0.7, 0.1)) +
  scale_y_continuous(breaks = seq(0, 150, .5)) + 
  scale_color_manual(values = viridis(2)[2:1]) +
  scale_x_date(breaks = as.Date(paste0(seq(1945, 2025, 5), "-01-01")),
               labels = date_format("%Y")) +
  xlab("") + ylab("Université du Michigan: Anticipations d'inflation (%)\nPrix du pétrole à la pompe ($/gallon)")


write.csv(graphique4, "graphique4.csv")
ggsave("graphique4.pdf", width = 7, height = 4, device = cairo_pdf)


ggsave("graphique4.png", width = 7, height = 4)



