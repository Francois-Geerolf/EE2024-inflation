
library(tidyverse)
library(eurostat)

## Load Eurostat datasets ------

datasets_eurostat <- c("nasq_10_nf_tr")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, stringsAsFactors = FALSE, cache = FALSE) |>
           rename(date = TIME_PERIOD)
  )
}

geo_colors <- tribble(~ geo, ~ Geo, ~ color,
                      "FR", "France", "#ED2939",
                      "DE", "Germany", "#000000",
                      "EA19", "Euro area", "#003399",
                      "ES", "Spain", "#FFC400",
                      "IT", "Italy", "#009246")

figure3 <- nasq_10_nf_tr %>%
  filter(geo %in% c("FR", "DE", "IT", "ES", "EA19"),
         na_item %in% c("B2A3G", "B1G"),
         direct == "PAID",
         unit == "CP_MNAC",
         s_adj == "SCA",
         sector == "S11") %>%
  select(geo, date, values, na_item) %>%
  spread(na_item, values) %>%
  filter(date >= as.Date("2017-01-01"),
         date <= as.Date("2023-04-01")) %>%
  arrange(date) %>%
  transmute(date, geo, profit_share = B2A3G / B1G)

figure3 %>%
  left_join(geo_colors, by = "geo") %>%
  ggplot() + 
  theme_minimal() + 
  xlab("") + 
  ylab("Gross operating surplus and mixed income (% of GDP)") +
  geom_line(aes(x = date, y = profit_share, color = Geo, linetype = Geo), size = 1) +
  scale_color_manual(values = c("#000000", "#FFC400", "#ED2939", "#009246", "#003399")) +
  scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "dashed", "solid")) +
  scale_x_date(breaks = as.Date(paste0(seq(1940, 2030, 1), "-01-01")),
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01 * seq(0, 100, 1),
                     labels = percent_format(accuracy = 1)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.45, 0.93),
        legend.direction = "horizontal")

write.csv(figure3, "figure3.csv")
ggsave("figure3.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("figure3.png", width = 7, height = 4)



