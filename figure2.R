library(tidyverse)
library(eurostat)

geo_colors <- tribble(~ geo, ~ Geo, ~ color,
                      "FR", "France", "#ED2939",
                      "DE", "Germany", "#000000",
                      "EA19", "Euro area", "#003399",
                      "ES", "Spain", "#FFC400",
                      "IT", "Italy", "#009246")

## Load Eurostat datasets ------

datasets_eurostat <- c("prc_hicp_midx", "lc_lci_r2_q")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, stringsAsFactors = FALSE, cache = FALSE) |>
           rename(time = TIME_PERIOD)
  )
}

## Figure 2 --------

lc_lci_r2_q_extract <- lc_lci_r2_q %>%
  filter(nace_r2 == "B-S",
         unit == "I16",
         s_adj == "SCA",
         lcstruct == "D11",
         geo %in% c("FR", "DE", "IT", "ES", "EA")) %>%
  select(geo, date = time, values) %>%
  # + 3 months
  mutate(date = date + months(3)) %>%
  filter(date >= as.Date("2017-01-01")) %>%
  group_by(geo) %>%
  arrange(date) %>%
  mutate(wages = 100*values/values[1]) %>%
  select(-values)

prc_hicp_midx_extract <- prc_hicp_midx %>%
  filter(unit == "I15",
         coicop %in% c("CP00"),
         geo %in% c("FR", "DE", "IT", "ES", "EA")) %>%
  select(geo, date = time, values) %>%
  filter(date >= as.Date("2017-01-01")) %>%
  group_by(geo) %>%
  arrange(date) %>%
  mutate(prices = 100*values/values[1]) %>%
  select(-values)

figure2 <- lc_lci_r2_q_extract %>%
  left_join(prc_hicp_midx_extract, by = c("date", "geo")) %>%
  ungroup %>%
  transmute(date, geo, real_wages = 100*wages/prices)

geo_colors <- tribble(~ geo, ~ Geo, ~ color,
                      "FR", "France", "#ED2939",
                      "DE", "Germany", "#000000",
                      "EA", "Euro area", "#003399",
                      "ES", "Spain", "#FFC400",
                      "IT", "Italy", "#009246")

figure2 %>%
  filter(date >= as.Date("2017-01-01"),
         date <= as.Date("2023-04-01")) %>%
  left_join(geo_colors, by = "geo") %>%
  ggplot(.) + 
  geom_line(aes(x = date, y = real_wages, color = Geo, linetype = Geo), size = 1) +
  theme_minimal() + 
  xlab("") + 
  ylab("Real wages (100 = January 2017)") +
  scale_color_manual(values = c("#000000", "#FFC400", "#ED2939", "#009246", "#003399")) +
  scale_linetype_manual(values = c("dotted", "longdash", "dotdash", "dashed", "solid")) +
  scale_x_date(breaks = seq(1960, 2024, 1) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.4, 0.2),
        legend.key.size = unit(0.4, "cm"),
        legend.direction = "horizontal") +
  scale_y_log10(breaks = seq(0, 400, 2))

write.csv(figure2, "figure2.csv")

ggsave("figure2.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("graphs/figure2a.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("graphs/figure2b.pdf", width = 6.5, height = 3.5, device = cairo_pdf)




