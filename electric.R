library(tidyverse)
library(emojifont)

setwd('bills')

electric_usage_1 <- read_csv(
  'data/pge/pge_electric_billing_data_0098839276_2020-08-27_to_2023-07-27.csv',
  ) %>%
  select(-c(UNITS, TYPE, NOTES)) %>%
  rename(
    'start_date' = 'START DATE',
    'end_date' = 'END DATE',
    'kwh' = 'USAGE',
    'cost' = 'COST'
  ) %>%
  print(n = 100)

electric_usage_2 <- read_csv(
  'data/pge/pge_2026-02-08/pge_electric_billing_billing_data_Service 2_2_2023-01-27_to_2026-01-27.csv',
  skip = 5
) %>%
  select(-c(TYPE, NOTES)) %>%
  rename(
    'start_date' = 'START DATE',
    'end_date' = 'END DATE',
    'kwh' = 'USAGE (kWh)',
    'cost' = 'COST'
  ) %>%
  print()

electric_total <- rbind(
  electric_usage_1,
  electric_usage_2
) %>%
  mutate(
    cost = as.numeric(substr(cost, 2, 6)),
    date_center = end_date - days(12),
    year = year(date_center),
    month = month(date_center),
    n_days = as.integer(difftime(end_date, start_date, units='days')),
  ) %>%
  select(year, month, date_center, start_date, end_date, n_days, kwh, cost) %>%
  print(n = 100)

electric_total <- electric_total %>%
  group_by(start_date) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(start_date) %>%
  print(n = 100)

ggplot(electric_total, aes(x = date_center, y = kwh, color = factor(year))) +
  geom_point() +
  geom_line()

ggplot(
  electric_total,
  aes(
    x = month,
    y = year,
    size = kwh,
    color = kwh,
    label = emoji('electric_plug')
  )
) +
  geom_text(
    aes(
      x = month + .15,
      y = year + .1
    ),
    family = "EmojiOne",
    vjust = 0,
    hjust = 0.5,
    # color = '#999999'
  ) +
  geom_text(
    aes(label = round(kwh, 0),
        x = month + .15,
        y = year + 0.44
    ),
    alpha = 0.7,
    # color = '#999999',
    vjust = 0,
    hjust = 0.5
  ) +
  scale_x_continuous(
    name = '',
    position = 'top',
    breaks = seq(1,12),
    limits = c(0.5, 12.5),
    labels = month(
      seq(1,12),
      label = TRUE
    )
  ) +
  scale_color_gradient(
    high = '#e45047',
    low = '#41d561'
    ) +
  scale_size(range = c(3, 7)) +
  scale_y_reverse(
    name = '',
    limits = c(2026.5, 2019.5),
    breaks = seq(2026, 2019, -1),
    labels = as.character(seq(2026, 2019, -1))
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    # panel.grid.minor.y = element_line(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = 'none',
    axis.text = element_text(size = 12),
    # plot.margin = unit(c(0,0,0,0), 'lines'),
    axis.text.y = element_text(
      margin = margin(l = -.5),
      hjust = .5
    )
  )

electric_total <- electric_total %>%
  mutate(dollars_per_kwh = cost / kwh)

ggplot(electric_total, aes(x = start_date, y = dollars_per_kwh)) +
  geom_point() +
  geom_line()
