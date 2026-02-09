library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
# library(ggtext)
library(emojifont)
library(plotly)

setwd('bills')

water_usage <- read_csv(
  'water usage.csv'
  ) %>%
  rename(
    'statement_date' = 'Statement Date',
    'read_date' = 'Reading Date',
    'ccf' = 'Consumption (CCF)',
    'days' = 'Days',
    'gpd' = 'Gallons Per Day'
  ) %>%
  mutate(
    statement_date = as_date(statement_date, format = '%m/%d/%Y'),
    read_date = as_date(read_date, format = '%m/%d/%Y'),
    mid_date = read_date - days(30),
    year = year(mid_date),
    month = month(mid_date)
    ) %>%
  print(n = 100)

ggplot(water_usage, aes(x = mid_date, y = gpd)) +
  geom_point() +
  geom_line()

ggplot(
  water_usage
  %>% filter(gpd > 0),
  aes(
    x = month,
    y = year,
    size = gpd,
    color = gpd,
    label = emoji('potable_water')
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
    aes(label = gpd,
        x = month + .15,
        y = year + 0.44
    ),
    alpha = 0.7,
    # color = '#999999',
    vjust = 0,
    hjust = 0.5
  ) +
  annotate(
    geom = 'text',
    x = 8.15,
    y = 2021.7,
    size = 3,
    color = '#999999',
    label = 'gallons\nper day'
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
    low = '#2f52ec',
    high = '#f1572c'
    ) +
  scale_size(range = c(4, 8)) +
  scale_y_reverse(
    name = '',
    limits = c(2024.8, 2019.5),
    breaks = seq(2024, 2019, -1),
    labels = as.character(seq(2024, 2019, -1))
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
  