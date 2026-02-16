library(tidyverse)
library(emojifont)

# setwd('../../bills')

# # get my personal info outa there!
# read_csv(
#   'data/pge/pge_electric_billing_data_0098839276_2020-08-27_to_2023-07-27.csv',
#   skip = 4
# ) %>%
#   write_csv('data/pge/pge_electric_billing_data_0098839276_2020-08-27_to_2023-07-27.csv')

gas_usage_1 <- read_csv(
  'data/pge/pge_gas_billing_data_0091651353_2020-08-28_to_2023-07-28.csv'
  ) %>%
  select(-c(UNITS, TYPE, NOTES)) %>%
  rename(
    'start_date' = 'START DATE',
    'end_date' = 'END DATE',
    'therms' = 'USAGE',
    'cost' = 'COST'
  ) %>%
  print()

gas_usage_2 <- read_csv(
  'data/pge/pge_2026-02-08/pge_natural_gas_billing_billing_data_Service 1_1_2023-01-28_to_2026-01-28.csv',
  skip = 5
) %>%
  select(-c(TYPE, NOTES)) %>%
  rename(
    'start_date' = 'START DATE',
    'end_date' = 'END DATE',
    'therms' = 'USAGE (therms)',
    'cost' = 'COST'
  ) %>%
  print()

gas_total <- rbind(
  gas_usage_1,
  gas_usage_2
) %>%
  mutate(
    cost = as.numeric(substr(cost, 2, 6)),
    date_center = end_date - days(12),
    year = year(date_center),
    month = month(date_center),
    n_days = as.integer(difftime(end_date, start_date, units='days')),
  ) %>%
  select(year, month, date_center, start_date, end_date, n_days, therms, cost) %>%
  print(n = 100)

gas_total <- gas_total %>%
  group_by(start_date) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(start_date) %>%
  print(n = 100)

# weather_files <- paste0('data/weather/', list.files(path = 'data/weather'))

# all_weather <- read_csv(weather_files, id = "file_name") %>%
#   rename(
#     'date' = '...1',
#     'midnite_temp' = '0'
#     ) %>%
#   mutate(date = as_date(date)) %>%
#   mutate(
#     year = year(date),
#     month = month(date),
#     day = day(date)
#   )

# temp_by_month <- all_weather %>%
#   group_by(year, month) %>%
#   summarize(mean_midnite_temp = mean(midnite_temp)) %>%
#   mutate(nite_temp_F = (mean_midnite_temp - 273) * 9/5 + 32) %>%
#   arrange(year, month)

# gas_usage <- gas_usage %>%
#   left_join(temp_by_month, by = c('year', 'month'))

g <- ggplot(gas_total, aes(x = date_center, y = therms, color = factor(year))) +
  geom_point() +
  geom_line()

ggplot(
  gas_total,
  aes(
    x = month,
    y = year,
    size = therms,
    label = emoji('fire')
  )
) +
  geom_text(
    aes(
      x = month + .15,
      y = year + .3
    ),
    family = "EmojiOne",
    vjust = -0.07,
    hjust = 0.5,
    color = '#B2401D',
    alpha = 0.3
  ) +
  geom_text(
    aes(label = therms,
        x = month + .15,
        y = year + 0.49
    ),
    alpha = 0.7,
    color = '#B2401D',
    vjust = 0,
    hjust = 0.5,
    size = 3
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
  scale_size(range = c(1, 12)) +
  # scale_y_reverse(
  #   name = '',
  #   limits = c(2026.5, 2019.5),
  #   breaks = seq(2026, 2019, -1),
  #   labels = as.character(seq(2026, 2019, -1))
  # ) +
  scale_y_continuous(
    name = '',
    limits = c(2017, 2027),
    breaks = seq(2020, 2026),
    # labels = as.character(seq(2026, 2019, -1))
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_line(),
    # axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = 'none',
    axis.text = element_text(size = 12),
    # plot.margin = unit(c(0,0,0,0), 'lines'),
    axis.text.y = element_text(
      margin = margin(l = -.5),
      hjust = .5
    ),
    axis.text.x = element_text(
      hjust = 0.2
    )
  ) +
  coord_polar()


gas_total <- gas_total %>%
  mutate(dollars_per_therm = cost / therms)

ggplot(gas_total, aes(x = start_date, y = dollars_per_therm)) +
  geom_point() +
  geom_line()
