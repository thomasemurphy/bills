library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
# library(ggtext)
library(emojifont)
library(plotly)

setwd('personal/bills')

# # get my personal info outa there!
# read_csv(
#   'data/pge/pge_electric_billing_data_0098839276_2020-08-27_to_2023-07-27.csv',
#   skip = 4
# ) %>%
#   write_csv('data/pge/pge_electric_billing_data_0098839276_2020-08-27_to_2023-07-27.csv')

gas_usage <- read_csv(
  'data/pge/pge_gas_billing_data_0091651353_2020-08-28_to_2023-07-28.csv'
  ) %>%
  rename(
    'start_date' = 'START DATE',
    'end_date' = 'END DATE',
    'therms' = 'USAGE',
    'cost' = 'COST'
  ) %>%
  mutate(
    cost = as.numeric(substr(cost, 2, 6)),
    date_center = end_date - days(12),
    year = year(date_center),
    month = month(date_center),
    n_days = as.integer(difftime(end_date, start_date, units='days')),
    ) %>%
  select(year, month, date_center, start_date, end_date, n_days, therms, cost) %>%
  print()

weather_files <- paste0('data/weather/', list.files(path = 'data/weather'))

all_weather <- read_csv(weather_files, id = "file_name") %>%
  rename(
    'date' = '...1',
    'midnite_temp' = '0'
    ) %>%
  mutate(date = as_date(date)) %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  )

temp_by_month <- all_weather %>%
  group_by(year, month) %>%
  summarize(mean_midnite_temp = mean(midnite_temp)) %>%
  mutate(nite_temp_F = (mean_midnite_temp - 273) * 9/5 + 32) %>%
  arrange(year, month)

temp_by_month
gas_usage

gas_usage <- gas_usage %>%
  left_join(temp_by_month, by = c('year', 'month'))

g <- ggplot(gas_usage, aes(x = nite_temp_F, y = therms, color = factor(year))) +
  geom_point()

ggplotly(g)

g <- ggplot(
  gas_usage,
  aes(
    y = year,
    x = month,
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
    vjust = 0,
    hjust = 0.5,
    color = '#B2401D'
  ) +
  geom_text(
    aes(label = therms,
        x = month + .15,
        y = year + 0.44
    ),
    alpha = 0.7,
    color = '#B2401D',
    vjust = 0,
    hjust = 0.5,
    size = 3.2
  ) +
  geom_text(
    aes(
      label = paste0(
        round(nite_temp_F, digits=0),
        '°'
        ),
      y = year - 0.12,
      x = month - 0.26,
      color = nite_temp_F
    ),
    size = 3,
    hjust = 0.5
  ) +
  geom_segment(
    aes(
      y = year - 0.21,
      x = month - 0.28,
      xend = month - 0.28,
      yend = (year - 0.2) - (67.1 - 40) / 100,
    ),
    color = '#222222',
    alpha = 1,
    size = 1.8
  ) +
  geom_segment(
    aes(
      y = year - 0.2,
      x = month - 0.28,
      xend = month - 0.28,
      yend = (year - 0.2) - (67 - 40) / 100,
    ),
    color = '#ffffff',
    alpha = 1,
    size = 1.7
  ) +
  geom_segment(
    aes(
      y = year - 0.2,
      x = month - 0.28,
      xend = month - 0.28,
      yend = (year - 0.2) - (nite_temp_F - 40) / 100,
      color = nite_temp_F
    ),
    size = 1.7
  ) +
  annotate(
    'text',
    x = 8.9,
    y = 2019.66,
    label = 'avg\nmidnite\ntemp',
    size = 2.3,
    color = '#cc6622',
    hjust = 0
  ) +
  annotate(
    'text',
    x = 9.15,
    y = 2020.1,
    label = 'therms',
    size = 2.3,
    color = '#b2401d',
    hjust = 0.5
  ) +
  # scale_color_gradient(
  #   low = '#57CBF0',
  #   # mid = '#BDC4C6',
  #   high = '#B2401D'
  #   # midpoint = 53
  # ) +
  # scale_color_gradient(
  #   low = '#57CBF0',
  #   # mid = '#BDC4C6',
  #   high = '#cc6622'
  #   # midpoint = 53
  # ) +
  scale_color_gradient2(low = "#57CBF0",
                       mid = "#dde23b",
                       high = "#cc6622",
                       midpoint = 55
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
  scale_y_reverse(
    name = '',
    limits = c(2023.8, 2019.5),
    breaks = seq(2023, 2019, -1),
    labels = as.character(seq(2023, 2019, -1))
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

g <- g + geom_tile(
  data = gas_usage,
  aes(
    y = year,
    x = month
  ),
  height = 1,
  width = 1,
  size = 1,
  color = "#CCCCCC",
  fill = 'white',
  alpha = 0.1,
  linewidth = 0.2
)

ggplotly(g)

library(data.table)

x = rep(c("1", "2", "3"), 3)
y = rep(c("K", "B","A"), each=3)
z = sample(c(NA,"A","L"), 9,replace = TRUE)

df <- data.table(x,y,z)

p <- ggplot(df)+
  geom_tile(
    aes(x=x,y=y),
    width=0.9,
    height=0.9,
    fill="grey"
    ) 


p <- p +
  geom_tile(
    data = filter(df,z=="A"),
    aes(x=x,y=y,fill=z),
    width=0.9,
    height=0.9
    )

p

ggplotly(p)

p <- ggplot(df) +
  geom_tile(
    aes(x=x,y=y,fill="G"),
    width=0.9,
    height=0.9
    ) 

p <- p +
  geom_tile(
    data = filter(df, z=="A"),
    aes(x=x,y=y,fill=z),
    width=0.9,
    height=0.9
    )

p <- p +
  scale_fill_manual(
    guide = guide_legend(title = "test",
                       override.aes = list(
                         fill =c("red","white")                  )
  ),
  values = c("red","grey"),
  labels=c("A",""))

p

ggplotly(p)
