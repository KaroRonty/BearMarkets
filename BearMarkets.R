library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(PerformanceAnalytics)
library(tidyr)library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(PerformanceAnalytics)
library(tidyr)
library(kableExtra)
library(stringr)

options(scipen = 1e9)

# Get Shiller data
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Add dates, drawdown and bear market indicators
full_data <- full_data %>% 
  select(dates, index, index_real, tenyear_real) %>% 
  mutate(dates = ymd(paste0(dates, "-01"))) %>% 
  mutate(drawdown = 1 - index / cummax(replace_na(index, 0))) %>% 
  mutate(bearmarket = ifelse(drawdown > 0.2, TRUE, FALSE))

# Make time series object for drawdown calculation
full_sample <- xts(lead(full_data$index) / full_data$index - 1,
                   order.by = full_data$dates + 2)
names(full_sample) <- "returns"

# Make chart of drawdowns
chart.Drawdown(full_sample, plot.engine = "ggplot2") +
  geom_hline(yintercept = -0.2, size = 1) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, lineheight = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = seq(as.Date("1870-01-01"),
                            as.Date("2020-01-01"), by = "10 years"),
               date_labels = "%Y") +
  ggtitle("Drawdowns of the S&P 500, monthly data",
          subtitle = "Black line indicates bear markets") +
  labs(caption = "Source: Shiller \n
Blog post at: databasedinvesting.blogspot.com")

# Get information from drawdowns
drawdowns <- findDrawdowns(full_sample[2:(length(full_sample) - 4),
                                       "returns"]) %>% 
  sortDrawdowns()

# Put the information to data frame and keep only bear markets
bearmarkets <- data.frame(return = drawdowns$return,
                     from = full_data$dates[drawdowns$from],
                     trough = full_data$dates[drawdowns$trough],
                     to = full_data$dates[drawdowns$to],
                     length = drawdowns$length,
                     peaktotrough = drawdowns$peaktotrough) %>% 
  filter(return < -0.2)

# Nest date sequences for plotting
bearmarkets <- bearmarkets %>% 
  group_by(return, from, trough, to, length, peaktotrough) %>% 
  summarise(dates = list(unique(seq.Date(ymd(unique(from)),
                                         ymd(unique(to)),
                                         by = "months"))))

# Plot bear markets on top of index
ggplot(full_data, aes(x = dates, y = index)) +
  geom_rect(data = bearmarkets,
            aes(xmin = as.Date(from),
                xmax = as.Date(to),
                ymin = 1,
                ymax = 1e7),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.4) +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, lineheight = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_date(breaks = seq(as.Date("1870-01-01"),
                            as.Date("2020-01-01"), by = "10 years"),
               date_labels = "%Y") +
  scale_y_log10() +
  ggtitle("Bear markets of the S&P 500, monthly data",
          subtitle = "From peak until end, nominal values") +
  labs(caption = "Source: Shiller \n
Blog post at: databasedinvesting.blogspot.com") +
  xlab("") +
  ylab("")

# Get all dates of all bear markets
bear_dates <- data.frame(dates = unique(as.Date(unlist(bearmarkets$dates))))

# Add the dates to the data frame
full_data <- full_data %>% 
  left_join(bear_dates)

# Get the beginning dates of all bear markets
beginning <- bearmarkets %>% 
  ungroup() %>% 
  select(from) %>% 
  mutate(bearstart = TRUE) %>% 
  group_by(from, bearstart) %>% 
  summarise(dates = list(unique(seq.Date(ymd(unique(from)),
                                         ymd(unique(from)) +
                                           years(10),
                                         by = "months")))) %>% 
  unnest()

# Add the dates to the data frame
full_data <- full_data %>% 
  left_join(beginning)

# Calculate percentage returns for each bear market
to_plot <- full_data %>% 
  select(dates, index, index_real, from) %>% 
  na.omit() %>% 
  group_by(from) %>% 
  mutate(percentage = index / index[1] - 1,
         percentage_real = index_real / index_real[1] - 1)

# Make colors indicating if the rise was above the loss
colors <- full_data %>% 
  select(dates, index, index_real, from) %>% 
  na.omit() %>% 
  group_by(from) %>% 
  summarise(above = ifelse((last(index) / first(index) < 1),
                           "#00BFC4", "#F8766D"),
            above_real = ifelse((last(index_real) / first(index_real) < 1),
                           "#00BFC4", "#F8766D"))

# Format the beginning dates into month names and years
to_plot <- to_plot %>% 
  full_join(colors) %>% 
  mutate(Date = paste(month(from, label = TRUE), year(from))) %>% 
  ungroup()

# Make the beginning dates to factor for ordering
to_plot <- to_plot %>% 
  mutate(Date = factor(Date, levels = unique(to_plot$Date)))

# Plot all bear markets in nominal values
ggplot(to_plot, aes(x = dates, y = percentage)) +
  geom_line(aes(color = above)) +
  facet_wrap(~Date, scales = "free") +
  geom_hline(yintercept = -0.2) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, lineheight = 0.5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Bear markets of the S&P 500",
          subtitle = paste("Ten-year nominal returns from the peak,",
                           "black lines indicate bear markets")) +
  labs(caption = "Source: Shiller \n
Blog post at: databasedinvesting.blogspot.com") +
  xlab("") +
  ylab("")

# Plot all bear markets in real values
ggplot(to_plot, aes(x = dates, y = percentage_real)) +
  geom_line(aes(color = above_real)) +
  facet_wrap(~Date, scales = "free") +
  geom_hline(yintercept = -0.2) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, lineheight = 0.5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Bear markets of the S&P 500",
          subtitle = paste("Ten-year real returns from the peak,",
                           "black lines indicate bear markets")) +
  labs(caption =
         "Source: Shiller \n
Blog post at: databasedinvesting.blogspot.com") +
  xlab("") +
  ylab("")

# Get stats of the bear markets
stats <- rbind(`Mean fall` = percent(mean(bearmarkets$return)),
               `Max fall` = percent(min(bearmarkets$return)),
               `Mean length in years` =
                 round(mean(bearmarkets$length) / 12, 2),
               `Max length in years` =
                 round(max(bearmarkets$length) / 12, 2),
               `Mean peak to trough length in years` =
                 round(mean(bearmarkets$peaktotrough) / 12, 2),
               `Max peak to trough length in years` =
                 round(max(bearmarkets$peaktotrough) / 12, 2))

# Format the stats
stats %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "condensed"))
