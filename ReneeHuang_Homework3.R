# Homework3
# Bus111a
# Lening Huang
# 11-19-2020

# Q1-a
install.packages("coronavirus")

read.csv("~/Desktop/coronavirus-master/csv/coronavirus.csv")

setwd("~/Desktop/")
library(data.table)
dta_coro = as.data.table(read.csv("~/Desktop/coronavirus-master/csv/coronavirus.csv"))

# Q1-b
head(coronavirus,100)

# Q1-c
#'date is representing      {Date of summary in YYYY-MM-DD format.}
#'province  is representing {Name of province/state, for countries where data is provided split across multiple provinces/states.}
#'country is representing   {Name of country/region.}
#'lat is representing       {Latitude of center of geographic region, defined as either country or, if available, province.}
#'long is representing      {Longitude of center of geographic region, defined as either country or, if available, province.}
#'type is representing      {An indicator for the type of cases (confirmed, death, recovered).}
#'cases is representing     {Number of cases on given date.}

# Q2-a
coronavirus = read.csv("~/Desktop/coronavirus-master/csv/coronavirus.csv")
coronavirus

library(dplyr)
summary_df <- coronavirus %>%

  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20)
# A tibble: 20 x 2
# country        total_cases
# <fct>                <int>
# 1 US                11357322
# 2 India              8912907
# 3 Brazil             5911758
# 4 France             2087183
# 5 Russia             1954912
# 6 Spain              1510023
# 7 United Kingdom     1414359
# 8 Argentina          1329005
# 9 Italy              1238072
# 10 Colombia           1211128
# 11 Mexico             1011153
# 12 Peru                938268
# 13 Germany             843757
# 14 Iran                788473
# 15 South Africa        754256
# 16 Poland              752940
# 17 Ukraine             573758
# 18 Belgium             540605
# 19 Chile               533610
# 20 Iraq                524503

# Q2-b
dta_top5 = summary_df %>% head(5)
library(ggplot2)
dta_top5$country = factor(dta_top5$country,levels =c("US", "India", "Brazil", "France", "Russia"))
                                             # arrange the bar to make it in order of high to low
bp = ggplot(data = dta_top5,aes(country, total_cases))+
       geom_bar(stat  = "identity",
           width = 0.5,                      # add width of the bar to make it clearer to see
           fill  = "#56B4E9" )+              # add color of the bar to make it beautiful
       geom_text(aes(label=total_cases),
            vjust    = -1.0,                 # add label of the bar to have clearer number of total_cases
            position = position_dodge(0.9),  # add outside label would be better for long-digit numbers
            size     = 3.5)                  # add size of the label would make it big and clear enough to be seen

# Q2-c
bp_flip = bp + coord_flip()

# Q2-d
bp +
  ggtitle("Top 5 countries by total cases") +                            # add title to the barplot
  labs(subtitle = "coronavirus data from JHU") +                         # add subtitle to the barplot
  mynamestheme
plot.title      = ggtitle("Top 5 countries by total cases")
plot.subtitle   = labs(subtitle = "coronavirus data from JHU")
mynamestheme <- theme(plot.title = element_text(hjust = 0.5,             # add middle position of the plot title
                                                family = "Helvetica",    # add font of the plot title
                                                face = "bold",           # add style of the plot title
                                                size = (15),             # add size of the plot title
                                                color = "steelblue4"),   # add color of the plot title

                      plot.subtitle = element_text(hjust = 0.5,          # add middle position of the plot subtitle
                                                family = "Helvetica",    # add font of the plot subtitle
                                                face = "bold",           # add style of the plot subtitle
                                                size = (8),             # add size of the plot subtitle
                                                color = "steelblue4"),   # add color of the plot subtitle

                      axis.title = element_text(family = "Helvetica",    # add font of the axis title
                                                size = (10),             # add size of the axis title
                                                color = "steelblue4"),   # add color of the axis title

                      axis.text = element_text(family = "Courier",       # add font of the axis text
                                               color = "cornflowerblue", # add color of the axis text
                                               size = (10)))             # add size of the axis text

# Q3-a
library(tidyr)
coronavirus$date = as.Date(coronavirus$date)
recent_cases <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(date) %>%
  summarise(total_cases = sum(cases))

# A tibble: 301 x 2
#      date       total_cases
#    <date>           <int>
# 1 2020-01-22         555
# 2 2020-01-23          99
# 3 2020-01-24         287
# 4 2020-01-25         493
# 5 2020-01-26         684
# 6 2020-01-27         809
# 7 2020-01-28        2651
# 8 2020-01-29         589
# 9 2020-01-30        2068
# 10 2020-01-31        1692
# … with 291 more rows

# Q3-b
library(ggplot2)
line_graph = ggplot(data = recent_cases,aes(date, total_cases))+
  geom_line(stat = "identity", color = "steelblue") +
  ggtitle("total_cases VS date") +                                       # add title to the line graph
  labs(subtitle = "coronavirus data from JHU") +                         # add subtitle to the line graph
  mynamestheme

plot.title    = ggtitle("total_cases VS date")
plot.subtitle = labs(subtitle = "coronavirus data from JHU")
mynamestheme <- theme(plot.title = element_text(hjust = 0.5,             # add middle position of the graph title
                                                family = "Helvetica",    # add font of the graph title
                                                face = "bold",           # add style of the graph title
                                                size = (15),             # add size of the graph title
                                                color = "steelblue4"),   # add color of the graph title

                      plot.subtitle = element_text(hjust = 0.5,          # add middle position of the graph subtitle
                                                   family = "Helvetica",    # add font of the graph subtitle
                                                   face = "bold",           # add style of the graph subtitle
                                                   size = (8),             # add size of the graph subtitle
                                                   color = "steelblue4"),   # add color of the graph subtitle

                      axis.title = element_text(family = "Helvetica",    # add font of the axis title
                                                size = (10),             # add size of the axis title
                                                color = "steelblue4"),   # add color of the axis title

                      axis.text = element_text(family = "Courier",       # add font of the axis text
                                               color = "cornflowerblue", # add color of the axis text
                                               size = (10)))









