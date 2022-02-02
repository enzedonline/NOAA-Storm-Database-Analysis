
packages = c('readr', 'dplyr', 'stringr', 'lubridate', 
             'ggplot2', 'kableExtra', 'plotly', 'selectr', 'Hmisc')
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

if (!dir.exists('./data/raw')){
  dir.create('./data/raw', recursive = T)
}

download.file(
  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
  "./data/raw/repdata_data_StormData.csv.bz2"
)

dfEvents <- read_csv(
  "./data/raw/repdata_data_StormData.csv.bz2",
  col_select = c("BGN_DATE", "EVTYPE", 
                 "FATALITIES", "INJURIES", 
                 "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP",
                 "REFNUM", "REMARKS"),
  col_types = cols(
    BGN_DATE = col_character(),
    EVTYPE = col_character(),
    FATALITIES = col_number(),
    INJURIES = col_number(),
    PROPDMG = col_number(),
    PROPDMGEXP = col_character(),
    CROPDMG = col_number(),
    CROPDMGEXP = col_character(),
    REFNUM = col_number(),
    REMARKS = col_character(),
  )
)

# https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
# 2.1.1  Storm Data Event Table
# all event types only since 1996 - https://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
dfEvents <- dfEvents %>%
  filter(FATALITIES>0 | INJURIES>0 | PROPDMG>0 | CROPDMG>0) %>% 
  mutate(BGN_DATE=mdy(word(BGN_DATE,1))) %>%
  filter(BGN_DATE > '1996-01-01')

event_types <- read_csv(
  "./data/processed/event_types_classified.csv", 
  col_types = 'c',
  show_col_types = FALSE
  )

set_allowed_event_type <- function(observation) {
  names(
    which(
      sapply(
        event_types, 
        function(x) any(x == toupper(observation['EVTYPE']))
        )
      )
    )[1]
}

dfEvents <- dfEvents %>% mutate(Event = apply(dfEvents, 1, set_allowed_event_type))

# check re-classified event types
unique(dfEvents$Event)

# check no values were missed - missed values would be returned as NA
sum(is.na(dfEvents$Event))

# 1996 CPI = 66.18, 2012 CPI = 96.87: https://www.bls.gov/
# calculate average inflation rate for the period 1996-2012
cpi1996 <- 66.18
cpi2012 <- 96.87
mean_inflation_rate <- (cpi2012-cpi1996)/cpi1996/16
end_date <- ymd("2012-01-01")
# create column to indicate how many years of inflation to apply to cost values
dfEvents <- dfEvents %>%
  mutate(inflation_period = as.numeric(interval(BGN_DATE, end_date) / years(1)))

# EXP discussion https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html
calculate_cost <- function(observation, prefix, annual_inflation){
  DMG=as.numeric(observation[prefix])
  if (DMG <= 0 | is.na(DMG)) {
    # skip if cost is zero
    0
  } else {
    # read EXP value and apply relative factoring to cost
    EXP <- paste0(prefix,'EXP')
    exponent <- toupper(observation[EXP])
    cost <- case_when(
      exponent == 'H' ~ DMG * 1e2,
      exponent == 'K' ~ DMG * 1e3,
      exponent == 'M' ~ DMG * 1e6,
      exponent == 'B' ~ DMG * 1e9,
      is.numeric(exponent) ~ DMG * 10,
      TRUE ~ DMG
    )
    # adjust for relevant years of inflation
    round(cost * ((1 + annual_inflation) ** as.numeric(observation['inflation_period'])))
  }
}

# create calculated property damage cost column
dfEvents <- dfEvents %>% 
  mutate(
    PROPERTY_DAMAGE = apply(
      dfEvents, 
      1, 
      calculate_cost, 
      prefix='PROPDMG', 
      annual_inflation = mean_inflation_rate
    )
  )

# create calculated crop damage cost column
dfEvents <- dfEvents %>% 
  mutate(
    CROP_DAMAGE = apply(
      dfEvents, 
      1, 
      calculate_cost, 
      prefix='CROPDMG', 
      annual_inflation = mean_inflation_rate
    )
  )

# Check for spurious values:
dfEvents %>% summarise(
  max(FATALITIES),
  max(INJURIES),
  max(PROPERTY_DAMAGE)/1e9,
  max(CROP_DAMAGE)/1e9
)

# Value of $137 Billion property damage seems excessive. Check NOAA website for any
# events recorded with this value (https://www.ncdc.noaa.gov/billions/events/US/2005-2006)
# No events. 
refnum <- dfEvents %>% 
  filter(PROPERTY_DAMAGE == max(PROPERTY_DAMAGE))
refnum <- as.numeric(refnum$REFNUM)

# From comments:
dfEvents$REMARKS[dfEvents$REFNUM == refnum]

# Conclusion, the event was miscoded with B instead of M
# Update value for that REFNUM

dfEvents$PROPDMGEXP[dfEvents$REFNUM == refnum] <- 'M'
dfEvents$PROPERTY_DAMAGE[dfEvents$REFNUM == 605943] <- dfEvents$PROPDMG[dfEvents$REFNUM == 605943] * 1e6

# Recheck for spurious values:
dfEvents %>% summarise(
  max(FATALITIES),
  max(INJURIES),
  max(PROPERTY_DAMAGE)/1e9,
  max(CROP_DAMAGE)/1e9
)

# Most harmful - deaths, injuries or both?
human_damage <- dfEvents %>% 
  group_by(Event) %>% 
  summarise(
    Deaths = sum(FATALITIES),
    Injuries = sum(INJURIES),
    Total = sum(Deaths + Injuries)
    ) %>% 
  arrange(desc(Total))  %>%
  slice_head(n=10) %>%
  mutate(Event = factor(Event, levels=unique(Event)))

plot_ly(data=human_damage, x = ~Event, y = ~Deaths, type = 'bar', name = 'Deaths') %>%
  add_trace(y = ~Injuries, name = 'Injuries') %>%
  layout(title = list(text = paste0('Ten Highest Causes of Human Costs for Period 1996 to 2011 (inclusive)')),
         xaxis = list(title = ""),
         yaxis = list(title = 'Total Count'), 
         barmode = 'stack'
  )


# Events cost - assume combined property and crop
financial_damage <- dfEvents %>% 
  group_by(Event) %>% 
  summarise(
    Property = round(sum(PROPERTY_DAMAGE) / 1e9, 2),
    Crop = round(sum(CROP_DAMAGE) / 1e9, 2),
    Total = sum(Property + Crop)
  ) %>% 
  arrange(desc(Total)) %>%
  slice_head(n=10) %>%
  mutate(Event = factor(Event, levels=unique(Event)))

plot_ly(data=financial_damage, x = ~Event, y = ~Property, type = 'bar', name = 'Property') %>%
  add_trace(y = ~Crop, name = 'Crop') %>%
  layout(title = list(text = paste0(
    'Ten Highest Causes of Damage Costs for Period 1996 to 2011 (inclusive)',
    '<br>',
    '<sup>',
    '(USD adjusted to 2012 US Dollar Value)',
    '</sup>'
  )
  ),
  xaxis = list(title = ""),
  yaxis = list(title = 'Total Damage Cost'), 
  barmode = 'stack'
  )

top5_by_year <- dfEvents %>% 
  group_by(Year=year(BGN_DATE), Event) %>% 
  summarise(
    Total = round(sum(PROPERTY_DAMAGE + CROP_DAMAGE) / 1e9, 2)
  ) %>% 
  arrange(desc(Total))  %>% 
  arrange(Year, desc(Total)) %>% 
  slice_max(order_by=Total, n=5) %>%
  slice_head(n=5)

#Effect of Katrina on skewing financial data

# Consider property and crop damage as one
dfEvents <- dfEvents %>%
  mutate(DAMAGE = PROPERTY_DAMAGE + CROP_DAMAGE)

#Top 10 financial events
top10_financial_events <- dfEvents %>%
  select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, DAMAGE, REFNUM) %>%
  arrange(desc(DAMAGE)) %>%
  slice_head(n=10)
top10_financial_events

#Katrina accounts of 5 of the top 6 events
#Filter by billion dollar events from 27th to 30th August 2005 
katrina <- dfEvents %>%
  filter(BGN_DATE>='2005-08-27' & BGN_DATE<='2005-08-30' & DAMAGE>1e9) %>%
  select(BGN_DATE, EVTYPE, DAMAGE, REFNUM, REMARKS)

katrina_sum <- sum(katrina$DAMAGE)/1e9
total_sum <- sum(dfEvents$DAMAGE)/1e9
# katrina effect
katrina_percent <- round(katrina_sum * 100 / total_sum)

# => Hurricanes have the greatest economic impact either directly or indirectly
# Consider all events outside of katrina
top10_financial_events_no_katrina <- dfEvents %>% 
  filter(REFNUM %nin% katrina$REFNUM) %>%
  group_by(Event) %>% 
  summarise(
    Property = round(sum(PROPERTY_DAMAGE) / 1e9, 2),
    Crop = round(sum(CROP_DAMAGE) / 1e9, 2),
    Total = sum(Property + Crop)
  ) %>% 
  arrange(desc(Total)) %>%
  slice_head(n=10) %>%
  mutate(Event = factor(Event, levels=unique(Event)))

# Cumulatively, hurricanes still are #1 cause, however flood and flash flood combined equal this value
# Are these floods linked to the hurricane events as happened with Katrina?

top5_by_year_no_katrina <- dfEvents %>% 
  filter(REFNUM %nin% katrina$REFNUM) %>%
  group_by(Year=year(BGN_DATE), Event) %>% 
  summarise(
    Total = DAMAGE / 1e9
  ) %>% 
  arrange(desc(Total))  %>% 
  arrange(Year, desc(Total)) %>% 
  slice_max(order_by=Total, n=5) %>%
  slice_head(n=5)

p <- ggplot(top5_by_year_no_katrina, aes(x=Year, y=Total, color=Event)) 
p + geom_line(aes(group=Event)) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_x_continuous(breaks = unique(top5_by_year$Year)) +
  scale_y_continuous(breaks = 0:ceiling(max(top5_by_year_no_katrina$Total))) +
  xlab(NULL) + 
  ylab("Total Damage Cost") +
  labs(
    title = "Total Damage Costs for Top 5 Event Types in Each Year with Katrina data ommitted", 
    subtitle = "(USD adjusted to 2012 US Dollar Value)",
    color = "Event Type", 
    ) + 
  theme_minimal()

# It looks as if the flood events are unrelated.
# Other than the hurricane spikes in 2004/2005, no particular event dominates each year,
# rather it's a cumulative effect over the years that dominates
# Conclusion: hurricanes and the various forms of flooding cause the greatest economic damage
#test
