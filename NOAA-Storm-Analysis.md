---
title: "Analysis of Health and Economic Impact of Reported Severe Weather Events Across USA"
author: "Richard Allen"
date: "02/02/2022"
output: 
  html_document: 
    toc: yes
    keep_md: yes
    toc_depth: 3
    fig_width: 8
    fig_height: 5
---

# Summary

This report will find that excessive heat was the biggest killer over the examined period, while tornadoes were the most injurious. Hurricanes and floods had the greatest economic impact with severe drought having the greatest economic impact on crop damage.

# Synopsis

The Storm Events Database contains data and information for the United States, its territories, and possessions. Data are available from 1950 to the present, and the Database contains over 1.2 million records ([*reference*](https://www.ncdc.noaa.gov/news/storm-events-database-version-30#:~:text=The%20Storm%20Events%20Database%20contains%20data%20and%20information%20for%20the,contains%20over%201.2%20million%20records)).

Data across all event types has been available since the beginning of 1996 ([*reference*](https://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype)), as this report is comparing all event types, only events of the period from 1996 until end of 2011 (where the supplied data finishes) were considered and consists of 902,297 event records.

The report addresses two questions:

***Across the United States, which types of events are most harmful with respect to population health?***

In terms of death, excessive heat was the cause of the highest mortality count while tornadoes by far caused the greatest amount of injury.

***Across the United States, which types of events have the greatest economic consequences?***

Drought was the cause of the highest economic damage to crops. Hurricanes had the greatest impact on property damage, however, if all flood types were considered a single event type, then these would exceed hurricane damage.

It was found that the singular Hurricane Katrina event contributed to over 25% of the total damage caused by all events over the 16 year period but after removing events linked to Hurricane Katrina, flooding and hurricanes were still the chief cause of economic damage ??? this is discussed in detail later in the report.

# Data Processing

## Setting Global Defaults

Set caching and echo to `TRUE` 


```r
# set defaults: cache chunks
knitr::opts_chunk$set(cache=TRUE, echo = TRUE)
```

## Loading required libraries

The following libraries are required to run the code in this analysis and will be installed if not present locally:

`readr`, `dplyr`, `stringr`, `lubridate`, `ggplot2`, `kableExtra`, `plotly`, `selectr`, `Hmisc`


```r
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
```

## Downloading and Reading the Data

The data is available for [download](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) in bzip2 compressed format. It is downloaded to the subdirectory `./data/raw` which is first created if it doesn't already exist. The file is left compressed due to size.


```r
if (!dir.exists('./data/raw')){
  dir.create('./data/raw', recursive = T)
}

download.file(
  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
  "./data/raw/repdata_data_StormData.csv.bz2"
)
```

The data is read using `read_csv()` from the `readr` package, selecting only those variables of interest:


```r
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
```

## Cleaning the Data

This required several steps:

 1. Subsetting the data
 2. Converting the date column to true date type
 3. Correcting EVTYPE entries to allowed types
 4. Calculating actual cost using the EXP columns 
 5. Adjusting values for inflation
 6. Checking for, and fixing, erroneous entries
 
### Subsetting the data & converting the date column

We are only interested in:

 - events that contribute towards one of the four key indicators (fatalities, injuries, property damage or crop damage)
 
 - events from 1996 as [this is the earliest year when all event types were being recorded](https://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype)

Before filtering by date, we eliminate non-contributors first to minimise processing:


```r
dfEvents <- dfEvents %>%
  filter(FATALITIES>0 | INJURIES>0 | PROPDMG>0 | CROPDMG>0) %>% 
  mutate(BGN_DATE=mdy(word(BGN_DATE,1))) %>%
  filter(BGN_DATE > '1996-01-01')
head(dfEvents, 10)
```

```
## # A tibble: 10 x 10
##    BGN_DATE   EVTYPE   FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
##    <date>     <chr>         <dbl>    <dbl>   <dbl> <chr>        <dbl> <chr>     
##  1 1996-01-06 WINTER ~          0        0     380 K               38 K         
##  2 1996-01-11 TORNADO           0        0     100 K                0 <NA>      
##  3 1996-01-11 TSTM WI~          0        0       3 K                0 <NA>      
##  4 1996-01-11 TSTM WI~          0        0       5 K                0 <NA>      
##  5 1996-01-11 TSTM WI~          0        0       2 K                0 <NA>      
##  6 1996-01-18 HIGH WI~          0        0     400 K                0 <NA>      
##  7 1996-01-19 TSTM WI~          0        0      12 K                0 <NA>      
##  8 1996-01-24 TSTM WI~          0        0       8 K                0 <NA>      
##  9 1996-01-24 TSTM WI~          0        0      12 K                0 <NA>      
## 10 1996-01-26 FLASH F~          0        0      75 K                0 <NA>      
## # ... with 2 more variables: REFNUM <dbl>, REMARKS <chr>
```

```r
dim(dfEvents)
```

```
## [1] 201313     10
```

The subsetted data now has 201313 rows.

### Correcting EVTYPE entries to allowed types

According to the [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf), only the following values should be allowed in the recorded EVTYPE:

|                       |                   |                        |
|-----------------------|-------------------|------------------------|
|Astronomical Low Tide  |Funnel Cloud       |Rip Current             |
|Astronomical High Tide |Freezing Fog       |Seiche                  |
|Avalanche              |Hail               |Sleet                   |
|Blizzard               |Heat               |Storm Surge/Tide        |
|Coastal Flood          |Heavy Rain         |Strong Wind             |
|Cold/Wind Chill        |Heavy Snow         |Thunderstorm Wind       |
|Debris Flow            |High Surf          |Tornado                 |
|Dense Fog              |High Wind          |Tropical Depression     |
|Dense Smoke            |Hurricane (Typhoon)|Tropical Storm          |
|Drought                |Ice Storm          |Tsunami                 |
|Dust Devil             |Lake-Effect Snow   |Volcanic Ash            |
|Dust Storm             |Lakeshore Flood    |Waterspout              |
|Excessive Heat         |Lightning          |Wildfire                |
|Extreme Cold/Wind Chill|Marine Hail        |Winter Storm            |
|Flash Flood            |Marine High Wind   |Winter Weather          |
|Flood                  |Marine Strong Wind |Marine Thunderstorm Wind|
|Frost/Freeze           |                   |                        |
*National Weather Service Storm Data Documentation, Section 2.1.1 Storm Data Event Table*

Very few of the actual recorded events have followed this directive, there are a huge number of variations, mixes and spelling errors to deal with. 

Without classifying the event records properly, any aggregate analysis would be somewhat meaningless.

As many of these entries needed to be examined on case-by-case process, in the end it was more reliable to just map each recorded `EVTYPE` to an actual allowed event type.

The first step for this is to create a mapping list:


```r
# Assign recorded EVTYPES to allowed EVTYPES
event_types <- list(
  `Astronomical Low Tide` = c('ASTRONOMICAL LOW TIDE'),
  `Astronomical High Tide` = c('ASTRONOMICAL HIGH TIDE','HIGH TIDES'),
  Avalanche = c('AVALANCE','AVALANCHE','HEAVY SNOW/BLIZZARD/AVALANCHE'),
  Blizzard = c('BLIZZARD AND EXTREME WIND CHIL','BLIZZARD AND HEAVY SNOW','BLIZZARD SUMMARY','BLIZZARD WEATHER','BLIZZARD','BLIZZARD/FREEZING RAIN','BLIZZARD/HEAVY SNOW','BLIZZARD/HIGH WIND','BLIZZARD/WINTER STORM','GROUND BLIZZARD'),
  `Coastal Flood` = c('BEACH EROSION/COASTAL FLOOD ','BEACH FLOOD','COASTAL  FLOODING/EROSION','COASTAL FLOOD','COASTAL FLOODING','COASTAL FLOODING/EROSION','COASTAL STORM','COASTAL/TIDAL FLOOD','COASTALFLOOD','COASTALSTORM','CSTL FLOODING/EROSION','EROSION/CSTL FLOOD'),
  `Cold/Wind Chill` = c('COLD','COLD/WIND CHILL','COLD TEMPERATURE','COLD TEMPERATURES','COLD WAVE','COLD WEATHER','COLD WIND CHILL TEMPERATURES','COLD/WIND CHILL','COLD/WINDS','UNSEASONABLE COLD','UNSEASONABLY COLD','UNSEASONABLY COOL & WET','UNSEASONABLY COOL','UNSEASONAL LOW TEMP','UNUSUALLY COLD'),
  `Debris Flow` = c('LANDSLIDE','LANDSLIDE/URBAN FLOOD','LANDSLIDES','LANDSLUMP','MUD SLIDE','MUD SLIDES URBAN FLOODING','MUD SLIDES','MUD/ROCK SLIDE','MUDSLIDE','MUDSLIDE/LANDSLIDE','MUDSLIDES','ROCK SLIDE'),
  `Dense Fog` = c('DENSE FOG','FOG AND COLD TEMPERATURES','FOG','PATCHY DENSE FOG'),
  `Dense Smoke` = c('DENSE SMOKE','SMOKE'),
  Drought = c('ABNORMALLY DRY','BELOW NORMAL PRECIPITATION','DRIEST MONTH','DROUGHT','DROUGHT/EXCESSIVE HEAT','DRY CONDITIONS ','DRY HOT WEATHER','DRY PATTERN','DRY SPELL','DRY WEATHER','DRY','DRYNESS','EXCESSIVELY DRY','HEAT DROUGHT','HEAT WAVE DROUGHT','HEAT/DROUGHT','LACK OF SNOW','RECORD DRY MONTH','RECORD DRYNESS','RECORD LOW RAINFALL','SNOW DROUGHT','UNSEASONABLY DRY','UNSEASONABLY WARM AND DRY','VERY DRY'),
  `Dust Devil` = c('DUST DEVEL','DUST DEVIL WATERSPOUT','DUST DEVIL'),
  `Dust Storm` = c('BLOWING DUST','DUST STORM','DUST STORM/HIGH WINDS','DUSTSTORM','HIGH WINDS DUST STORM','SAHARAN DUST'),
  `Excessive Heat` = c('EXCESSIVE HEAT','EXCESSIVE HEAT/DROUGHT','EXTREME HEAT','HEAT WAVE','HEAT WAVES','HEAT','HEATBURST','HIGH TEMPERATURE RECORD','HOT AND DRY','HOT PATTERN','HOT SPELL','HOT WEATHER','HOT/DRY PATTERN','PROLONG WARMTH','RECORD HEAT WAVE','RECORD HEAT','RECORD HIGH TEMPERATURE','RECORD HIGH TEMPERATURES','RECORD HIGH','RECORD TEMPERATURE','RECORD TEMPERATURES','RECORD WARM TEMPS.','RECORD WARM','RECORD WARMTH','RECORD/EXCESSIVE HEAT','TEMPERATURE RECORD','UNSEASONABLY HOT'),
  `Extreme Cold/Wind Chill` = c('BITTER WIND CHILL TEMPERATURES','BITTER WIND CHILL ','EXCESSIVE COLD','EXTENDED COLD','EXTREME COLD','EXTREME COLD/WIND CHILL','EXTREME WIND CHILL','EXTREME WIND CHILL/BLOWING SNO','EXTREME WIND CHILLS','EXTREME WINDCHILL TEMPERATURES','EXTREME WINDCHILL','EXTREME/RECORD COLD','HIGH WIND/WIND CHILL','HIGH WIND/WIND CHILL/BLIZZARD','HIGH WINDS AND WIND CHILL','HYPERTHERMIA/EXPOSURE','HYPOTHERMIA','HYPOTHERMIA/EXPOSURE','LOW TEMPERATURE RECORD','LOW TEMPERATURE','LOW WIND CHILL','PROLONG COLD','PROLONG COLD/SNOW','RECORD  COLD','RECORD COLD AND HIGH WIND','RECORD COLD','RECORD COLD/FROST','RECORD COOL','SEVERE COLD','SNOW/ BITTER COLD','WIND CHILL','WIND CHILL/HIGH WIND'),
  `Flash Flood` = c('BREAKUP FLOODING','DAM BREAK','DAM FAILURE','FLASH FLOOD - HEAVY RAIN','FLASH FLOOD FROM ICE JAMS','FLASH FLOOD LANDSLIDES','FLASH FLOOD WINDS','FLASH FLOOD','FLASH FLOOD/ FLOOD','FLASH FLOOD/ STREET','FLASH FLOOD/','FLASH FLOOD/FLOOD','FLASH FLOOD/HEAVY RAIN','FLASH FLOOD/LANDSLIDE','FLASH FLOODING','FLASH FLOODING/FLOOD','FLASH FLOODING/THUNDERSTORM WI','FLASH FLOODS','FLASH FLOOODING','FLOOD FLASH','FLOOD FLOOD/FLASH','FLOOD/FLASH FLOOD','FLOOD/FLASH FLOODING','FLOOD/FLASH','FLOOD/FLASH/FLOOD','FLOOD/FLASHFLOOD','LOCAL FLASH FLOOD','RAPIDLY RISING WATER'),
  Flood = c('FLOOD & HEAVY RAIN','FLOOD WATCH/','FLOOD','FLOOD/RAIN/WIND','FLOOD/RAIN/WINDS','FLOOD/RIVER FLOOD','FLOOD/STRONG WIND','FLOODING','FLOODING/HEAVY RAIN','FLOODS','HEAVY RAIN AND FLOOD','HEAVY RAIN/MUDSLIDES/FLOOD','HEAVY RAIN/URBAN FLOOD','HEAVY RAIN; URBAN FLOOD WINDS;','HIGH WATER','HIGHWAY FLOODING','ICE JAM FLOOD (MINOR','ICE JAM FLOODING','LOCAL FLOOD','MAJOR FLOOD','MINOR FLOOD','MINOR FLOODING','RIVER AND STREAM FLOOD','RIVER FLOOD','RIVER FLOODING','RURAL FLOOD','SMALL STREAM AND URBAN FLOOD','SMALL STREAM AND URBAN FLOODIN','SMALL STREAM AND','SMALL STREAM FLOOD','SMALL STREAM FLOODING','SMALL STREAM URBAN FLOOD','SMALL STREAM','SMALL STREAM/URBAN FLOOD','SML STREAM FLD','SNOWMELT FLOODING','STREAM FLOODING','STREET FLOOD','STREET FLOODING','URBAN AND SMALL STREAM FLOOD','URBAN AND SMALL STREAM FLOODIN','URBAN AND SMALL STREAM','URBAN AND SMALL','URBAN FLOOD LANDSLIDE','URBAN FLOOD','URBAN FLOODING','URBAN FLOODS','URBAN SMALL STREAM FLOOD','URBAN SMALL','URBAN/SMALL FLOODING','URBAN/SMALL STREAM  FLOOD','URBAN/SMALL STREAM FLOOD','URBAN/SMALL STREAM FLOODING','URBAN/SMALL STREAM','URBAN/SMALL STRM FLDG','URBAN/SMALL','URBAN/SML STREAM FLD','URBAN/SML STREAM FLDG','URBAN/STREET FLOODING'),
  `Frost/Freeze` = c('AGRICULTURAL FREEZE','BLACK ICE','COLD AND FROST','DAMAGING FREEZE','EARLY FREEZE','EARLY FROST','FREEZE','FREEZING DRIZZLE AND FREEZING','FREEZING DRIZZLE','FREEZING RAIN AND SLEET','FREEZING RAIN AND SNOW','FREEZING RAIN SLEET AND LIGHT','FREEZING RAIN SLEET AND','FREEZING RAIN','FREEZING RAIN/SLEET','FREEZING RAIN/SNOW','FREEZING SPRAY','FROST','FROST/FREEZE','FROST\\FREEZE','GLAZE ICE','GLAZE','GLAZE/ICE STORM','HARD FREEZE','PATCHY ICE','SNOW FREEZING RAIN'),
  `Funnel Cloud` = c('COLD AIR FUNNEL','COLD AIR FUNNELS','FUNNEL CLOUD','FUNNEL CLOUD.','FUNNEL CLOUD/HAIL','FUNNEL CLOUDS','FUNNEL','FUNNELS','WALL CLOUD/FUNNEL CLOUD','WHIRLWIND'),
  `Freezing Fog` = c('FREEZING FOG','ICE FOG'),
  Hail = c('DEEP HAIL','HAIL 0.75','HAIL 0.88','HAIL 075','HAIL 088','HAIL 1.00','HAIL 1.75','HAIL 1.75)','HAIL 100','HAIL 125','HAIL 150','HAIL 175','HAIL 200','HAIL 225','HAIL 275','HAIL 450','HAIL 75','HAIL 80','HAIL 88','HAIL ALOFT','HAIL DAMAGE','HAIL FLOODING','HAIL STORM','HAIL','HAIL(0.75)','HAIL/ICY ROADS','HAIL/WIND','HAIL/WINDS','HAILSTORM','HAILSTORMS','NON SEVERE HAIL','SMALL HAIL','THUNDERSTORM HAIL','THUNDERSTORM HAIL'),
  Heat = c('ABNORMAL WARMTH','UNSEASONABLY WARM & WET','UNSEASONABLY WARM YEAR','UNSEASONABLY WARM','UNSEASONABLY WARM/WET','UNUSUAL WARMTH','UNUSUAL/RECORD WARMTH','UNUSUALLY WARM','VERY WARM','WARM DRY CONDITIONS','WARM WEATHER'),
  `Heavy Rain` = c('ABNORMALLY WET','DOWNBURST','EARLY RAIN','EXCESSIVE PRECIPITATION','EXCESSIVE RAIN','EXCESSIVE RAINFALL','EXCESSIVE WETNESS','EXTREMELY WET','HEAVY PRECIPATATION','HEAVY PRECIPITATION','HEAVY RAIN AND WIND','HEAVY RAIN EFFECTS','HEAVY RAIN','HEAVY RAIN/FLOODING','HEAVY RAIN/HIGH SURF','HEAVY RAIN/LIGHTNING','HEAVY RAIN/SEVERE WEATHER','HEAVY RAIN/SMALL STREAM URBAN','HEAVY RAIN/SNOW','HEAVY RAIN/WIND','HEAVY RAINFALL','HEAVY RAINS','HEAVY RAINS/FLOODING','HEAVY SHOWER','HEAVY SHOWERS','HIGH WINDS HEAVY RAINS','HIGH WINDS/HEAVY RAIN','HVY RAIN','LOCALLY HEAVY RAIN','PROLONGED RAIN','RAIN (HEAVY)','RAIN AND WIND','RAIN DAMAGE','RAIN','RAIN/SNOW','RAINSTORM','RECORD PRECIPITATION','RECORD RAINFALL','RECORD/EXCESSIVE RAINFALL','TORRENTIAL RAIN','TORRENTIAL RAINFALL','TSTM HEAVY RAIN','UNSEASONAL RAIN','WET MICOBURST','WET MICROBURST','WET MONTH','WET WEATHER','WET YEAR','UNSEASONABLY WET'),
  `Heavy Snow` = c('ACCUMULATED SNOWFALL','DRIFTING SNOW','EXCESSIVE SNOW','HEAVY SNOW   FREEZING RAIN','HEAVY SNOW & ICE','HEAVY SNOW AND HIGH WINDS','HEAVY SNOW AND ICE STORM','HEAVY SNOW AND ICE','HEAVY SNOW AND STRONG WINDS','HEAVY SNOW AND','HEAVY SNOW ANDBLOWING SNOW','HEAVY SNOW SHOWER','HEAVY SNOW SQUALLS','HEAVY SNOW','HEAVY SNOW-SQUALLS','HEAVY SNOW/BLIZZARD','HEAVY SNOW/BLOWING SNOW','HEAVY SNOW/FREEZING RAIN','HEAVY SNOW/HIGH WIND','HEAVY SNOW/HIGH WINDS & FLOOD','HEAVY SNOW/HIGH WINDS','HEAVY SNOW/HIGH WINDS/FREEZING','HEAVY SNOW/HIGH','HEAVY SNOW/ICE STORM','HEAVY SNOW/ICE','HEAVY SNOW/SLEET','HEAVY SNOW/SQUALLS','HEAVY SNOW/WIND','HEAVY SNOW/WINTER STORM','HIGH WIND/ BLIZZARD','HIGH WIND/BLIZZARD','HIGH WIND/BLIZZARD/FREEZING RA','HIGH WIND/HEAVY SNOW','MOUNTAIN SNOWS','NEAR RECORD SNOW','RECORD SNOW','RECORD SNOW/COLD','RECORD SNOWFALL','RECORD WINTER SNOW','RECORD MAY SNOW','SNOW ACCUMULATION','SNOW ADVISORY','SNOW AND COLD','SNOW AND HEAVY SNOW','SNOW AND ICE STORM','SNOW AND ICE','SNOW AND SLEET','SNOW AND WIND','SNOW SQUALL','SNOW SQUALLS','SNOWFALL RECORD'),
  `High Surf` = c('HAZARDOUS SURF','HEAVY SEAS','HEAVY SURF AND WIND','HEAVY SURF COASTAL FLOODING','HEAVY SURF','HEAVY SURF/HIGH SURF','HEAVY SWELLS','HIGH  SWELLS','HIGH SEAS','HIGH SURF ADVISORIES','HIGH SURF ADVISORY','HIGH SURF','HIGH SWELLS','HIGH WAVES','ROUGH SEAS','ROUGH SURF'),
  `High Wind` = c('DOWNBURST WINDS','DRY MICROBURST 50','DRY MICROBURST 53 ','DRY MICROBURST 58','DRY MICROBURST 61','DRY MICROBURST 84','DRY MICROBURST WINDS','DRY MICROBURST','DRY MIRCOBURST WINDS','GUSTY LAKE WIND','GUSTY WIND','GUSTY WIND/HAIL','GUSTY WIND/HVY RAIN','GUSTY WIND/RAIN','GUSTY WINDS','HIGH  WINDS','HIGH WIND (G40)','HIGH WIND 48','HIGH WIND 63','HIGH WIND 70','HIGH WIND AND HEAVY SNOW','HIGH WIND DAMAGE','HIGH WIND','HIGH WIND/LOW WIND CHILL','HIGH WINDS 55','HIGH WINDS 57','HIGH WINDS 58','HIGH WINDS 63','HIGH WINDS 66','HIGH WINDS 67','HIGH WINDS 73','HIGH WINDS 76','HIGH WINDS 80','HIGH WINDS 82','HIGH WINDS','HIGH WINDS/','HIGH WINDS/COASTAL FLOOD','HIGH WINDS/COLD','HIGH WINDS/FLOODING','MICROBURST WINDS','MICROBURST','STORM FORCE WINDS','STRONG WIND GUST','STRONG WIND','STRONG WINDS','WIND ADVISORY','WIND AND WAVE','WIND DAMAGE','WIND GUSTS','WIND STORM','WIND','WIND/HAIL','WINDS','WND'),
  `Hurricane (Typhoon)` = c('HURRICANE EDOUARD','HURRICANE EMILY','HURRICANE ERIN','HURRICANE FELIX','HURRICANE GORDON','HURRICANE OPAL','HURRICANE OPAL/HIGH WINDS ','HURRICANE','HURRICANE-GENERATED SWELLS','HURRICANE/TYPHOON','REMNANTS OF FLOYD','TYPHOON'),
  `Ice Storm` = c('ICE STORM AND SNOW','ICE STORM','ICE STORM/FLASH FLOOD','ICE/STRONG WINDS','ICESTORM/BLIZZARD'),
  `Lake-Effect Snow` = c('HEAVY LAKE SNOW','LAKE EFFECT SNOW','LAKE-EFFECT SNOW'),
  `Lakeshore Flood` = c('LAKE FLOOD','LAKESHORE FLOOD'),
  Lightning = c('LIGHTING','LIGHTNING  WAUSEON','LIGHTNING AND HEAVY RAIN','LIGHTNING AND THUNDERSTORM WIN','LIGHTNING AND WINDS','LIGHTNING DAMAGE','LIGHTNING FIRE','LIGHTNING INJURY','LIGHTNING','LIGHTNING.','LIGHTNING/HEAVY RAIN','LIGNTNING'),
  `Marine Hail` = c('MARINE HAIL'),
  `Marine High Wind` = c('HIGH WIND AND SEAS','HIGH WIND AND HIGH TIDES','HIGH WIND/SEAS'),
  `Marine Strong Wind` = c('MARINE HIGH WIND','MARINE STRONG WIND'),
  `Marine Thunderstorm Wind` = c('MARINE THUNDERSTORM WIND','MARINE TSTM WIND'),
  `Rip Current` = c('RIP CURRENT','RIP CURRENTS HEAVY SURF','RIP CURRENTS','RIP CURRENTS/HEAVY SURF'),
  Seiche = c('ROGUE WAVE','SEICHE'),
  Sleet = c('SLEET & FREEZING RAIN','SLEET STORM','SLEET','SLEET/FREEZING RAIN','SLEET/ICE STORM','SLEET/RAIN/SNOW','SLEET/SNOW'),
  `Storm Surge/Tide` = c('BLOW-OUT TIDE','BLOW-OUT TIDES','COASTAL SURGE','STORM SURGE','STORM SURGE/TIDE','TIDAL FLOOD','TIDAL FLOODING'),
  `Strong Wind` = c('GRADIENT WIND','GRADIENT WINDS','NON TSTM WIND','NON-SEVERE WIND DAMAGE','NON-TSTM WIND'),
  `Thunderstorm Wind` = c('GUSTNADO','GUSTNADO AND','GUSTY THUNDERSTORM WIND','GUSTY THUNDERSTORM WINDS','LARGE WALL CLOUD','LIGHTNING THUNDERSTORM WINDS','LIGHTNING THUNDERSTORM WINDSS','ROTATING WALL CLOUD','SEVERE THUNDERSTORM WINDS','SEVERE THUNDERSTORM','SEVERE THUNDERSTORMS','SEVERE TURBULENCE','THUDERSTORM WINDS','THUNDEERSTORM WINDS','THUNDERESTORM WINDS','THUNDERSTORM  WINDS','THUNDERSTORM DAMAGE TO','THUNDERSTORM DAMAGE','THUNDERSTORM W INDS','THUNDERSTORM DAMAGE TO','THUNDERSTORM DAMAGE','THUNDERSTORM W INDS','THUNDERSTORM WIND (G40)','THUNDERSTORM WIND 50','THUNDERSTORM WIND 52','THUNDERSTORM WIND 56','THUNDERSTORM WIND 59 MPH','THUNDERSTORM WIND 59 MPH.','THUNDERSTORM WIND 59','THUNDERSTORM WIND 60 MPH','THUNDERSTORM WIND 65 MPH','THUNDERSTORM WIND 65MPH','THUNDERSTORM WIND 69','THUNDERSTORM WIND 98 MPH','THUNDERSTORM WIND G50','THUNDERSTORM WIND G51','THUNDERSTORM WIND G52','THUNDERSTORM WIND G55','THUNDERSTORM WIND G60','THUNDERSTORM WIND G61','THUNDERSTORM WIND TREES','THUNDERSTORM WIND','THUNDERSTORM WIND.','THUNDERSTORM WIND/ TREE','THUNDERSTORM WIND/ TREES','THUNDERSTORM WIND/AWNING','THUNDERSTORM WIND/HAIL','THUNDERSTORM WIND/LIGHTNING','THUNDERSTORM WINDS      LE CEN','THUNDERSTORM WINDS 13','THUNDERSTORM WINDS 2','THUNDERSTORM WINDS 50','THUNDERSTORM WINDS 52','THUNDERSTORM WINDS 53','THUNDERSTORM WINDS 60','THUNDERSTORM WINDS 61','THUNDERSTORM WINDS 62','THUNDERSTORM WINDS 63 MPH','THUNDERSTORM WINDS AND','THUNDERSTORM WINDS FUNNEL CLOU','THUNDERSTORM WINDS G','THUNDERSTORM WINDS G60','THUNDERSTORM WINDS HAIL','THUNDERSTORM WINDS HEAVY RAIN','THUNDERSTORM WINDS LIGHTNING','THUNDERSTORM WINDS SMALL STREA','THUNDERSTORM WINDS URBAN FLOOD','THUNDERSTORM WINDS','THUNDERSTORM WINDS.','THUNDERSTORM WINDS/ FLOOD','THUNDERSTORM WINDS/ HAIL','THUNDERSTORM WINDS/FLASH FLOOD','THUNDERSTORM WINDS/FLOODING','THUNDERSTORM WINDS/FUNNEL CLOU','THUNDERSTORM WINDS/HAIL','THUNDERSTORM WINDS/HEAVY RAIN','THUNDERSTORM WINDS53','THUNDERSTORM WINDSHAIL','THUNDERSTORM WINDSS','THUNDERSTORM WINS','THUNDERSTORM','THUNDERSTORMS WIND','THUNDERSTORMS WINDS','THUNDERSTORMS','THUNDERSTORMW 50','THUNDERSTORMW WINDS','THUNDERSTORMW','THUNDERSTORMWINDS','THUNDERSTROM WIND','THUNDERSTROM WINDS','THUNDERTORM WINDS','THUNDERTSORM WIND','THUNDESTORM WINDS','THUNERSTORM WINDS','TSTM WIND  (G45)','TSTM WIND (41)','TSTM WIND (G35)','TSTM WIND (G40)','TSTM WIND (G45)','TSTM WIND 40','TSTM WIND 45','TSTM WIND 50','TSTM WIND 51','TSTM WIND 52','TSTM WIND 55','TSTM WIND 65)','TSTM WIND AND LIGHTNING','TSTM WIND DAMAGE','TSTM WIND G45','TSTM WIND G58','TSTM WIND','TSTM WIND/HAIL','TSTM WINDS','TSTM WND','TSTM','TSTMW','TUNDERSTORM WIND','WAKE LOW WIND','WALL CLOUD'),
  Tornado = c('COLD AIR TORNADO','LANDSPOUT','TORNADO DEBRIS','TORNADO F0','TORNADO F1','TORNADO F2','TORNADO F3','TORNADO','TORNADO/WATERSPOUT','TORNADOES','"TORNADOES',' TSTM WIND',' HAIL"','TORNADOS','TORNDAO'),
  `Tropical Depression` = c('TROPICAL DEPRESSION'),
  `Tropical Storm` = c('TROPICAL STORM ALBERTO','TROPICAL STORM DEAN','TROPICAL STORM GORDON','TROPICAL STORM JERRY','TROPICAL STORM'),
  Tsunami = c('TSUNAMI'),
  `Volcanic Ash` = c('VOG','VOLCANIC ASH PLUME','VOLCANIC ASH','VOLCANIC ASHFALL','VOLCANIC ERUPTION'),
  Waterspout = c('WATER SPOUT','WATERSPOUT FUNNEL CLOUD','WATERSPOUT TORNADO','WATERSPOUT','WATERSPOUT-','WATERSPOUT-TORNADO','WATERSPOUT/ TORNADO','WATERSPOUT/','WATERSPOUT/TORNADO','WATERSPOUTS','WAYTERSPOUT'),
  Wildfire = c('BRUSH FIRE','BRUSH FIRES','FOREST FIRES','GRASS FIRES','RED FLAG FIRE WX','WILD FIRES','WILD/FOREST FIRE','WILD/FOREST FIRES','WILDFIRE','WILDFIRES'),
  `Winter Storm` = c('HIGH WINDS/SNOW','SNOW- HIGH WIND- WIND CHILL','SNOW/ICE STORM','SNOWSTORM','THUNDERSNOW SHOWER','THUNDERSNOW','WINTER STORM HIGH WINDS','WINTER STORM','WINTER STORM/HIGH WIND','WINTER STORM/HIGH WINDS','WINTER STORMS'),
  `Winter Weather` = c('BLOWING SNOW & EXTREME WIND CH','BLOWING SNOW','BLOWING SNOW- EXTREME WIND CHI','BLOWING SNOW/EXTREME WIND CHIL','COLD AND SNOW','COLD AND WET CONDITIONS','COOL AND WET','COOL SPELL ','EARLY SNOW','EARLY SNOWFALL','FALLING SNOW/ICE','FIRST FROST','FIRST SNOW','HEAVY SNOWPACK','HEAVY WET SNOW','ICE AND SNOW','ICE FLOES','ICE JAM','ICE ON ROAD','ICE PELLETS','ICE ROADS','ICE','ICE/SNOW','ICY ROADS','LATE FREEZE','LATE SEASON HAIL','LATE SEASON SNOW','LATE SEASON SNOWFALL','LATE SNOW','LIGHT FREEZING RAIN','LIGHT SNOW AND SLEET','LIGHT SNOW','LIGHT SNOW/FLURRIES','LIGHT SNOW/FREEZING PRECIP','LIGHT SNOWFALL','MODERATE SNOW','MODERATE SNOWFALL','SEASONAL SNOWFALL','SNOW SHOWERS','SNOW SLEET','SNOW','SNOW/ ICE','SNOW/BLOWING SNOW','SNOW/COLD','SNOW/FREEZING RAIN','SNOW/HEAVY SNOW','SNOW/HIGH WINDS','SNOW/ICE','SNOW/RAIN','SNOW/RAIN/SLEET','SNOW/SLEET','SNOW/SLEET/FREEZING RAIN','SNOW/SLEET/RAIN','SNOW\\COLD','UNUSUALLY LATE SNOW','WET SNOW','WINTER MIX','WINTER WEATHER MIX','WINTER WEATHER','WINTER WEATHER/MIX','WINTERY MIX','WINTRY MIX'),
  Unknown = c('APACHE COUNTY','DROWNING','EXCESSIVE','HEAVY MIX','HIGH','MARINE ACCIDENT','MARINE MISHAP','"METRO STORM',' MAY 26"','MILD AND DRY PATTERN','MILD PATTERN','MILD/DRY PATTERN','MIXED PRECIPITATION','MIXED PRECIP','MONTHLY PRECIPITATION','MONTHLY RAINFALL','MONTHLY SNOWFALL','MONTHLY TEMPERATURE','NO SEVERE WEATHER','NONE','NORMAL PRECIPITATION','NORTHERN LIGHTS','OTHER','RECORD LOW','RED FLAG CRITERIA','SOUTHEAST','SUMMARY AUGUST 10','SUMMARY AUGUST 11','SUMMARY AUGUST 17','SUMMARY AUGUST 2-3','SUMMARY AUGUST 21','SUMMARY AUGUST 28','SUMMARY AUGUST 4','SUMMARY AUGUST 7','SUMMARY AUGUST 9','SUMMARY JAN 17','SUMMARY JULY 23-24','SUMMARY JUNE 18-19','SUMMARY JUNE 5-6','SUMMARY JUNE 6','SUMMARY OF APRIL 12','SUMMARY OF APRIL 13','SUMMARY OF APRIL 21','SUMMARY OF APRIL 27','SUMMARY OF APRIL 3RD','SUMMARY OF AUGUST 1','SUMMARY OF JULY 11','SUMMARY OF JULY 2','SUMMARY OF JULY 22','SUMMARY OF JULY 26','SUMMARY OF JULY 29','SUMMARY OF JULY 3','SUMMARY OF JUNE 10','SUMMARY OF JUNE 11','SUMMARY OF JUNE 12','SUMMARY OF JUNE 13','SUMMARY OF JUNE 15','SUMMARY OF JUNE 16','SUMMARY OF JUNE 18','SUMMARY OF JUNE 23','SUMMARY OF JUNE 24','SUMMARY OF JUNE 3','SUMMARY OF JUNE 30','SUMMARY OF JUNE 4','SUMMARY OF JUNE 6','SUMMARY OF MARCH 14','SUMMARY OF MARCH 23','SUMMARY OF MARCH 24','SUMMARY OF MARCH 24-25','SUMMARY OF MARCH 27','SUMMARY OF MARCH 29','SUMMARY OF MAY 10','SUMMARY OF MAY 13','SUMMARY OF MAY 14','SUMMARY OF MAY 22 AM','SUMMARY OF MAY 22 PM','SUMMARY OF MAY 22','SUMMARY OF MAY 26 AM','SUMMARY OF MAY 26 PM','SUMMARY OF MAY 31 AM','SUMMARY OF MAY 31 PM','SUMMARY OF MAY 9-10','SUMMARY SEPT. 25-26','SUMMARY SEPTEMBER 20','SUMMARY SEPTEMBER 23','SUMMARY SEPTEMBER 3','SUMMARY SEPTEMBER 4','SUMMARY: NOV. 16','SUMMARY: NOV. 6-7','SUMMARY: OCT. 20-21','SUMMARY: OCTOBER 31','SUMMARY: SEPT. 18','BEACH EROSIN','BEACH EROSION','COASTAL EROSION')
)
```

The mapping process involves finding which element of the list the `EVTYPE` exists in, and using that element name as the corrected value in a new `Event` column:


```r
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

dfEvents <- dfEvents %>% 
  mutate(Event = apply(dfEvents, 1, set_allowed_event_type)) %>%
  relocate(Event, .after = EVTYPE)

dfEvents %>%
  select(BGN_DATE, EVTYPE, Event) %>%
  slice_head(n=10)
```

```
## # A tibble: 10 x 3
##    BGN_DATE   EVTYPE       Event            
##    <date>     <chr>        <chr>            
##  1 1996-01-06 WINTER STORM Winter Storm     
##  2 1996-01-11 TORNADO      Tornado          
##  3 1996-01-11 TSTM WIND    Thunderstorm Wind
##  4 1996-01-11 TSTM WIND    Thunderstorm Wind
##  5 1996-01-11 TSTM WIND    Thunderstorm Wind
##  6 1996-01-18 HIGH WIND    High Wind        
##  7 1996-01-19 TSTM WIND    Thunderstorm Wind
##  8 1996-01-24 TSTM WIND    Thunderstorm Wind
##  9 1996-01-24 TSTM WIND    Thunderstorm Wind
## 10 1996-01-26 FLASH FLOOD  Flash Flood
```

Note: in the case that an `EVTYPE` has been added more than once to the `event_types` list, only the first result is returned.

The official allowed event types can be somewhat arbitrary (what draws the line between a 'Heat' event and an 'Excessive Heat' event for example?), and this is really a weakness in the analysis. How the entries have been mapped has a direct impact on the final results.

Having said that, it is a simple task to revisit the mapping and rerun the analysis once that has been done if necessary.

In case of future analysis, we convert the list to a dataframe and push out to CSV:


```r
max_length <- max(sapply(event_types, length))
event_types <- lapply(
  event_types,function(x) {
    ans <- rep(NA,length=max_length);
    ans[1:length(x)]<- x;
    return(ans)
  }
)
event_types <- do.call(cbind,event_types)

if (!dir.exists('./data/processed')){
  dir.create('./data/processed', recursive = T)
}
write.csv(event_types, './data/processed/map_evtypes.csv', row.names = F)
```


### Calculating actual cost using the EXP columns & adjusting values for inflation
 
#### Inflation 

Since we are comparing costs across a lengthy period of time, we need to adjust values for inflation as the currency devalues with time. $1 Billion in 1996 was worth considerably more than the same value in 2011, at the end of the analysis period.

To get an approximation of inflation over the period, we can use the CPI values for start and end using the following formula:

`(B - A)/A` where `A` is the starting CPI and `B` is the ending CPI

The average annual inflation rate is approximated by dividing this number by the number of years.

The [US Bureau of Labor Statistics](https://www.bls.gov/) gives the CPI for 1996 as 66.18, and for 2012 as 96.87.


```r
cpi1996 <- 66.18
cpi2012 <- 96.87
mean_inflation_rate <- (cpi2012-cpi1996)/cpi1996/(2012-1996)
mean_inflation_rate
```

```
## [1] 0.02898345
```

The average inflation rate over this period was 2.9%.
 
For each year in the period, we can calculate the equivalent 2012 value using the formula:

`Value2012 = PV ?? (1 + i)^n` where PV is the recorded value and n is the years until 2012. 

As an example, $1000 in 1996 is calculated as follows:

`$1,000 ?? (1 + 0.029)^16 = $1,579.96`

We create a row to indicate this n value for each entry and apply it later when we calculate the property and crop costs:


```r
end_date <- ymd("2012-01-01")
dfEvents <- dfEvents %>%
  mutate(inflation_period = as.numeric(interval(BGN_DATE, end_date) / years(1)))
```

#### Using the EXP Values

The EXP columns have [generated much discussion](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html) as once again, they do not follow allowed entry values. These should be one of B or b = Billion, M or m = Million, K or k = Thousand, H or h = Hundred.

Fortunately, since we are only approaching this after subsetting the data, the values remaining all fit within those allowed values:


```r
unique(dfEvents$PROPDMGEXP)
```

```
## [1] "K" NA  "M" "B"
```

```r
unique(dfEvents$CROPDMGEXP)
```

```
## [1] "K" NA  "M" "B"
```

For the sake of completeness, I have included and catered for all encountered values in the original dataset in the case that a newer set of data is imported in the future. 

We use a function to apply to the dataset where we calculate either property or crop damage and apply the inflation adjustment at the same time:


```r
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
```

Now we use this function for the property and crop values:


```r
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
dfEvents %>%
  select(BGN_DATE, Event, PROPERTY_DAMAGE, CROP_DAMAGE) %>%
  slice_head(n=10)
```

```
## # A tibble: 10 x 4
##    BGN_DATE   Event             PROPERTY_DAMAGE CROP_DAMAGE
##    <date>     <chr>                       <dbl>       <dbl>
##  1 1996-01-06 Winter Storm               599995       60000
##  2 1996-01-11 Tornado                    157832           0
##  3 1996-01-11 Thunderstorm Wind            4735           0
##  4 1996-01-11 Thunderstorm Wind            7892           0
##  5 1996-01-11 Thunderstorm Wind            3157           0
##  6 1996-01-18 High Wind                  630981           0
##  7 1996-01-19 Thunderstorm Wind           18928           0
##  8 1996-01-24 Thunderstorm Wind           12614           0
##  9 1996-01-24 Thunderstorm Wind           18921           0
## 10 1996-01-26 Flash Flood                118235           0
```

### Checking for, and fixing, erroneous entries

Looking at max values of each of the indicator columns, we can check for spurious values:


```r
dfEvents %>% summarise(
  max(FATALITIES),
  max(INJURIES),
  max(PROPERTY_DAMAGE)/1e9,
  max(CROP_DAMAGE)/1e9
)
```

```
## # A tibble: 1 x 4
##   `max(FATALITIES)` `max(INJURIES)` `max(PROPERTY_DAMAGE)/~ `max(CROP_DAMAGE)/1~
##               <dbl>           <dbl>                   <dbl>                <dbl>
## 1               158            1150                    137.                 1.81
```

The value of $137 Billion property damage seems excessive. 

Check the date of the event:


```r
refnum <- dfEvents %>% 
  filter(PROPERTY_DAMAGE == max(PROPERTY_DAMAGE))
refnum <- as.numeric(refnum$REFNUM)
dfEvents$BGN_DATE[dfEvents$REFNUM == refnum]
```

```
## [1] "2006-01-01"
```

Checking the [NOAA website](https://www.ncdc.noaa.gov/billions/events/US/2005-2006) for any events recorded with this value during this period shows no events even close.

We can look at the remarks for this event:


```r
dfEvents$REMARKS[dfEvents$REFNUM == refnum]
```

```
## [1] "Major flooding continued into the early hours of January 1st, before the Napa River finally fell below flood stage and the water receeded. Flooding was severe in Downtown Napa from the Napa Creek and the City and Parks Department was hit with $6 million in damage alone. The City of Napa had 600 homes with moderate damage, 150 damaged businesses with costs of at least $70 million."
```

The discussion appears to be talking of values in the league of \$100 Million rather than \$100 Billion. 

We can assume this was mis-keyed with a `B` instead of an `M`.

Correcting this:


```r
dfEvents$PROPDMGEXP[dfEvents$REFNUM == refnum] <- 'M'
dfEvents$PROPERTY_DAMAGE[dfEvents$REFNUM == refnum] <- 
  dfEvents$PROPERTY_DAMAGE[dfEvents$REFNUM == refnum] / 1000

dfEvents$PROPERTY_DAMAGE[dfEvents$REFNUM == refnum]
```

```
## [1] 136504884
```

The corrected value is now \$137 Million.


```r
# Recheck for spurious values:
dfEvents %>% summarise(
  max(FATALITIES),
  max(INJURIES),
  max(PROPERTY_DAMAGE)/1e9,
  max(CROP_DAMAGE)/1e9
)
```

```
## # A tibble: 1 x 4
##   `max(FATALITIES)` `max(INJURIES)` `max(PROPERTY_DAMAGE)/~ `max(CROP_DAMAGE)/1~
##               <dbl>           <dbl>                   <dbl>                <dbl>
## 1               158            1150                    37.5                 1.81
```

The maximum values now appear realistic.

---

# Results

## Across the United States, which types of events are most harmful with respect to population health?

For this analysis, we look at deaths and injuries attributed to each event type over the 16 year period:


```r
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

human_damage %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("condensed"))
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Event </th>
   <th style="text-align:right;"> Deaths </th>
   <th style="text-align:right;"> Injuries </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Tornado </td>
   <td style="text-align:right;"> 1511 </td>
   <td style="text-align:right;"> 20667 </td>
   <td style="text-align:right;"> 22178 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Excessive Heat </td>
   <td style="text-align:right;"> 2036 </td>
   <td style="text-align:right;"> 7683 </td>
   <td style="text-align:right;"> 9719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Flood </td>
   <td style="text-align:right;"> 447 </td>
   <td style="text-align:right;"> 6838 </td>
   <td style="text-align:right;"> 7285 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Thunderstorm Wind </td>
   <td style="text-align:right;"> 379 </td>
   <td style="text-align:right;"> 5129 </td>
   <td style="text-align:right;"> 5508 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lightning </td>
   <td style="text-align:right;"> 650 </td>
   <td style="text-align:right;"> 4140 </td>
   <td style="text-align:right;"> 4790 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Flash Flood </td>
   <td style="text-align:right;"> 887 </td>
   <td style="text-align:right;"> 1674 </td>
   <td style="text-align:right;"> 2561 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> High Wind </td>
   <td style="text-align:right;"> 371 </td>
   <td style="text-align:right;"> 1503 </td>
   <td style="text-align:right;"> 1874 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wildfire </td>
   <td style="text-align:right;"> 87 </td>
   <td style="text-align:right;"> 1458 </td>
   <td style="text-align:right;"> 1545 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Winter Storm </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 1292 </td>
   <td style="text-align:right;"> 1483 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hurricane (Typhoon) </td>
   <td style="text-align:right;"> 125 </td>
   <td style="text-align:right;"> 1328 </td>
   <td style="text-align:right;"> 1453 </td>
  </tr>
</tbody>
</table>

*Ten most severe Storm Event Types ranked in order of greatest US human cost over 1996 - 2011 period*


```r
plot_ly(data=human_damage, x = ~Event, y = ~Deaths, type = 'bar', name = 'Deaths') %>%
  add_trace(y = ~Injuries, name = 'Injuries') %>%
  layout(title = list(text = paste0('Ten Highest Causes of Human Costs for Period 1996 to 2011 (inclusive)')),
         xaxis = list(title = ""),
         yaxis = list(title = 'Total Count'), 
         barmode = 'stack'
  ) %>% 
  config(displayModeBar = F) %>%
  layout(xaxis=list(fixedrange=TRUE)) %>% 
  layout(yaxis=list(fixedrange=TRUE))
```

```{=html}
<div id="htmlwidget-7d71708a1644f6c9f9c1" style="width:768px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-7d71708a1644f6c9f9c1">{"x":{"visdat":{"31e068e676f0":["function () ","plotlyVisDat"]},"cur_data":"31e068e676f0","attrs":{"31e068e676f0":{"x":{},"y":{},"name":"Deaths","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"31e068e676f0.1":{"x":{},"y":{},"name":"Injuries","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"Ten Highest Causes of Human Costs for Period 1996 to 2011 (inclusive)"},"xaxis":{"domain":[0,1],"automargin":true,"title":"","fixedrange":true,"type":"category","categoryorder":"array","categoryarray":["Tornado","Excessive Heat","Flood","Thunderstorm Wind","Lightning","Flash Flood","High Wind","Wildfire","Winter Storm","Hurricane (Typhoon)"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Total Count","fixedrange":true},"barmode":"stack","hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":["Tornado","Excessive Heat","Flood","Thunderstorm Wind","Lightning","Flash Flood","High Wind","Wildfire","Winter Storm","Hurricane (Typhoon)"],"y":[1511,2036,447,379,650,887,371,87,191,125],"name":"Deaths","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Tornado","Excessive Heat","Flood","Thunderstorm Wind","Lightning","Flash Flood","High Wind","Wildfire","Winter Storm","Hurricane (Typhoon)"],"y":[20667,7683,6838,5129,4140,1674,1503,1458,1292,1328],"name":"Injuries","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

From this we can discern that the event type with the highest mortality rate is due to excessive heat, followed by tornadoes.

By far, the most injurious type are tornadoes.

## Across the United States, which types of events have the greatest economic consequences?

For this analysis, we look at property and crop damage costs attributed to each event type over the 16 year period:


```r
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


financial_damage %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("condensed"))
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Event </th>
   <th style="text-align:right;"> Property </th>
   <th style="text-align:right;"> Crop </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Hurricane (Typhoon) </td>
   <td style="text-align:right;"> 100.87 </td>
   <td style="text-align:right;"> 7.02 </td>
   <td style="text-align:right;"> 107.89 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm Surge/Tide </td>
   <td style="text-align:right;"> 56.88 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 56.88 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Flood </td>
   <td style="text-align:right;"> 36.16 </td>
   <td style="text-align:right;"> 6.07 </td>
   <td style="text-align:right;"> 42.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tornado </td>
   <td style="text-align:right;"> 29.10 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 29.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hail </td>
   <td style="text-align:right;"> 17.92 </td>
   <td style="text-align:right;"> 3.19 </td>
   <td style="text-align:right;"> 21.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Flash Flood </td>
   <td style="text-align:right;"> 18.98 </td>
   <td style="text-align:right;"> 1.64 </td>
   <td style="text-align:right;"> 20.62 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Drought </td>
   <td style="text-align:right;"> 1.37 </td>
   <td style="text-align:right;"> 17.82 </td>
   <td style="text-align:right;"> 19.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Thunderstorm Wind </td>
   <td style="text-align:right;"> 9.92 </td>
   <td style="text-align:right;"> 1.29 </td>
   <td style="text-align:right;"> 11.21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tropical Storm </td>
   <td style="text-align:right;"> 10.14 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 11.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wildfire </td>
   <td style="text-align:right;"> 9.81 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 10.31 </td>
  </tr>
</tbody>
</table>

*Ten most severe Storm Event Types ranked in order of greatest US damage cost over 1996 - 2011 period.<br>Damage costs in USD Billions adjusted to 2012 USD value.*


```r
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
  ) %>% 
  config(displayModeBar = F) %>%
  layout(xaxis=list(fixedrange=TRUE)) %>% 
  layout(yaxis=list(fixedrange=TRUE))
```

```{=html}
<div id="htmlwidget-28400490c982f3744d85" style="width:768px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-28400490c982f3744d85">{"x":{"visdat":{"31e041db7c1c":["function () ","plotlyVisDat"]},"cur_data":"31e041db7c1c","attrs":{"31e041db7c1c":{"x":{},"y":{},"name":"Property","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"31e041db7c1c.1":{"x":{},"y":{},"name":"Crop","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"Ten Highest Causes of Damage Costs for Period 1996 to 2011 (inclusive)<br><sup>(USD adjusted to 2012 US Dollar Value)<\/sup>"},"xaxis":{"domain":[0,1],"automargin":true,"title":"","fixedrange":true,"type":"category","categoryorder":"array","categoryarray":["Hurricane (Typhoon)","Storm Surge/Tide","Flood","Tornado","Hail","Flash Flood","Drought","Thunderstorm Wind","Tropical Storm","Wildfire"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Total Damage Cost","fixedrange":true},"barmode":"stack","hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":["Hurricane (Typhoon)","Storm Surge/Tide","Flood","Tornado","Hail","Flash Flood","Drought","Thunderstorm Wind","Tropical Storm","Wildfire"],"y":[100.87,56.88,36.16,29.1,17.92,18.98,1.37,9.92,10.14,9.81],"name":"Property","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Hurricane (Typhoon)","Storm Surge/Tide","Flood","Tornado","Hail","Flash Flood","Drought","Thunderstorm Wind","Tropical Storm","Wildfire"],"y":[7.02,0,6.07,0.34,3.19,1.64,17.82,1.29,0.87,0.5],"name":"Crop","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

The greatest damage to property comes from Hurricanes, however if we look at the three flood related categories combined (Storm Surge/Tide, Flood and Flash Flood), these add up to a greater cost. 

The analysis is a product of the categorization of the allowed event types and perhaps these should in reality be grouped together. It should also be kept in mind that some of these flood events will be related to the hurricanes.

The greatest crop damage is due to drought.

## Effect of Hurricane Katrina on the economic data

Looking at the table of top ten events, we can see five of the first six events are all related to Hurricane Katrina:


```r
# Consider property and crop damage as one
dfEvents <- dfEvents %>%
  mutate(DAMAGE = PROPERTY_DAMAGE + CROP_DAMAGE)

#Top 10 financial events
top10_financial_events <- dfEvents %>%
  select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, DAMAGE) %>%
  mutate(DAMAGE=round((DAMAGE/1e9),2)) %>%
  arrange(desc(DAMAGE)) %>%
  slice_head(n=10)

top10_financial_events %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("condensed"))
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> BGN_DATE </th>
   <th style="text-align:left;"> EVTYPE </th>
   <th style="text-align:right;"> FATALITIES </th>
   <th style="text-align:right;"> INJURIES </th>
   <th style="text-align:right;"> DAMAGE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2005-08-29 </td>
   <td style="text-align:left;"> STORM SURGE </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 37.52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2005-08-28 </td>
   <td style="text-align:left;"> HURRICANE/TYPHOON </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 20.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2005-08-29 </td>
   <td style="text-align:left;"> STORM SURGE </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 13.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2005-10-24 </td>
   <td style="text-align:left;"> HURRICANE/TYPHOON </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 11.93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2005-08-29 </td>
   <td style="text-align:left;"> HURRICANE/TYPHOON </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 8.86 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2005-08-28 </td>
   <td style="text-align:left;"> HURRICANE/TYPHOON </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 8.81 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2004-08-13 </td>
   <td style="text-align:left;"> HURRICANE/TYPHOON </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 780 </td>
   <td style="text-align:right;"> 7.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2001-06-05 </td>
   <td style="text-align:left;"> TROPICAL STORM </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 6.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2004-09-04 </td>
   <td style="text-align:left;"> HURRICANE/TYPHOON </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 6.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1999-09-15 </td>
   <td style="text-align:left;"> HURRICANE </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4.97 </td>
  </tr>
</tbody>
</table>

*Ten most severe storm events ranked in order of greatest US damage cost over 1996 - 2011 period.<br>Damage costs in USD Billions adjusted to 2012 USD value.*

We can look at these by filtering on billion dollar events over the period 27th to 30th August 2005:


```r
katrina <- dfEvents %>%
  filter(BGN_DATE>='2005-08-27' & BGN_DATE<='2005-08-30' & DAMAGE>1e9) %>%
  select(BGN_DATE, EVTYPE, DAMAGE, REFNUM, REMARKS)

katrina_sum <- sum(katrina$DAMAGE)/1e9
total_sum <- sum(dfEvents$DAMAGE)/1e9
# katrina effect
katrina_percent <- round(katrina_sum * 100 / total_sum)
```

\$90 billion - total of Katrina Billion Dollar events<br>
\$354 billion - total of all events over 1995-2011 period<br>
\------------<br>
25% - contribution of Katrina events to total damage over the 1995-2011 period

**Does the effect of this mega event skew the data for the rest of the period?**

We can consider the data again with the Katrina events omitted:


```r
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

top10_financial_events_no_katrina %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("condensed"))
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Event </th>
   <th style="text-align:right;"> Property </th>
   <th style="text-align:right;"> Crop </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Hurricane (Typhoon) </td>
   <td style="text-align:right;"> 63.51 </td>
   <td style="text-align:right;"> 5.21 </td>
   <td style="text-align:right;"> 68.72 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Flood </td>
   <td style="text-align:right;"> 36.16 </td>
   <td style="text-align:right;"> 6.07 </td>
   <td style="text-align:right;"> 42.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tornado </td>
   <td style="text-align:right;"> 29.10 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 29.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hail </td>
   <td style="text-align:right;"> 17.92 </td>
   <td style="text-align:right;"> 3.19 </td>
   <td style="text-align:right;"> 21.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Flash Flood </td>
   <td style="text-align:right;"> 18.98 </td>
   <td style="text-align:right;"> 1.64 </td>
   <td style="text-align:right;"> 20.62 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Drought </td>
   <td style="text-align:right;"> 1.37 </td>
   <td style="text-align:right;"> 17.82 </td>
   <td style="text-align:right;"> 19.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Thunderstorm Wind </td>
   <td style="text-align:right;"> 9.92 </td>
   <td style="text-align:right;"> 1.29 </td>
   <td style="text-align:right;"> 11.21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tropical Storm </td>
   <td style="text-align:right;"> 10.14 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 11.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wildfire </td>
   <td style="text-align:right;"> 9.81 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 10.31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> High Wind </td>
   <td style="text-align:right;"> 6.63 </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:right;"> 7.52 </td>
  </tr>
</tbody>
</table>

*Ten most severe storm events ranked in order of greatest US damage cost over 1996 - 2011 period with Katrina events omitted.<br>Damage costs in USD Billions adjusted to 2012 USD value.*

Cumulatively, hurricanes still are #1 cause, however flood and flash flood combined almost equal this value.

Conclusion, the Katrina event does not skew the data.

**Are these floods linked to the hurricane events as happened with Katrina?**

We can look at the top five contributors for each year:


```r
top5_by_year_no_katrina <- dfEvents %>% 
  filter(REFNUM %nin% katrina$REFNUM) %>%
  group_by(Year=year(BGN_DATE), Event) %>% 
  summarise(
    Total = round(sum(DAMAGE) / 1e9, 2)
  ) %>% 
  arrange(desc(Total))  %>% 
  arrange(Year, desc(Total)) %>% 
  slice_max(order_by=Total, n=5) %>%
  slice_head(n=5)
```

```
## `summarise()` has grouped output by 'Year'. You can override using the `.groups` argument.
```

```r
p <- ggplot(top5_by_year_no_katrina, aes(x=Year, y=Total, color=Event)) 
p + geom_line(aes(group=Event)) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_x_continuous(breaks = unique(top5_by_year_no_katrina$Year)) +
  scale_y_continuous(breaks = 0:ceiling(max(top5_by_year_no_katrina$Total))) +
  xlab(NULL) + 
  ylab("Total Damage Cost") +
  labs(
    title = "Total Damage Costs for Top 5 Event Types in Each Year with Katrina data omitted", 
    subtitle = "(USD adjusted to 2012 US Dollar Value)",
    color = "Event Type", 
    ) + 
  theme_minimal()
```

![](NOAA-Storm-Analysis_files/figure-html/top5_yr_no_kat-1.png)<!-- -->

The graph doesn't show any strong relationship between hurricane peaks and flooding.

Other than the hurricane spikes in 2004/2005, no particular event dominates each year, rather it's a cumulative effect over the years that dominates.

**Conclusion:** 

Hurricanes and the various forms of flooding cause the greatest economic damage even when omitting the Hurricane Katrina mega event.

---

&nbsp;

&nbsp;

&nbsp;



