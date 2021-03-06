
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
head(dfEvents, 10)


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
write.csv(ev2, './data/processed/map_evtypes.csv', row.names = F)

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

dfEvents %>%
  select(BGN_DATE, Event, PROPERTY_DAMAGE, CROP_DAMAGE) %>%
  slice_head(n=10)

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
dfEvents$BGN_DATE[dfEvents$REFNUM == refnum]

# From comments:
dfEvents$REMARKS[dfEvents$REFNUM == refnum]

# Conclusion, the event was miscoded with B instead of M
# Update value for that REFNUM

dfEvents$PROPDMGEXP[dfEvents$REFNUM == refnum] <- 'M'
newval <- dfEvents$PROPDMG[dfEvents$REFNUM == refnum] * 1e6
inflation_period <- as.numeric(dfEvents$inflation_period[dfEvents$REFNUM == refnum])
dfEvents$PROPERTY_DAMAGE[dfEvents$REFNUM == refnum] <-
  round(newval * ((1 + mean_inflation_rate) ** inflation_period))
dfEvents$PROPERTY_DAMAGE[dfEvents$REFNUM == refnum]

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
    Total = sum(DAMAGE) / 1e9
  ) %>% 
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




