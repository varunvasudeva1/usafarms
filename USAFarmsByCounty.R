library(tidyverse)
library(readxl)
library(ggthemes)
library(mapproj)
library(plotly)
library(stringr)

county_outlines <- map_data("county") 
head(county_outlines)
county_fips <- read_excel(path = "FoodEnvironmentAtlas.xls", sheet = "Supplemental Data - County") %>% 
  select(FIPS, State, County)
head(county_fips)
local_food <- read_excel(path="FoodEnvironmentAtlas.xls", sheet="LOCAL") %>% 
  select(FIPS, State, County, FRESHVEG_FARMS12, ORCHARD_ACRES12, 
         GHVEG_FARMS12, GHVEG_SQFTPTH12,DIRSALES_FARMS12)
head(local_food)


county_fips <- county_fips %>%
  mutate(County = str_replace(County, County, tolower(County))) %>%
  mutate(County = str_replace(County, " county", ""))

local_food <- local_food %>%
  mutate(County = str_replace(County, County, tolower(County)))

countiestemp <- inner_join(county_outlines, county_fips, by = c("subregion" = "County"))
head(countiestemp)
counties <- inner_join(countiestemp, local_food, by = c("subregion" = "County"))
head(counties)

counties <- counties %>%
  mutate(subregion = str_to_title(subregion))

# Warning: Plotly object takes time to generate due to large dataset
p <- ggplot(counties) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = DIRSALES_FARMS12, text = paste0("County: ", subregion, ", ", State.y, "<br>", "Number of Farms: ", DIRSALES_FARMS12))) +
  labs(title = "Number of Farms with Direct Sales by US County, 2012", x = "Longitude", y = "Latitude", fill = "Number of Farms") +
  scale_fill_gradient(trans = "log10") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(p, tooltip = c("text"))
