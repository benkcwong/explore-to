### https://github.com/sharlagelfand/opendatatoronto

# install.packages("opendatatoronto", dep = T)

library(opendatatoronto)
library(magrittr)
library(janitor)
library(jsonlite)
library(scales)
library(rnaturalearth)
library(tidyverse)

# packages <- list_packages(limit = 25)
# packages %>% View()
# marriage_licence_packages <- search_packages("Marriage Licence Statistics")
# marriage_licence_resources <- marriage_licence_packages %>%
#     list_package_resources()
# marriage_licence_resources
# list_package_resources("https://open.toronto.ca/dataset/sexual-health-clinic-locations-hours-and-services/")

# search_packages("Daily Shelter Occupancy") %>% list_package_resources()

dta <- list_package_resources("https://open.toronto.ca/dataset/daily-shelter-occupancy/") %>% 
    filter(name == "Daily shelter occupancy current ") %>% 
    get_resource()

dta_cln <- dta %>% 
    janitor::clean_names() %>% 
    select(-c(id_2, date_range)) %>% 
    mutate(
        occupancy_date = occupancy_date %>% lubridate::as_datetime(),
        occupancy_datetime = occupancy_date,
        occ_time = format(occupancy_datetime, "%H:%M:%S"),
        occupancy_date = occupancy_date %>% lubridate::ymd(),
        occ_year =  occupancy_date %>% lubridate::year(),
        occ_mth =  occupancy_date %>% lubridate::month() %>% 
            as_factor() %>% fct_recode(
              "January" = "1",
              "February" = "2",
              "March" = "3",
              "April" = "4",
              "May" = "5",
              "June" = "6",
              "July" = "7",
              "August" = "8",
              "September" = "9",
              "October" = "10",
              "November" = "11",
              "December" = "12",
            ) %>% fct_drop(),
        occ_week = occupancy_date %>% lubridate::isoweek(),
        occ_day = occupancy_date %>% lubridate::wday(week_start = 1) %>% 
            as_factor() %>% fct_recode("Monday" = "1",
                                       "Tuesday" = "2",
                                       "Wednesday" = "3",
                                       "Thursday" = "4",
                                       "Friday" = "5",
                                       "Saturday" = "6",
                                       "Sunday" = "7")
        # addresses = paste(shelter_address,
        #                   shelter_city,
        #                   shelter_postal_code,
        #                   shelter_province,
        #                   sep = ", ")
    )


# Change in % Occupancy ---------------------------------------------------

library(ggplot2)
dta_cln %>% 
    group_by(sector, occ_mth) %>% 
    summarize(
        sum_occ = sum(occupancy),
        sum_cap = sum(capacity),
    ) %>% 
    ungroup() %>% 
    mutate(
        pct_occ = sum_occ / sum_cap
    ) %>%
    ggplot(aes(x = occ_mth, y = pct_occ, fill = sector)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 20L)) +
    facet_wrap(~sector) +
    theme_minimal()

dta_cln %>% 
    group_by(shelter_name, occ_mth, sector) %>% 
    summarize(
        sum_occ = sum(occupancy),
        sum_cap = sum(capacity),
    ) %>% 
    ungroup() %>% 
    mutate(
        pct_occ = sum_occ / sum_cap
    ) %>%
    filter(pct_occ <= 1,
           pct_occ != 0) %>% 
    ggplot(aes(x = occ_mth, y = pct_occ, fill = sector)) +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(width = 0.2) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 20L)) +
    facet_wrap(~sector) +
    theme_minimal() +
    theme(
     axis.text.x = element_text(angle = 90)   
    )
 
dta_cln %>% 
    group_by(shelter_name, occ_day, sector) %>% 
    summarize(
        sum_occ = sum(occupancy),
        sum_cap = sum(capacity),
    ) %>% 
    ungroup() %>% 
    mutate(
        pct_occ = sum_occ / sum_cap
    ) %>%
    filter(pct_occ <= 1,
           pct_occ != 0) %>% 
    ggplot(aes(x = occ_day, y = pct_occ, fill = sector)) +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(width = 0.2) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 20L)) +
    facet_wrap(~sector) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90)   
    )
   
# Geocoding Shelter Addresses ---------------------------------------------

## Geocoding function using OSM Nominatim API
## details: https://towardsdatascience.com/geocoding-tableau-and-r-integration-c5b32dc0eda6

osm_geocode <- function(name, address, city, postal_code){
    # NOMINATIM SEARCH API URL
    src_url <- "https://nominatim.openstreetmap.org/search?q="
    
    # CREATE A FULL ADDRESS
    addr <- paste(address, city, postal_code, sep = "%2C")
    
    # CREATE A SEARCH URL BASED ON NOMINATIM API TO RETURN GEOJSON
    requests <- paste0(src_url, address, "&format=geojson")
    
    # ITERATE OVER THE URLS AND MAKE REQUEST TO THE SEARCH API
    for (i in 1:length(requests)) {
        
        # QUERY THE API TRANSFORM RESPONSE FROM JSON TO R LIST
        response <- xml2::read_html(requests[i]) %>%
            rvest::html_node("p") %>%
            rvest::html_text() %>%
            jsonlite::fromJSON()
        
        # FROM THE RESPONSE EXTRACT LATITUDE AND LONGITUDE COORDINATES
        lon <- response$features$geometry$coordinates[[1]][1]
        lat <- response$features$geometry$coordinates[[1]][2]
        
        # CREATE A COORDINATES DATAFRAME
        if(i == 1) {
            loc <- tibble(name = name[i], 
                          address = str_replace_all(addr[i], "%2C", ","),
                          latitude = lat, longitude = lon)
        }else{
            df <- tibble(name = name[i], 
                         address = str_replace_all(addr[i], "%2C", ","),
                         latitude = lat, longitude = lon)
            loc <- bind_rows(loc, df)
        }
    }
    return(loc)
}

locations <- dta_cln %>% 
    mutate(
        shelter_address = gsub("Bathrust", "Bathurst", shelter_address),
        shelter_address = gsub("St$", "Street", shelter_address),
        shelter_address = gsub(", 2nd floor", "", shelter_address),
    ) %>% 
    distinct(shelter_address, .keep_all = TRUE)

geocodes <- osm_geocode(name = locations$shelter_name,
                        address = locations$shelter_address,
                        city = locations$shelter_city,
                        postal_code = locations$shelter_postal_code)

### Merge Back to the Original Dataset

dta2 <- dta_cln %>% 
    inner_join(geocodes, by = c("shelter_name" = "name"))

# Map of Ontario/Canada 
# https://birdstudiescanada.github.io/naturecounts/articles/region-spatial.html

# library(GADM)
# library(raster)
# states <- getData(country="USA", level=1)
# provinces <- getData(country="Canada", level=1)

library(sf)

ontario <- rnaturalearth::ne_states(country = "Canada", returnclass = "sf") %>%
    filter(name == "Ontario") %>%
    sf::st_transform(3347)

obs_sf <- sf::st_as_sf(dta2, coords = c("longitude", "latitude"), 
                       crs = 4326) %>%
    sf::st_transform(3347) %>% 
    sf::st_join(distinct(ontario), left = FALSE)

# Need to specify zoom
zoom <- obs_sf %>% 
    st_bbox()

zoom_to <- obs_sf %>% 
    filter(shelter_city == "Toronto") %>% 
    st_bbox()

# Plot
ggplot() + 
    theme_bw() + 
    geom_sf(data = ontario) +  # Map of Ontario
    geom_sf(data = obs_sf, 
            size = 2.25,
            aes(colour = sector),
            alpha = 0.5) +
    coord_sf(xlim = zoom[c(1,3)], ylim = zoom[c(2,4)])

# Map Toronto

library(cancensus)

# usethis::edit_r_profile()

# retrieve sf dataframe
# toronto <- get_census(dataset='CA16', regions = list(CMA="35535"),
#                       # vectors=c("median_hh_income"="v_CA16_2397"), 
#                       level='CSD', quiet = TRUE, 
#                       geo_format = 'sf', 
#                       labels = 'short')

# Create Map Inset
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html

# Make an Animation

