library(pacman)
p_load(cowplot, tidyverse, purrr, comf, extrafont, ggrepel, xkcd, geofacet)


# how to get the xkcd font?
# I got it from http://simonsoftware.se/other/xkcd.ttf

# How to get the data?
# http://eklima.met.no
# you'll need to register first
# also, even if you select text + semicolon delimiter, 
# there's still some lines of metadata before and after the data.

# then normals -> monthly normal values -> next
# -> select the weather stations you want 
temperatures = read_csv2("data/temperature_1.csv") %>%
    # had to split the download in two
    bind_rows(read_csv2("data/temperature_2.csv")) %>%
    mutate(St.no = str_match(St.no, "\\d+")) %>%
    mutate_all(as.numeric)

# you can get region + lat/lon data with any data you download, just pick 
# counties + coordinates in the second screen
station = read_csv2("data/stations_1.csv") %>%
    rename(`Region/Country` = Region) %>%
    bind_rows(read_csv2("data/stations_2.csv"))

# Month -> monthly overview
humidity = read_csv2("data/monthly_overview.csv") %>%
    mutate(St.no = str_match(STATION, "\\d+"),
           Hum = str_match(Hum, "\\d+"),
           RR = str_match(RR, "\\d+")) %>%
    select(St.no, Hum, RR) %>%
    mutate_all(as.numeric)

combined_data = left_join(temperatures, station, by = c("St.no" = "Stnr")) %>%
    left_join(humidity) %>%
    mutate(winter_temperature = pmap_dbl(list(Dec, Jan, Feb), ~mean(.)),
           summer_temperature = pmap_dbl(list(Jun, Jul), ~mean(.)),
           hum_index = calcHumx(ta = summer_temperature,
                                rh = Hum)) %>%
    filter(complete.cases(hum_index),
           complete.cases(winter_temperature))

county_avg = combined_data %>%
    group_by(County) %>%
    summarise(winter_temperature = mean(c(Dec, Jan, Feb)),
              summer_temperature = mean(c(Jun, Jul)),
              Hum = mean(Hum),
              hum_index = calcHumx(ta = summer_temperature,
                                   rh = Hum),
              Longitude = mean(Longitude),
              Latitude = mean(Latitude),
              rainfall = mean(RR, na.rm = T))


xrange <- range(county_avg$hum_index) + c(-1, 1)
yrange <- range(county_avg$winter_temperature) + c(-1, 1)

# scatterplot
county_avg %>%
    ggplot(aes(hum_index, winter_temperature)) +
    geom_text_repel(aes(label = County), family = "xkcd",
                    max.iter = 50000, size = 4)+
    geom_point(aes(colour = rainfall), size=3) +
    ggtitle("Where to live in Norway \nbased on your temperature preferences",
            subtitle = "Data from MET Norway, 2016-2017, averaged per county") +
    xlab("Summer heat and humidity via Humidex")+
    ylab("Winter temperature in Celsius degrees") +
    xkcdaxis(xrange = xrange,
             yrange = yrange)+
    theme_xkcd() +
    guides(colour = guide_legend(title = "Yearly rainfall (mm)")) +
    theme(text = element_text(size = 13, family = "xkcd"),
          axis.line = element_blank(),
          legend.position = c(.1, .8))

# select(county_avg, County) %>%
#     write.csv("data/county_avg.csv")


# on a map
norway_map <- map_data(map = "world",region = "Norway") %>%
    filter(!subregion %in% c("Svalbard", "Jan Mayen"))

county_avg %>% 
    ggplot( aes(Longitude, Latitude))+
    geom_polygon(data = norway_map, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
    geom_label_repel(aes(label = County, fill = winter_temperature),
    family = "xkcd", size = 3.5,
                    max.iter = 50000)+
        geom_point(aes(colour = hum_index), size = 5)+
    coord_map()+
    labs(title = "Winter temperature and summer temperature/humidity in Norway",
         subtitle = "data from MET Norway, 2016-2017",
         x = "", y = "")+
    theme_xkcd()+
    theme(text = element_text(size = 16, family = "xkcd"),
          legend.position = c(.6, .35)) +
    scale_colour_continuous(high = "#d7191c", low = "#2b83ba") +
    scale_fill_continuous(low = "#2b83ba", high = "#ffffff") +
    guides(colour = guide_legend(title = "Summer heat index", reverse = T),
           fill = guide_legend(title = "Winter Temperature", reverse = T))



## Trying out geofacet
no_counties <- data.frame(
    # name.County = c("4", "17", "9", "8", "7", "15", "5", "14", "10", "3", "6", "11", "13", "12", "19", "16","2", "18"),
    code = c("Finnmark", "Troms", "Nordland", "Nord-Trøndelag", "Møre Og Romsdal", "Sør-Trøndelag", "Hedmark", "Sogn Og Fjordane", "Oppland", "Buskerud", "Hordaland", "Oslo", "Rogaland", "Østfold", "Vestfold", "Telemark", "Aust-Agder", "Vest-Agder"),
    name = c("Finnmark", "Troms", "Nordland", "Nord-Trøndelag", "Møre Og Romsdal", "Sør-Trøndelag", "Hedmark", "Sogn Og Fjordane", "Oppland", "Buskerud", "Hordaland", "Oslo", "Rogaland", "Østfold", "Vestfold", "Telemark", "Aust-Agder", "Vest-Agder"),
    row = c(1, 1, 2, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8),
    col = c(5, 4, 3, 2, 1, 2, 3, 1, 2, 2, 1, 3, 1, 4, 3, 2, 3, 2),
    stringsAsFactors = FALSE
)

county_avg %>%
    # filter(County %in% c("Hordaland, Akershus, Troms")) %>%
    # gather("key", "value", winter_temperature, hum_index) %>%
    ggplot(aes(factor(1), factor(1), colour = hum_index , size = winter_temperature)) +
    coord_flip() +
    # geom_bar(stat = "identity") +
    geom_point() +
    facet_geo(~ County, grid = no_counties) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(.2,.9)) +
    labs(x = "", y = "", title = "Where to live in Norway, based on Winter Temperature and Summer Humidity Index") +
    guides(colour = guide_colourbar(title = "Summer Humidity Index", direction = "horizontal", title.position = "top"),
           size = guide_legend(title = "Winter Temperature °C", direction = "horizontal", title.position = "top")) +
    scale_size_continuous(range = c(2,10))
