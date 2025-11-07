

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(ggtext)
library(paletteer)

library(osmdata)
library(sf)


# map data --------

# Define bounding box around Thessaloniki
thess_bbox <- getbb("Thessaloniki, Greece")

# Download highways and coastline
thess_roads <- opq(thess_bbox) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()

thess_water <- opq(thess_bbox) %>%
    add_osm_feature(key = "natural", value = "water") %>%
    osmdata_sf()



# metro data -----

# Define metro stations 
metro_stations <- data.frame(
    name = c(
        "New Railway Station", "Dimokratias", "Venizelou", "Agia Sofia",
        "Syntrivani", "Panepistimio", "Papafi", "Efklidi", "Fleming",
        "Analipsi", "25th Martiou", "Voulgari", "Nea Elvetia"
    ),
    lon = c(
        22.9173, 22.9265, 22.9365, 22.9413,
        22.9469, 22.9508, 22.9554, 22.9608, 22.9646,
        22.9676, 22.9702, 22.9726, 22.9753
    ),
    lat = c(
        40.6423, 40.6398, 40.6340, 40.6303,
        40.6268, 40.6242, 40.6213, 40.6188, 40.6159,
        40.6125, 40.6089, 40.6032, 40.5968
    )
)


# Convert to sf points
metro_sf <- st_as_sf(metro_stations, coords = c("lon", "lat"), crs = 4326)

# Extract coordinates and create the line
coords <- st_coordinates(metro_sf)[, c("X", "Y")]
metro_line <- st_sfc(st_linestring(coords), crs = 4326)



# plot -----

ggplot() +
    
    geom_sf(data = thess_roads$osm_lines, color = "grey85", size = 0.3) +
    
    geom_sf(data = metro_line, color = "#f09a8c", size = 1.2) +
    
    geom_sf(data = metro_sf, color = "#f09a8c", size = 3) +
    
    geom_text(data = metro_stations, aes(x = lon, y = lat, label = name), 
              size = 3, color = "#222222", vjust = -1) +
    
    coord_sf(xlim = c(22.9, 22.98), ylim = c(40.59, 40.65)) +
    
    theme_void(base_family = "Candara") +
    
    labs(
        title = "Thessaloniki Metro Accessibility",
        subtitle = "Only one metro line under construction — limited coverage across the city.",
        caption = "30DayMapChallenge 2025: Day 07 | Graphic: Natasa Anastasiadou",
    ) +
    
    theme(
        
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_text(size = 13, hjust = 0.35, color = "grey30", margin = margin(t = 5, b = 15)),
        plot.caption = element_text(margin = margin(t = 35), size = 8, hjust = 1.2),
        
        plot.margin = margin(20, 20, 20, 20)
    )



gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 8, height = 8, units = "in", dpi = 600
)


