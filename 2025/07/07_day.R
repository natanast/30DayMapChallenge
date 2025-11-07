

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
        "New Railway Station", "Dimokratias", "Venizelou", 
        "Agia Sofia", "Syntrivani", "Panepistimio", 
        "Papafi", "Efklidi", "Fleming",
        "Analipsi", "25th Martiou", "Voulgari", "Nea Elvetia"
    ),
    lon = c(
        22.92898117154671, 22.935477058942265, 22.942617522228826, 
        22.947456227287372, 22.95392229930323, 22.960509261148136, 
        22.963204827853094, 22.960744825821006, 22.95695488076687,
        22.958521336006672, 22.958381861132526, 22.960204446926262, 
        22.969962196984152
    ),
    lat = c(
        40.64411438780753, 40.64095164511105, 40.63722698411093, 
        40.634654201821455, 40.630686038344244, 40.62600379214026, 
        40.619886212314064, 40.615651478573454, 40.61189490484137,
        40.60611910856832, 40.600759252360135, 40.5955332897329, 
        40.59311882251412
    )
)


# Convert to sf points
metro_sf <- st_as_sf(metro_stations, coords = c("lon", "lat"), crs = 4326)

# Extract coordinates and create the line
coords <- st_coordinates(metro_sf)[, c("X", "Y")]
metro_line <- st_sfc(st_linestring(coords), crs = 4326)



# plot -----

gr = ggplot() +
    
    geom_sf(data = thess_roads$osm_lines, color = "grey78", size = 0.3) +
    
    geom_sf(data = metro_line, color = "#f09a8c", size = 1) +
    
    geom_sf(data = metro_sf, color = "#a50026", size = 2.5) +
    
    geom_text(data = metro_stations, aes(x = lon, y = lat, label = name), 
              size = 3, color = "grey10", vjust = -1) +
    
    geom_text(aes(x = 22.945, y = 40.625, label = "17 min end-to-end"),
              color = "#d73027", size = 3, fontface = "bold") +

    
    coord_sf(xlim = c(22.9, 23), ylim = c(40.59, 40.665)) +
    
    theme_void(base_family = "Candara") +
    
    labs(
        title = "The entire Thessaloniki metro: 13 stations, 17 minutes.",
        subtitle = "A long-awaited system that covers only a small part of the city.",
        caption = "30DayMapChallenge 2025: Day 07 | Graphic: Natasa Anastasiadou",
    ) +
    
    theme(
        
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_text(size = 13, hjust = 0.35, color = "grey30", margin = margin(t = 5, b = 15)),
        plot.caption = element_text(margin = margin(t = 35), size = 8, hjust = 1.2),
        
        plot.background = element_rect(fill = "grey96", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )



gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 8, height = 8, units = "in", dpi = 600
)


