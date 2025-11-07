

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(ggtext)
library(paletteer)


# load data --------

london_marathon <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-25/london_marathon.csv')


# data cleaning ------

df <- london_marathon[, .(Year, Starters, Finishers)]

df <- df[!is.na(Starters), ]

df_long <- melt(df, id.vars = "Year", measure.vars = c("Starters", "Finishers"), 
                variable.name = "Category", value.name = "Runners")

# labels -------

# Labels Preparation
label_data <- df[, .(Year, Starters)]
label_data[, id := .I]  # Assign an ID from 1 to N

number_of_bar <- nrow(label_data)

# Calculate angles
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data[, angle := ifelse(angle < -90, angle + 180, angle)]
label_data[, hjust := ifelse(angle < -90, 1, 0)]

label_data$Starters <- ifelse(label_data$Year < 2001, label_data$Starters + 1000, label_data$Starters + 6000)



# Plot -----------

gr = ggplot(df_long, aes(x = factor(Year), y = Runners, fill = Category)) +
    
    geom_bar(width = 0.7, stat = "identity", position = "dodge") +
    
    coord_radial(
        start = 0,
        inner.radius = 0.15
    ) +

    labs(
        title = "London Marathon",
        subtitle = "A year-by-year comparison of starters and finishers in the London Marathon.",
        caption = "Source: <b>  {LondonMarathon} R package</b> | Graphic: <b>Natasa Anastasiadou</b>",
        fill = ""
    ) +
    
    
    scale_fill_manual(values = c("Starters" = "#f09a8c", "Finishers" = "#5f899d")) +
    
    geom_text(data = label_data, aes(x = id, y = Starters, label = Year, hjust = hjust, angle = angle, vjust = 0.5), 
              color = "black", fontface = "bold", alpha = 0.6, size = 2.5, , inherit.aes = FALSE) +
    

    theme_minimal() +
    
    theme(
        
        # legend.position = c(0.5, 0.10),
        # legend.position = "bottom",
        legend.position = c(0.5, 0.1),  # Centered horizontally (0.5), moved up (0.1)
        legend.direction = "horizontal",
        # legend.title = element_text(size = 10, hjust = 0.5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 10)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(t = 15)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.2),
        
        plot.margin = margin(20, 20, 20, 20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),  
        axis.title = element_text(size = 14),  
        
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
    )
    

gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 8, height = 8, units = "in", dpi = 600
)









library(osmdata)
library(sf)
library(ggplot2)




# Define bounding box around Thessaloniki
thess_bbox <- getbb("Thessaloniki, Greece")

# Download highways and coastline
thess_roads <- opq(thess_bbox) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()

thess_water <- opq(thess_bbox) %>%
    add_osm_feature(key = "natural", value = "water") %>%
    osmdata_sf()




# Define metro stations (approximate coordinates)
metro_stations <- data.frame(
    name = c(
        "Νέος Σιδηροδρομικός Σταθμός", "Δημοκρατίας", "Βενιζέλου", "Αγία Σοφία",
        "Σιντριβάνι", "Πανεπιστήμιο", "Παπάφη", "Ευκλείδης", "Φλέμινγκ",
        "Ανάληψη", "25ης Μαρτίου", "Βούλγαρη", "Νέα Ελβετία"
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



ggplot() +
    
    geom_sf(data = thess_roads$osm_lines, color = "grey80", size = 0.3) +
    
    # geom_sf(data = thess_water$osm_multipolygons, fill = "grey10", color = NA) +
    
    geom_sf(data = metro_line, color = "#f09a8c", size = 1.2) +
    
    geom_sf(data = metro_sf, color = "#f09a8c", size = 3) +
    
    geom_text(data = metro_stations, aes(x = lon, y = lat, label = name), 
              size = 3, color = "#222222", vjust = -1, family = "Candara") +
    
    coord_sf(xlim = c(22.9, 22.98), ylim = c(40.59, 40.65)) +
    
    theme_void() +
    
    labs(
        title = "Thessaloniki Metro Accessibility",
        subtitle = "Only one metro line under construction — limited coverage across the city.",
        caption = "#30DayMapChallenge Day 7: Accessibility"
    ) +
    
    theme(
        plot.margin = margin(20, 20, 20, 20)
    )



