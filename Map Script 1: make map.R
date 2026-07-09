# Code author: Rani Davis
# Last updated: 6 July 2026
#

# You can view a Google MyMaps version here: https://www.google.com/maps/d/u/0/edit?mid=1g2LIjJGQlzjZi-X1RkhEzPyY_QI3TCk&ll=48.795843511676125%2C1.8881380987979357&z=10

# ----------------------------------------
# 0. Load packages
# ----------------------------------------
# install.packages(c("leaflet", "dplyr", "readr"))  # first time only
library(readr)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# ----------------------------------------
# 1. Load data
# ----------------------------------------
# This dataset is based on the 'Closest.city' entries in each survey
# I have added jitter to spread out points which would otherwise overlap 
#   -   e.g. when the closest city for two different studies was Moree
map_data <- read_csv("clean data/Map data_study system coordinates_wJitter.csv")


# ----------------------------------------
# 2. Wrangle
# ----------------------------------------
# wrangle data
map_data <- map_data %>%
  mutate(across(where(is.character), ~iconv(.x, from = "macroman", to = "UTF-8")))

map_data <- map_data %>%
  mutate(
    point_colour = unname(crop_colours[Crop.type.clean]),
    hover_label = paste0(Crop.clean, " — ", Respondent.clean),
    popup = paste0(
      "<b>", Crop.clean, "</b> (", Crop.type.clean, ")<br>",
      "Respondent: ", Respondent.clean, " (", Respondent.ID, ")<br>",
      "Region: ", Region.within.country.clean, "<br>",
      "Country: ", Country.clean, "<br>",
      "World region: ", World.region.clean, "<br>",
      "Closest city: ", Closest.city.clean
    )
  )

map_data <- map_data %>%
  mutate(
    Crop.type.clean = trimws(Crop.type.clean),
    fill_col = crop_colours[Crop.type.clean],
    fill_col = ifelse(is.na(fill_col), "#888888", fill_col)
  )

map_data <- map_data %>%
  mutate(
    lat_plot = Latitude,
    lon_plot = Longitude
  )


# ----------------------------------------
# 3. Specify colour palettes for mapping
# ----------------------------------------
# Specify colour palettes:
crop_colours <- c(
  "Vegetable crops" = "#1B5E20",
  "Perennial fruit & forestry crops" = "#C62828",
  "Vineyard" = "#7B1FA2",
  "Pasture & mixed cropping" = "#C8E6C9",
  "Field crops" = "#FDD835",
  "Grain crops" = "#FF8C00",
  "Agroforestry cacao" = "#3E2000")

region_colours <- c(
  "Europe" = "#2166AC", "Africa" = "#D6604D",
  "Latin America / Caribbean" = "#33A02C",
  "Australasia / Pacific" = "#00B4D8",
  "North America" = "#7B2D8B",
  "Middle East / Western Asia" = "#FF8C00",
  "South / Southeast Asia" = "#E7298A",
  "East Asia" = "#E6C619")

crop_clean_colours <- c(
  # Orchard / tree crops (greens)
  "Apple" = "#CD1C24",
  "Peach" = "#FFCBA4",
  "Blueberries" = "#4F86F7",
  "Macadamia" = "#B8946E",
  "Pecan" = "#8C510A",
  "Olive groves" = "#606E45",
  "Citrus" = "#F4C430",
  
  # Vineyard (purple family)
  "Vineyard" = "#5E3A56",
  
  # Vegetables (reds/oranges)
  "Tomato" = "#E41A1C",
  "Onion" = "#7D2248",
  "Brassica" = "#2E5A1C",
  "Melon" = "#D81B60",
  "Watermelon" = "#FC4E2A",
  "Pumpkin" = "#E06000",
  "Potato" = "#F0F0B1",
  
  # Grains (yellows)
  "Wheat" = "#F5DEB3",
  "Rice" = "#FBF8EB",
  "Corn" = "#FBEC5D",
  
  # Industrial crops (brown/orange)
  "Cotton" = "#F2F0EA",
  "Sugar cane" = "#90C048",
  
  # Mixed systems (greys)
  "Multi- crop" = "#8A9A86",
  "Multi- crop & pasture" = "#90B060",
  "Multi- grain & vegetable" = "#D4A373",
  "Multi- fruit & orchard" = "pink",
  
  # Tropical systems (teal/blue-green)
  "Date palms" = "#422511",
  "Arecanut" = "#D2B48C",
  "Agroforestry cacao" = "#4A3B32",
  "Pine" = "#2D4C3A",
  
  # Pasture
  "Pasture" = "#77DD77"
)


# ----------------------------------------
# 4. Map option 1 - simple points
# ----------------------------------------
# make simple map
crop_levels <- names(crop_colours)
map_data$Crop.type.clean <- factor(
  map_data$Crop.type.clean,
  levels = crop_levels) 

pal <- colorFactor(
  palette = crop_colours,
  domain = map_data$Crop.type.clean,
  na.color = "#888888")

leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lat = ~lat_plot,
    lng = ~lon_plot,
    fillColor = ~pal(Crop.type.clean),
    fillOpacity = 0.9,
    color = "black",
    weight = 1,
    radius = 6,
    popup = ~popup) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~Crop.type.clean,
    title = "Crop type",
    opacity = 1)
# htmlwidgets::saveWidget(map, "bat_crop_survey_map.html", selfcontained = TRUE)


# ----------------------------------------
# 4. Map option 2 - Build a teardrop pin map 
# ----------------------------------------
# build teardrop shapes:
teardrop_svg <- function(hex) {
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="30" height="42" viewBox="0 0 30 42" shape-rendering="geometricPrecision">
       <path d="M15 0.75C7.13 0.75 0.75 7.13 0.75 15c0 10.5 14.25 25.6 14.25 25.6s14.25-15.1 14.25-25.6C29.25 7.13 22.87 0.75 15 0.75z"
             fill="%s" stroke="#1a1a1a" stroke-width="1" vector-effect="non-scaling-stroke"/>
       <circle cx="15" cy="15" r="5" fill="white" fill-opacity="0.92" stroke="#1a1a1a" stroke-width="0.5" vector-effect="non-scaling-stroke"/>
     </svg>', hex)
}

crop_icon_list <- do.call(iconList, lapply(crop_colours, function(hex) {
  uri <- paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(charToRaw(teardrop_svg(hex))))
  makeIcon(
    iconUrl = uri,
    iconWidth = 22, iconHeight = 28,     # <- reduce these to resize the pin
    iconAnchorX = 7, iconAnchorY = 20,   # keep anchor = (width/2, height) so the tip stays pinned to the coordinate
    popupAnchorX = 0, popupAnchorY = -18
  )
}))


map_data <- map_data %>%
  mutate(
    hover_label = paste0(Crop.clean, " — ", Respondent.clean),
    popup = paste0(
      "<b>", Crop.clean, "</b> (", Crop.type.clean, ")<br>",
      "Respondent: ", Respondent.clean, " (", Respondent.ID, ")<br>",
      "Region: ", Region.within.country.clean, "<br>",
      "Country: ", Country.clean, "<br>",
      "World region: ", World.region.clean, "<br>",
      "Closest city: ", Closest.city.clean
    )
  )

teardrop_map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(
    lat = ~Latitude,
    lng = ~Longitude,
    label = ~hover_label,                        # hover = crop + respondent
    popup = ~popup,                              # click = full details
    icon = ~crop_icon_list[Crop.type.clean]       # coloured teardrop pin per crop type
    # no clusterOptions -> every point stays visible individually, even overlapping ones
  ) %>%
  addLegend(
    position = "bottomright",
    colors = crop_colours,
    labels = names(crop_colours),
    title = "Crop type"
  )

teardrop_map
htmlwidgets::saveWidget(teardrop_map, "map exports/teardrop map_coloured by crop.html", selfcontained = TRUE)




# Now do the same but all pins are blue:
blue_pin <- "#1E88E5"   # nice medium blue (you can change this)

teardrop_svg <- function(hex) {
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="30" height="42" viewBox="0 0 30 42" shape-rendering="geometricPrecision">
       <path d="M15 0.75C7.13 0.75 0.75 7.13 0.75 15c0 10.5 14.25 25.6 14.25 25.6s14.25-15.1 14.25-25.6C29.25 7.13 22.87 0.75 15 0.75z"
             fill="%s" stroke="#1a1a1a" stroke-width="1" vector-effect="non-scaling-stroke"/>
       <circle cx="15" cy="15" r="5" fill="white" fill-opacity="0.92" stroke="#1a1a1a" stroke-width="0.5" vector-effect="non-scaling-stroke"/>
     </svg>', hex)
}

blue_icon <- makeIcon(
  iconUrl = paste0(
    "data:image/svg+xml;base64,",
    jsonlite::base64_enc(charToRaw(teardrop_svg(blue_pin)))
  ),
  iconWidth = 22,
  iconHeight = 28,
  iconAnchorX = 7,
  iconAnchorY = 20,
  popupAnchorX = 0,
  popupAnchorY = -18
)

teardrop_map_blue <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(
    lat = ~Latitude,
    lng = ~Longitude,
    label = ~hover_label,
    popup = ~popup,
    icon = blue_icon
  ) %>%
  addLegend(
    position = "bottomright",
    colors = "#1E88E5",
    labels = "All sites",
    title = "Crop type"
  )
teardrop_map_blue

# Create html of the map 
htmlwidgets::saveWidget(teardrop_map_blue, "map exports/teardrop map_consisent colour.html", selfcontained = TRUE)



# ----------------------------------------
# 5. Map option 4 - Interactive map with dropdown menu
# ----------------------------------------
map_data$popup <- paste0(
  "<b>", map_data$Crop.clean, "</b><br>",
  "Crop type: ", map_data$Crop.type.specific.clean, "<br>",
  "Respondent: ", map_data$Respondent.clean, "<br>",
  "Region: ", map_data$Region.within.country.clean, "<br>",
  "World region: ", map_data$World.region.clean, "<br>",
  "Country: ", map_data$Country.clean, "<br>",
  "Closest city: ", map_data$Closest.city.clean
)

#a. Precompute a color column for EACH "colour by" option
get_cols <- function(vals, pal_lookup) {
  cols <- unname(pal_lookup[as.character(vals)])
  cols[is.na(cols)] <- "#888888"
  cols
}

map_data$col_croptype <- get_cols(map_data$Crop.type.clean, crop_colours)
map_data$col_crop     <- get_cols(map_data$Crop.clean, crop_clean_colours)
map_data$col_region   <- get_cols(map_data$World.region.clean, region_colours)
map_data$col_country  <- get_cols(map_data$Country.clean, make_palette(map_data$Country.clean))


#b. Build the base map (default = crop type colours)
m <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  setView(lng = 150, lat = -25, zoom = 4) %>%
  addCircleMarkers(
    lng = map_data$lon_plot,
    lat = map_data$lat_plot,
    radius = 6,
    fillColor = map_data$col_croptype,
    fillOpacity = 0.9,
    color = "black",
    weight = 1,
    popup = map_data$popup,
    layerId = paste0("pt", seq_len(nrow(map_data)))  # stable IDs so JS can target each marker
  ) %>%
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  )


#c.insert a dropdown control + JS that recolors markers on change
color_data_js <- sprintf(
  "var colorSets = {
     'Crop.type.clean': %s,
     'Crop.clean': %s,
     'World.region.clean': %s,
     'Country.clean': %s
   };",
  jsonlite::toJSON(map_data$col_croptype),
  jsonlite::toJSON(map_data$col_crop),
  jsonlite::toJSON(map_data$col_region),
  jsonlite::toJSON(map_data$col_country)
)

m <- m %>% htmlwidgets::onRender(paste0("
  function(el, x) {
    var map = this;
    ", color_data_js, "
 
    // Grab this map's circle marker layers in the order they were added
    var markers = [];
    map.eachLayer(function(layer) {
      if (layer instanceof L.CircleMarker) { markers.push(layer); }
    });
 
    function applyColours(key) {
      var cols = colorSets[key];
      markers.forEach(function(mk, i) {
        mk.setStyle({ fillColor: cols[i] });
      });
    }
 
    // Build a simple dropdown control, mimicking the Shiny sidebar
    var Ctrl = L.control({position: 'topleft'});
    Ctrl.onAdd = function() {
      var div = L.DomUtil.create('div', 'leaflet-bar');
      div.style.background = 'white';
      div.style.padding = '8px';
      div.innerHTML =
        '<label style=\"font-weight:bold;\">Colour by:</label><br>' +
        '<select id=\"colourSelect\">' +
        '<option value=\"Crop.type.clean\">Crop type</option>' +
        '<option value=\"Crop.clean\">Crop</option>' +
        '<option value=\"World.region.clean\">World region</option>' +
        '<option value=\"Country.clean\">Country</option>' +
        '</select>';
      L.DomEvent.disableClickPropagation(div);
      return div;
    };
    Ctrl.addTo(map);
 
    document.getElementById('colourSelect').addEventListener('change', function(e) {
      applyColours(e.target.value);
    });
  }
"))

# d. Save as a single self-contained HTML file
saveWidget(m, file = "map exports/survey map_drop down options.html", selfcontained = TRUE)

