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
map_data <- read_csv("Analysis/clean data/Map data_study system coordinates_wJitter.csv")


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
## -- Colour palette specified in 'Colour palette script' --
# Call 'crop_colours' and 'region_colours' for plotting


# ----------------------------------------
# 4. Set up the plotting options (coloured by crop, region etc)
# ----------------------------------------
# a. work out the hex colour for each "colour by" option
blue_pin <- "#1E88E5"

get_hex <- function(vals, pal_lookup) {
  cols <- unname(pal_lookup[as.character(vals)])
  cols[is.na(cols)] <- "#888888"
  cols}

map_data$hex_croptype <- get_hex(map_data$Crop.type.clean, crop_colours)
map_data$hex_crop     <- get_hex(map_data$Crop.clean, crop_clean_colours)
map_data$hex_region   <- get_hex(map_data$World.region.clean, region_colours)
map_data$hex_blue     <- blue_pin

map_data$hover_label <- paste0(map_data$Crop.clean, " — ", map_data$Respondent.clean)
map_data$popup <- paste0(
  "<b>", map_data$Crop.clean, "</b> (", map_data$Crop.type.clean, ")<br>",
  "Respondent: ", map_data$Respondent.clean, " (", map_data$Respondent.ID, ")<br>",
  "Region: ", map_data$Region.within.country.clean, "<br>",
  "Country: ", map_data$Country.clean, "<br>",
  "World region: ", map_data$World.region.clean, "<br>",
  "Closest city: ", map_data$Closest.city.clean
)

# b. legend contents per colour option
legend_data_js <- sprintf(
  "var legendSets = {
     'croptype': { colours: %s, labels: %s },
     'crop':     { colours: %s, labels: %s },
     'region':   { colours: %s, labels: %s },
     'blue':     { colours: ['%s'], labels: ['Consistent colour'] }
   };",
  jsonlite::toJSON(unname(crop_colours)), jsonlite::toJSON(names(crop_colours)),
  jsonlite::toJSON(unname(crop_clean_colours)), jsonlite::toJSON(names(crop_clean_colours)),
  jsonlite::toJSON(unname(region_colours)), jsonlite::toJSON(names(region_colours)),
  blue_pin
)


# c. set the dropdown options and legend controls:
dropdown_js_template <- "
  function(el, x) {
    var map = this;
    %s
    %s

    %s

    function applyColours(key) {
      %s
      updateLegend(key);
    }

    // Colour-by dropdown control
    var Ctrl = L.control({position: 'topleft'});
    Ctrl.onAdd = function() {
      var div = L.DomUtil.create('div', 'leaflet-bar');
      div.style.background = 'white';
      div.style.padding = '8px';
      div.innerHTML =
        '<label style=\"font-weight:bold;\">Colour by:</label><br>' +
        '<select id=\"colourSelect\">' +
        '<option value=\"croptype\">Crop type</option>' +
        '<option value=\"crop\">Crop</option>' +
        '<option value=\"region\">World region</option>' +
        '<option value=\"blue\">Consistent colour</option>' +
        '</select>';
      L.DomEvent.disableClickPropagation(div);
      return div;
    };
    Ctrl.addTo(map);

    // Legend control
    var Legend = L.control({position: 'bottomright'});
    Legend.onAdd = function() {
      var div = L.DomUtil.create('div', 'info legend');
      div.style.background = 'white';
      div.style.padding = '8px';
      div.id = 'dynamicLegend';
      return div;
    };
    Legend.addTo(map);

    function updateLegend(key) {
      var set = legendSets[key];
      var html = '<b>Legend</b><br>';
      for (var i = 0; i < set.colours.length; i++) {
        html += '<i style=\"background:' + set.colours[i] +
          ';width:12px;height:12px;display:inline-block;margin-right:6px;border:1px solid #333;\"></i>' +
          set.labels[i] + '<br>';
      }
      document.getElementById('dynamicLegend').innerHTML = html;
    }

    updateLegend('croptype');

    document.getElementById('colourSelect').addEventListener('change', function(e) {
      applyColours(e.target.value);
    });
  }
"

# d. base tile
base_map <- function() {
  leaflet(map_data) %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Satellite") %>%
    addLayersControl(
      baseGroups = c("Map", "Satellite"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topright"
    )
}


## ----------------------------------------
# 5. Map Option 1: Make the map with 'teardrop' / pin markers
## ----------------------------------------
teardrop_svg <- function(hex) {
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="30" height="42" viewBox="0 0 30 42" shape-rendering="geometricPrecision">
       <path d="M15 0.75C7.13 0.75 0.75 7.13 0.75 15c0 10.5 14.25 25.6 14.25 25.6s14.25-15.1 14.25-25.6C29.25 7.13 22.87 0.75 15 0.75z"
             fill="%s" stroke="#1a1a1a" stroke-width="1" vector-effect="non-scaling-stroke"/>
       <circle cx="15" cy="15" r="5" fill="white" fill-opacity="0.92" stroke="#1a1a1a" stroke-width="0.5" vector-effect="non-scaling-stroke"/>
     </svg>', hex)
}
svg_uri <- function(hex) paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(charToRaw(teardrop_svg(hex))))

map_data$icon_croptype <- vapply(map_data$hex_croptype, svg_uri, character(1))
map_data$icon_crop     <- vapply(map_data$hex_crop, svg_uri, character(1))
map_data$icon_region   <- vapply(map_data$hex_region, svg_uri, character(1))
map_data$icon_blue     <- vapply(map_data$hex_blue, svg_uri, character(1))

crop_icon_list <- do.call(iconList, lapply(crop_colours, function(hex) {
  makeIcon(iconUrl = svg_uri(hex), iconWidth = 22, iconHeight = 28,
           iconAnchorX = 7, iconAnchorY = 20, popupAnchorX = 0, popupAnchorY = -18)
}))

teardrop_dropdown <- base_map() %>%
  addMarkers(
    lat = ~Latitude, lng = ~Longitude,
    label = ~hover_label, popup = ~popup,
    icon = ~crop_icon_list[Crop.type.clean],
    layerId = paste0("pt", seq_len(nrow(map_data)))
  )

icon_data_js <- sprintf(
  "var iconSets = { 'croptype': %s, 'crop': %s, 'region': %s, 'blue': %s };",
  jsonlite::toJSON(map_data$icon_croptype), jsonlite::toJSON(map_data$icon_crop),
  jsonlite::toJSON(map_data$icon_region), jsonlite::toJSON(map_data$icon_blue)
)

teardrop_apply_js <- "
      var urls = iconSets[key];
      markers.forEach(function(mk, i) {
        mk.setIcon(L.icon({ iconUrl: urls[i], iconSize: [22, 28], iconAnchor: [7, 20], popupAnchor: [0, -18] }));
      });
"

teardrop_dropdown <- teardrop_dropdown %>% htmlwidgets::onRender(sprintf(
  dropdown_js_template,
  icon_data_js,
  legend_data_js,
  "var markers = []; map.eachLayer(function(layer) { if (layer instanceof L.Marker) { markers.push(layer); } });",
  teardrop_apply_js
))

# view map:
teardrop_dropdown

# export map:
htmlwidgets::saveWidget(teardrop_dropdown, "docs/map exports/teardrop map_interactive.html", selfcontained = TRUE)


## ----------------------------------------
# 6. Map Option 2: Make the map with circle markers
## ----------------------------------------
circle_dropdown <- base_map() %>%
  addCircleMarkers(
    lat = ~Latitude, lng = ~Longitude,
    label = ~hover_label, popup = ~popup,
    radius = 6, weight = 1, color = "black",
    fillColor = ~hex_croptype, fillOpacity = 0.9,
    layerId = paste0("pt", seq_len(nrow(map_data)))
  )

hex_data_js <- sprintf(
  "var hexSets = { 'croptype': %s, 'crop': %s, 'region': %s, 'blue': %s };",
  jsonlite::toJSON(map_data$hex_croptype), jsonlite::toJSON(map_data$hex_crop),
  jsonlite::toJSON(map_data$hex_region), jsonlite::toJSON(map_data$hex_blue)
)

circle_apply_js <- "
      var hexes = hexSets[key];
      circles.forEach(function(c, i) { c.setStyle({ fillColor: hexes[i] }); });
"

circle_dropdown <- circle_dropdown %>% htmlwidgets::onRender(sprintf(
  dropdown_js_template,
  hex_data_js,
  legend_data_js,
  "var circles = []; map.eachLayer(function(layer) { if (layer instanceof L.CircleMarker) { circles.push(layer); } });",
  circle_apply_js
))

# view map:
circle_dropdown

# export map:
htmlwidgets::saveWidget(circle_dropdown, "docs/map exports/circle map_interactive.html", selfcontained = TRUE)
