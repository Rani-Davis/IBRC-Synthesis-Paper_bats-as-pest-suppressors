# Code author: Rani Davis
# Last updated: 6 July 2026, Started 2nd July 2026
# You can view the drafted pathway on Canva here: # Can view our 'knowledge pathway' here: https://www.canva.com/design/DAGs2rCMnYQ/0oo2xr8_waggqfzTKeBudg/edit

# ----------------------------------------
# Simplified knowledge pathway diagram — to match Score plots in 'Script 2'
# (Evidence > Limiting Factors > Available Strategies > Monitoring >Implementing)
# Representativeness (Q11) intentionally excluded, since it wasn't plotted
# ----------------------------------------

# install.packages(c("ggplot2", "ggtext", "dplyr"))
library(ggplot2)
library(ggtext)
library(dplyr)

navy      <- "#3949AB"
lightblue <- "#BBDEFB"

# ----------------------------------------
# 1. Specify one block per Score.type in the correct pathway order
# ----------------------------------------
steps <- tibble::tibble(
  step  = 1:5,
  x     = 1:5,
  score_type = c("Evidence", "Limiting Factors", "Available Strategies",
                 "Monitoring", "Implementing"),
  title = c(
    "Evidence bats suppress pests",
    "Limiting factors to bats",
    "Available management strategies",
    "Monitoring of strategies",
    "Implementing strategies at scale"
  ),
  desc = c(
    "Q) What is the current state of evidence that bats suppress pests in this system?",
    "Q) What is the level of understanding of factors limiting bat populations or activity?",
    "Q) To what extent are effective, context-appropriate management strategies known and developed?",
    "Q) How systematically are outcomes of management strategies monitored and used to inform improvements?",
    "Q) To what extent are bat-supportive management strategies being adopted and implemented at scale?"
  )
)

y_tag   <- 4.15
y_title <- 3.7
y_desc  <- 2.95

# ----------------------------------------
# 2. Specify Knowledge > Action background gradient
# ----------------------------------------
# A wide strip of thin rectangles fading from "knowledge" blue to "action" red
# a solid triangle in the end colour caps it into an arrowhead
bar_xmin  <- 0.3
bar_xmax  <- 5.4    # body ends here; the triangle continues on to 5.7
head_tip  <- 5.7
bar_ymin  <- 2.05
bar_ymax  <- 2.35
end_colour <- "#B0142F"

gradient_raster <- matrix(
  colorRampPalette(c("#1B5E9A", end_colour))(256),
  nrow = 1
)

arrowhead <- tibble::tibble(
  x = c(bar_xmax, bar_xmax, head_tip),
  y = c(bar_ymin, bar_ymax, (bar_ymin + bar_ymax) / 2)
)

p <- ggplot() +
  annotation_raster(gradient_raster, xmin = bar_xmin, xmax = bar_xmax, ymin = bar_ymin, ymax = bar_ymax) +
  geom_polygon(data = arrowhead, aes(x = x, y = y), fill = end_colour, colour = NA) +
  annotate("text", x = 0.55, y = 2.2, label = "KNOWLEDGE", hjust = 0,
           fontface = "bold", size = 3.3, colour = "white") +
  annotate("text", x = 5.15, y = 2.2, label = "ACTION", hjust = 1,
           fontface = "bold", size = 3.3, colour = "white")

# ----------------------------------------
# 3. Specify the step boxes below the score types, with descriptions and connecting arrows
# ----------------------------------------
p <- p +
  geom_segment(
    data = steps %>% filter(step < 5),
    aes(x = x + 0.42, xend = x + 0.58, y = y_title, yend = y_title),
    arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
    colour = "black", linewidth = 0.6
  ) +
  geom_textbox(
    data = steps,
    aes(x = x, y = y_tag, label = score_type),
    fill = lightblue, box.colour = NA, width = unit(1.1, "inch"),
    halign = 0.5, valign = 0.5, size = 3, fontface = "bold",
    box.padding = unit(c(2, 4, 2, 4), "pt"), colour = "#0D47A1"
  ) +
  geom_textbox(
    data = steps,
    aes(x = x, y = y_title, label = title),
    fill = navy, box.colour = NA, width = unit(1.1, "inch"),
    halign = 0.5, valign = 0.5, size = 3.4, fontface = "bold",
    colour = "white", box.r = unit(6, "pt")
  ) +
  geom_textbox(
    data = steps,
    aes(x = x, y = y_desc, label = desc),
    fill = NA, box.colour = NA, width = unit(1.1, "inch"),
    halign = 0, valign = 1, size = 2.6, colour = "#1A237E"
  )

# ----------------------------------------
# 4. Title + caption
# ----------------------------------------
p <- p +
  coord_cartesian(xlim = c(0.3, 5.7), ylim = c(1.95, 4.4), clip = "off", expand = FALSE) +
  theme_void() +
  labs(
    title = "Conceptual 'Knowledge Pathway' towards supporting bats in agricultural systems",
    caption = paste(
      "Each step moves further from foundational knowledge towards on-ground action.",
      "Evidence and understanding are typically strongest early in this pathway"
    )
  ) +
  theme(
    plot.title = element_text(face = "bold.italic", size = 13, hjust = 0),
    plot.caption = element_textbox_simple(
      size = 8, colour = "grey30", lineheight = 1.3, halign = 0,
      margin = margin(t = 2, b = 2)
    ),
    plot.margin = margin(t = 10, r = 15, b = 5, l = 15),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

p

ggsave("figure exports/Our Conceptual Knowledge Pathway_with questions.png", 
       p, width = 800/96, height = 320/96, units = "in", dpi = 300, bg = "white")

