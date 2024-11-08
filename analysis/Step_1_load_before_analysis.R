#load in working directory before analysis scripts

#  packages
library(tidyverse)  
library(forcats)     
library(stringr)     
library(scales)      
library(grid)        
library(ggtext)      
library(ggridges)    
library(RColorBrewer)
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(officer)
library(flextable)
library(systemfonts)
library(huxtable)



my_theme <- theme_minimal(base_size = 12, base_family = "Helvetica") +
  theme(
    axis.text.x = element_text(face = "plain", angle = 0, vjust = 0.5, hjust = 0.5, size = 10),
    axis.text.y = element_text(face = "italic", size = 10),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    strip.text = element_text( size = 11),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.justification = "left",
    legend.box.just = "left",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )




