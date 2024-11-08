library(ggplot2)
library(tidyr)
library(scales)
library(cowplot)
library(tidyverse)
t2 <- read_csv("data/processed_data/Tier_2_all.csv")

t2<-t2 %>% 
  rename(dim = attribute_dimension, method = assessment_considered) %>% 
  mutate(method = case_when(
    method == "Combined" ~ "ERA+SRA",
    TRUE ~ method
  ))

dim_count<- read_csv("data/attribute_meta.csv") %>% 
  rename(dim = attribute_dimension) %>% 
  group_by(dim) %>% 
  summarise(att.per.dim = length(unique(attribute)))


# Aggregate by method and dim
df_agg <- t2 %>% 
  group_by(method, dim) %>% 
  summarize(total_points = sum(coverage_score))

df_agg_dim <- left_join(df_agg, dim_count) %>% 
  mutate(total_possible = att.per.dim * 3) %>% 
  mutate(coverage = round((total_points / total_possible)
                          * 100)) 



df_agg_overall <- t2 %>% 
  group_by(method) %>% 
  summarize(total_p = sum(coverage_score)) %>% 
  mutate(total_possible = 27 * 3 , coverage = round((total_p / total_possible) * 100))




# Function to create plots with colored titles
create_plot <- function(data, title, fill_color) {
  data %>%
    mutate(missing = 100 - coverage) %>%
    pivot_longer(cols = c(coverage, missing), names_to = "type", values_to = "percent") %>%
    ggplot(aes(x = method, y = percent, fill = ifelse(type == "missing", "Missing", "coverage"))) +
    geom_bar(stat = "identity", width = 0.6, position = position_stack(reverse = TRUE)) +
    geom_text(
      aes(label = paste0(percent, "%")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 3.5,
      family = "Helvetica"
    ) +
    scale_y_continuous(
      labels = percent_format(scale = 1),
      limits = c(0, 100)
    ) +
    scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
    scale_fill_manual(values = c("Missing" = "#d0784b", "coverage" = fill_color)) +
    labs(title = title, x = NULL) +  # Remove x-axis label
    theme_classic(base_size = 10) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        family = "Helvetica",
        size = 12,
        face = "bold",
        color = fill_color
      ),
      axis.text = element_text(family = "Helvetica", size = 10),
      axis.title.y = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
    )
}

# Create plots with original dimension labels and matching title colors
plot_overall <- create_plot(df_agg_overall, "Overall resilience", "#6c6d4b")  # Dark gray for overall resilience
plot_ecological <- create_plot(df_agg_dim %>% filter(dim == 'Ecological'), "Ecological resilience", "#4f8857")
plot_governance <- create_plot(df_agg_dim %>% filter(dim == 'Governance'), "Governance resilience", "#3580b8")
plot_socioecon <- create_plot(df_agg_dim %>% filter(dim == 'Socio-economic'), "Socio-economic resilience", "#e3d008")

# Combine plots into one figure with consistent labels
combined_plot <- cowplot::plot_grid(
  cowplot::plot_grid(
    plot_overall,
    plot_ecological,
    plot_governance,
    plot_socioecon,
    labels = c("a", "b", "c", "d"),
    label_size = 12,
    ncol = 2,
    align = 'v'
  )
)


# Save plot
ggsave("main_figures/Figure_3.png", plot = combined_plot, width = 6, height = 5, dpi = 600, device = "png")




