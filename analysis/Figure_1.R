library(tidyverse)
library(scales)
library(ggtext)  # Load ggtext for markdown-based styling



i<- read_csv("data/indicator_meta.csv") %>% 
  select(indicator_code, code, i_short, core, levels_i)


df <- read_csv("data/processed_data/tier1_data.csv") %>% 
  rename(assessment_p= assessment_principle) %>% 
  rename(score= indicator_attribute_score) %>% 
  rename(att_dimension = attribute_dimension)



df<- left_join(df, i) 

# Create the indicator_order column
df <- df %>%
  arrange(assessment, principle, code) %>%
  mutate(indicator_order = as.integer(factor(interaction(assessment, principle, code),
                                             levels = unique(interaction(assessment, principle, code))))) %>% 
  mutate(assessment_principle = paste(assessment, principle, sep = " - ")) 





dimension_order <- c("Ecological", "Governance", "Socio-economic")

# Sort the dataframe by the specified dimension order, category, attribute, and indicator_order
df_sorted <- df %>%
  arrange(factor(att_dimension, levels = dimension_order), attribute, indicator_order)

# Define a color palette for dimensions
dimension_colors <- c("Ecological" = "#007F54",  # Darker green for Ecological
                      "Governance" = "#005EA2",  # Darker blue for Governance
                      "Socio-economic" = "#C9B800")  # Darker yellow for Socio-economic


# Create the dim_color column based on att_dimension
df_sorted <- df_sorted %>%
  mutate(dim_color = recode(att_dimension,
                            "Ecological" = "#007F54",
                            "Governance" = "#005EA2",
                            "Socio-economic" = "#C9B800"))

# Arrange indicator_code by assessment, assessment_principle, and descending order
df_sorted <- df_sorted %>%
  arrange(assessment, assessment_principle, desc(code))

# Identify the indicators that should be bolded
bold_indicators <- df_sorted %>%
  filter(score %in% c(3)) %>%
  pull(i_short) %>%
  unique()

# Create a new column for labels with markdown-style bolding
df_sorted <- df_sorted %>%
  mutate(indicator_label = ifelse(i_short %in% bold_indicators, paste0("**", i_short, "**"), i_short))

# Create a logical order for attributes based on the computed average scores

# Calculate the average score by attribute to define the correct order
attribute_order <- df_sorted %>%
  group_by(attribute) %>%
  summarise(avg_score = mean(score, na.rm = TRUE)) %>%
  arrange(desc(avg_score))  # Order by descending average score

# Ensure 'attribute_order' column is available for sorting
attribute_order <- attribute_order %>%
  mutate(attribute_ordered = factor(attribute, levels = attribute))

#attribute_order <- attribute_order %>%
 # ungroup() %>%
  #mutate(attribute_ordered = factor(attribute, levels = unique(attribute)))

# Now apply this order to the dataframe
df_sorted <- df_sorted %>%
  mutate(attribute = factor(attribute, levels = attribute_order$attribute_ordered)) %>%
  arrange(factor(attribute, levels = levels(attribute_order$attribute_ordered)))


# Re-sort the dataframe based on the new attribute order
df_sorted <- df_sorted %>%
  mutate(attribute = factor(attribute, levels = attribute_order$attribute_ordered)) %>%
  arrange(factor(attribute, levels = levels(attribute_order$attribute_ordered)))


library(forcats)  # for fct_reorder

# Reorder the indicator_label based on level_i
df_sorted <- df_sorted %>%
  arrange(indicator_order) %>%
  mutate(indicator_label = fct_reorder(i_short, indicator_order))


ggplot(df_sorted, aes(
                      x = factor(attribute, levels = unique(attribute)),
                      y = factor(indicator_label, levels = rev(levels(indicator_label))),  
                      fill = dim_color, alpha = as.factor(score))) +
  geom_tile(color = "white") +
   scale_fill_identity(name = "Resilience dimension", guide = "legend", labels = c("Governance", "Ecological", "Socio-economic")) +  # Add legend title and labels
  scale_alpha_manual(values = c("0" = 0.2, "1" = 0.4, "2" = 0.6, "3" = 1),
                     labels = c("No", "Low", "Moderate", "High")) +
  facet_grid(assessment_p ~ att_dimension, scales = "free", space = "free") +  # Facet by assessment_principle on the y-axis and dimension on the x-axis
  theme_minimal() + 
  theme(
    axis.text.y = element_markdown(angle = 0, hjust = 1),  # Use element_markdown for bolding with ggtext
    axis.text.x = element_text(face = "plain", angle = 90, vjust = 0.5, hjust = 1),  # Adjust the x-axis labels for readability
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_blank(), 
    axis.line = element_line(colour = "black"),
    strip.text = element_blank()) +
  theme(
    plot.margin = margin(t = 10, r = 10, b = 50, l = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )+
  labs(y = "FIP indicators", x = "Resilence attributes", alpha = "Correspondence level")

quartz.save("main_figures/Figure_1_unedited.png", type = "png", dpi = 600)  
