library(tidyverse)
library(cowplot)
library(forcats)
library(stringr)
library(scales)
library(grid)



combined_max_scores<-read_csv("data/processed_data/max_a_scores.csv")

# Process df_tier1 (Indicator Level Data)
df_tier1_max <- t1 %>%
  rename(
    attribute = attribute,
    assessment = assessment,
    score = indicator_attribute_score
  ) %>%
  group_by(assessment, attribute) %>%
  summarise(
    max_score = max(score, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(level = "Indicator Level") %>%
  filter(max_score != 0)

# Process df_tier2 (Assessment Level Data)
df_tier2_processed <- t2 %>%
  rename(
    attribute = attribute,
    assessment = assessment_considered,
    max_score = coverage_score
  ) %>%
  mutate(level = "Assessment Level") %>%
  select(attribute, assessment, max_score, level)

# Combine both dataframes into one
combined_data <- bind_rows(df_tier1_max, df_tier2_processed)


# Clean assessment names
combined_data <- combined_data %>%
  mutate(assessment = str_trim(assessment)) %>%
  mutate(assessment = toupper(assessment))

# Pivot the data
combined_max_scores <- combined_data %>%
  pivot_wider(
    names_from = assessment,
    values_from = max_score,
    values_fill = list(max_score = 0),
    names_prefix = "max_"
  )

# Handle missing attributes
has_score <- combined_max_scores %>%
  select(attribute) %>%
  distinct()

a <- read_csv(attribute_meta_path) %>%
  rename(dim = attribute_dimension) %>%
  select(attribute)


missing_score1 <- anti_join(a, has_score, by = "attribute") %>%
  mutate(level = "Assessment Level")

missing_score2 <- anti_join(a, has_score, by = "attribute") %>%
  mutate(level = "Indicator Level")

combined_max_scores <- full_join(combined_max_scores, missing_score1)
combined_max_scores <- full_join(combined_max_scores, missing_score2)

# Add dimension metadata
a_meta <- read_csv(attribute_meta_path) %>%
  rename(dim = attribute_dimension) %>%
  select(attribute, dim)

combined_max_scores <- left_join(combined_max_scores, a_meta, by = "attribute")

# Replace NA values with 0
combined_max_scores <- combined_max_scores %>%
  mutate(
    max_SRA = coalesce(max_SRA, 0),
    max_ERA = coalesce(max_ERA, 0)
  ) %>%
  mutate(total_both = max_SRA + max_ERA)

# Calculate comparison and change
combined_max_scores <- combined_max_scores %>%
  mutate(
    comparison = case_when(
      max_SRA > max_ERA ~ "SRA stronger",
      max_ERA > max_SRA ~ "ERA stronger",
      TRUE ~ "Equal strength"
    ),
    change = abs(max_SRA - max_ERA),
    min_score = pmin(max_SRA, max_ERA),
    max_score = pmax(max_SRA, max_ERA)
  )

#confirm no changes in combined score
tc<- read_csv(t2_path) 

tc<-tc %>% 
  filter(assessment_considered == "Combined") %>% 
  rename(capture_level = coverage_score) %>% 
  mutate(capture_level= as.factor(capture_level)) %>% 
  select(attribute, capture_level)  


combined_max_scores<- left_join(combined_max_scores, tc, by ="attribute")
# no changes

# Arrange data
combined_max_scores <- combined_max_scores %>%
  arrange(level, max_score, desc(attribute))

# Set factor levels for 'comparison'
combined_max_scores$comparison <- factor(
  combined_max_scores$comparison,
  levels = c("ERA stronger", "Equal strength", "SRA stronger")
)

combined_max_scores$level <- factor(
  combined_max_scores$level,
  levels = c( "Indicator Level", "Assessment Level")
)

# Reorder attributes for plotting

#%>%
# filter(!(max_SRA == 0 & max_ERA == 0))


combined_max_scores <- combined_max_scores %>%
  mutate(
    capture_level = as.character(capture_level),
    capture_level = case_when(
      is.na(capture_level) ~ "Not captured",
      capture_level == "1" ~ "Minimally",
      capture_level == "2" ~ "Moderately",
      capture_level == "3" ~ "Comprehensively",
      TRUE ~ capture_level
    )
  ) %>%
  mutate(
    capture_level = factor(capture_level, levels = c("Comprehensively", "Moderately", "Minimally", "Not captured"))
  )
combined_max_scores <- combined_max_scores %>%
  mutate(attribute = fct_rev(factor(attribute)))

# Order attributes by capture_level, change, and attribute

combined_max_scores$comparison <- factor(
  combined_max_scores$comparison,
  levels = c("Equal strength", "ERA stronger","SRA stronger")
)

ordered_attributes <- combined_max_scores %>%
  filter(level == "Indicator Level") %>% 
  arrange(comparison, capture_level, change, min_score, attribute) %>%
  pull(attribute) %>%
  unique()

# Apply the ordering to the attribute column
combined_max_scores <- combined_max_scores %>%
  mutate(attribute = factor(attribute, levels = ordered_attributes))


# Prepare data for geom_point
points_data <- combined_max_scores %>%
  select(attribute, min_score, max_score, level, comparison, capture_level) %>%
  pivot_longer(cols = c(min_score, max_score), names_to = "ScoreType", values_to = "Score") %>%
  mutate(ScoreType = recode(ScoreType,
                            min_score = "Min. score",
                            max_score = "Max. score"))

# Define colors
assessment_colors <- c(
  "ERA stronger" = "#5e8069",
  "Equal strength" = "#DFC27D",
  "SRA stronger" = "#7e5c85"
)

level_labels <- c(
  "Indicator Level" = "Indicator level",
  "Assessment Level" = "Assessment level"
)

# Plot
figure_2<-ggplot(combined_max_scores, aes(x = attribute)) +
  geom_hline(yintercept = c(1, 2, 3), color = "grey80", linetype = "solid") +
  geom_segment(aes(x = attribute, xend = attribute, y = min_score, yend = max_score, color = comparison), size = 4) +
  geom_point(data = points_data, aes(x = attribute, y = Score, shape = ScoreType), size = 3) +
  scale_x_discrete(labels = function(x) {
    ifelse(x %in% c("Population abundance", "Participatory", "Access to economic opportunity", "Accountable"),
           paste0("", x, " (HR)"), x)
  }) +
  coord_flip() +
  facet_grid(rows = vars(capture_level), cols = vars(level), scales = "free_y", space = "free", labeller = labeller(level = level_labels)) +
  scale_color_manual(values = assessment_colors, breaks = c("ERA stronger", "SRA stronger"),  labels = c("ERA PI(s)", "SRA PI(s)")) + 
  scale_shape_manual(values = c("Max. score" = 16, "Min. score" = 21), labels = c("Max. coverage from best assessment", "Other assessment")) +
  theme_minimal() +
  theme(axis.text.x = element_markdown()) +
  labs(
    title = "",
    x = "Resilience attribute",
    y = " ",
    color = "Origin of value added",
    shape = "Coverage"
  ) +
  my_theme +
  theme(
    legend.position = "bottom", legend.box = "vertical"
  )

ggsave("main_figures/Figure_2.png", plot = Figure_2, width = 6, height = 5, dpi = 600, device = "png")