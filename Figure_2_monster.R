t2<-read_csv("data/processed_data/Figure_2_data.csv")

t1<- read_csv("data/processed_data/tier1_data.csv")

library(tidyverse)
library(forcats)

#tier1
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

#tier2
df_tier2_processed <- t2 %>%
  rename(
    attribute = attribute,
    assessment = assessment_considered,
    max_score = coverage_score
  ) %>%
  mutate(level = "Assessment Level") %>%
  select(attribute, assessment, max_score, level)


combined_data <- bind_rows(df_tier1_max, df_tier2_processed)

combined_data <- combined_data %>%
  mutate(assessment = str_trim(assessment)) %>%
  mutate(assessment = toupper(assessment))

combined_max_scores <- combined_data %>%
  pivot_wider(
    names_from = assessment,
    values_from = max_score,
    values_fill = list(max_score = 0),
    names_prefix = "max_"
  )
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

a_meta <- read_csv(attribute_meta_path) %>%
  rename(dim = attribute_dimension) %>%
  select(attribute, dim)

combined_max_scores <- left_join(combined_max_scores, a_meta, by = "attribute")

combined_max_scores <- combined_max_scores %>%
  mutate(
    max_SRA = coalesce(max_SRA, 0),
    max_ERA = coalesce(max_ERA, 0)
  ) %>%
  mutate(total_both = max_SRA + max_ERA)

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

combined_max_scores <- combined_max_scores %>%
  arrange(level, max_score, desc(attribute))
combined_max_scores$comparison <- factor(
  combined_max_scores$comparison,
  levels = c("ERA stronger", "Equal strength", "SRA stronger")
)

combined_max_scores$level <- factor(
  combined_max_scores$level,
  levels = c( "Indicator Level", "Assessment Level")
)

# Reorder attributes for plotting
combined_max_scores <- combined_max_scores %>%
  mutate(attribute = fct_reorder(attribute, max_score)) 
#%>%
# filter(!(max_SRA == 0 & max_ERA == 0))

combined_max_scores <- combined_max_scores %>%
  mutate(capture_level = as.character(capture_level),
         capture_level = case_when(
           is.na(capture_level) ~ "Missing",
           capture_level == "1" ~ "Minimal",
           capture_level == "2" ~ "Moderate",
           capture_level == "3" ~ "Comprehensive",
           TRUE ~ capture_level
         ),
         capture_level = factor(capture_level, levels = c("Missing", "Minimal", "Moderate", "Comprehensive"))
  ) %>%rename(Overall = capture_level)

combined_max_scores <- combined_max_scores %>%
  mutate(level = recode(level, "Coverage by individual PI" = "Coverage within assessment"))

points_data <- combined_max_scores %>%
  select(attribute, min_score, max_score, level, comparison, Overall) %>%
  pivot_longer(cols = c(min_score, max_score), names_to = "ScoreType", values_to = "Score") %>%
  mutate(ScoreType = recode(ScoreType,
                            min_score = "Min. score",
                            max_score = "Max. score"))

assessment_colors <- c(
  "ERA stronger" = "#5e8069",
  "Equal strength" = "#DFC27D",
  "SRA stronger" = "#7e5c85"
)

ggplot(combined_max_scores, aes(x = attribute)) +
  geom_hline(yintercept = c(1, 2, 3), color = "grey80", linetype = "solid") +
  geom_segment(aes(x = attribute, xend = attribute, y = min_score, yend = max_score, color = comparison), size = 4) +
  geom_point(data = points_data, aes(x = attribute, y = Score, shape = ScoreType), size = 3) +
  coord_flip() +
  facet_grid(rows = vars(Overall), cols = vars(level), scales = "free_y", space = "free") +
  scale_color_manual(values = assessment_colors, breaks = c("ERA stronger", "SRA stronger")) +
  scale_shape_manual(values = c("Min. score" = 21, "Max. score" = 16)) +
  scale_y_continuous(
    breaks = 0:3
  ) +
  theme_minimal() +
  labs(
    title = "",
    x = "Attribute",
    #y = "Score range between assessments",
    # color = "Strengthened correspondence/coverage",
    shape = ""
  ) +
  my_theme +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

ggplot(combined_max_scores, aes(x = attribute)) +
  geom_hline(yintercept = c(1, 2, 3), color = "grey80", linetype = "solid") +
  geom_segment(aes(x = attribute, xend = attribute, y = min_score, yend = max_score, color = comparison), size = 4) +
  scale_color_manual(
    name = "Strengthened correspondence/coverage",
    values = assessment_colors,
    breaks = c("ERA stronger", "SRA stronger")
  ) +
  geom_point(
    data = points_data,
    aes(x = attribute, y = Score, shape = ScoreType, fill = factor(Score)),
    size = 3,
    color = "black",
    alpha = 0.7,
    stroke = 1
  ) +
  scale_fill_manual(
    name = "Coverage score - extent",
    values = score_colors,
    breaks = c("0 - No coverage", "1 - Minimal", "2 - Moderate", "3 - Comprehensive"),
    labels = c("0 - No coverage", "1 - Minimal", "2 - Moderate", "3 - Comprehensive")
  ) +
  scale_shape_manual(
    name = "Score Type",
    values = c("Min. score" = 16, "Max. score" = 17)
  ) +
  coord_flip() +
  facet_grid(rows = vars(Overall), cols = vars(level), scales = "free_y", space = "free") +
  theme_minimal() +
  labs(
    title = "",
    x = "Attribute",
    y = "Score range between assessments",
    shape = ""
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.box = "vertical"
  )
