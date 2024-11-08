library(tidyverse)
library(cowplot)
library(forcats)
library(stringr)
library(scales)
library(grid)

t1<- read_csv("data/processed_data/tier1_data.csv")
t2<- read_csv("data/processed_data/Tier_2_all.csv") %>% 
  filter(assessment_considered != "Combined")

# Process df_tier1 (Indicator Level Data)
df_tier1_max <- t1 %>%
  # Rename columns for consistency
  rename(
    attribute = attribute,
    assessment = assessment,
    score = indicator_attribute_score
  ) %>%
  # Compute max score per attribute and assessment
  group_by(assessment, attribute) %>%
  summarise(
    max_score = max(score, na.rm = TRUE) 
  ) %>%
  ungroup() %>%
  mutate(level = "Indicator Level") %>% 
  filter(max_score != 0)

# Process df_tier2 (Assessment Level Data)
df_tier2_processed <- t2 %>%
  # Rename columns for consistency
  rename(
    attribute = attribute,
    assessment = assessment_considered,
    max_score = coverage_score
  ) %>%
  mutate(level = "Assessment Level") %>%
  select(attribute, assessment, max_score, level)

# Combine both dataframes into one
combined_data <- bind_rows(df_tier1_max, df_tier2_processed)




# Pivot the data to have separate columns for max scores from SRA and ERA
combined_max_scores <- combined_data %>%
  pivot_wider(
    names_from = assessment,
    values_from = max_score,
    values_fill = list(max_score = 0),
    names_prefix = "max_"
  )


has_score<- combined_max_scores %>% 
  select(attribute) %>% 
  distinct()

a<- read_csv("attribute_meta.csv") %>% 
  rename(dim= attribute_dimension) %>% 
  select(attribute)

missing_score1<- anti_join(a, has_score, by = "attribute") %>% 
  mutate(level = "Assessment Level")

missing_score2<- anti_join(a, has_score, by = "attribute") %>% 
  mutate(level = "Indicator Level")

combined_max_scores<- full_join(combined_max_scores, missing_score1)
combined_max_scores<- full_join(combined_max_scores, missing_score2)

a_meta<- read_csv("attribute_meta_path") %>% 
  rename(dim= attribute_dimension) %>% 
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
    change = abs(max_SRA - max_ERA)
  )

combined_max_scores <- combined_max_scores %>%
  arrange(level, attribute)

combined_max_scores$comparison <- factor(
  combined_max_scores$comparison,
  levels = c("Equal strength", "ERA stronger","SRA stronger")
)

write_csv(combined_max_scores, "data/processed_data/Figure_2_data.csv")

