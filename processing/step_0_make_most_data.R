# Libraries
library(readr)
library(dplyr)

library(tidyr)

# Load data
component_meta <- read_csv("data/component_meta_data.csv")
df <- read_csv("data/final_raw_components.csv") %>% 
  select(indicator_code, attribute_component, score) %>% 
  rename(indicator_component_score = score)
attribute_meta <- read_csv("data/attribute_meta.csv")
indicator_meta <- read_csv("data/indicator_meta.csv")

# All combinations
all_combinations <- expand_grid(
  indicator_code = indicator_meta$indicator_code,
  attribute_component = component_meta$attribute_component
)

# Join data
df1 <- all_combinations %>%
  left_join(df, by = c("indicator_code", "attribute_component")) %>%
  mutate(indicator_component_score = coalesce(indicator_component_score, 0)) %>%
  left_join(component_meta %>% select(attribute_component, attribute, attribute_complexity), 
            by = "attribute_component")

# Tier 0
df_tier0 <- df1 %>%
  left_join(attribute_meta %>% select(attribute, attribute_dimension), 
            by = "attribute")

write_csv(df_tier0, "data/processed_data/df_tier0.csv")

# Multi-c and single-c
multi_c_df <- df_tier0 %>% 
  filter(attribute_complexity == "multi-c") %>% 
  group_by(indicator_code, attribute) %>% 
  summarise(indicator_attribute_score = sum(indicator_component_score) / 2, .groups = 'drop') %>%
  mutate(indicator_attribute_score = case_when(
    indicator_attribute_score >= 2.5 ~ 3,
    indicator_attribute_score >= 1.5 ~ 2,
    indicator_attribute_score >= 0.5 ~ 1,
    TRUE ~ 0
  )) 

sdf <- df_tier0 %>% 
  filter(attribute_complexity == "single-c") %>% 
  mutate(indicator_attribute_score = indicator_component_score) %>%
  select(-indicator_component_score, -attribute_component)

# Combine data
df_indicator_attribute_corr_score <- bind_rows(sdf, multi_c_df)

# Tier 1
df_tier1 <- df_indicator_attribute_corr_score %>%
  left_join(indicator_meta %>% 
              select(indicator_code, assessment_c, assessment_principle, assessment, principle, indicator), 
            by = "indicator_code")

write_csv(df_tier1, "data/processed_data/tier1_data.csv")

# Filter tier1
df_tier1_share <- df_tier1 %>% 
  filter(indicator_attribute_score > 0) %>%
  arrange(attribute_dimension, indicator_attribute_score) %>% 
  rename(correspondence_score = indicator_attribute_score) %>% 
  select(assessment, assessment_principle, indicator_code, indicator, attribute_dimension, attribute, correspondence_score)

write_csv(df_tier1_share, "data/processed_data/tier1_nozero.csv")

# ERA assessment
ERA <- df_tier1 %>% 
  filter(assessment == "ERA") %>% 
  group_by(attribute) %>%
  summarise(max_score_att = max(indicator_attribute_score, na.rm = TRUE)) %>% 
  left_join(attribute_meta %>% select(attribute, attribute_dimension), by = "attribute") %>% 
  mutate(
    coverage_score = case_when(
      attribute %in% c("Transparent", "Efficient and effective", "Accountable") ~ 2,
      TRUE ~ max_score_att
    ),
    assessment_considered = "ERA"
  ) %>% 
  filter(coverage_score > 0)

# SRA assessment
SRA <- df_tier1 %>% 
  filter(assessment == "SRA") %>% 
  group_by(attribute) %>%
  summarise(max_score_att = max(indicator_attribute_score, na.rm = TRUE)) %>%
  left_join(attribute_meta %>% select(attribute, attribute_dimension), by = "attribute") %>% 
  mutate(
    coverage_score = case_when(
      attribute %in% c("Participatory", "Agency", "Equitable and inclusive", 
                       "Access to economic opportunity", "Wealth and reserves", 
                       "Accountable") ~ 3,
      TRUE ~ max_score_att
    ),
    assessment_considered = "SRA"
  ) %>% 
  filter(coverage_score > 0)

# Combine SRA and ERA
SRA_1 <- SRA %>% 
  select(attribute, coverage_score, assessment_considered, attribute_dimension) %>% 
  rename(as_SRA = coverage_score)

ERA_1 <- ERA %>% 
  select(attribute, coverage_score) %>% 
  rename(as_ERA = coverage_score) 

Combined <- full_join(SRA_1, ERA_1, by = "attribute") %>% 
  mutate(
    coverage_score = pmax(as_SRA, as_ERA, na.rm = TRUE),
    coverage_score = ifelse(attribute == "Transparent", 3, coverage_score),
    assessment_considered = "Combined"
  ) %>% 
  select(attribute, coverage_score, assessment_considered)

adim <- attribute_meta %>% select(attribute, attribute_dimension)
Combined <- left_join(Combined, adim)
overall_attribute_coverage <- left_join(Combined, attribute_meta)

write_csv(overall_attribute_coverage, "data/processed_data/Tier_2_combined_only.csv")

# Combine all assessments
SRA2 <- SRA %>% 
  select(attribute, coverage_score, assessment_considered, attribute_dimension)
ERA2 <- ERA %>% 
  select(attribute, coverage_score, assessment_considered, attribute_dimension)

attribute_coverage_by_assessment <- bind_rows(Combined, SRA2, ERA2)
write_csv(attribute_coverage_by_assessment, "data/processed_data/Tier_2_all.csv")
