library(ggridges)
library(scales)

supp_2 <- read_csv("data/processed_data/tier1_data.csv") %>%
  rename(score = indicator_attribute_score)

s2 <- supp_2 %>% 
  filter(score > 0) %>% 
  group_by(assessment, indicator_code) %>% 
  summarise(indicator_n = length(unique(attribute))) %>% 
  mutate(n = as.numeric(indicator_n)) %>% 
  arrange(rev(assessment)) %>% 
  select(-indicator_n)

wix <- s2 %>% 
  select(assessment, n)
wixr <- wilcox.test(n ~ assessment, data = wix)
print(wixr)

assessment_colors <- c("ERA" = "#00441B", "SRA" = "#40004B")

ggplot(s2, aes(x = n, y = assessment, fill = assessment)) + stat_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = 0.3, scale = .9, linetype = "dashed") + geom_density_ridges(scale = .9, alpha = 0.4) + scale_fill_manual(name = "FIP assessment and median n", values = assessment_colors) + scale_x_continuous(breaks = 1:5, labels = c("1", "2", "3", "4", "5"), limits = c(1, 5)) + my_theme + labs(title = "", x = "Number of attributes (n) per performance indicator", y = "Assessment") + scale_y_discrete(expand = c(0, 0))

quartz.save("supp_figures/Figure_S2.png", type = "png", dpi = 600)

supp_1 <- supp_2 %>% 
  filter(score > 0) 

s1 <- supp_1 %>%
  filter(score > 0) %>% 
  mutate(score = as.factor(score)) %>%
  select(assessment, score) %>% 
  group_by(assessment, score) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(assessment) %>% 
  mutate(total_n = c(27, 27, 27, 54, 54, 54)) %>% 
  mutate(prop = (n / total_n)) %>% 
  mutate(percent = percent(prop, accuracy = 1))

s1$score <- as.factor(s1$score)

s1 <- s1 %>%
  group_by(assessment) %>%
  mutate(cum_pos = cumsum(n) - (n / 2))

ggplot(s1, aes(x = assessment, y = n, fill = assessment)) + geom_bar(aes(alpha = score), stat = "identity", color = "white", position = position_stack(reverse = TRUE)) + scale_alpha_manual(values = c("1" = 0.4, "2" = 0.6, "3" = 1), labels = c("1 — Minimal", "2 — Moderate", "3 — Comprehensive")) + geom_text(aes(y = cum_pos, label = percent, color = ifelse(score == "3", "white", "black")), size = 3.5, family = "Helvetica") + scale_fill_manual(name = "FIP assessment", values = assessment_colors) + scale_color_identity() + my_theme + labs(y = "Instances of coverage of attributes (n = 81)", x = "Assessment", alpha = "Strength of coverage")

quartz.save("supp_figures/Figure_S1.png", type = "png", dpi = 600)
