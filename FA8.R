library(dplyr)
library(car)
library(ggplot2)

data("PlantGrowth")
PlantGrowth$group <- as.factor(PlantGrowth$group)

group_stats <- PlantGrowth %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(weight),
    sd = sd(weight),
    shapiro_p = shapiro.test(weight)$p.value
  )
print(group_stats)

plot_box <- ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  geom_boxplot() +
  labs(title = "Plant Weight by Group", x = "Treatment Group", y = "Dried Weight") +
  theme_classic()
print(plot_box)

levene_test <- leveneTest(weight ~ group, data = PlantGrowth)
print(levene_test)

anova_model <- aov(weight ~ group, data = PlantGrowth)
anova_summary <- summary(anova_model)
print(anova_summary)

ss_between <- anova_summary[[1]]$`Sum Sq`[1]
ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
eta_squared <- ss_between / ss_total
print(paste("Eta Squared:", round(eta_squared, 3)))

tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

plot_violin <- ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  labs(title = "Distribution of Plant Weights by Group", 
       y = "Weight", x = "Group") +
  theme_classic()
print(plot_violin)