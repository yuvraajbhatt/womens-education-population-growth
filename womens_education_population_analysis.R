# ============================================================
# Women's Education and Population Growth
# Cross-National Statistical Investigation Using Gapminder Data, 2015
# Author: Yuvraj Bhatt
# ============================================================
# Data Source: Gapminder Foundation (https://www.gapminder.org)
# Dataset: 185 countries, 2015
# ============================================================

# ============================================================
# SECTION 1: LIBRARIES, DATA LOADING & PREPARATION
# ============================================================
library(readxl)
library(tidyverse)
library(ggplot2)
library(car)
library(lmtest)
library(nortest)
library(dunn.test)

# Load raw data -- update paths to your local data files
hdi_raw    <- read_excel("hdi_human_development_index.xlsx")
school_raw <- read_csv("mean_years_in_school_women_percent_men_25_to_34_years.csv")
pop_raw    <- read_csv("population_growth_annual_percent.csv")

# Check dimensions
cat("HDI dimensions:    ", dim(hdi_raw), "\n")
cat("School dimensions: ", dim(school_raw), "\n")
cat("Pop dimensions:    ", dim(pop_raw), "\n")

# Extract year 2015 from each dataset
hdi_2015 <- hdi_raw %>%
  select(name, hdi = `2015`) %>%
  drop_na(hdi)

school_2015 <- school_raw %>%
  select(name, school = `2015`) %>%
  drop_na(school)

pop_2015 <- pop_raw %>%
  select(name, pop_growth = `2015`) %>%
  drop_na(pop_growth)

# Merge all three datasets by country name
df <- hdi_2015 %>%
  inner_join(school_2015, by = "name") %>%
  inner_join(pop_2015,   by = "name")

# Convert HDI to numeric
df$hdi <- as.numeric(df$hdi)

cat("\nSample size after merging:", nrow(df), "countries\n")
cat("Missing values per variable:\n")
print(colSums(is.na(df)))

# Create HDI categorical variable using official UNDP cut-points
df <- df %>%
  mutate(
    hdi_cat = case_when(
      hdi < 0.550                ~ "1_Low",
      hdi >= 0.550 & hdi < 0.700 ~ "2_Medium",
      hdi >= 0.700 & hdi < 0.800 ~ "3_High",
      hdi >= 0.800               ~ "4_VeryHigh"
    ),
    hdi_cat = factor(hdi_cat,
                     levels = c("1_Low", "2_Medium",
                                "3_High", "4_VeryHigh"))
  )

cat("\nHDI Category Distribution:\n")
print(table(df$hdi_cat))

# Summary Statistics Table
summary_table <- data.frame(
  Variable = c("Women's Schooling (% of men)",
               "Population Growth (%)",
               "HDI"),
  N      = c(nrow(df), nrow(df), nrow(df)),
  Mean   = c(round(mean(df$school),     3),
             round(mean(df$pop_growth), 3),
             round(mean(df$hdi),        3)),
  Median = c(round(median(df$school),     3),
             round(median(df$pop_growth), 3),
             round(median(df$hdi),        3)),
  SD     = c(round(sd(df$school),     3),
             round(sd(df$pop_growth), 3),
             round(sd(df$hdi),        3)),
  Min    = c(round(min(df$school),     3),
             round(min(df$pop_growth), 3),
             round(min(df$hdi),        3)),
  Max    = c(round(max(df$school),     3),
             round(max(df$pop_growth), 3),
             round(max(df$hdi),        3))
)

cat("\nTable 1: Summary Statistics\n")
print(summary_table)

# ============================================================
# SECTION 2: EXPLORATORY DATA ANALYSIS
# ============================================================

# FIGURE 1: Histogram of Women's Schooling
ggplot(df, aes(x = school)) +
  geom_histogram(bins = 25, fill = "#2196F3",
                 colour = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(school)),
             colour = "red", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(school)),
             colour = "darkgreen", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = mean(df$school) - 12, y = 17,
           label = paste("Mean =", round(mean(df$school), 1)),
           colour = "red", size = 3.5) +
  annotate("text", x = median(df$school) + 12, y = 19,
           label = paste("Median =", round(median(df$school), 1)),
           colour = "darkgreen", size = 3.5) +
  labs(
    title = "Figure 1: Distribution of Women's Mean Years in School (% of Men), 2015",
    x     = "Women's Mean Years in School (% of Men, Age 25-34)",
    y     = "Number of Countries"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 11))

ggsave("images/Figure1_hist_school.png", width = 8, height = 5, dpi = 300)

# FIGURE 2: Histogram of Population Growth
ggplot(df, aes(x = pop_growth)) +
  geom_histogram(bins = 25, fill = "#E53935",
                 colour = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(pop_growth)),
             colour = "navy", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(pop_growth)),
             colour = "darkorange", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = mean(df$pop_growth) + 1.2, y = 28,
           label = paste("Mean =", round(mean(df$pop_growth), 2)),
           colour = "navy", size = 3.5) +
  annotate("text", x = median(df$pop_growth) - 1.3, y = 25,
           label = paste("Median =", round(median(df$pop_growth), 2)),
           colour = "darkorange", size = 3.5) +
  labs(
    title = "Figure 2: Distribution of Annual Population Growth (%), 2015",
    x     = "Annual Population Growth (%)",
    y     = "Number of Countries"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 11))

ggsave("images/Figure2_hist_popgrowth.png", width = 8, height = 5, dpi = 300)

# FIGURE 3: Boxplot of Population Growth by HDI Category
ggplot(df, aes(x = hdi_cat, y = pop_growth, fill = hdi_cat)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21,
               outlier.fill = "white", outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.3,
              size = 1.2, colour = "black") +
  scale_fill_manual(values = c(
    "1_Low"      = "#E53935",
    "2_Medium"   = "#FB8C00",
    "3_High"     = "#43A047",
    "4_VeryHigh" = "#1E88E5"
  )) +
  scale_x_discrete(labels = c(
    "1_Low"      = "Low",
    "2_Medium"   = "Medium",
    "3_High"     = "High",
    "4_VeryHigh" = "Very High"
  )) +
  labs(
    title = "Figure 3: Annual Population Growth (%) by HDI Category, 2015",
    x     = "HDI Category",
    y     = "Annual Population Growth (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 11),
    legend.position = "none"
  )

ggsave("images/Figure3_boxplot_hdi.png", width = 8, height = 5, dpi = 300)

# ============================================================
# SECTION 3: LINEAR REGRESSION
# ============================================================

# FIGURE 4: Scatter Plot with Regression Line
ggplot(df, aes(x = school, y = pop_growth)) +
  geom_point(alpha = 0.55, colour = "#1E88E5", size = 2) +
  geom_smooth(method = "lm", colour = "red",
              fill = "pink", alpha = 0.25, linewidth = 1) +
  geom_smooth(method = "loess", colour = "darkgreen",
              fill = "lightgreen", alpha = 0.2,
              linewidth = 0.8, linetype = "dashed") +
  labs(
    title    = "Figure 4: Women's Schooling vs. Annual Population Growth, 2015",
    subtitle = "Red = linear fit | Green dashed = LOESS smooth",
    x        = "Women's Mean Years in School (% of Men, Age 25-34)",
    y        = "Annual Population Growth (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )

ggsave("images/Figure4_scatter_regression.png", width = 8, height = 5, dpi = 300)

# Fit linear model
lm_model <- lm(pop_growth ~ school, data = df)
summary(lm_model)

resid_df <- data.frame(
  fitted    = fitted(lm_model),
  residuals = residuals(lm_model)
)

# Assumption testing
shapiro.test(residuals(lm_model))       # Normality
nortest::ad.test(residuals(lm_model))   # Normality (Anderson-Darling)
lmtest::bptest(lm_model)               # Homoscedasticity (Breusch-Pagan)

# Non-parametric alternative (Spearman) due to normality violation
spearman_result <- cor.test(df$school, df$pop_growth, method = "spearman")
print(spearman_result)

# FIGURE 5: Q-Q Plot of Residuals
ggplot(resid_df, aes(sample = residuals)) +
  stat_qq(colour = "#1E88E5", alpha = 0.7, size = 2) +
  stat_qq_line(colour = "red", linewidth = 0.9) +
  labs(
    title    = "Figure 5: Normal Q-Q Plot of Regression Residuals",
    subtitle = "Points close to the red line indicate normality",
    x        = "Theoretical Quantiles",
    y        = "Sample Quantiles"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )
ggsave("images/Figure5_QQ_residuals.png", width = 7, height = 5, dpi = 300)

# FIGURE 6: Residuals vs Fitted Values
ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.55, colour = "#1E88E5", size = 2) +
  geom_hline(yintercept = 0, colour = "red",
             linetype = "dashed", linewidth = 0.9) +
  geom_smooth(method = "loess", colour = "darkgreen",
              se = FALSE, linewidth = 0.8) +
  labs(
    title    = "Figure 6: Residuals vs Fitted Values",
    subtitle = "Random scatter around 0 indicates homoscedasticity",
    x        = "Fitted Values",
    y        = "Residuals"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )
ggsave("images/Figure6_residuals_vs_fitted.png", width = 7, height = 5, dpi = 300)

# FIGURE 7: Scale-Location Plot
resid_df$sqrt_abs_resid <- sqrt(abs(scale(residuals(lm_model))))

ggplot(resid_df, aes(x = fitted, y = sqrt_abs_resid)) +
  geom_point(alpha = 0.55, colour = "#8E24AA", size = 2) +
  geom_smooth(method = "loess", colour = "red",
              se = FALSE, linewidth = 0.9) +
  labs(
    title    = "Figure 7: Scale-Location Plot",
    subtitle = "A flat red line indicates equal variance (homoscedasticity)",
    x        = "Fitted Values",
    y        = expression(sqrt("|Standardised Residuals|"))
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )
ggsave("images/Figure7_scale_location.png", width = 7, height = 5, dpi = 300)

# FIGURE 8: Histogram of Residuals
ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "#1E88E5", colour = "white", alpha = 0.8) +
  geom_density(colour = "red", linewidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(resid_df$residuals),
                            sd   = sd(resid_df$residuals)),
                colour = "darkgreen", linewidth = 1,
                linetype = "dashed") +
  labs(
    title    = "Figure 8: Histogram of Regression Residuals",
    subtitle = "Red = actual density | Green dashed = normal distribution",
    x        = "Residuals",
    y        = "Density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )
ggsave("images/Figure8_hist_residuals.png", width = 7, height = 5, dpi = 300)

# ============================================================
# SECTION 4: HYPOTHESIS TESTING
# ============================================================
# H0: Median population growth is equal across all HDI categories
# H1: At least one HDI category has a different median population growth

# One-way ANOVA
anova_result <- aov(pop_growth ~ hdi_cat, data = df)
summary(anova_result)

# Shapiro-Wilk per group
df %>%
  group_by(hdi_cat) %>%
  summarise(
    n         = n(),
    SW_stat   = round(shapiro.test(pop_growth)$statistic, 4),
    SW_pvalue = round(shapiro.test(pop_growth)$p.value, 4)
  )

# Levene's test for homogeneity of variance
car::leveneTest(pop_growth ~ hdi_cat, data = df)

# Kruskal-Wallis (non-parametric alternative due to normality violations)
kw_result <- kruskal.test(pop_growth ~ hdi_cat, data = df)
print(kw_result)

# Dunn's post-hoc test with Bonferroni correction
dunn.test::dunn.test(df$pop_growth, df$hdi_cat, method = "bonferroni")

# FIGURE 9: Histograms of Population Growth by HDI Group
ggplot(df, aes(x = pop_growth, fill = hdi_cat)) +
  geom_histogram(bins = 15, colour = "white", alpha = 0.85) +
  scale_fill_manual(values = c(
    "1_Low"      = "#E53935",
    "2_Medium"   = "#FB8C00",
    "3_High"     = "#43A047",
    "4_VeryHigh" = "#1E88E5"
  )) +
  facet_wrap(~hdi_cat,
             labeller = labeller(hdi_cat = c(
               "1_Low"      = "Low HDI",
               "2_Medium"   = "Medium HDI",
               "3_High"     = "High HDI",
               "4_VeryHigh" = "Very High HDI"
             ))) +
  labs(
    title = "Figure 9: Distribution of Population Growth by HDI Category, 2015",
    x     = "Annual Population Growth (%)",
    y     = "Number of Countries"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 11),
    legend.position = "none"
  )
ggsave("images/Figure9_facet_hist_hdi.png", width = 9, height = 6, dpi = 300)

# ANOVA residuals dataframe
anova_resid_df <- data.frame(
  fitted    = fitted(anova_result),
  residuals = residuals(anova_result)
)

# FIGURE 10: Q-Q Plot of ANOVA Residuals
ggplot(anova_resid_df, aes(sample = residuals)) +
  stat_qq(colour = "#E53935", alpha = 0.7, size = 2) +
  stat_qq_line(colour = "navy", linewidth = 0.9) +
  labs(
    title    = "Figure 10: Normal Q-Q Plot of ANOVA Residuals",
    subtitle = "Points close to the blue line indicate normality",
    x        = "Theoretical Quantiles",
    y        = "Sample Quantiles"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )
ggsave("images/Figure10_QQ_anova_residuals.png", width = 7, height = 5, dpi = 300)

# FIGURE 11: ANOVA Residuals vs Fitted Values
ggplot(anova_resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.55, colour = "#E53935", size = 2) +
  geom_hline(yintercept = 0, colour = "navy",
             linetype = "dashed", linewidth = 0.9) +
  geom_smooth(method = "loess", colour = "darkgreen",
              se = FALSE, linewidth = 0.8) +
  labs(
    title    = "Figure 11: ANOVA Residuals vs Fitted Values",
    subtitle = "Points should be randomly scattered around 0",
    x        = "Fitted Values (Group Means)",
    y        = "Residuals"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )
ggsave("images/Figure11_anova_residuals_vs_fitted.png", width = 7, height = 5, dpi = 300)

# FIGURE 12: Histogram of ANOVA Residuals
ggplot(anova_resid_df, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "#E53935", colour = "white", alpha = 0.8) +
  geom_density(colour = "navy", linewidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(anova_resid_df$residuals),
                            sd   = sd(anova_resid_df$residuals)),
                colour = "darkgreen", linewidth = 1,
                linetype = "dashed") +
  labs(
    title    = "Figure 12: Histogram of ANOVA Residuals",
    subtitle = "Navy = actual density | Green dashed = normal distribution",
    x        = "Residuals",
    y        = "Density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )
ggsave("images/Figure12_hist_anova_residuals.png", width = 7, height = 5, dpi = 300)
