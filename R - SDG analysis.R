#### Packages and libraries used
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("skimr")
install.packages("Amelia")
install.packages("zoo")
install.packages("tidyr")
install.packages("RColorBrewer")
install.packages("reshape2")


library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(skimr)
library(Amelia)
library(zoo)
library(tidyr)
library(RColorBrewer)
library(reshape2)


#### Importing data

## SDG dataset
SDG2000_2023 <- read_excel(path = 'C:/Users/Admin/Documents/MTHM501 Dataset/SDR2024-data.xlsx', sheet = 'Backdated SDG Index')

## continents dataset
continents <- read.csv("C:/Users/Admin/Documents/MTHM501 Dataset/continents2.csv")


#### Data wrangling

## SDG dataset
names(SDG2000_2023) <- gsub(" ", "_", names(SDG2000_2023)) #replace spaces in column names to underscores

SDG2000_2023 <- SDG2000_2023[, c(1:21)] # omit unnecessary columns

SDG2000_2023 <- SDG2000_2023 %>%
  rename('SDG_overall_score' = 'SDG_Index_Score', 'country' = 'Country')   #rename columns in SDG dataframe


## continents data set
continents <- continents %>%
  select(alpha.3, region)

continents <- continents %>%
  rename("country_code" = alpha.3)

## SDG2023 with regions update:
SDG2000_2023 <- merge(x = SDG2000_2023,
                      y = continents,
                      by.x = "id",
                      by.y = "country_code",
                      all.x = TRUE)

SDG2000_2023 <- SDG2000_2023 %>%
  filter(!is.na(country))

SDG2000_2023 <- SDG2000_2023 %>%
  mutate(country = recode(country, 
                          "Vietnam" = "Viet Nam",
                          "Czechia" = "Czech Republic",
                          "Türkiye" = "Turkey"))

SDG2000_2023 <- SDG2000_2023[!grepl("^_",SDG2000_2023$id), ]    

## GDP data set
gdp <- read.csv("C:/Users/Admin/Documents/MTHM501 Dataset/gdp.csv")

gdp <- gdp[, c(2,1,43:63)]

gdp <- gdp %>%
  rename("country" = Country.Name, "country_code" = Code)

gdp$X2021 <- NA
gdp$X2022 <- NA
gdp$X2023 <- NA


gdp_long <- gdp %>%
  pivot_longer(
    cols = starts_with("X"),     # Select columns that start with "X"
    names_to = "year",           # Name of the new column for years
    values_to = "GDP"     # Name of the new column for values
  ) 

gdp_long <- gdp_long %>%
  mutate(year = gsub("X", "", year)) %>%   # Remove the "X" from year values
  mutate(year = as.numeric(year))           # Convert to numeric if needed   


gdp_long <- gdp_long %>%
  filter(!str_detect(country, "World")) %>%  # Remove values that are not countries
  filter(!str_detect(country, "Africa")) %>% # Remove values that are not countries
  filter(!str_detect(country, "Europe")) %>% # Remove values that are not countries
  filter(!str_detect(country, "Asia")) %>% # Remove values that are not countries
  filter(!str_detect(country, "income")) %>% # Remove values that are not countries
  filter(!str_detect(country, "countries")) %>% # Remove values that are not countries
  filter(!str_detect(country, "Small states")) %>%  # Remove values that are not countries 
  filter(!country_code %in% c("GIB","VGB","VIR","PST","PRK",'PRE','OSS','OED','NAC','LTE','LDC','LCN','INX','IDX','IDA','IDB','IBT','IBD','HPC','FCS','EMU','EAR')) %>% # Remove values that are not countries
  filter(!country=="Czechia") %>% # Replace countries' names
  filter(!country=="Turkiye")  # Replace countries' names

## GDP per capita growth data set

gdp_per_capita_growth <- read.csv("C:/Users/Admin/Documents/MTHM501 Dataset/gdp_per_capita_growth.csv")

gdp_per_capita_growth <- gdp_per_capita_growth[, c(2,1,45:68)]      # Keep country code, name, and values from 2000-2023

gdp_per_capita_growth <- gdp_per_capita_growth %>%
  rename("country_code" = "Country.Code", "country" = "Country.Name") %>%  # Rename columns
  mutate(country = recode(country,                                         # Replace countries' names 
                          "Czechia" = "Czech Republic",
                          "Turkiye" = "Turkey"))

gdp_per_capita_long <- gdp_per_capita_growth %>%
  pivot_longer(
    cols = starts_with("X"),     # Select columns that start with "X"
    names_to = "year",           # Name of the new column for years
    values_to = "GDP_per_capita_growth"     # Name of the new column for values
  )

gdp_per_capita_long <- gdp_per_capita_long %>%
  mutate(year = gsub("X", "", year)) %>%   # Remove the "X" from year values
  mutate(year = as.numeric(year))           # Convert to numeric if needed

gdp_per_capita_long <- gdp_per_capita_long %>%
  mutate(country = recode(country, 
                          "Czechia" = "Czech Republic",
                          "Turkiye" = "Turkey"))                               

#Linear interpolation for GDP per capita growth
gdp_per_capita_long <- gdp_per_capita_long %>%
  group_by(country_code) %>%  # Group by Country
  mutate(GDP_per_capita_growth = {
    # Check the number of non-NA values
    if (sum(!is.na(GDP_per_capita_growth)) < 2) {
      NA  # Leave as NA if fewer than 2 non-NA values
    } else {
      approx(x = year, y = GDP_per_capita_growth, method = "linear", xout = year, rule = 2)$y
    }
  }) %>%
  ungroup()  # Ungroup after operation

# #### population dataset:
population <- read.csv("C:/Users/Admin/Documents/MTHM501 Dataset/world_population.csv", skip = 4)  

population <- population[c(1,2, 45:68)]

population <- population %>%
  rename("country" = "Country.Name") %>%
  rename("country_code" = "Country.Code")

population <- population %>%
  mutate(country = recode(country, 
                          "Czechia" = "Czech Republic",
                          "Turkiye" = "Turkey"))

population_long <- population %>%
  pivot_longer(
    cols = starts_with("X"),     # Select columns that start with "X"
    names_to = "year",           # Name of the new column for years
    values_to = "population"     # Name of the new column for values
  )

population_long <- population_long %>%
  mutate(year = gsub("X", "", year)) %>%   # Remove the "X" from year values
  mutate(year = as.numeric(year))           # Convert to numeric if needed  

### Consol demographic dataset (incl. GDP, GDP per capita, population, region)

consol_demographic <- merge(
  x = gdp_long, 
  y = continents, 
  by = c("country_code"), 
  all.x = TRUE)    

consol_demographic <- merge(
  x = consol_demographic, 
  y = population_long, 
  by = c("country_code","country", "year"), 
  all.x = TRUE)

consol_demographic <- merge(
  x = consol_demographic, 
  y = gdp_per_capita_long, 
  by = c("country_code", "country", "year"), 
  all.x = TRUE)

# Add GDP per capita
consol_demographic$GDP_per_capita <- consol_demographic$GDP/consol_demographic$population


#Calculate GDP & GDP per capita based on last year's population and GDP per capita growth:
for (i in 2:nrow(consol_demographic)) {
  if (is.na(consol_demographic$GDP_per_capita[i])) {
    consol_demographic$GDP_per_capita[i] <- (1 + consol_demographic$GDP_per_capita_growth[i]/100) * consol_demographic$GDP_per_capita[i - 1]
  }
}

for (i in 2:nrow(consol_demographic)) {
  if (is.na(consol_demographic$GDP[i])) {
    consol_demographic$GDP[i] <- consol_demographic$population[i] * consol_demographic$GDP_per_capita[i]
  }
}

## Missing data
missmap(SDG2000_2023,
        rank.order = FALSE,
        margins = c(7, 5), plot = FALSE)

## Mean imputation into missing SDG 1, SDG 10, SDG 14 values -- filling in regional mean values
# Create new df for means of all goal-specific scores
avg_SDG_2000_2023 <- SDG2000_2023 %>%
  group_by(region, year) %>%
  summarise(across(starts_with("goal"), ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"), .groups = 'drop')

SDG2000_2023 <- SDG2000_2023 %>%
  left_join(avg_SDG_2000_2023, by = c("region", "year")) %>%
  mutate(across(starts_with("goal"), 
                ~ ifelse(is.na(.x), get(paste0("mean_", cur_column())), .x))) %>%
  select(-starts_with("mean_"))

### Summary dataset (avg. and total figures included)
SDG_summary_country <- merge(
  x = consol_demographic,
  y = SDG2000_2023,
  by.x = c("country_code","country","year","region"),
  by.y = c("id","country","year","region"),
  all.y = TRUE)            

SDG_summary_region <- SDG_summary_country %>%
  group_by(year, region) %>%
  summarise(
    avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
    total_GDP = sum(GDP, na.rm = TRUE),
    avg_SDG_score = mean(SDG_overall_score, na.rm = TRUE)
  ) %>%
  arrange(year, region)

#### Correlation heatmap
avg_SDG_region <- SDG_summary_country %>%
  group_by(region, year) %>%
  summarise(
    mean_goal1 = mean(goal1, na.rm = TRUE),
    mean_goal2 = mean(goal2, na.rm = TRUE),
    mean_goal3 = mean(goal3, na.rm = TRUE),
    mean_goal4 = mean(goal4, na.rm = TRUE),
    mean_goal5 = mean(goal5, na.rm = TRUE),
    mean_goal6 = mean(goal6, na.rm = TRUE),
    mean_goal7 = mean(goal7, na.rm = TRUE),
    mean_goal8 = mean(goal8, na.rm = TRUE),
    mean_goal9 = mean(goal9, na.rm = TRUE),
    mean_goal10 = mean(goal10, na.rm = TRUE),
    mean_goal11 = mean(goal11, na.rm = TRUE),
    mean_goal12 = mean(goal12, na.rm = TRUE),
    mean_goal13 = mean(goal13, na.rm = TRUE),
    mean_goal14 = mean(goal14, na.rm = TRUE),
    mean_goal15 = mean(goal15, na.rm = TRUE),
    mean_goal16 = mean(goal16, na.rm = TRUE),
    mean_goal17 = mean(goal17, na.rm = TRUE),
    .groups = 'drop'
  )

avg_SDG_region <- avg_SDG_region[,-c(1,2)]

avg_SDG_region <- avg_SDG_region %>%
  rename(SDG1 = mean_goal1,
         SDG2 = mean_goal2,
         SDG3 = mean_goal3,
         SDG4 = mean_goal4,
         SDG5 = mean_goal5,
         SDG6 = mean_goal6,
         SDG7 = mean_goal7,
         SDG8 = mean_goal8,
         SDG9 = mean_goal9,
         SDG10 = mean_goal10,
         SDG11 = mean_goal11,
         SDG12 = mean_goal12,
         SDG13 = mean_goal13,
         SDG14 = mean_goal14,
         SDG15 = mean_goal15,
         SDG16 = mean_goal16,
         SDG17 = mean_goal17
  )

corr_mat <- round(cor(avg_SDG_region),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)

# Plot correlation heatmap
ggplot(data = melted_corr_mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#D73027","#FEE08B","#1A9850")) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Question 1 - Visualization
SDG2023 <- SDG2000_2023 %>%
  filter(year == '2023')

ggplot(data = subset(SDG2023, !is.na(region)), aes(x = region, y = SDG_overall_score, fill = region)) + 
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(x = 'Region',y = 'SDG Overall Score',colour = 'Region') +
  scale_fill_brewer(palette = "Set1")

#### Question 2a - Visualization
ggplot(SDG_summary_region,aes(x = year, y = avg_SDG_score, colour = factor(region))) +
  geom_point() +   # Specifying a scatter plot
  geom_smooth(method = "lm") +   # Adding a linear trend to the plot
  geom_vline(xintercept = 2015, colour = 'blue', linewidth = 1) +   # 
  scale_color_brewer(palette = 'Set1') +   # Color palette
  labs(x = 'Year', y = 'Mean SDG Overall Score', colour = 'Region')   # Axes labels

#### Question 2b - Visualization
SDG_summary_region_2015 <- SDG_summary_region %>%
  filter(year >= 2015)

## Graph
scale_factor2 <- (40000/ 1000)/100

ggplot(SDG_summary_region, aes(x = year)) +
  geom_bar(aes(y = avg_GDP_per_capita / (scale_factor2 *1000), fill = "Avg. GDP per capita"), stat = "identity", position = "dodge") +  # Bar for Avg. GDP per capita in trillions
  geom_line(aes(y = avg_SDG_score, color = region, group = region), size = 1) +
  scale_y_continuous(
    name = "Average SDG Score",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . * scale_factor2, name = "Average GDP per capita (in $000)")  # Adjust scaling to match GDP scale
  ) +
  scale_x_discrete(
    name = "Year",
    breaks = seq(min(SDG_summary_region$year), max(SDG_summary_region$year), by = 2)  # Set breaks every 5 years
  ) +
  labs(x = "Year") +
  scale_fill_manual(values = c("Avg. GDP per capita" = "#BDBDBD")) +  # Fill color for GDP bars
  scale_color_brewer(palette = "Set1") + # Line color for SDG
  facet_wrap(~ region, ncol = 2) +                   # Facet by region
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F0F0F0", color = NA), # Set panel background to light grey
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

### Question 2b - statistical analysis
## New dataframe - country level analysis from 2015-2023
SDG_summary_country_2015 <- SDG_summary_country %>%
  filter(year >= 2015)

# Correlation between average SDG score & GDP per capita
cor.test(SDG_summary_country_2015$SDG_overall_score, 
         SDG_summary_country_2015$GDP_per_capita, 
         method = "pearson")

# Linear regression -- relationship between average SDG score & GDP per capita
lm_GDP_pc = lm(GDP_per_capita ~ SDG_overall_score, data = SDG_summary_country_2015)
summary(lm_GDP_pc)

## Plotting the Linear regression graph
ggplot(SDG_summary_region_2015, aes(x = avg_GDP_per_capita, y = avg_SDG_score)) +
  geom_point() +  # Plot the data points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add linear regression line with confidence interval
  labs(x = "Average GDP per capita ($)",
       y = "Average SDG score") +
  theme_minimal()  # Apply a minimal theme

## ANOVA test between average SDG score (DV), region and year (IV)
SDG_summary_country <- SDG_summary_country %>%
  mutate(year = as.character(year))

SDG_summary_region_2015 <- SDG_summary_region_2015 %>%
  mutate(year = as.character(year))

anova_result <- aov(avg_SDG_score ~ region + year, data = SDG_summary_region_2015)
summary(anova_result)

## Residual plots for ANOVA model
par(mfrow=c(2,2))
plot(anova_result)
par(mfrow=c(1,1))

## Linear regression -- relationship between region and average SDG score (no heteroscedasticity)
lm_year <- aov(SDG_overall_score ~ year, data = SDG_summary_country)
summary(lm_year)

## Kruskal-Wallis test -- relationship between region and average SDG score (due to heteroscedasticity)
kruskal_results <- kruskal.test(SDG_overall_score ~ region, data = SDG_summary_country) 
summary(kruskal_results)
