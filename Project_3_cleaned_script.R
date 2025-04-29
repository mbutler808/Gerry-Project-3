# Welcome to Project 3 for ZOOL710 at UH Manoa.

# My name is Jerry and I work on Maize Genetics.

# In our lab the project we are working on focuses on trying to make temperate 
# corn insensitive todaylength so that it can flower under the longer days in 
# temperate lattitudes and breeders can access and incroporate desireble traits 
# from the the highlydiverse tropical maize genome into temperate maize through 
# cross pollination. 

# Here in this project we'll explore the shedding and silking days
# of different maize lines grown in the Waimanalo research station. 

# First you're going to want to download the file to your correct folder, then
# navigate to the correct location where the file is. This is what it looks like
# for me:

#### TALK ABOUT THE VARIETES AND WHATS UNIQUE ABOUT EACH #####

## ---- Loaddata ----
library(here)
library(dplyr)
library(ggplot2)
library(gt)
library(ggrepel)
library(stringr)

data_location <- here("HI_SU2024_SHD_SLK.csv")
HI_Data <- read.csv("HI_SU2024_SHD_SLK.csv")

## ---- Exploredata ----
library(dplyr)
library(skimr)
dplyr::glimpse(HI_Data)
summary(HI_Data)
skimr::skim(HI_Data)

## ---- Addcolumns -----
library(dplyr)

HI_Data <- HI_Data %>%
  mutate(
    Days_diff = Days_SK - Days_SHD,    # How many days between shedding and silking
    GDD_diff = GDD_SK - GDD_SHD     # How many growing degree days between shedding and silking
  )

# View the new columns

dplyr::glimpse(HI_Data)
HI_Data

## ---- Summarystats ----
# We'll look at how early or late each pedigree is and 
# how synchronized shedding and silking are on average.

library(dplyr)

HI_Data %>%
  group_by(PEDIGREE) %>%
  summarise(
    mean_Days_SHD = mean(Days_SHD, na.rm = TRUE),
    mean_Days_SK = mean(Days_SK, na.rm = TRUE),
    mean_Days_diff = mean(Days_diff, na.rm = TRUE),
    mean_GDD_diff = mean(GDD_diff, na.rm = TRUE)
  )

## ---- Fixrows ------
# Oops, two CML277 rows showed up, but we just want one.
# lets remove that extra space then look at the summary table again.
    
install.packages("stringr")
library(stringr)
    
HI_Data <- HI_Data %>%
  mutate(PEDIGREE = str_trim(PEDIGREE))  # trims leading/trailing spaces
    
HI_Data
    
# Run the HI_Data %>% etc. code again
    
HI_Data %>%
  group_by(PEDIGREE) %>%
      summarise(
        mean_Days_SHD = mean(Days_SHD, na.rm = TRUE),
        mean_Days_SK = mean(Days_SK, na.rm = TRUE),
        mean_Days_diff = mean(Days_diff, na.rm = TRUE),
        mean_GDD_diff = mean(GDD_diff, na.rm = TRUE)
      )

# Looks better
    

# Lets fix the titles so its really clear what we are looking at, then analyze the data and make some graphs.

## ---- Fixtitles -----

HI_Data

# Create the summarized table
HI_Summary <- HI_Data %>%
  group_by(PEDIGREE) %>%
  summarise(
    mean_Days_SHD = mean(Days_SHD, na.rm = TRUE),
    mean_Days_SK = mean(Days_SK, na.rm = TRUE),
    mean_Days_diff = mean(Days_diff, na.rm = TRUE),
    mean_GDD_diff = mean(GDD_diff, na.rm = TRUE)
  ) %>%
  rename(
    `Avg days to shedding` = mean_Days_SHD,
    `Avg days to silking` = mean_Days_SK,
    `Avg days between shedding and silking` = mean_Days_diff,
    `Avg difference of GDD between shedding and silking` = mean_GDD_diff
  )

# Lets also clean the original data set so its easier to read when we make graphs from it.

library(dplyr)

HI_Data <- HI_Data %>%
  rename(
    `Shed Date` = SHD,
    `Days to Shedding` = Days_SHD,
    `Shed GDD` = GDD_SHD,
    `Silk Date` = SK,
    `Days to Silking` = Days_SK,
    `Silk GDD` = GDD_SK,
    `Days between Shedding and Silking` = Days_diff,
    `Difference in GDD between Shedding and Silking` = GDD_diff
  )
    

## ---- Looknice -----

library(gt)

HI_Data %>%
  gt()

HI_Summary %>%
  gt()


## ---- Setorder -----

# Reorder PEDIGREE factor
HI_Data$PEDIGREE <- factor(HI_Data$PEDIGREE, levels = c("B73", "B104", "Mo17", "Tzi8", "Tzi9", "CML10", "CML277", "CML258"))

# Also reorder HI_Summary if you're using it
HI_Summary$PEDIGREE <- factor(HI_Summary$PEDIGREE, levels = c("B73", "B104", "Mo17", "Tzi8", "Tzi9", "CML10", "CML277", "CML258"))



## ---- Boxplot1 ----
library(ggplot2)

ggplot(HI_Data, aes(x = PEDIGREE, y = `Days to Shedding`)) +
  geom_boxplot() +
  labs(title = "Days to Shedding by Variety", x = "Variety", y = "Days After Planting") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ---- Boxplot2 -------
ggplot(HI_Data, aes(x = PEDIGREE, y = `Days between Shedding and Silking`)) +
  geom_boxplot() +
  labs(title = "Days Between Shedding and Silking by Variety", x = "Variety", y = "Days Difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ---- Scatterplot1 ------
library(ggplot2)

ggplot(HI_Data, aes(x = `Shed GDD`, y = `Silk GDD`, color = PEDIGREE)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Relationship between Shed GDD and Silk GDD", x = "Shed GDD", y = "Silk GDD") +
  scale_color_brewer(palette = "Dark2") +  
  theme_minimal()

## ---- Barplot1 -----
library(dplyr)

count_data <- HI_Data %>%
  group_by(PEDIGREE, `Days between Shedding and Silking`) %>%
  summarise(count = n(), .groups = "drop")

ggplot(count_data, aes(x = `Days between Shedding and Silking`, y = count, fill = PEDIGREE, label = PEDIGREE)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = PEDIGREE), 
            position = position_dodge(width = 0.9), 
            angle = 90,        # <-- rotate vertical
            vjust = 0.5, 
            hjust = -0.2, 
            size = 3) +
  labs(title = "Days between Shedding and Silking (by Variety)", 
       x = "Days Difference", 
       y = "Count") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(from = min(count_data$`Days between Shedding and Silking`),
                                  to = max(count_data$`Days between Shedding and Silking`),
                                  by = 1)) +
  theme_minimal()


## ---- Anova1 ----
anova_days_diff <- aov(`Days between Shedding and Silking` ~ PEDIGREE, data = HI_Data)
summary(anova_days_diff)

## ---- Barplot2 ------
ggplot(HI_Summary, aes(x = PEDIGREE, y = `Avg days to shedding`)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Average Days to Shedding by Variety", x = "Variety", y = "Avg Days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ---- Barplot3 ----
ggplot(HI_Summary, aes(x = PEDIGREE, y = `Avg days between shedding and silking`)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Average Days between Shedding and Silking by Variety", x = "Variety", y = "Avg Days Difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ---- Correlation1 -------
install.packages("ggrepel")
library(ggrepel)

ggplot(HI_Summary, aes(x = `Avg days to shedding`, y = `Avg days to silking`, label = PEDIGREE)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +    # <-- smart labeling
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation between Avg Days to Shedding and Avg Days to Silking", 
       x = "Avg Days to Shedding", 
       y = "Avg Days to Silking") +
  theme_minimal()

## ---- Correlation2 -----
cor.test(HI_Summary$`Avg days to shedding`, HI_Summary$`Avg days to silking`)


## ---- Groupsummary ------
HI_Data$Group <- ifelse(HI_Data$PEDIGREE %in% c("B73", "B104", "Mo17"), "Temperate", "Tropical")

HI_Data %>%
  group_by(Group) %>%
  summarise(
    mean_shedding = mean(`Days to Shedding`, na.rm = TRUE),
    mean_silking = mean(`Days to Silking`, na.rm = TRUE),
    mean_diff = mean(`Days between Shedding and Silking`, na.rm = TRUE),
    .groups = "drop"
  )


## ---- Tempvstrop ------
ggplot(HI_Data, aes(x = Group, y = `Days to Shedding`, fill = Group)) +
  geom_boxplot() +
  labs(title = "Days to Shedding: Temperate vs. Tropical", y = "Days", x = "Group") +
  theme_minimal()


## ---- Ttest -----
t.test(`Days to Shedding` ~ Group, data = HI_Data)
t.test(`Days between Shedding and Silking` ~ Group, data = HI_Data)













    