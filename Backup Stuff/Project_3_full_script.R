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


# getwd()
# setwd("C:/Users/jerry/Documents/Git")
# getwd()
# setwd("C:/Users/jerry/Documents/Git/Project-1-2-3")
# setwd("C:/Users/jerry/Documents/Git/Project-1-2-3/Project-3")
# getwd()

# Lets load the data first

## ---- Loaddata ----

# Lets set up the data location, then read and save the data under a preferred name

# Setup data location

install.packages("here")
library(here)
data_location <- here("HI_SU2024_SHD_SLK.csv")

# Read the data and save it as HI_Data

HI_Data <- read.csv("HI_SU2024_SHD_SLK.csv")


## ---- Exploredata ----

# Now lets explore the data a little bit

# Load dplyr and skmir first

library(dplyr)
library(skimr)

# Explore the Data

dplyr::glimpse(HI_Data)
summary(HI_Data)
skimr::skim(HI_Data)

# What we have:

# Pedigree: Genotype / variety
# SHD: Shedding date (Pollen starts)
# SK: Silking Date (Female flowers have silks and are ready for reproduction)
# Days_SHD: Days after planting to shedding
# Days_SK: Dats after planting to silking
# GDD_SHD: Growing degree days to shedding
# GDD_SK: Growing degree days to silking

# Okay, we have some good information, but we should add two columns that calculate 
# 1. the difference in Days after planting (DAP) between silking and shedding (DAP_SK - DAP_SHD)
# 2. the difference in Growing degree days (GDD) between silking and shedding (GDD_SK - GDD_SHD)

# This will be good to add because the time between shedding and silking (especially thermal time)
# is extremely important in corn development and flowering time. 

# Now lets add the columns

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

# Nice! Now we have our new columns added!

# Now lets do some Summary Statistics to see what's going on with each variety.

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

# Looks better. 

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

# Lets add some code to make the tables look really nice when we view it in the website html.
library(gt)

HI_Data %>%
  gt()

HI_Summary %>%
  gt()

# Now let's view the data with some added lines
# to separate the columns and titles and make it easier to read
library(knitr)
kable(HI_Data)
kable(HI_Summary)

# Okay so what are we looking at here? We basically have averages of the values for each pedigree from the original data.
# We want to look at how early they are flowering, but also synchronization between shedding and silking
# because if they are not synchronized, thus can create problems in the field. 


# We can see that Mo17 has the earliest shed and silk date, while CML258 has the latest shed and silk date. 
# It's important to note that B73 is a positive control and has perfectly synchronized flowering time. 
# Some varietes have negative values for Avg days between shedding and silking meaning that 
# the silks were ready before the pollen was shedding. This is bad because ideally the 
# pollen shedding and silk emergence should overlap, with pollen emergence slightly before or at the same time as the silks.
# If the silks come out too early, they are more sensitive and could get old and dry out quickly before pollen is available
# meaning bad fertilization and kernel set. 

# Now that we have some idea of what we are looking at and it cleaner, lets make some graphs


# Now time to make some graphs and do some statistical analysis!

# Lets make some graphs for the Original Data

## now lets make some graphs based on the original data to look at some relationships


## ---- Boxplot1 ----

# Box Plot 1
## This boxplot will plot Days to shedding by variety.
## It will show which varieites shed pollen earlier or later and variation within each.

library(ggplot2)

ggplot(HI_Data, aes(x = PEDIGREE, y = `Days to Shedding`)) +
  geom_boxplot() +
  labs(title = "Days to Shedding by Variety", x = "Variety", y = "Days After Planting") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## We can see that CML277, CML 258, and Tzi9 shed pretty late
## while B104, B73, and Mo17 shed pretty early. 


## ---- Boxplot2 -------

## this boxplot will plot Days between shedding and silking.
## it will show how synchornized flowering is within varieties.

ggplot(HI_Data, aes(x = PEDIGREE, y = `Days between Shedding and Silking`)) +
  geom_boxplot() +
  labs(title = "Days Between Shedding and Silking by Variety", x = "Variety", y = "Days Difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## We can see that the most synchronized (besides the B73 control) are
## CML10, CML258, and Mo17.


## ---- Scatterplot1 ------

## This scatterplot will plot Shed GDD vs Silk GDD

## Plants on the 1:1 line are perfectly synchronized; points below = silking earlier
## and points above = silking later

library(ggplot2)

ggplot(HI_Data, aes(x = `Shed GDD`, y = `Silk GDD`, color = PEDIGREE)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Relationship between Shed GDD and Silk GDD", x = "Shed GDD", y = "Silk GDD") +
  scale_color_brewer(palette = "Dark2") +  
  theme_minimal()


## ---- Barplot1 -----

## This barplot will plot days between shedding and silking.
## It will show overall how much delay or mismatch exsts across all plants.

library(dplyr)

count_data <- HI_Data %>%
  group_by(PEDIGREE, `Days between Shedding and Silking`) %>%
  summarise(count = n(), .groups = "drop")

ggplot(count_data, aes(x = `Days between Shedding and Silking`, y = count, fill = PEDIGREE, label = PEDIGREE)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = PEDIGREE), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Days between Shedding and Silking (by Variety)", x = "Days Difference", y = "Count") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(from = min(count_data$`Days between Shedding and Silking`),
                                  to = max(count_data$`Days between Shedding and Silking`),
                                  by = 1)) +   # << this is the fix!
  theme_minimal()

## We can see that Tzi9 did pretyy bad and the silks came out way before (4-5 days) before the pollen shed. This is bad for successfull ferlitization
## We can also see that B104 had a pretty large difference between shed and silk date being 3 days apart

## ---- Anova1 ----

## Now lets do an ANOVA for Days between shedding and silking between varieties

# ANOVA for Days between Shedding and Silking
anova_days_diff <- aov(`Days between Shedding and Silking` ~ PEDIGREE, data = HI_Data)
summary(anova_days_diff)

# We can see the P value is 0.00000000112 meaning that there is a highly significant difference among varieties (Pedigree)
# in their days between shedding and silking. 

# Some varieties are much more synchronized than others and it's a real biological difference. 


# Now lets do some graphs for the Summarized Data


## ---- Barplot2 ------

## This barplot will plot the average days to shedding by variety

ggplot(HI_Summary, aes(x = PEDIGREE, y = `Avg days to shedding`)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Average Days to Shedding by Variety", x = "Variety", y = "Avg Days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We can see that Tzi9 and CML258 had the latest shedding, while Mo17 had the earliest shedding

## ---- Barplot3 ----

## This barplot will plot the average days between shedding and silking
## We'll be able to easily spot which varieties have better synchrony (closer to 0)

ggplot(HI_Summary, aes(x = PEDIGREE, y = `Avg days between shedding and silking`)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Average Days between Shedding and Silking by Variety", x = "Variety", y = "Avg Days Difference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We can see that CML10 and CML258 have really good synchrony, while TZi9 and B104 have really bad synchrony

## ---- Correlation1 -------

## This will show the average days ti shedding vs the average days to silking
## This tests of varities that shed earlier also silk earlier - correlation acorss averages.

## If the point is right on the line then shedding and silking happens at the same time
## Points above the line mean silking happens after shedding.
## Points below the line mean that silking happens earlier then shedding.

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

# We can see that Mo17 is the most synchornized, while CML258 is the least synchronized.

## ---- Correlation2 -----

# Correlation between Avg Days to Shedding and Avg Days to Silking

cor.test(HI_Summary$`Avg days to shedding`, HI_Summary$`Avg days to silking`)

# We can see the correlation value was r = 0.898
# This means that when the average days to shedding increases, the average days to silking also increases.
# This shows there is a strong posisitve correlation between shedding and silk and varieties that shed later also tend to silk later. 

#### CONCLUSION #######


# Thanks for listening!

  