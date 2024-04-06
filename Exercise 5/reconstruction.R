##### Question 5 ####

## Install and load packages ##
if (!require(pacman)) install.packages("pacman")
pacman::p_load( effectsize, tidyverse, dplyr, ggplot2, rstatix)

# Import Data and Preprocess

## Outliers Removal Process and Logic ##

# Step 1: Standardization of Variables
# - The variables of interest, Symbols, L_Words, S_Words, and MatchAcc, are standardized using the 
# scale() function.
# - Standardization transforms the variables to have a mean of 0 and a standard deviation of 1.

### from the article: "statistical software was used to exclude participants whose performance was 
## ±3 SDs from the mean for recall of symbols, recall of words, or accuracy on the symbol matching task".

# Step 2: Outlier Identification
# - Create an Outlier_Filter column in the Raw_Data dataframe, initialized with a value of 1 for all rows.
# - Check the absolute values of each standardized variable against a threshold of 3.

# - If any value exceeds the threshold, set the corresponding Outlier_Filter value to 0 for that row.
# - Otherwise, the Outlier_Filter value remains 1.

# Step 3: Counting Outliers
# - Use the sum() function to count the number of outliers for each variable.
# - For each standardized variable, calculate the number of values exceeding 2.99999 and less than -2.99999 using logical comparisons (> and <).

# Step 4: Filtering Outliers
# - Apply the filter() function from the dplyr package to the Raw_Data dataframe.
# - Retain only the rows where the Outlier_Filter value is equal to 1.
# - This creates a new dataframe called Final_Data without the identified outliers.

# Step 5: Proportional Transformation
# - Transform the variables Symbols, L_Words, and S_Words into proportional values by dividing them by 25.
# - Store the resulting proportions in the new columns Symbols_Prop, L_Words_Prop, and S_Words_Prop in Final_Data.

# The outliers removal process helps ensure the validity and reliability of subsequent statistical analyses
# by eliminating extreme values that could disproportionately affect the results.
# The process involves standardizing variables, identifying outliers based on a threshold,
# counting outliers, filtering the dataset to exclude outliers,
# and transforming certain variables into proportional values for further analysis.

Cleaned_Data <- read.csv('Symbols_Exp1_Data.csv', header = TRUE) %>%
  mutate(
    Rand_ID = as.factor(Rand_ID),
    Sex = as.factor(Sex),
    Symbols = round(Symbols, digits = 2),
    L_Words = round(L_Words, digits = 2),
    S_Words = round(S_Words, digits = 2),
    MatchAcc = round(MatchAcc, digits = 2),
    z_Symbols_Recall = scale(Symbols),
    z_L_Words_Recall = scale(L_Words),
    z_S_Words_Recall = scale(S_Words),
    z_MatchAcc = scale(MatchAcc),
    Outlier_Filter = ifelse(
      abs(z_Symbols_Recall) > 3 |
        abs(z_L_Words_Recall) > 3 |
        abs(z_S_Words_Recall) > 3 |
        abs(z_MatchAcc) > 3, 0, 1
    ),
    Outlier_Count = rowSums(abs(select(., starts_with("z_"))) > 2.99999 | abs(select(., starts_with("z_"))) < -2.99999),
    Symbols_Prop = as.double(Symbols / 25),
    L_Words_Prop = as.double(L_Words / 25),
    S_Words_Prop = as.double(S_Words / 25)
  ) %>%
  filter(Outlier_Filter == 1)

# Reshape the data from wide to long format
Cleaned_Data_Long <- Cleaned_Data %>%
  gather(key = "Condition", value = "Proportional_Recall", Symbols_Prop, L_Words_Prop) %>%
  convert_as_factor(Rand_ID, Condition)

## Statistical test ##
t.test(Cleaned_Data$Symbols_Prop, Cleaned_Data$L_Words_Prop, paired = TRUE)

effectsize::cohens_d(x = Proportional_Recall ~ Condition, data = Cleaned_Data_Long, pooled_sd = FALSE, paired = TRUE) # didn't assumed equal variance for the two groups,
# so we should calculate the standard deviation separately for each group.

##  plotting the relationship between the independent and dependent variables ##

# Reshape the data from wide to long format
Cleaned_Data_Long_Plot <- Cleaned_Data %>%
  gather(key = "Condition", value = "Proportional_Recall", Symbols_Prop, L_Words_Prop)

# Calculate the mean and standard error (SE) for each condition
Cleaned_Data_Summary <- Cleaned_Data_Long %>%
  group_by(Condition) %>%
  summarise(
    Mean_Prop_Recall = mean(Proportional_Recall),
    SE = sd(Proportional_Recall) / sqrt(n())
  )

# Define color palette
my_colors <- c("#0072B2", "#E69F00")  # Blue and orange colors

# Create the violin plot with error bars
ggplot(Cleaned_Data_Long, aes(x = Condition, y = Proportional_Recall, fill = Condition)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = "black") +
  geom_errorbar(
    data = Cleaned_Data_Summary,
    aes(x = Condition, y = Mean_Prop_Recall, ymin = Mean_Prop_Recall - SE, ymax = Mean_Prop_Recall + SE),
    width = 0.2,
    color = "black",
    linewidth = 1.5
  ) +
  geom_point(
    data = Cleaned_Data_Summary,
    aes(x = Condition, y = Mean_Prop_Recall),
    color = "black",
    size = 4,
    shape = 21,
    fill = "white"
  ) +
  labs(
    x = "Condition",
    y = "Proportion Recalled",
    title = "Impact of Presentation Type on Recall: Words vs. Symbols"
  ) +
  scale_fill_manual(values = my_colors) +
  scale_x_discrete(labels = c("Words", "Symbols")) +  # Update x-axis labels
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", size = 1.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 12, face = "bold")
  )

