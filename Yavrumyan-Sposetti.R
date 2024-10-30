# Load required packages
library(caret)
library(tidyverse)
library(scales)
library(haven)
library(janitor)
library(kableExtra)
library(broom)
library(knitr)
library(DMwR2)
library(openxlsx)
library(forcats)
library(RColorBrewer)

##########################################
##### DATASET CLEANING AND ADJUSTING #####
##########################################


# Read the dataset, it is originally in .dta format for stata
data <- read_dta("SCIF_original.dta")

# To get an idea of the variables, view all missing values in order (percentage - most missing to least)
NA_perc <- data %>% summarise(across(everything(), ~ mean(is.na(.))))  %>%  pivot_longer(cols = everything(), names_to = "variable", 
                 values_to = "missing_pct") %>%
  arrange(desc(missing_pct))

View(NA_perc)
kable(NA_perc) 

# Since the dataset is large, keep variables with less than 60% missing values only
vars_to_keep <- NA_perc %>% 
  filter(missing_pct <= 0.60) %>%
  pull(variable)

# Subset the original data to keep only the selected variables, more than 60% not missing
cleaned_data <- data[, vars_to_keep]

# Clarify which variables are categorical, since R reads all as numerical from .dta
cleaned_data <- cleaned_data %>%
  mutate(across(where(is.labelled), as_factor))

# View and Deal with our variable of interest which is employment status and recode it to 2 categories
# Remove missing rows for dependent variable
cleaned_data <- cleaned_data %>%
  filter(!is.na(COND5))

summary(cleaned_data$COND5)

# Recoding using grepl to check if COND5 starts with "Occupato"
cleaned_data <- cleaned_data %>%
  mutate(COND5_recode = case_when(
    as.numeric(COND5) %in% 1:3 ~ 1,      # Assuming '1', '2', '3' are the categories for working
    as.numeric(COND5) %in% 4:5 ~ 0,      # Assuming '4', '5' are the categories for not working
    TRUE ~ 2                             # Catch-all for any unexpected or unhandled values
  ))

# Finally view the summary of the new variable
table(cleaned_data$COND5_recode)

# We will perform imputation for the missing values to be able to fit models easier
# For categorical it will be mode imputation and for numerical - mean imputation
#mode function for imputation
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

cleaned_data <- cleaned_data %>%
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))), # Replace NAs in numeric columns with mean
         across(where(is.factor), ~replace_na(.,get_mode(.))))            # Replace NAs in factor columns with mode

# Save the cleaned dataset as a new Stata file
write_dta(cleaned_data, "SCIF_cleaned.dta")

# List of variables of interest to our research question, related to language integration and discrimination 
# Which are similar (will portray intracorrelation ) transformed to numerical and combined through mean for categorization into a new vatiable
cleaned_data <- cleaned_data %>%
  mutate(across(c(INT_LI2NEW, INT_LI3NEW, INT_LI4NEW, INT_LI5NEW, INT_LI6NEW, INT_LI7, INT_LI8, INT_LI9),
                ~factor(.,
                        levels = c("Molto", "Abbastanza", "Poco", "Per niente"),
                        labels = c(1, 2, 3, 4))))

cleaned_data <- cleaned_data %>%
  mutate(across(c(INT_LI2NEW, INT_LI3NEW, INT_LI4NEW, INT_LI5NEW, INT_LI6NEW, INT_LI7, INT_LI8, INT_LI9), as.numeric))

cleaned_data <- cleaned_data %>%
  rowwise() %>%
  mutate(Avg_Score = round(mean(c_across(c(INT_LI2NEW, INT_LI3NEW, INT_LI4NEW, INT_LI5NEW, INT_LI6NEW, INT_LI7, INT_LI8, INT_LI9)), na.rm = TRUE), 0)) %>%
  ungroup()

cleaned_data <- cleaned_data %>%
  mutate(Summ_Lang_Skill = case_when(
    Avg_Score == "1" ~ "Well",
    Avg_Score == "2" ~ "Quite a bit",
    Avg_Score == "3" ~ "Not so much",
    Avg_Score == "4" ~ "Not at all" # Handles any NAs or unexpected values
  ))

cleaned_data$Summ_Lang_Skill <- factor(cleaned_data$Summ_Lang_Skill,
                                       levels = c("Well", "Quite a bit", "Not so much", "Not at all"))


##########################################
#####     DESCRIPTIVE PLOTS          #####
##########################################



# PLOT 1 #
cleaned_data2 <- cleaned_data %>%
  mutate(
    INT_LI4NEW = factor(INT_LI4NEW, levels = c(1, 2, 3, 4),
                        labels = c("Quite well", "Moderately", "Poorly", "Not at all")),
    FORM_LM1NEW = case_when(
      FORM_LM1NEW == "Albanese" ~ "Albanian",
      FORM_LM1NEW == "Serbo; Croato; Bosniaco; Montenegrino" ~ "Serbo-Crotian",
      FORM_LM1NEW == "Rumeno" ~ "Romanian",
      FORM_LM1NEW == "Ucraino" ~ "Ukrainian",
      FORM_LM1NEW == "Polacco" ~ "Polish",
      FORM_LM1NEW == "Francese" ~ "French",
      FORM_LM1NEW == "Russo" ~ "Russian",
      FORM_LM1NEW == "Arabo (16 idiomi)" ~ "Arabic",
      FORM_LM1NEW == "Spagnolo" ~ "Spanish",
      FORM_LM1NEW == "Cinese (12 idiomi)" ~ "Chinese",
      TRUE ~ FORM_LM1NEW  # Default case to keep original if no match
    )
  )

cleaned_data2$INT_LI2NEWNEW <- factor(cleaned_data2$INT_LI2NEW, levels = c(1, 2, 3, 4),
                                      labels = c("A lot", "Quite", "Little", "Not at all"))
cleaned_data2$INT_LI3NEWNEW <- factor(cleaned_data2$INT_LI3NEW, levels = c(1, 2, 3, 4),
                                      labels = c("A lot", "Quite", "Little", "Not at all"))
combined_data <- rbind(
  data.frame(Skill = "Reading", Level = cleaned_data2$INT_LI2NEWNEW),
  data.frame(Skill = "Writing", Level = cleaned_data2$INT_LI3NEWNEW)
)

combined_data <- combined_data %>%
  group_by(Skill, Level) %>%
  summarize(Count = n()) %>%
  group_by(Skill) %>%
  mutate(Prop = Count/sum(Count))

ggplot(combined_data, aes(x = Level, y=Prop, fill = Skill, label=label_percent(0.1)(Prop))) +
  geom_col(position = "stack", color = "black", na.rm = TRUE) + 
  geom_text(position = position_stack(vjust = 0.5), 
            color = "white", size = 3) +  # Add text labels
  labs(title = "READING AND WRITING KNOWLEDGE OF THE ITALIAN LANGUAGE",
       x = "Level", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme(plot.title = element_text(hjust = 0.5))


# PLOT 2 #


# Convert INT_LI4NEW and translate FORM_LM1NEW into English

# Identify the top 10 mother tongues by total occurrences
top_mother_tongues <- cleaned_data2 %>%
  group_by(FORM_LM1NEW) %>%
  summarise(total = n(), .groups = 'drop') %>%
  top_n(10, total) %>%
  pull(FORM_LM1NEW)

# Filter data for only the top 10 mother tongues
filtered_data_graph <- cleaned_data2 %>%
  filter(FORM_LM1NEW %in% top_mother_tongues)

# Count the occurrences and calculate proportions
data_to_plot <- filtered_data_graph %>%
  group_by(FORM_LM1NEW, INT_LI4NEW) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),  # Calculate total responses per mother tongue
         proportion = count / total)  # Calculate proportion

# Plotting
ggplot(data_to_plot, aes(x = FORM_LM1NEW, y = proportion, fill = INT_LI4NEW)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  # Flip the coordinates to make bars horizontal
  labs(x = "Mother Tongue", y = "Proportion", fill = "Understanding Level",
       title = "Level of Spoken Italian (How well can an Italian understand you)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "top")



##########################################
##### Prepping data for stat analysis#####
##########################################


vars_of_interest <- c("LAV_ATT27","TIT_STUD", "LAV_ATT15", "LAV_ATT15BIS", "LAV_ATT22", "LAV_ATT26", "LAV_ATT28", 
                      "LAV_ATT40", "LAV_ATT40BIS", "ANNI_PRIMO_LAV", "COND5_recode",
                      "LAV_ITA2", "LAV_ITA4", "LAV_ITA5","DISCR_44", "DISCR_47", "LAV_ATT1", "LAV_ITA1", 
                      "LAV_PER1", "DISCR_44", "INT_LI1", "FORM_LM1NEW", "FORM_LM1BIS", "FORM_LM2", "INT_LI9", "INT_LI10", 
                      "FORM_LM1NEW", "FORM48",  "SG11", "SG21", "FORM1", "FORM3", "SG4", "SG13", "SG16", "INT_LI2NEW",
                      "INT_LI3NEW","INT_LI4NEW", "INT_LI5NEW", "Summ_Lang_Skill")

# Define which of these are numeric originally
numerical_vars <- c("LAV_ATT26", "LAV_ITA2", "SG21", "SG4")

# Define new dataset with vars of interest
data_interest <- cleaned_data %>%
  select(all_of(vars_of_interest))


# Transform all numerical variables to categorical for ease of model fitting 
# Starting with LAV_ATT26
data_interest <- data_interest %>%
  mutate(LAV_ATT26 = as.numeric(na_if(as.character(LAV_ATT26), "Non sa"))) # Convert to numeric, "Non sa" to NA

data_interest <- data_interest %>%
  mutate(LAV_ATT26_Cat = cut(LAV_ATT26,
                             breaks = c(1943, 1989, 2000, 2011),
                             labels = c("1944-1989", "1990-2000", "2001-2011"),
                             right = TRUE,
                             include.lowest = TRUE))

# View the changes
table(data_interest$LAV_ATT26_Cat)

# Do the same for LAV_ITA2
data_interest <- data_interest %>%
  mutate(LAV_ITA2 = round(LAV_ITA2))

data_interest <- data_interest %>%
  mutate(LAV_ITA2_Categories = cut(LAV_ITA2,
                                   breaks = c(1937, 1980, 1990, 2001, 2012),
                                   labels = c("1937-1980", "1981-1990", "1991-2001", "2002-2012"),
                                   include.lowest = TRUE, right = FALSE))
# View the changes
table(data_interest$LAV_ITA2_Categories)


# Do the same for SG21
data_interest <- data_interest %>%
  mutate(
    SG21_Category = cut(SG21,
                        breaks = c(15,30, 64, Inf),  # Using -Inf and Inf to cover all possible ages
                        labels = c("15-30: Young Workers", "31-64: Older Workers", "65+: Elderly"),
                        right = FALSE)  # Ensures that 15 is included in the second interval
  )

# View the changes
table(data_interest$SG21_Category)

# Do the same for SG4
data_interest <- data_interest %>%
  mutate(
    SG4_Category = cut(SG4,
                       breaks = c(-Inf, 3, 6, Inf),  # Defining the ranges
                       labels = c("Small (up to 3 members)", "Medium (4-6 members)", "Large (7 and above)"),
                       right = TRUE)  # Ensures that 3 is included in the 'Small' category, 6 in 'Medium'
  )

# View the changes
table(data_interest$SG4_Category)
# clump the rest of the variables which have too many categories
data_interest <- data_interest %>%
  mutate(ANNI_PRIMO_LAV = as.numeric(ANNI_PRIMO_LAV),
         LAV_ATT27 = as.numeric(LAV_ATT27),
         LAV_ATT40 = as.numeric(LAV_ATT40),
         LAV_ATT28 = fct_lump_n(LAV_ATT28, n = 3),
         LAV_PER1 = fct_lump_n(LAV_PER1, n = 5),
         FORM_LM1NEW = fct_lump_min(FORM_LM1NEW, min = 174, other_level = "Other Mother Tongues")
  )

# Perform mutation again for redundant categories like - "Dont know", "Not Avaialble"
data_interest <- data_interest %>%
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))), # Replace NAs in numeric columns with mean
         across(where(is.factor), ~replace_na(.,get_mode(.))))            # Replace NAs in factor columns with mode

vars_of_interest_cat <- c("LAV_ATT27", "LAV_ATT15", "LAV_ATT15BIS", "LAV_ATT22", "LAV_ATT26_Cat", "LAV_ATT28", 
                          "LAV_ATT40", "LAV_ATT40BIS", "ANNI_PRIMO_LAV", "COND5_recode",
                          "LAV_ITA2_Categories", "LAV_ITA4", "LAV_ITA5","DISCR_44", "DISCR_47", "LAV_ATT1", "LAV_ITA1", 
                          "LAV_PER1", "DISCR_44", "INT_LI1", "FORM_LM1NEW", "FORM_LM1BIS", "FORM_LM2", "INT_LI10", 
                          "FORM48",  "SG11", "SG21_Category", "FORM1", "FORM3", "SG4_Category", "SG13", "SG16",
                          "Summ_Lang_Skill", "TIT_STUD")

# From the interesting variables remove all the ones related to work (LAV), since work categories will be
# Highly correlated and won't make for good covariates
data_interest_cat <- data_interest %>%
  select(all_of(vars_of_interest_cat)) %>%
  select(-all_of(starts_with("LAV")))


##########################################
#####     MODELS FITTED.             #####
##########################################
# Fit a glm and a LASSO model (penalizes corellation and unimportant variables)

# GLM
mod_glm <- train(as.factor(COND5_recode) ~ .,
                 data = data_interest_cat,
                 method = "glm", family = "binomial")
mod_glm
view(varImp(mod_glm, scale = T))
summary(mod_glm)

renaming_dict_glm <- list(
  "SG11Femmina" = "Gender: Female",
  "SG21_Category31-64: Older Workers" = "Workers Aged 31-64",
  "SG4_CategoryMedium (4-6 members)" = "Household Size: 4-6 Members (medium)",
  "DISCR_44In Italia non ho mai cercato lavoro" = "Never Sought Work in Italy",
  "Summ_Lang_SkillNot so much" = "Basic Italian Language Skills",
  "Summ_Lang_SkillNot at all" = "Italian Language Skills: None",
  "SG21_Category65+: Elderly" = "Age: Elderly (65+ years)",
  "DISCR_47No" = "No Discrimination Experienced",
  "Summ_Lang_SkillQuite a bit" = "Moderate Italian Language Skills",
  "FORM_LM1NEWCinese (12 idiomi)" = "Mother Tongue: Chinese",
  "DISCR_44No" = "Was not Discriminated Against",
  "FORM_LM1NEWFilippino" = "Mother Tongue: Filipino",
  "ANNI_PRIMO_LAV" = "Years Passed Since the First Job",
  "SG13All estero" = "Born Abroad",
  "SG4_CategoryLarge (7 and above)" = "Household Size: 7+ Members (large)",
  "FORM_LM1NEWSingalese" = "Mother Tongue: Sinhalese (Sri Lanka)",
  "FORM_LM1NEWUcraino" = "Mother Tongue: Ukrainian",
  "INT_LI10Altra lingua" = "Speaks Another Language with Friends",
  "FORM_LM1NEWRumeno" = "Mother Tongue: Romanian",
  "FORM_LM1NEWBengalese" = "Mother Tongue: Bengali",
  "FORM_LM1NEWOther Mother Tongues" = "Mother Tongue: Other Languages",
  "FORM_LM1NEWHindi" = "Mother Tongue: Hindi",
  "FORM_LM1NEWBulgaro" = "Mother Tongue: Bulgarian",
  "FORM_LM1NEWRusso" = "Mother Tongue: Russian",
  "TIT_STUDIstruzione primaria (Licenza elementare)" = "Primary Education: Completed",
  "TIT_STUDIstruzione secondaria inferiore (Licenza media)" = "Lower Secondary Education: Middle School Diploma",
  "FORM_LM1NEWPolacco" = "Mother Tongue: Polish",
  "FORM_LM1NEWSpagnolo" = "Mother Tongue: Spanish",
  "FORM1No, ma sa leggere e scrivere" = "Literate",
  "FORM_LM1NEWInglese" = "Mother Tongue: English")
#,"FORM_LM1BISNo" = "Low Native Language Proficiency")

imp_glm <- varImp(mod_glm, scale = TRUE)
final_glm <- mod_glm$finalModel


# Linear Models: the absolute value of the t-statistic for each model parameter is used for var importance

# Define the dataframe of importance, and again cut at top 30
imp_glm_df <- as.data.frame(imp_glm$importance) %>%
  rownames_to_column(var = "Variable") %>%
  arrange(desc(Overall)) %>%
  slice(1:30) %>%
  mutate(Variable = factor(Variable, levels = Variable))

# Rename variables using the dictionary
imp_glm_df$ReadVar <- sapply(imp_glm_df$Variable, function(x) renaming_dict_glm[[x]])  
kable(imp_glm_df, caption = "Top 30 Important Variables from GLM Model")

coefficients_glm <- final_glm$coefficients
keys_to_filter <- names(renaming_dict_glm)

# Now plot using ggplot2, showing the barplot of the variable importances
ggplot(imp_glm_df, aes(x = reorder(ReadVar, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Variable Importance - GLM", x = "Variable", y = "Importance") +
  coord_flip()  # Flips the axes to make labels more readable

# Define the dataframe of importance, and again cut at top 30
imp_glm_df$Coefficient <- coefficients_glm[as.character(imp_glm_df$Variable)]

# Rename variables using the dictionary

# Show the coefficients to understand positive and negative impact
# Now plot using ggplot2, showing the barplot of the variable importances
imp_glm_df %>%
  select(ReadVar, Coefficient, Overall) %>%
  arrange(desc(Coefficient)) %>% 
  select(-Overall) %>%
  kable(caption = "Coefficients of the GLM Model",
        col.names = c("Variable", "Coefficient"),
        format = "html") %>%
  kable_styling(latex_options = c("striped","HOLD_position"),
                stripe_color = "gray!30")



# LASSO
# For interpretation of results and comparsion, display variable importance and coefficients for both
# For glmboost and glmnet: the absolute value of the coefficients corresponding the the tuned model are used.
# Define a renaming dictionary for Lasso for the table output
mod_lasso <- train(as.factor(COND5_recode) ~ .,
                   data = data_interest_cat,
                   method = "glmnet")

mod_lasso
view(varImp(mod_lasso, scale=T))
summary(mod_lasso)
renaming_dict_LASSO <- list(
  "SG21_Category65+: Elderly" = "Age: Elderly (65+ years)",
  "SG13All estero" = "Born Abroad",
  "DISCR_44In Italia non ho mai cercato lavoro" = "Never Sought Work in Italy",
  "FORM_LM1NEWCinese (12 idiomi)" = "Mother Tongue: Chinese",
  "FORM_LM1NEWFilippino" = "Mother Tongue: Filipino",
  "SG11Femmina" = "Gender: Female",
  "Summ_Lang_SkillNot at all" = "No Italian Language Skills",
  "Summ_Lang_SkillNot so much" = "Basic Italian Language Skills",
  "SG21_Category31-64: Older Workers" = "Workering Mid Age: 31-64",
  "SG4_CategoryMedium (4-6 members)" = "Household Size: 4-6 Members (medium)",
  "TIT_STUDDottorato di ricerca" = "Education: Doctorate",
  "DISCR_47No" = "No Discrimination Experienced",
  "SG4_CategoryLarge (7 and above)" = "Household Size: 7+ Members (large)",
  "FORM_LM1NEWSingalese" = "Mother Tongue: Sinhalese(Sri Lanka)",
  "Summ_Lang_SkillQuite a bit" = "Moderate Italian Language Skills",
  "FORM_LM1NEWUcraino" = "Mother Tongue: Ukrainian",
  "FORM_LM1NEWTedesco" = "Mother Tongue: German",
  "FORM_LM1NEWPortoghese" = "Mother Tongue: Portuguese",
  "FORM_LM1NEWArabo (16 idiomi)" = "Mother Tongue: Arabic",
  "TIT_STUDIstruzione primaria (Licenza elementare)" = "Primary Education: Completed",
  "FORM_LM1BISNo" = "Low Native Language Proficiency",
  "FORM_LM1NEWRumeno" = "Mother Tongue: Romanian",
  "TIT_STUDIstruzione professionale (scuole professionali)" = "Upper Secondary Education: Vocational School Diploma",
  "FORM_LM1NEWBengalese" = "Mother Tongue: Bengali",
  "TIT_STUDIstruzione secondaria inferiore (Licenza media)" = "Lower Secondary Education: Middle School Diploma" ,
  "FORM1No, ma sa leggere e scrivere" = "Literate",
  "FORM_LM1NEWHindi" = "Mother Tongue: Hindi",
  "FORM48No" = "No Foreign Language Acquired",
  "INT_LI10Altra lingua" = "Speaks Another Language with Friends",
  "FORM_LM1NEWBulgaro" = "Mother Tongue: Bulgarian")


# Store the variable importance factor  
imp_lasso <- varImp(mod_lasso, scale = TRUE)

# Make a dataframe, cutting the top 30 variables
imp_lasso
imp_df_lasso <- as.data.frame(imp_lasso$importance) %>%
  rownames_to_column(var = "Variable") %>%
  arrange(desc(Overall)) %>%
  slice(1:30) %>%
  mutate(Variable = factor(Variable, levels = Variable))

imp_df_lasso$Variable 

# Rename variables using the dictionary
imp_df_lasso$ReadableVariable <- sapply(imp_df_lasso$Variable, function(x) renaming_dict_LASSO[[x]])  


# Convert the variable importance to a data frame
imp_df_lasso <- imp_df_lasso %>%
  arrange(desc(Overall)) %>%
  mutate(ReadableVariable = factor(ReadableVariable, levels = unique(ReadableVariable)))

# Now plot using ggplot2, to show the most important factors in absolute value
ggplot(imp_df_lasso, aes(x = reorder(ReadableVariable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Variable Importance - LASSO", x = "Variable", y = "Importance") +
  coord_flip()  # Flips the axes to make labels more readable


final_lasso <- mod_lasso$finalModel
optimal_lambda <- mod_lasso$bestTune$lambda
coefficients_lasso <- coef(final_lasso, s = optimal_lambda)

# Convert to a regular dense matrix
coefficients_matrix_lasso <- as.matrix(coefficients_lasso)

# Create a data frame from the matrix
coefficients_df_lasso <- data.frame(
  Variable = rownames(coefficients_matrix_lasso),
  Coefficient = coefficients_matrix_lasso[, 1]
)

# Define the coefficients to filter through
technical_names_lasso <- names(renaming_dict_LASSO)
filtered_lasso_df <- coefficients_df_lasso[rownames(coefficients_df_lasso) %in% technical_names_lasso, , drop = FALSE]
filtered_lasso_df <- filtered_lasso_df[order(-filtered_lasso_df$Coefficient), ]
filtered_lasso_df$ReadableName <- renaming_dict_LASSO[rownames(filtered_lasso_df)]

# Filter the dataframe to only include those names and show it in a table

filtered_lasso_df_norows <- filtered_lasso_df
rownames(filtered_lasso_df_norows) <- NULL
kable(filtered_lasso_df_norows[, c("ReadableName", "Coefficient")], caption = "Coefficients of the Lasso Model",
      col.names = c("Variable", "Coefficient"), format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

kable(filtered_lasso_df_norows[, c("ReadableName", "Coefficient")], caption = "Coefficients of the Lasso Model",
      col.names = c("Variable", "Coefficient"), format = "html") %>%
  kable_styling(latex_options = c("striped","HOLD_position", "scale_down"),
                stripe_color = "gray!30") 

#library(openxlsx)
#write.xlsx(coefficients_df, file = "LassoModelCoefficients.xlsx", sheetName = "Coefficients", overwrite = TRUE)



# Similarly do the same for the GLM model 
# Linear Models: the absolute value of the t-statistic for each model parameter is used for var importance


##########################################
#####     INTERACTION                #####
##########################################
women_data <- cleaned_data %>% 
  filter(SG11 == "Femmina")

women_lang_map <- c(
  "Albanese" = "Albanian",
  "Arabo (16 idiomi)" = "Arabic (16 dialects)",
  "Bulgaro" = "Bulgarian",
  "Cinese (12 idiomi)" = "Chinese (12 dialects)",
  "Francese" = "French",
  "Polacco" = "Polish",
  "Rumeno" = "Romanian",
  "Russo" = "Russian",
  "Spagnolo" = "Spanish",
  "Ucraino" = "Ukrainian",
  "Other" = "Other"
)

women_data2 <- women_data %>%
  filter(SG21 >= 15, SG21 <= 70) %>%
  mutate(age_group = cut(SG21,
                         breaks = seq(15, 75, by = 10),
                         include.lowest = TRUE,
                         labels = paste(seq(15, 65, by = 10),
                                        seq(24, 74, by = 10),
                                        sep = "-"),
                         right = TRUE))

# Step 3: Recode languages to top 10 languages, excluding "Italiano"
women_data2 <- women_data2 %>%
  mutate(FORM_LM1NEWo = ifelse(FORM_LM1NEW == "Italiano", "Other", as.character(FORM_LM1NEW)),
         FORM_LM1NEW_w = fct_lump_n(as.factor(FORM_LM1NEWo), n = 10, other_level = "Other"))

women_data2 <- women_data2 %>%
  mutate(FORM_LM1NEW_w = recode(FORM_LM1NEW_w, !!!women_lang_map))

# Step 4: Convert necessary columns to factors
women_data2 <- women_data2 %>%
  mutate(FORM_LM1NEW_w = as.factor(FORM_LM1NEW_w),
         age_group = as.factor(age_group),
         COND5_recode = as.factor(COND5_recode))


#  Fit logistic regression model with interaction term
women_model <- glm(COND5_recode ~ FORM_LM1NEW_w * age_group, family = binomial(link = "logit"), data = women_data2)

#  Summarize the model
summary(women_model)

#  Predict probabilities
women_data2$predicted_prob <- predict(women_model, type = "response")

# Create the ggplot showing the interaction effects
ggplot(women_data2, aes(x = age_group, y = predicted_prob, color = FORM_LM1NEW_w, group = FORM_LM1NEW_w)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Interaction Effect of Ethnic Language and Age Group on Employment Status",
       x = "Age Group",
       y = "Predicted Probability of Employment",
       color = "Ethnic Language") +
  theme_minimal() +
  theme(legend.position = "right")



