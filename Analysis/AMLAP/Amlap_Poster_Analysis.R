#####################
#####################
# Stefan Pophristic
# August 20th, 2024
# NELLAB
# 2023_Spanish_Verbs-Behavioral_3 + 2
##################### 
# This script is an analysis of the second and third behavioral studies for the project
# This is the analysis presented at AMLAP 2024
#
##################### 

# Load necessary packages

library(tidyverse)
library(lmerTest)
library(boot)
library(reshape2)
library(scales)
library(cowplot) # for extracting just the legend 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########
# Import Data
#########

# Subject Data
df_trials_2 <- read.csv('../../Data/Behavioral_2/2023_Spanish_Verbs-behavioral_2_1-trials.csv', header = TRUE)
df_subjects_2 <- read.csv('../../Data/Behavioral_2/2023_Spanish_Verbs-behavioral_2_1-subject_information.csv', header = TRUE)

df_trials_3 <- read.csv('../../Data/Behavioral_3/2023_Spanish_Verbs-behavioral_3-trials.csv', header = TRUE)
df_subjects_3 <- read.csv('../../Data/Behavioral_3/2023_Spanish_Verbs-behavioral_3-subject_information.csv', header = TRUE)


df_trials_2$exp <- 2
df_trials_3$exp <- 3

df_trials <- rbind(df_trials_2, df_trials_3)
df_subjects <- rbind(df_subjects_2, df_subjects_3)


total_trials <- nrow(df_trials) 
total_trials
#total data points: 74412

totalParticipants <- length(unique(df_trials$workerid))
totalParticipants #total participants: 53

#Import target word Variables
df_variables <- read.csv('../../Materials/stim_variables.csv', header = TRUE)
df_bigrams <- read.csv('../../Materials/bigram_avg_freq.csv', header = TRUE)

#########
# Clean Data
#########

###
# Participant Exclusion
###

# Exclude participants if they had an error rate of >15%
df <- df_trials %>%
  mutate(correctTrial = if_else((response == "f" & trial_type == "distractor") | (response == "j" & trial_type == "target"), 1, 0)) %>%
  group_by(workerid) %>%
  filter(mean(correctTrial == 0) <= 0.151)

participants_post_errorRate = length(unique(df$workerid))

print(totalParticipants - participants_post_errorRate)
# 4 participants excluded 

excluded_participants <- df_trials %>%
  mutate(correctTrial = if_else((response == "f" & trial_type == "distractor") | (response == "j" & trial_type == "target"), 1, 0)) %>%
  group_by(workerid) %>%
  summarise(error_rate = mean(correctTrial == 0)) %>%
  filter(error_rate > 0.151)


# Get unique values of excluded groups and their error rates
unique_excluded_groups <- df_trials %>%
  mutate(correctTrial = if_else((response == "f" & trial_type == "distractor") | (response == "j" & trial_type == "target"), 1, 0)) %>%
  filter(workerid %in% excluded_participants$workerid) %>%
  group_by(workerid) %>%
  summarise(error_rate = mean(correctTrial == 0)) %>%
  distinct()

# Print unique workerids and their error rates
print(unique_excluded_groups)
# participant who was excluded had an error rate of 29.2%


###
# Trial Exclusion
###

trial_exclusion_1 = nrow(df)

# Get rid of trials whose average RT is more than two standard deviations from participant's average
df <- df %>%
  group_by(workerid) %>%
  mutate(mean_rt = mean(rt), sd_rt = sd(rt)) %>%
  filter(!(rt > mean_rt + 2 * sd_rt)) %>%
  ungroup() %>%
  select(-mean_rt, -sd_rt)

trial_exclusion_2 = nrow(df)

print(trial_exclusion_1 - trial_exclusion_2)
# Num trials excluded due to extreme RTs per participant: 2176

# Get rid of incorrect trials 
df <- df %>% 
  filter(correctTrial == 1)


trial_exclusion_3 <- nrow(df)
print(trial_exclusion_2 - trial_exclusion_3)
# Num trials excluded due to incorrect resposne: 3311

# Get rid of trials whose RT is more than two standard deviations from the average
df <- df %>%
  mutate(mean_rt = mean(rt), sd_rt = sd(rt)) %>%
  mutate(rt_exclude = if_else(rt > (mean_rt + 2 * sd_rt) | rt < (mean_rt - 2 * sd_rt), TRUE, FALSE)) %>%
  filter(!rt_exclude) %>%
  select(-mean_rt, -sd_rt, -rt_exclude)

trial_exclusion_4 <- nrow(df)

print(trial_exclusion_3 - trial_exclusion_4)
# Num trials excluded due to extreme RT compared to grand average: 925 trials excluded 

print((trial_exclusion_1 - trial_exclusion_4)/trial_exclusion_1)
# Total data excluded: 9.3%

###
# Final Cleaning
###

df <- df %>%
  select(-error, -proliferate.condition, -correctTrial) %>%
  rename(trial_num = slide_number_in_experiment)

#Counting of trials started at 9 
df$trial_num <- df$trial_num - 8

# Note that the trial numbers will be off by "1" for every break preceding that trial 
#   and will potentially be off by any number of key presses during each break. 
#   if it becomes necessary to encode that, this will have to be done participant by participant

#########
# Participant Information
#########

# Gender
genderData <- df_subjects %>%
  count(gender)

genderData
#     gender    n
# 1   Female    14
# 2   Male      16
# 3   Nonbinary 0


# Age
ggplot(df_subjects, aes(x = age)) +
  geom_histogram(binwidth = 3, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")


#########
# Merge Variable Information
#########

df_target <- df %>%
  filter(trial_type == "target")

df_distractor <- df %>%
  filter(trial_type == "distractor")

df_target <- left_join(df_target, df_variables, by = c("word", "lemma"))

df_bigrams <- df_bigrams %>%
  select(-X) %>%
  rename(word = target)
  
df_target <- left_join(df_target, df_bigrams, by="word")

#########
# Run Regressions
#########

###
# 2 part suffix regression models 
###

df_target_2 <- df_target %>%
  filter(suffix %in% c('aba', 'ábamos', 'aban', 'abas', 
                       'ía', 'ías', 'ían', 'íamos',
                       'aré', 'arás', 'ará', 'aremos', 'arán',
                       'eré', 'erás', 'erá', 'eremos', 'erán',
                       'iré', 'irás', 'irá', 'iremos', 'irán'
                       ))

##
# Basic Model
##

two_basic = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + (1|workerid) + (1|word), data=df_target_2)
summary(two_basic)
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      1.109e+02  8.375e+01  4.779e+01   1.324    0.192    
# trial_num       -8.978e-02  4.796e-03  2.125e+04 -18.722   <2e-16 ***
# avg_bigram_freq  3.205e-07  5.898e-07  4.527e+02   0.543    0.587    
# word_log_freq   -3.822e+01  3.126e+00  4.714e+02 -12.225   <2e-16 ***
# exp              4.884e+02  3.394e+01  4.700e+01  14.392   <2e-16 ***

two_basic_AIC <- AIC(two_basic)
two_basic_AIC
# 304844.2

##
# Suffix Model
##

two_suffix = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + suffix + (1|workerid) + (1|word), data=df_target_2)
summary(two_suffix)

two_suffix_AIC <- AIC(two_suffix)
two_suffix_AIC
# 304598.3


##
# RE Model
##

two_RE = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + RE + (1|workerid) + (1|word), data=df_target_2)
summary(two_RE)
#                     Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      1.095e+02  8.397e+01  4.828e+01   1.304    0.198    
# trial_num       -8.978e-02  4.796e-03  2.125e+04 -18.722   <2e-16 ***
# avg_bigram_freq  3.058e-07  5.938e-07  4.527e+02   0.515    0.607    
# word_log_freq   -3.817e+01  3.137e+00  4.692e+02 -12.168   <2e-16 ***
# exp              4.884e+02  3.394e+01  4.700e+01  14.392   <2e-16 ***
# RE               2.480e+00  1.055e+01  4.587e+02   0.235    0.814    

two_RE_AIC <- AIC(two_RE)
two_RE_AIC
# 304839.6


##
# Single TP Model (Root to whole suffix cluster)
##

two_root_suffixCluster = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + root_wholeSuffix_TP + (1|workerid) + (1|word), data=df_target_2)
summary(two_root_suffixCluster)
# (Intercept)          1.018e+02  8.382e+01  4.791e+01   1.214  0.23061    
# trial_num           -8.980e-02  4.795e-03  2.125e+04 -18.728  < 2e-16 ***
# avg_bigram_freq      1.001e-07  5.906e-07  4.534e+02   0.169  0.86554    
# word_log_freq       -7.087e+01  1.164e+01  4.819e+02  -6.088 2.33e-09 ***
# exp                  4.884e+02  3.394e+01  4.700e+01  14.391  < 2e-16 ***
# root_wholeSuffix_TP  1.836e+02  6.304e+01  4.862e+02   2.912  0.00376 ** 

two_root_suffixCluster_AIC <- AIC(two_root_suffixCluster)
two_root_suffixCluster_AIC
# 304827.7


##
# Full Decomposition Model
##

two_fullDecomp = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + root_firstSuffix_TP + tense_person_TP + (1|workerid) + (1|word), data=df_target_2)
summary(two_fullDecomp)
#                     Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)         -3.330e+01  1.030e+02  1.062e+02  -0.323   0.7470    
# trial_num           -8.983e-02  4.795e-03  2.125e+04 -18.732   <2e-16 ***
# avg_bigram_freq      2.764e-07  6.009e-07  4.513e+02   0.460   0.6458    
# word_log_freq       -4.573e+01  4.182e+00  4.742e+02 -10.935   <2e-16 ***
# exp                  4.884e+02  3.394e+01  4.700e+01  14.392   <2e-16 ***
# root_firstSuffix_TP  5.077e+01  3.642e+01  4.712e+02   1.394   0.1640    
# tense_person_TP      1.492e+02  5.382e+01  4.642e+02   2.772   0.0058 ** 


two_fullDecomp_AIC <- AIC(two_fullDecomp)
two_fullDecomp_AIC
# 304822

##
# Root to last suffix Model (not included in AMLAP)
##
two_rootLast = lmer(rt ~ trial_num +  avg_bigram_freq + word_log_freq + exp + root_lastSuffix_TP + (1|workerid) + (1|word), data=df_target_2)
summary(two_rootLast)
#                     Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)         1.023e+03  3.154e+01  1.875e+02  32.431  < 2e-16 ***
# trial_num          -8.978e-02  4.795e-03  2.125e+04 -18.723  < 2e-16 ***
# avg_bigram_freq       1.336e+02  4.847e+01  4.610e+02   2.756  0.00608 ** 
# word_log_freq      -3.869e+01  3.744e+00  4.688e+02 -10.334  < 2e-16 ***
# exp3                4.884e+02  3.393e+01  4.700e+01  14.393  < 2e-16 ***
# root_lastSuffix_TP  3.446e+00  2.547e+01  4.739e+02   0.135  0.89244        

two_rootLast_AIC <- AIC(two_rootLast)
two_rootLast_AIC
# 304792.6

##
# Full Decomp + root to last (not included in AMLAP)
##

two_fullDecomp_rootLast = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + root_firstSuffix_TP + tense_person_TP + root_lastSuffix_TP + (1|workerid) + (1|word), data=df_target_2)
summary(two_fullDecomp_rootLast)
#                     Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)          8.478e+02  7.120e+01  5.102e+02  11.908  < 2e-16 ***
# trial_num           -8.985e-02  4.795e-03  2.125e+04 -18.738  < 2e-16 ***
# avg_bigram_freq        1.304e+02  4.822e+01  4.598e+02   2.705  0.00709 ** 
# word_log_freq       -4.784e+01  4.927e+00  4.689e+02  -9.710  < 2e-16 ***
# exp3                 4.884e+02  3.393e+01  4.700e+01  14.393  < 2e-16 ***
# root_firstSuffix_TP  7.753e+01  3.775e+01  4.697e+02   2.054  0.04053 *  
# tense_person_TP      1.573e+02  5.354e+01  4.611e+02   2.938  0.00347 ** 
# root_lastSuffix_TP   1.476e+01  2.695e+01  4.758e+02   0.548  0.58409    

two_fullDecomp_rootLast_AIC <- AIC(two_fullDecomp_rootLast)
two_fullDecomp_rootLast_AIC
# 304769.7





###
# 2 part suffix model comparison 
###

# Create dataframe with AIC values
two_comparisons <- data.frame(
  model_name = c("two_basic", "two_RE", "two_root_suffixCluster", "two_fullDecomp", 
                 "two_rootLast", "two_fullDecomp_rootLast"),
  AIC_value = c(two_basic_AIC, two_RE_AIC, two_root_suffixCluster_AIC, two_fullDecomp_AIC, 
                two_rootLast_AIC, two_fullDecomp_rootLast_AIC)
)

# Calculate differences in AIC values
two_AIC_diff <- outer(two_comparisons$AIC_value, two_comparisons$AIC_value, "-")

# Create a matrix with model names as row and column names, so that we can create a comparison table
rownames(two_AIC_diff) <- two_comparisons$model_name
colnames(two_AIC_diff) <- two_comparisons$model_name

# Convert the matrix to a dataframe
two_AIC_diff_df <- as.data.frame(as.table(two_AIC_diff))
names(two_AIC_diff_df) <- c("Model1", "Model2", "Difference")

# Neutralize non-significant differences
two_AIC_diff_df <- two_AIC_diff_df %>%
  mutate(Difference = ifelse(Difference > -10 & Difference < 10, 0, Difference))

# Give everything discriptive names
two_AIC_diff_df <- two_AIC_diff_df %>%
  mutate(across(everything(), ~case_when(
    . == "two_RE" ~ "Relative Entropy Model",
    . == "two_basic" ~ "Basic Model",
    . == "two_root_suffixCluster" ~ "Single TP Model",
    . == "two_rootLast" ~ "Only non-local TP Model",
    . == "two_fullDecomp_rootLast" ~ "Full Decomp. \n + Non-local TP Model",
    . == "two_fullDecomp" ~ "Full Decomp Model",
    TRUE ~ as.character(.)
  )))

two_AIC_diff_df$Difference <- as.numeric(two_AIC_diff_df$Difference)

two_AIC_diff_df$Model1 <- factor(two_AIC_diff_df$Model1, levels = c("Basic Model", "Relative Entropy Model", "Single TP Model", "Full Decomp Model", "Full Decomp. \n + Non-local TP Model", "Only non-local TP Model"))
two_AIC_diff_df$Model2 <- factor(two_AIC_diff_df$Model2, levels = c("Basic Model", "Relative Entropy Model", "Single TP Model", "Full Decomp Model", "Full Decomp. \n + Non-local TP Model", "Only non-local TP Model"))



# ANY VALUES BETWEEN -10 and 10 were converted to 0 for visualization purposes
two_full_plot <- ggplot(two_AIC_diff_df, aes(x = Model1, y = Model2, fill = Difference)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("red", "white", "blue"),
                       values = scales::rescale(c(-Inf, -10, 0, 10, Inf)),
                       guide = "colorbar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 11)) +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA)) +
  labs(title = "Pairwise Comparison of AIC Values",
       x = "Model 1", y = "Model 2",
       fill = "AIC Difference")

two_full_plot
# This is very confusing, when in the top half, blue = you are doing better
# in the bottom have red means you are doing better

ggsave("figures/two_suffix_full.png", plot = two_full_plot, width = 9.75, height = 7.5, units = "in")


##########
# Graph for AMLAP Poster
#########
# Grey out the right bottom half and only keep models of interest

# Create a logical vector for the models to keep
models_to_keep <- !(rownames(two_AIC_diff) %in% c("two_rootLast", "two_fullDecomp_rootLast"))

# Subset the existing matrix to keep only the desired models
two_updated_AIC_diff <- two_AIC_diff[models_to_keep, models_to_keep]

# Update row and column names to reflect the remaining models
two_updated_model_names <- rownames(two_updated_AIC_diff)
rownames(two_updated_AIC_diff) <- two_updated_model_names
colnames(two_updated_AIC_diff) <- two_updated_model_names

# Convert the updated matrix to a DataFrame
two_updated_AIC_diff_df <- as.data.frame(as.table(two_updated_AIC_diff))
names(two_updated_AIC_diff_df) <- c("Model1", "Model2", "Difference")

# Adjust differences based on threshold
two_updated_AIC_diff_df <- two_updated_AIC_diff_df %>%
  mutate(Difference = ifelse(Difference > -10 & Difference < 10, 0, Difference))

# Replace model names with descriptive names
two_updated_AIC_diff_df <- two_updated_AIC_diff_df %>%
  mutate(across(everything(), ~case_when(
    . == "two_RE" ~ "Relative Entropy Model",
    . == "two_basic" ~ "Basic Model",
    . == "two_root_suffixCluster" ~ "Single TP Model",
    . == "two_fullDecomp_rootLast" ~ "Full Decomp. \n + Non-local TP Model",
    . == "two_fullDecomp" ~ "Full Decomp Model",
    TRUE ~ as.character(.)
  )))

# Convert Differences to numeric
two_updated_AIC_diff_df$Difference <- as.numeric(two_updated_AIC_diff_df$Difference)

# Set factor levels for models
two_updated_AIC_diff_df$Model1 <- factor(two_updated_AIC_diff_df$Model1, levels = c("Basic Model", "Relative Entropy Model", "Single TP Model", "Full Decomp Model", "Full Decomp. \n + Non-local TP Model"))
two_updated_AIC_diff_df$Model2 <- factor(two_updated_AIC_diff_df$Model2, levels = c("Basic Model", "Relative Entropy Model", "Single TP Model", "Full Decomp Model", "Full Decomp. \n + Non-local TP Model"))

# Create a matrix indicating the positions to gray out (bottom-right half)
gray_out_matrix <- outer(
  seq_along(two_updated_model_names),
  seq_along(two_updated_model_names),
  FUN = function(i, j) i > j
)

# Convert the gray-out matrix to a DataFrame
gray_out_df <- as.data.frame(as.table(gray_out_matrix))
names(gray_out_df) <- c("Model1", "Model2", "gray_out")

# Replace indices with descriptive model names
gray_out_df <- gray_out_df %>%
  mutate(Model1 = two_updated_model_names[as.integer(Model1)],
         Model2 = two_updated_model_names[as.integer(Model2)],
         gray_out = as.logical(gray_out)) %>%
  mutate(across(c(Model1, Model2), ~ case_when(
    . == "two_RE" ~ "Relative Entropy Model",
    . == "two_basic" ~ "Basic Model",
    . == "two_root_suffixCluster" ~ "Single TP Model",
    . == "two_fullDecomp_rootLast" ~ "Full Decomp. \n + Non-local TP Model",
    . == "two_fullDecomp" ~ "Full Decomp Model",
    TRUE ~ as.character(.)
  )))


# Filter the DataFrame to keep only the lower triangle of the matrix
two_suffixes_graph_df <- two_updated_AIC_diff_df %>%mutate(keep = case_when(
  Model1 == "Basic Model" & Model2 == "Basic Model" ~ TRUE,
  Model1 == "Basic Model" & Model2 == "Relative Entropy Model" ~ TRUE,
  Model1 == "Basic Model" & Model2 == "Single TP Model" ~ TRUE,
  Model1 == "Basic Model" & Model2 == "Full Decomp Model" ~ TRUE,
  Model1 == "Relative Entropy Model" & Model2 == "Relative Entropy Model" ~ TRUE,
  Model1 == "Relative Entropy Model" & Model2 == "Single TP Model" ~ TRUE,
  Model1 == "Relative Entropy Model" & Model2 == "Full Decomp Model" ~ TRUE,
  Model1 == "Single TP Model" & Model2 == "Single TP Model" ~ TRUE,
  Model1 == "Single TP Model" & Model2 == "Full Decomp Model" ~ TRUE,
  Model1 == "Full Decomp Model" & Model2 == "Full Decomp Model" ~ TRUE,
  TRUE ~ FALSE  # All other combinations are not kept
  )) %>%
  filter(keep)
  
color_scale <- scale_fill_gradientn(
  colors = c(
    scales::alpha("red", 0.8),    # Strong red for strong negative values
    scales::alpha("red", 0.2),    # Light red for values closer to -10
    scales::alpha("white", 0),    # Transparent white for values near 0
    scales::alpha("blue", 0.2),   # Light blue for values closer to 10
    scales::alpha("blue", 0.8)    # Strong blue for strong positive values
  ),
  values = scales::rescale(c(-25, -10, 0, 10, 25)),  # Map colors to these breakpoints
  limits = c(-25, 25),                               # Define the overall scale limits
  guide = "colorbar"
)

# Apply the color scale to the graph
two_suffix_graph <- ggplot() +
  geom_tile(data = two_suffixes_graph_df, aes(x = Model1, y = Model2, fill = Difference)) +
  geom_tile(data = gray_out_df %>% filter(gray_out), 
            aes(x = Model1, y = Model2), fill = scales::alpha("black", 0.4)) +  # 60% transparent gray
  color_scale +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL)

two_suffix_graph

ggsave("figures/2_suffixes_export-graph.png", plot = two_suffix_graph, width = 9.75, height = 7.5, units = "in")


two_suffix_legend_graph <-  ggplot(two_AIC_diff_df, aes(x = Model1, y = Model2, fill = Difference)) +
  geom_tile() +
  color_scale +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(angle = 45, hjust = 1, size = 11),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  labs(
    title = "Pairwise Comparison of AIC Values",
    x = "Model 1", y = "Model 2",
    fill = "AIC Δ"
  )

two_suffix_legend_graph

legend <- get_legend(two_suffix_legend_graph)

plot_grid(legend, ncol = 1)

ggsave("figures/2_suffixes_export-legend.png", plot =plot_grid(legend, ncol = 1), width = 2, height = 3, units = "in")




###
# 3 part suffix regression models 
###

df_target_3 <- df_target %>%
  filter(suffix %in% c('aría', 'aríamos', 'arían', 'arías', 
                       'ería', 'eríamos', 'erían', 'erías', 
                       'iría', 'iríamos', 'irían', 'irías'))

##
# Basic Model
##

three_basic = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + (1|workerid) + (1|word), data=df_target_3)
summary(three_basic)
#                     Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      1.843e+02  8.910e+01  4.835e+01   2.069    0.044 *  
# trial_num       -9.148e-02  7.620e-03  9.186e+03 -12.005  < 2e-16 ***
# avg_bigram_freq  1.253e-06  9.388e-07  2.051e+02   1.335    0.183    
# word_log_freq   -4.648e+01  5.442e+00  2.106e+02  -8.541 2.68e-15 ***
# exp              4.767e+02  3.600e+01  4.700e+01  13.243  < 2e-16 ***

three_basic_AIC <- AIC(three_basic)
three_basic_AIC
# 132826

##
# Suffix Model
##

three_suffix = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + suffix + (1|workerid) + (1|word), data=df_target_3)
summary(three_suffix)

three_suffix_AIC <- AIC(three_suffix)
three_suffix_AIC
# 132704.7

##
# RE Model
##

three_RE = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + RE + (1|workerid) + (1|word), data=df_target_3)
summary(three_RE)
#                     Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      2.062e+02  8.954e+01  4.933e+01   2.303   0.0255 *  
# trial_num       -9.148e-02  7.620e-03  9.188e+03 -12.006  < 2e-16 ***
# avg_bigram_freq  1.681e-06  9.419e-07  2.034e+02   1.784   0.0759 .  
# word_log_freq   -4.728e+01  5.377e+00  2.073e+02  -8.793 5.64e-16 ***
# exp              4.767e+02  3.599e+01  4.700e+01  13.245  < 2e-16 ***
# RE              -4.075e+01  1.685e+01  2.060e+02  -2.418   0.0165 *  

three_RE_AIC <- AIC(three_RE)
three_RE_AIC
#132814.7


##
# Root to whole suffix cluster
##

three_root_suffixCluster = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + root_wholeSuffix_TP + (1|workerid) + (1|word), data=df_target_3)
summary(three_root_suffixCluster)
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)          1.795e+02  8.915e+01  4.845e+01   2.014  0.04962 *  
# trial_num           -9.152e-02  7.620e-03  9.187e+03 -12.011  < 2e-16 ***
# avg_bigram_freq      9.988e-07  9.489e-07  2.041e+02   1.053  0.29377    
# word_log_freq       -9.077e+01  2.861e+01  2.030e+02  -3.173  0.00174 ** 
# exp                  4.767e+02  3.600e+01  4.700e+01  13.242  < 2e-16 ***
# root_wholeSuffix_TP  2.417e+02  1.533e+02  2.041e+02   1.577  0.11625     

three_root_suffixCluster_AIC <- AIC(three_root_suffixCluster)
three_root_suffixCluster_AIC
#132813.6

##
# full Decomp Model
##

three_fullDecomp = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + root_firstSuffix_TP + mood_tense_TP + tense_person_TP + (1|workerid) + (1|word), data=df_target_3)
summary(three_fullDecomp)
#                     Estimate Std. Error         df t value Pr(>|t|)    
#   (Intercept)          8.310e+02  2.215e+02  2.478e+02   3.751 0.000219 ***
#   trial_num           -9.116e-02  7.619e-03  9.190e+03 -11.965  < 2e-16 ***
#   avg_bigram_freq      2.858e-06  1.021e-06  2.022e+02   2.800 0.005601 ** 
#   word_log_freq       -4.620e+01  6.478e+00  2.093e+02  -7.132 1.58e-11 ***
#   exp                  4.767e+02  3.599e+01  4.699e+01  13.246  < 2e-16 ***
#   root_firstSuffix_TP -2.824e+02  1.172e+02  2.064e+02  -2.409 0.016869 *  
#   mood_tense_TP       -5.680e+02  1.812e+02  2.059e+02  -3.134 0.001975 ** 
#   tense_person_TP      7.733e+01  7.273e+01  2.067e+02   1.063 0.288961  

three_fullDecomp_AIC <- AIC(three_fullDecomp)
three_fullDecomp_AIC
# 132781.3


##
# Root to last suffix Model
##
three_rootLast = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + root_lastSuffix_TP + (1|workerid) + (1|word), data=df_target_3)
summary(three_rootLast)
#                     Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)        1147.51383   72.08932  250.14214  15.918  < 2e-16 ***
# trial_num            -0.09147    0.00762 9186.10703 -12.004  < 2e-16 ***
# avg_bigram_freq       117.42366  113.60650  205.77310   1.034   0.3025    
# word_log_freq       -37.90731    7.46373  212.95764  -5.079 8.28e-07 ***
# exp3                476.66109   35.99230   46.99617  13.243  < 2e-16 ***
# root_lastSuffix_TP -104.39573   58.93487  224.09997  -1.771   0.0779 .  

three_rootLast_AIC <- AIC(three_rootLast)
three_rootLast_AIC
# 132778.3

##
# Full Decomp + root to last
##

three_fullDecomp_rootLast = lmer(rt ~ trial_num + avg_bigram_freq + word_log_freq + exp + root_firstSuffix_TP + mood_tense_TP + tense_person_TP + root_lastSuffix_TP + (1|workerid) + (1|word), data=df_target_3)
summary(three_fullDecomp_rootLast)
#                     Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)          1.575e+03  1.858e+02  2.084e+02   8.475 4.27e-15 ***
# trial_num           -9.127e-02  7.618e-03  9.190e+03 -11.980  < 2e-16 ***
# avg_bigram_freq        2.111e+02  1.155e+02  2.028e+02   1.827  0.06912 .  
# word_log_freq       -3.812e+01  7.497e+00  2.092e+02  -5.084 8.17e-07 ***
# exp3                 4.766e+02  3.598e+01  4.700e+01  13.245  < 2e-16 ***
# root_firstSuffix_TP -2.320e+02  1.171e+02  2.055e+02  -1.981  0.04897 *  
# mood_tense_TP       -4.491e+02  1.692e+02  2.052e+02  -2.655  0.00856 ** 
# tense_person_TP      1.659e+02  7.871e+01  2.086e+02   2.108  0.03620 *  
# root_lastSuffix_TP  -1.606e+02  6.218e+01  2.229e+02  -2.583  0.01043 *  


three_fullDecomp_rootLast_AIC <- AIC(three_fullDecomp_rootLast)
three_fullDecomp_rootLast_AIC
# 132733.7



###
# 3 part suffix model comparison 
###

# Create dataframe with AIC values
three_comparisons <- data.frame(
  model_name = c("three_basic", "three_RE", "three_root_suffixCluster", "three_fullDecomp", 
                 "three_rootLast", "three_fullDecomp_rootLast"),
  AIC_value = c(three_basic_AIC, three_RE_AIC, three_root_suffixCluster_AIC, three_fullDecomp_AIC, 
                three_rootLast_AIC, three_fullDecomp_rootLast_AIC)
)

# Calculate differences in AIC values
three_AIC_diff <- outer(three_comparisons$AIC_value, three_comparisons$AIC_value, "-")

# Create a matrix with model names as row and column names, so that we can create a comparison table
rownames(three_AIC_diff) <- three_comparisons$model_name
colnames(three_AIC_diff) <- three_comparisons$model_name

# Convert the matrix to a dataframe
three_AIC_diff_df <- as.data.frame(as.table(three_AIC_diff))
names(three_AIC_diff_df) <- c("Model1", "Model2", "Difference")

# Neutralize non-significant differences
three_AIC_diff_df <- three_AIC_diff_df %>%
  mutate(Difference = ifelse(Difference > -10 & Difference < 10, 0, Difference))

# Give everything discriptive names
three_AIC_diff_df <- three_AIC_diff_df %>%
  mutate(across(everything(), ~case_when(
    . == "three_RE" ~ "Relative Entropy Model",
    . == "three_basic" ~ "Basic Model",
    . == "three_root_suffixCluster" ~ "Single TP Model",
    . == "three_rootLast" ~ "Only non-local TP Model",
    . == "three_fullDecomp_rootLast" ~ "Full Decomp. \n + Non-local TP Model",
    . == "three_fullDecomp" ~ "Full Decomp Model",
    TRUE ~ as.character(.)
  )))

three_AIC_diff_df$Difference <- as.numeric(three_AIC_diff_df$Difference)

three_AIC_diff_df$Model1 <- factor(three_AIC_diff_df$Model1, levels = c("Basic Model", "Relative Entropy Model", "Single TP Model", "Full Decomp Model", "Full Decomp. \n + Non-local TP Model", "Only non-local TP Model"))
three_AIC_diff_df$Model2 <- factor(three_AIC_diff_df$Model2, levels = c("Basic Model", "Relative Entropy Model", "Single TP Model", "Full Decomp Model", "Full Decomp. \n + Non-local TP Model", "Only non-local TP Model"))





# ANY VALUES BETWEEN -10 and 10 were converted to 0 for visualization purposes
three_full_plot <- ggplot(three_AIC_diff_df, aes(x = Model1, y = Model2, fill = Difference)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("red", "white", "blue"),
                       values = scales::rescale(c(-Inf, -10, 0, 10, Inf)),
                       guide = "colorbar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 11)) +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA)) +
  labs(title = "Pairwise Comparison of AIC Values",
       x = "Model 1", y = "Model 2",
       fill = "AIC Difference")

three_full_plot
# This is very confusing, when in the top half, blue = you are doing better
# in the bottom have red means you are doing better

ggsave("figures/two_suffix_full.png", plot = two_full_plot, width = 9.75, height = 7.5, units = "in")



##########
# Graph for AMLAP Poster
#########
# Grey out the right bottom half and only keep models of intereswt

# Create a logical vector for the models to keep
three_models_to_keep <- !(rownames(three_AIC_diff) %in% c("three_rootLast", "three_fullDecomp_rootLast"))

# Subset the existing matrix to keep only the desired models
three_updated_AIC_diff <- three_AIC_diff[three_models_to_keep, three_models_to_keep]

# Update row and column names to reflect the remaining models
three_updated_model_names <- rownames(three_updated_AIC_diff)
rownames(three_updated_AIC_diff) <- three_updated_model_names
colnames(three_updated_AIC_diff) <- three_updated_model_names

# Convert the updated matrix to a DataFrame
three_updated_AIC_diff_df <- as.data.frame(as.table(three_updated_AIC_diff))
names(three_updated_AIC_diff_df) <- c("Model1", "Model2", "Difference")

# Adjust differences based on threshold
three_updated_AIC_diff_df <- three_updated_AIC_diff_df %>%
  mutate(Difference = ifelse(Difference > -10 & Difference < 10, 0, Difference))

# Replace model names with descriptive names
three_updated_AIC_diff_df <- three_updated_AIC_diff_df %>%
  mutate(across(everything(), ~case_when(
    . == "three_RE" ~ "Relative Entropy Model",
    . == "three_basic" ~ "Basic Model",
    . == "three_root_suffixCluster" ~ "Single TP Model",
    . == "three_fullDecomp_rootLast" ~ "Full Decomp. \n + Non-local TP Model",
    . == "three_fullDecomp" ~ "Full Decomp Model",
    TRUE ~ as.character(.)
  )))

# Convert Differences to numeric
three_updated_AIC_diff_df$Difference <- as.numeric(three_updated_AIC_diff_df$Difference)

# Set factor levels for models
three_updated_AIC_diff_df$Model1 <- factor(three_updated_AIC_diff_df$Model1, levels = c("Basic Model", "Relative Entropy Model", "Single TP Model", "Full Decomp Model", "Full Decomp. \n + Non-local TP Model"))
three_updated_AIC_diff_df$Model2 <- factor(three_updated_AIC_diff_df$Model2, levels = c("Basic Model", "Relative Entropy Model", "Single TP Model", "Full Decomp Model", "Full Decomp. \n + Non-local TP Model"))

# Create a matrix indicating the positions to gray out (bottom-right half)
gray_out_matrix <- outer(
  seq_along(three_updated_model_names),
  seq_along(three_updated_model_names),
  FUN = function(i, j) i > j
)

# Convert the gray-out matrix to a DataFrame
gray_out_df <- as.data.frame(as.table(gray_out_matrix))
names(gray_out_df) <- c("Model1", "Model2", "gray_out")

# Replace indices with descriptive model names
gray_out_df <- gray_out_df %>%
  mutate(Model1 = three_updated_model_names[as.integer(Model1)],
         Model2 = three_updated_model_names[as.integer(Model2)],
         gray_out = as.logical(gray_out)) %>%
  mutate(across(c(Model1, Model2), ~ case_when(
    . == "three_RE" ~ "Relative Entropy Model",
    . == "three_basic" ~ "Basic Model",
    . == "three_root_suffixCluster" ~ "Single TP Model",
    . == "three_fullDecomp_rootLast" ~ "Full Decomp. \n + Non-local TP Model",
    . == "three_fullDecomp" ~ "Full Decomp Model",
    TRUE ~ as.character(.)
  )))


# Filter the DataFrame to keep only the lower triangle of the matrix
three_suffixes_graph_df <- three_updated_AIC_diff_df %>%mutate(keep = case_when(
  Model1 == "Basic Model" & Model2 == "Basic Model" ~ TRUE,
  Model1 == "Basic Model" & Model2 == "Relative Entropy Model" ~ TRUE,
  Model1 == "Basic Model" & Model2 == "Single TP Model" ~ TRUE,
  Model1 == "Basic Model" & Model2 == "Full Decomp Model" ~ TRUE,
  Model1 == "Relative Entropy Model" & Model2 == "Relative Entropy Model" ~ TRUE,
  Model1 == "Relative Entropy Model" & Model2 == "Single TP Model" ~ TRUE,
  Model1 == "Relative Entropy Model" & Model2 == "Full Decomp Model" ~ TRUE,
  Model1 == "Single TP Model" & Model2 == "Single TP Model" ~ TRUE,
  Model1 == "Single TP Model" & Model2 == "Full Decomp Model" ~ TRUE,
  Model1 == "Full Decomp Model" & Model2 == "Full Decomp Model" ~ TRUE,
  TRUE ~ FALSE  # All other combinations are not kept
)) %>%
  filter(keep)

three_color_scale <- scale_fill_gradientn(
  colors = c(
    scales::alpha("red", 0.8),    # Strong red for strong negative values
    scales::alpha("red", 0.2),    # Light red for values closer to -10
    scales::alpha("white", 0),    # Transparent white for values near 0
    scales::alpha("blue", 0.2),   # Light blue for values closer to 10
    scales::alpha("blue", 0.8)    # Strong blue for strong positive values
  ),
  values = scales::rescale(c(-65, -10, 0, 10, 65)),  # Map colors to these breakpoints
  limits = c(-65, 65),                               # Define the overall scale limits
  guide = "colorbar"
)

# Apply the color scale to the graph
three_suffix_graph <- ggplot() +
  geom_tile(data = three_suffixes_graph_df, aes(x = Model1, y = Model2, fill = Difference)) +
  geom_tile(data = gray_out_df %>% filter(gray_out), 
            aes(x = Model1, y = Model2), fill = scales::alpha("black", 0.4)) +  # 60% transparent gray
  three_color_scale +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL)

three_suffix_graph


ggsave("figures/3_suffixes_export-graph.png", plot = three_suffix_graph, width = 9.75, height = 7.5, units = "in")


three_suffix_legend_graph <-  ggplot(three_AIC_diff_df, aes(x = Model1, y = Model2, fill = Difference)) +
  geom_tile() +
  three_color_scale +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(angle = 45, hjust = 1, size = 11),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  labs(
    title = "Pairwise Comparison of AIC Values",
    x = "Model 1", y = "Model 2",
    fill = "AIC Δ"
  )

three_suffix_legend_graph

three_legend <- get_legend(three_suffix_legend_graph)

plot_grid(three_legend, ncol = 1)

ggsave("figures/3_suffixes_export-legend.png", plot =plot_grid(three_legend, ncol = 1), width = 2, height = 3, units = "in")


