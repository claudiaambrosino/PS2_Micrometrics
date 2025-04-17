# Template of R script to answer problem set
# Group number: 8
# Group composition: Claudia Ambrosino, Flavia Grasso, and Titouan Renault

# Get the username
user <- Sys.info()["user"]
print(user)

# Define file path conditionally
if (user == "erick") {
  filepath <- "/home/erick/TEMP/"
} else if (user == "titouanrenault") {
  filepath <- "/Users/titouanrenault/Desktop/Master/micrometrics/Problem set 2"
} else if (user == "B") {
  filepath <- "/Users/grasso/Documents/Bocconi ESS/2024-2025/Semester 2/20295 - Microeconometrics/Problem Set 2"
} else if (user == "C") {
  filepath <- "/FILE/PATH/C/"
} else {
  filepath <- ""  # Default case if user is not listed
}

# Print the selected file path
print(paste("File path set to:", filepath))

#Library
library(grf)
library(dplyr)
library(labelled)
library(ggplot2)
library(tidyr)

#Download data
ps3 = read.csv(file.path(filepath, "files/expanded_data.csv"))

#-------------------------------------------------------------------------------
#EXERCISE 2
#-------------------------------------------------------------------------------

# Question 1.a

#Create binary for urbanization (tau forest does not accept non-numeric variable)
ps3$urban_bin = ifelse(ps3$urbanization == "Urban", 1, 0)

#Create dummy for state
ps3$st_bin = as.integer(relevel(factor(ps3$st), ref = "AK")) - 1

#Structure the dataset to compute the difference between 1978 and 1968

ps3_diff <- ps3 %>%
  filter(lfdivlaw %in% c(1969, 1970, 1971, 1972, 1973, 2000))%>% #keep only the year relevant for analysis (as in question1)c))
  select(county_id, st, year, div_rate_sim, lfdivlaw, everything()) %>%
  mutate(treated = ifelse(lfdivlaw <= 1973, 1, 0))%>% #create treatment 
  pivot_wider(
    names_from = year,
    values_from = c(div_rate_sim, education_rate, childcare_availability, 
                    unemployment_rate, median_income, urbanization,
                    marriage_rate, religious_adherence, alcohol_consumption, 
                    domestic_violence_rate, women_labor_force_participation, 
                    housing_cost, crime_rate, social_services_spending, treated, urban_bin, st_bin)
  )%>%
  mutate(
    delta_divorce = div_rate_sim_1978 - div_rate_sim_1968 #compute diff after - before
  )

#Create label of variables for plot

label_names <- c("education_rate_1968" = "Education (%)",
                 "religious_adherence_1968"= "Religious Adherence (%)",
                 "women_labor_force_participation_1968" = "Women in labor market (%)", 
                 "childcare_availability_1968" = "Childcare availability", 
                 "marriage_rate_1968" = "Marriage (%)", 
                 "unemployment_rate_1968" = "Unemployment (%)", 
                 "urban_bin_1968" = "Urbanization", 
                 "alcohol_consumption_1968" = "Alcohol consumption", 
                 "median_income_1968" = "Median income", 
                 "social_services_spending_1968" = "Social services spending", 
                 "housing_cost_1968" = "Housing cost",
                 "crime_rate_1968" = "Crime rate (%)", 
                 "domestic_violence_rate_1968" = "Domestic violence (%)"
)


# Outcome variable
Y <- ps3_diff$delta_divorce

# Treatment indicator
W <- ps3_diff$treated_1968

#Covariates: we keep only covariates for year 1968, to see what are the drivers of heterogeneity.
#We remove 1978 covariates, because they may also have changed as a result of the policy. 
X <- ps3_diff[, c("education_rate_1968", "childcare_availability_1968", "unemployment_rate_1968", "median_income_1968", 
             "marriage_rate_1968", "religious_adherence_1968", "alcohol_consumption_1968",
             "domestic_violence_rate_1968", "women_labor_force_participation_1968",
             "housing_cost_1968", "crime_rate_1968", "social_services_spending_1968", "urban_bin_1968")] #check urban and state binary 



#estimate causal forest
tau.forest <- causal_forest(X, Y, W)
#Compute  ATE
ate <- average_treatment_effect(tau.forest)
print(ate)
#estimate    std.err 
#0.03769186 0.07154094 

# Comment: The average treatment effect (ATE) estimated using the causal forest is 0.038, 
# and it is not statistically significantly different from zero at the 5% level. In question 1.c), 
# using the DiD specification, we obtained an estimate of 0.03 for the treatment effect, 
# which was not statistically significant. We obtain a similar estimate using the two approaches.

#Question 1.b)i)

#Compute best-linear projection (BLP) of conditional average treatment effect
blp <- best_linear_projection(tau.forest, X)
blp
# The best linear projection fits a linear approximation of the conditional average treatment effects (CATEs) 
# on the covariates in our dataset. This is useful for identifying which variables are most strongly associated 
# with heterogeneity in the treatment effect.
# 
# In our case, the most relevant covariates are: domestic violence rate, religious adherence, and women’s labor force 
# participation. All three variables are statistically significantly different from zero. This suggests that higher levels 
# of education, stronger religious adherence, and greater female labor force participation are associated 
# with higher treatment effects.
#
# In the next step, we compute variable importance to quantify how frequently each covariate is used in tree splits. 
# We also visualize the relative importance of each variable compared to the most influential one.


#Obtain variable importance
vi = variable_importance(tau.forest)
#Set variable importance relative to the largest (and rescale to 100)
vi_relative = (vi /max(vi))*100 

#Create df to plot variable importance
vi_df <- data.frame(
  Variable = colnames(X),
  Importance = vi_relative
) %>% arrange(-Importance)

#Final Plot using ggplot
variable_importance = ggplot(vi_df, aes(x = Importance, y = reorder(Variable, -Importance))) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Variable Importance", y = NULL,
       title = "Variable Importance in 1968 (relative to max)", 
       labeller = labeller(Variable = label_names)) +theme_classic()+
  scale_y_discrete(labels = label_names)

#Save to the computer
ggsave(file.path(filepath, "output/variable_importance.pdf"))

##Question 1.b)ii): Compute Targeting Operator Characteristic

# Split sample into train and eval
n <- nrow(X)
set.seed(123)
train <- sample(1:n, n / 2)

# Train causal forest on training set
train.forest <- causal_forest(X[train, ], Y[train], W[train])

# Train separate forest on evaluation set
eval.forest <- causal_forest(X[-train, ], Y[-train], W[-train])

# Get predicted CATEs from training forest on the evaluation data
rate <- rank_average_treatment_effect(eval.forest,
                                      predict(train.forest, X[-train, ])$predictions)
rate
pdf(file.path(filepath,"output/TOC_plot.pdf")) # Open PDF device
plot(rate, main = "Targeting Operator Characteristic (TOC)")
dev.off()


# Comment: The TOC (Targeting Operator Characteristic) is the area under the curve of the Rank-Weighted Average Treatment Effect (RATE).
# The RATE is a metric that measures how well the CATE estimator ranks units according to their estimated treatment benefit.
# Specifically, the TOC quantifies, for each quantile q, the incremental benefit of treating only the q% of units with the largest 
# estimated CATEs, compared to the overall average treatment effect (ATE).
# 
# We observe that targeting the most effective states (based on their estimated CATE) significantly increases the treatment effect.
# For example, treating the top 20% of states with the largest estimated CATEs would lead to an ATE of approximately 0.6, 
# whereas treating the bottom 80% would yield a much lower ATE of only 0.1.When we treat 
# the whole sample, the ate becomes almost 0. 

#Question 1)b)iii): Plot CATE by distribution of variables that could drive heterogeneity

library(ggplot2)
library(dplyr)
library(tidyr)

# Predict CATEs using out-of-bag estimates
tau.hat.oob <- predict(tau.forest)
# Add the CATE predictions to your original data
ps3_diff$CATE <- tau.hat.oob$predictions

# Pick variables chosen in the first step (variable importance) to explore heterogeneity
hetero_vars <- c("religious_adherence_1968", 
                 "women_labor_force_participation_1968", "domestic_violence_rate_1968")

# Reshape data to long format for faceted plotting
long_data <- ps3_diff %>%
  select("CATE", "religious_adherence_1968", "women_labor_force_participation_1968", 
         "domestic_violence_rate_1968")%>%
  pivot_longer(cols = all_of(hetero_vars), names_to = "Variable", values_to = "Value")

# Plot: one plot, multiple facets
ggplot(long_data, aes(x = Value, y = CATE)) +
  geom_smooth(method = "loess", color = "blue")+
  facet_wrap(~ Variable, scales = "free_x", labeller = labeller(Variable = label_names), 
             ncol = 2) +
  theme_minimal() +
  labs(title = "CATEs by Potential Heterogeneity Drivers (1968)",
       x = "Value of Variable",
       y = "Estimated CATE")+
  theme_classic()

ggsave(file.path(filepath, "output/heterogeneity_drivers.pdf"))

# Question 1.c

# We found strong evidence of heterogeneous treatment effects in the previous analysis.
# The Best Linear Projection (BLP) showed that key domestic violence, religious 
# adherence, and the share of women in the labor market significantly explain
# the variation in treatment effects across observations.
#
# The Targeting Operator Characteristic (TOC) curve further supports this finding. It
# demonstrates that the average treatment effect varies across different fractions of the
# population, ranked by their predicted responsiveness to the treatment. In particular,
# states in the top 10%—those expected to increase the most unilateral divorce as a result
#of the law — experience the highest treatment effects, with the effect decreasing 
#gradually across lower-ranked groups.This highlights the presence of heterogeneity 
#and suggests that the unilateral divorce law policy had varying effects on states 
#depending on the composition of their population.
#
# While the Average Treatment Effect (ATE) estimated in Question 1.a) was approximately
# 0.037, the Conditional Average Treatment Effects (CATEs) vary  across
# different values of key covariates. For instance, in state where the religious adherence
#was of 20% of individuals the CATE reaches around 0.4. This indicates that the treatment has 
#a stronger impact in areas with lower religious adherence in 1968. This finding is consistent
#with the previous results, particularly the variable importance graph, where religious adherence 
#emerged as by far the most important driver of treatment effect heterogeneity. 
#
# However, when computing the average treatment effect across different values 
# of the domestic violence rate, we do not find strong evidence of heterogeneous 
# treatment effects — although the effect tends to increase slightly with higher 
# rates of domestic violence.
#
# In states where the female labor force participation rate is low (around 40%), 
# the reform appears to reduce the number of divorces. The effect becomes positive 
# once the participation rate reaches approximately 50%, but plateaus beyond 60%, 
# indicating no further increase in the treatment effect.


#Question 1.d

tau.forest_2 = causal_forest(X, Y, W, honesty = FALSE)
#Compute new ATE
ate_2 = average_treatment_effect(tau.forest_2)
ate_2
#estimate    std.err 
#0.03161632 0.07152127
#The estimate of the ate and standard error is almost unchanged. 

#Compute BLP
blp_2 <- best_linear_projection(tau.forest_2, X)
blp_2

#The best linear remains unchanged. domestic violence, religious adherence and women labor
#force participation are still the three variables statistically different from 0. 

# Comment: 
# We obtain the same ATE. When we do not use honest causal trees, we do not split the
# data into separate subsets for determining the tree structure and estimating the treatment effect
# within each leaf.
# This makes the model appear "more precise" in-sample, but may lead to bias in the 
# average treatment effect due to overfitting. 
# We would expect this bias to be more important in small samples. 
# This is because, in small samples, the model is more likely to fit spurious correlation, leading to 
# overfitting. In contrast, in large samples, random noise tends to cancel out, reducing 
# the impact of overfitting on the estimated treatment effects.
# It is particularly relevant for our estimation of CATE, since we care mostly
# of credible inference and not only prediction. 




