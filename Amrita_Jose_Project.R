---
title: "Analytics Project"
---
#The project tries to find the correlations between loan default rate and various other feature variables in the loan dataset. A few 
#exploratory analysis questions help to identify significant correlations, whether positive or negative. The project also aims to build a
#model to predict whether a customer will default on his loan. 

## Add R libraries here
library(tidyverse)
library(tidymodels)


# Load data
loans_df <- read_rds(url('https://gmubusinessanalytics.netlify.app/data/loan_data.rds'))

The section covers 5 relevant questions that explore the relationship between `loan_default` and the other variables in the `loan_df` data set. This analysis helps to discover which variables drive the differences between customers who do and do not default on their loans.

# Question 1


**Question**:Are loan default rates influenced by the rate of interest?

**Answer**:Yes, the data indicates that loans with interest rates less than 9.75% have not defaulted(0% default) and loans with greater interest rates have tendency to default. All loans with interest rates greater than or equal to 14% have defaulted.


```{r}
#Summary Table for Question 1
loans_df %>%
  group_by(interest_rate) %>% 
  summarise(n_customers = n(),
            customers_default = sum(loan_default == 'yes'),
            default_percent = 100 * mean(loan_default == 'yes'))

#Data Visualization for Question 1
default_rates_interest <- loans_df %>% group_by(interest_rate) %>% 
                              summarise(n_customers = n(),
                              customers_default = sum(loan_default == 'yes'),
                              default_percent = 100 * mean(loan_default == 'yes'))

ggplot(data = default_rates_interest, mapping = aes(x = interest_rate, fill = default_percent)) +
geom_histogram(aes(y=..density..), color = "green", bins = 15) +
facet_wrap(default_percent ~ .)+
labs(title = "Loan Default Rate by Rate of Interest", x = "Interest Rate", y = "Proportion of Customers")

```



# Question 2


**Question**:Is lower annual income related to a higher chance of defaulting on loans?

**Answer**: No, the data does not considerably indicate that annual income significantly drives differences in loan default rates. Customers with higher annual income are almost as likely to default on loans as customers with low annual income. Hence, annual income does not seem to be a significant predictor of loan default rate. 


```{r}
#Summary Table for Question 2
loans_df %>%
  group_by(annual_income) %>% 
  summarise(n_customers = n(),
            customers_default = sum(loan_default == 'yes'),
            default_percent = 100 * mean(loan_default == 'yes'))

#Data Visualization for Question 2
default_rates_income <- loans_df %>% group_by(annual_income) %>% 
                              summarise(n_customers = n(),
                              customers_default = sum(loan_default == 'yes'),
                              default_percent = 100 * mean(loan_default == 'yes'))

ggplot(data = default_rates_income, mapping = aes(x = default_percent, y = annual_income)) +
geom_point(position=position_jitter(w=0.1,h=0),alpha = 0.5,color = 'red') +
labs(title = 'Loan Default Rate by Annual Income',x = 'Default Percentage',y = 'Annual Income')

```


# Question 3


**Question**:Does history of bankruptcy drive differences in loan default rates?
Yes, the data indicates that credit card and medical loans have significantly larger default rates than any other type of loan. In fact, both of these loan types have default rates at more than 50%. This is nearly two times the average default rate for all other loan types.

**Answer**: Yes, the data indicates that history of bankruptcy influences loan default rates to some extent. Loans given to customers having a bankruptcy history are 41.77% likely to default while customers without bankruptcy history are also not far behind (36.62%)


```{r}
#Summary Table for Question 3
loans_df %>%
  group_by(history_bankruptcy) %>% 
  summarise(n_customers = n(),
            customers_default = sum(loan_default == 'yes'),
            default_percent = 100 * mean(loan_default == 'yes'))

#Data Visualization for Question 3
default_rates_bankruptcy <- loans_df %>% group_by(history_bankruptcy) %>% 
                              summarise(n_customers = n(),
                              customers_default = sum(loan_default == 'yes'),
                              default_percent = 100 * mean(loan_default == 'yes'))

ggplot(data = default_rates_bankruptcy, mapping = aes(x = default_percent, y = history_bankruptcy)) +
    geom_bar(stat = 'identity', fill = '#009CB1') +
    coord_flip()+
    labs(title = 'Loan Default Rate by History of Bankruptcy',
         x = 'Default Percentage ',
         y = 'History of Bankruptcy') 

```



# Question 4


**Question**:Are loan default rates influenced by the term of the loans? 

**Answer**:Yes, the data indicates that customers with 5 year loans have significantly larger default rates that customers with 3 year loans. In fact, the default rate for 5 year loans(54.99%) is more than twice the default rate for 3 year loans(26.77)


```{r}
#Summary Table for Question 4
loans_df %>%
  group_by(term) %>% 
  summarise(n_customers = n(),
            customers_default = sum(loan_default == 'yes'),
            default_percent = 100 * mean(loan_default == 'yes'))

#Data Visualization for Question 4
default_rates_term <- loans_df %>% group_by(term) %>% 
                              summarise(n_customers = n(),
                              customers_default = sum(loan_default == 'yes'),
                              default_percent = 100 * mean(loan_default == 'yes'))

ggplot(data = loans_df, aes(x = term, fill = loan_default)) +
    geom_bar(stat = "count") +
    labs(title = "Loan Defaults by Term",
         x = "Term", y = "Number of Customers")

```



# Question 5


**Question**: Does home ownership influence differences in loan default rates?

**Answer**:Yes, the data indicates that customers with rented homes have higher default rate than any other home ownership type. Another interesting finding is that customers who own homes have higher default rates(37.28%) than customers who have mortgaged homes(32.42%)


```{r}
#Summary Table for Question 5
loans_df %>%
  group_by(homeownership) %>% 
  summarise(n_customers = n(),
            customers_default = sum(loan_default == 'yes'),
            default_percent = 100 * mean(loan_default == 'yes'))

#Data Visualization for Question 5
default_rates_ownership <- loans_df %>% group_by(homeownership) %>% 
                              summarise(n_customers = n(),
                              customers_default = sum(loan_default == 'yes'),
                              default_percent = 100 * mean(loan_default == 'yes'))

ggplot(data = loans_df, aes(x = loan_default, fill = homeownership)) +
    geom_bar(stat = "count") +
    labs(title = "Loan Defaults by Home Ownership",
         x = "Loan Default (Yes/No)", y = "Number of Customers")

ggplot(data = default_rates_ownership, mapping = aes(x = homeownership, y = default_percent, fill=homeownership)) +
    geom_bar(stat = 'identity') +
    labs(title = 'Loan Default Rate for Home Ownership Types',
         x = 'Home Ownership',
         y = 'Default Percentage') 


```



#Predictive Modelling

#Two regression/classification algorithms are used to predict the response variable,`loan_default`. The models used are - Logistic
#Regression and Decision Trees.

`# Model 1 - Logistic Regression

logistic_model <- logistic_reg() %>% 
                  set_engine('glm') %>% 
                  set_mode('classification')

# - Package recipe and model into a workflow
loans_wf <- workflow() %>% 
            add_model(logistic_model) %>% 
            add_recipe(loans_recipe)

#- Fit workflow to the training data
loans_logistic_fit <- loans_wf %>% 
                      fit(data = loans_training)


#Performance Evaluation of the Model
#Obtain the predicted category for each row in the test set
predictions_categories <- predict(loans_logistic_fit, new_data = loans_test)
predictions_categories
#Obtain the estimated probabilities for each category of the response variable.
predictions_probabilities <- predict(loans_logistic_fit, new_data = loans_test, type = 'prob')
predictions_probabilities
#Combining results with the true response variable values in the test data set.
test_results <- loans_test %>% select(loan_default) %>% 
                bind_cols(predictions_categories) %>% 
                bind_cols(predictions_probabilities)
test_results


#Performance Metrics of the Model
#Computing F1 Score
f_meas(test_results, truth = loan_default, estimate = .pred_class)
#Computing Confusion Matrix
conf_mat(test_results, truth = loan_default, estimate = .pred_class)
#Plotting ROC Curve using autoplot()
roc_curve(test_results, truth = loan_default, estimate = .pred_yes) %>% 
  autoplot()
#Calculating the area under the ROC curve on the test data
roc_auc(test_results, truth = loan_default, .pred_yes)

```


# Model 2 - Decision Trees

```{r}

# Specify a model object for Decision Trees
tree_model <- decision_tree(cost_complexity = tune(),
                            tree_depth = tune(),
                            min_n = tune()) %>% 
              set_engine('rpart') %>% 
              set_mode('classification')

# - Package recipe and model into a workflow
tree_wf <- workflow() %>% 
                 add_model(tree_model) %>% 
                 add_recipe(loans_recipe)

#Perform Hyperparameter Tuning
set.seed(345)
loans_folds <- vfold_cv(loans_training, v = 5)
#Create a grid of hyperparameter values to test
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          min_n(), 
                          levels = 3)
#Tuning Hyperparameters with `tune_grid()' - Tune decision tree workflow
set.seed(345)
tree_tuning <- tree_wf %>% 
               tune_grid(resamples = loans_folds,
                         grid = tree_grid)
#Show the top 5 best models based on roc_auc metric
tree_tuning %>% show_best('roc_auc')
#Select best model based on roc_auc
best_tree <- tree_tuning %>% 
             select_best(metric = 'roc_auc')
best_tree
#Finalize Workflow
final_tree_wf <- tree_wf %>% 
                       finalize_workflow(best_tree)
#- Fit workflow to the training data
tree_wf_fit <- final_tree_wf %>% 
               fit(data = loans_training)
tree_fit <- tree_wf_fit %>% 
            pull_workflow_fit()
#Plot the decision tree
library(rpart.plot)
rpart.plot(tree_fit$fit, roundint = FALSE)
#Train and Evaluate With `last_fit()`
tree_last_fit <- final_tree_wf %>% 
                 last_fit(loans_split)


#Performance Metrics
tree_last_fit %>% collect_metrics()
#Computing Confusion Matrix
tree_predictions <- tree_last_fit %>% collect_predictions()
conf_mat(tree_predictions, truth = loan_default, estimate = .pred_class)
#Plotting ROC Curve using autoplot()
tree_last_fit %>% collect_predictions() %>% 
                  roc_curve(truth  = loan_default, estimate = .pred_yes) %>% 
                  autoplot()

```



**Summary**

- There are a few variables that considerably influence the probability of a customer defaulting on the loan. Bank Interest Rates influence loan default rates of the customer. Using data analysis techniques, it was observed that loans with interest rates less than 9.75% have never defaulted and all loans with interest rates greater than or equal to 14% have defaulted. This promotes the need to lower interest rates or encourage customers to pick low interest rate loan options to avoid unnecessary defaults. It was also observed, that even though the impact is lesser, customers with a history of bankruptcy have more tendency to default than customers without a history. The bank must perform thorough background checks for each customer before approving loans in order to avoid such cases. Loan default rates are also influenced by the term of the loan. Customers with 5 year loans have more than twice the default rate than customers with 3 year loans. Based on customer background, banks must encourage customers to apply for 3 year loans than 5 year loans. The interest earned in the surplus 2 years may be lesser than the defaulted loan which results in a loss for the banks. Hence encouraging customers to opt for a shorter tenure would help in reducing losses. It was also observed that customers with rented homes have higher default rate than any other home ownership type. Another finding is that customers who own homes have higher default rates(37.28%) than customers who have mortgaged homes(32.42%). While home ownership may not be significant factor while approving loans, it indicates the financial stability of a customer and enables the bank to make informed decisions regarding loan approvals.

- The two models that have been used on the dataset are logistic regression and decision tree. Both the models are machine learning models that have been trained over the dataset in order to recognize patterns and trends and learn from the data about various factors that can influence or have the capability to predict loans that will be defaulted. This learning can be applied to any set of future customer loan data and make predictions whether those customers will default on their loans. On running both models and comparing their ROC AOC values (ROC AOC is a performance metric that is used to evaluate the capability of models to distinguish between classes correctly - that is, the ROC AUC model enables to evaluate how well the models are able to distinguish 'Yes' and 'No' classes in the loan default, thus indicating that the model's capability to accurately predict which class of loan default any new customer loan data will fall under). An AUC value of 0.5 indicates no discrimination, 0.7 to 0.8 is considered as an acceptable value for distinguishing, 0.8 to 0.9 is considered excellent and above 0.9 is considered outstanding. The Logistic Regression model has an AUC value of 0.987 and the Decision Tree model has an AUC value of 0.96, thus indicating that both models have very good capability in distinguishing classes and hence predicting unseen customer loan default rates with higher accuracy. Based on the AUC model, logistic regression is the best classification model in terms of performance.

- The expected error is calculated as (1 - Accuracy) or (False Positive+False Negative)/Total in terms of confusion matrix. For logistic model, the expected error is (24+33) / (24+33+349+621) = 57/1027 = 0.055%. This indicates that the logistic model will have a 0.055% error rate while predicting default rates for future customer loan data. 
For decision tree model, the expected error is (37+59) / (37+59+323+608) = 96/1027 = 0.093%. This indicates that the decision tree model will have a 0.093% error rate while predicting default rates for future customer loan data.
Hence, it can be observed that decision tree models will have more error while classifying future customer loan data than logistic regression. Hence, it can be concluded that logistic regression is the best classification model. 

- A few recommendations to reduce financial losses from default would be as follows: -
    - Encourage customers to pick low interest rate loan options to avoid unnecessary defaults. The loan default would cause more monetary loss than the  
    monetary gain from high interest rates. 
    - Perform thorough background checks for each customer before approving loans in order to ensure they have zero or negligible history of bankruptcy. 
    - Encourage customers to apply for short term (3 year) loans than long term (5 year) loans. The interest earned in the surplus 2 years may be lesser than 
    the monetary loss from the loan default.
    - Perform thorough background checks for customers with rented homes. Customers with rented homes and a history of bankruptcy may be at a higher risk to
    default. Scrutinized verification must be done for such customers in order to reduce financial losses due to default. 
    
