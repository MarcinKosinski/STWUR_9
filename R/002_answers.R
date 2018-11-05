

# load data and packages --------------------------------------------------

# install.packages(c('ggplot2', 'survminer', 'dplyr'))
library(ggplot2)
library(survminer)
library(dplyr)
library(survival)

survey <- readRDS('data/survey.rds')
transactional <- readRDS('data/transactional.rds')
clinical <- readRDS('data/clinical.rds') # created in R/999_extra_prepare_clinical_data.R

head(survey)
head(transactional)
head(clinical)

# exercise 01 -------------------------------------------------------------

# 1.1 Use survey, transactional and clinical data inputs to prepare risk set tables
# with the help of survival::survfit function and ?Surv object.

summary(survival::survfit(Surv(times, patient.vital_status) ~ 1, data = clinical))
transactional2 <-
  transactional %>%
  mutate(time = ifelse(is.na(ended_at),
                       difftime(max(ended_at, na.rm = TRUE), free_trial_started_at, units = 'days'),
                       difftime(ended_at, free_trial_started_at, units = 'days'))) %>%
  mutate(status = (!is.na(ended_at)) %>% as.integer) %>%
  select(-c(1:2))



# Can below snippet can help with the survey data?
survey2 <-
  survey %>%
  mutate(event = 
           (subscribed =='Subscribed to this brand/company in the past, but cancelled my subscription to this brand/company') %>% as.integer) %>%
  select(-subscribed) %>% 
  mutate(time = ifelse(is.na(`How long did you keep your subscription before you cancelled?`),
                          `How long ago did you begin your subscription?` %>% as.character() %>% gsub(' ago', '', x = .),
                          `How long did you keep your subscription before you cancelled?` %>% as.character() %>% gsub(' ago', '', x = .)
                          )) %>%
  select(-c(1:2)) %>%
  mutate(id = 1:nrow(.)) %>%
  group_by(id) %>%
  mutate(
    time = case_when(
      time == "Less than 5 months"     ~  runif(n = 1, min = 1, max = 150),
      time == "Between 5 and 6 months" ~  runif(n = 1, min = 150, max = 180), 
      time == "Between 6 and 7 months" ~  runif(n = 1, min = 180, max = 210),
      time == "Between 7 and 8 months" ~  runif(n = 1, min = 210, max = 240),
      time == "Between 8 and 9 months" ~  runif(n = 1, min = 240, max = 270),
      time == "Between 9 and 10 months" ~ runif(n = 1, min = 270, max = 300),
      time == "Between 10 and 11 months" ~runif(n = 1, min = 300, max = 330),
      time == "Between 11 and 12 months" ~runif(n = 1, min = 330, max = 365),
      time == "Between 1 and 2 years" ~   runif(n = 1, min = 365, max = 365*2),
      time == "Between 2 and 3 years" ~   runif(n = 1, min = 365*2, max = 365*3), 
      time == "Between 3 and 4 years" ~   runif(n = 1, min = 365*3, max = 365*4),
      time == "Between 4 and 5 years" ~   runif(n = 1, min = 365*4, max = 365*5)
    )
  ) %>% 
  select(-id)

# 1.2 Do every survey respondent can have different value of time?


# exercise 2 --------------------------------------------------------------

# Use survminer::ggsurvplot function to show survey and transactional data on one 
# survival plot, and the clinical data with risk set table on the other plot.

# 2.0 Bind survey and transactional data.

ggsurv <- 
  
  # Few below lines can help customize the plot
  
  ggsurvplot(
    fit, 
    data = YOUR_DATA, 
    size = 1,                 # change line size
    palette = 
      c("#E7B800", "#2E9FDF"),# custom color palettes
    conf.int = TRUE,          # Add confidence interval
    pval = TRUE,              # Add p-value
    risk.table = TRUE,        # Add risk table
    risk.table.col = "strata",# Risk table color by groups
    legend.labs = 
      c("Male", "Female"),    # Change legend labels
    risk.table.height = 0.25, # Useful to change when you have multiple groups
    ggtheme = theme_bw()      # Change ggplot2 theme
  )

# 2.1 Try setting `xlim` and `break.time.by` parameters to adjust the OX axis.

# 2.2 Remove censoring marks from the plot.

# 2.3 Is the plot more informative when having `median survival pointer` (surv.median.line = "hv")

# 2.4 Let's remove text labels from risk set table legend and append them with colour bars

risk.table.y.text.col = T,# colour risk table text annotations.
# risk.table.height = 0.25, # the height of the risk table
risk.table.y.text = FALSE,# show bars instead of names in text annotations

# 2.5 Have you ever tried calling do.call(ggsurvplot, list_of_named_parameters)?


# exercise 3 --------------------------------------------------------------

# Try changing labels and fonts of the plot

ggsurv$plot <- ggsurv$plot + labs(
  title    = "Survival curves",                     
  subtitle = "Based on Kaplan-Meier estimates",  
  caption  = "created with survminer"             
)

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(
  title    = "Note the risk set sizes",          
  subtitle = "and remember about censoring.", 
  caption  = "source code: website.com"        
)

ggpar(
  ggsurv,
  font.title    = c(16, "bold", "darkblue"),         
  font.subtitle = c(15, "bold.italic", "purple"), 
  font.caption  = c(14, "plain", "orange"),        
  font.x        = c(14, "bold.italic", "red"),          
  font.y        = c(14, "bold.italic", "darkred"),      
  font.xtickslab = c(12, "plain", "darkgreen"),
  legend = "top"
)


# exercise 4 --------------------------------------------------------------

# 4.1 Create your own Kaplan-Meier estimates without using survfit and Surv functions.

# 4.2 Then create your own survival curves plot using ggplot2::ggplot and ggplot2::geom_step

# exercise 5 --------------------------------------------------------------

# 5.1 Divide survival curves by a categorical variable by modifying formula in the survfit function.

# 5.2 Divide the survival curves by a continuous variables. What are the options to cut the continuous variable into groups?

# 5.3 Use survminer::surv_cutpoint and surv::surv_categorize to divide continuous variable with the maxstat method.

# 5.4 Create your own code for the maxstat:
# a) cut continuous variable to, e.g., N pieces
# b) for each cutpoint calculate the logrank test (survival::survdiff)
# c) select the point that maximizes the statistic in the logrank test

######################################## Second hour should start here #####################################3


# exercise 6 --------------------------------------------------------------

# The Cox proportional hazards model makes sevral assumptions. 
# Thus, it is important to assess whether a fitted Cox regression model adequately describes the data.

# Here, we’ll disscuss three types of diagonostics for the Cox model:
# - Testing the proportional hazards assumption.
# - Examining influential observations (or outliers).
# - Detecting nonlinearity in relationship between the log hazard and the covariates.
# In order to check these model assumptions, Residuals method are used. The common residuals for the Cox model include:
# - Schoenfeld residuals to check the proportional hazards assumption
# - Martingale residual to assess nonlinearity
# - Deviance residual (symmetric transformation of the Martinguale residuals), to examine influential observations

# 6.1 Go over the cox ph diagnostics code.

library("survival")
res.cox <- coxph(Surv(time, status) ~ age + sex + wt.loss, data =  lung)
res.cox

#### The proportional hazards (PH) assumption can be checked using statistical 
# tests and graphical diagnostics based on the scaled Schoenfeld residuals.
test.ph <- cox.zph(res.cox)
test.ph

ggcoxzph(test.ph)
# In principle, the Schoenfeld residuals are independent of time. 
# A plot that shows a non-random pattern against time is evidence of violation of the PH assumption.

#### Testing influential observations
# To test influential observations or outliers, we can visualize either:
# - the deviance residuals or
# - the dfbeta values
ggcoxdiagnostics(fit, type = , linear.predictions = TRUE)
# linear.predictions: a logical value indicating whether to show linear predictions for observations (TRUE)
# or just indexed of observations (FALSE) on X axis.

ggcoxdiagnostics(res.cox, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
# The above index plots show that comparing the magnitudes of the largest dfbeta values 
# to the regression coefficients suggests that none of the observations is terribly 
# influential individually, even though some of the dfbeta values for age and wt.loss
# are large compared with the others.

#### Testing non linearity
# Often, we assume that continuous covariates have a linear form. However, this assumption should be checked
ggcoxfunctional(Surv(time, status) ~ age + log(age) + sqrt(age), data = lung)


# exercise 7 --------------------------------------------------------------

# 7.1 Based on the diagnostic for example `lung` datasets, provide diagnostic 
# of cox model assumptions for survey+transactional and clinical datasets.


# exercise 8 --------------------------------------------------------------

# Plot the baseline survival function
ggsurvplot(survfit(res.cox), color = "#2E9FDF", ggtheme = theme_minimal())
# Having fit a Cox model to the data, it’s possible to visualize the predicted survival
# proportion at any given point in time for a particular risk group. 
# The function survfit() estimates the survival proportion, by default at the mean values of covariates.


# exercise 9 --------------------------------------------------------------

# Based on the predict code for the lung example, create the predict for survey+transactional and clinical datasets.
# Create the new data  
sex_df <- with(lung,
               data.frame(sex = c(1, 2), 
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)
sex_df
# Survival curves
fit <- survfit(res.cox, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"), ggtheme = theme_minimal())



# exercise 9 --------------------------------------------------------------

# Fit complex survival curves

fit2 <- survfit( Surv(time, status) ~ sex + rx + adhere, data = colon )
ggsurv <- ggsurvplot(fit2, fun = "event", conf.int = TRUE, ggtheme = theme_bw())

ggsurv$plot + 
  theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(rx ~ adhere)
