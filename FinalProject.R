# Final-Project

# Part 1: Load data set and data cleaning

# setting working directory and reading data from csv file
setwd("C:/Users/cassi/Final-Project")
maternal_data <- read.csv("Maternal_Health_Risk_Assessment_Original_Dataset.csv", 
                                   sep = ",", header = TRUE)
# clean up the column names
setnames(maternal_data, "Blood.glucose", "BloodGlucose")

# convert to data table
library(data.table)
maternal_dt <- as.data.table(maternal_data)

# convert to numeric
cols <- c("Age", "SystolicBP", "DiastolicBP", "BloodGlucose", "BodyTemp", 
          "HeartRate")
maternal_dt[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

# Part 2: Generate summary statistics

# summary statistics to see differences by RiskLevel
summary_by_risk <- maternal_dt[, lapply(.SD, mean), by=RiskLevel]
print(summary_by_risk)

# Part 3: Reshape the data

# put in long format for visualizations
maternal_long <- melt(
  maternal_dt,
  id.vars = "RiskLevel",
  measure.vars = c("Age", "SystolicBP", "DiastolicBP", "BloodGlucose", 
                   "BodyTemp", "HeartRate"),
  variable.name = "Indicator",
  value.name = "Value"
)

# Part 4: Create data visualization

# box plot by risk level
library("ggplot2")
ggplot(maternal_long, aes(x = factor(RiskLevel), 
                          y = Value, fill = factor(RiskLevel))) +
  geom_boxplot() +
  facet_wrap(~Indicator, scales = "free_y") +
  labs(x = "Risk Level", y = "Value", 
       title = "Health Indicators vs Risk Level")

ggsave("maternal_data_visualization.png", 
       width = 10, height = 7)
shell.exec("maternal_data_visualization.png")


# Part 5: Build linear regression model

# create linear regression model for metrics against risk level
maternal_model <- lm(as.numeric(RiskLevel) ~ Age 
                     + SystolicBP 
                     + DiastolicBP 
                     + BloodGlucose 
                     + BodyTemp 
                     + HeartRate, 
                     data = maternal_dt)
summary(maternal_model)

# Part 6: Create prediction function

# we can calculate the coefficients from the linear model for an equation that 
# shows the weight (influence) of each predictor
coef(maternal_model)

# create a function to plug in metrics to predict risk
predict_risk <- function(Age, SystolicBP, 
                         DiastolicBP, BloodGlucose, 
                         BodyTemp, HeartRate) {
  -18.68303 +
    (-0.0005867 * Age) +
    (0.01201164 * SystolicBP) +
    (0.0008868 * DiastolicBP) +
    (0.1140881 * BloodGlucose) +
    (0.1656677 * BodyTemp) +
    (0.0107635 * HeartRate)
}

# example 
predict_risk(Age = 35, SystolicBP = 120, DiastolicBP = 90, BloodGlucose = 15, BodyTemp = 98, HeartRate = 85)
round(predict_risk(35, 120, 75, 15, 98, 85))

# Part 7: Fit linear models for each metric compared to age

# Create linear model for metrics over age, then predict with confidence 
# interval and plot

# SystolicBP vs Age
lm_sys <- lm(SystolicBP ~ Age, data = maternal_dt)
pred_sys <- predict(lm_sys, interval = "confidence")
summary(lm_sys)

ggplot(maternal_dt, aes(x = Age, y = SystolicBP)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_sys[, "fit"]), color = "red") +
  geom_ribbon(aes(ymin = pred_sys[, "lwr"], ymax = pred_sys[, "upr"]), alpha = 0.3) +
  labs(title = "SystolicBP vs Age with 95% CI", x = "Age", y = "SystolicBP")

ggsave("SystolicBP_vs_Age.png", width = 7, height = 5)
shell.exec("SystolicBP_vs_Age.png")

# DiastolicBP vs Age
lm_dia <- lm(DiastolicBP ~ Age, data = maternal_dt)
pred_dia <- predict(lm_dia, interval = "confidence")
summary(lm_dia)

ggplot(maternal_dt, aes(x = Age, y = DiastolicBP)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_dia[, "fit"]), color = "red") +
  geom_ribbon(aes(ymin = pred_dia[, "lwr"], ymax = pred_dia[, "upr"]), alpha = 0.3) +
  labs(title = "DiastolicBP vs Age with 95% CI", x = "Age", y = "DiastolicBP")

ggsave("DiastolicBP_vs_Age.png", width = 7, height = 5)
shell.exec("DiastolicBP_vs_Age.png")

# BloodGlucose vs Age
lm_bgl <- lm(BloodGlucose ~ Age, data = maternal_dt)
pred_bgl <- predict(lm_bgl, interval = "confidence")
summary(lm_bgl)

ggplot(maternal_dt, aes(x = Age, y = BloodGlucose)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_bgl[, "fit"]), color = "red") +
  geom_ribbon(aes(ymin = pred_bgl[, "lwr"], ymax = pred_bgl[, "upr"]), alpha = 0.3) +
  labs(title = "Blood Glucose vs Age with 95% CI", x = "Age", y = "BloodGlucose")

ggsave("BloodGlucose_vs_Age.png", width = 7, height = 5)
shell.exec("BloodGlucose_vs_Age.png")

# BodyTemp vs Age
lm_temp <- lm(BodyTemp ~ Age, data = maternal_dt)
pred_temp <- predict(lm_temp, interval = "confidence")
summary(lm_temp)

ggplot(maternal_dt, aes(x = Age, y = BodyTemp)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_temp[, "fit"]), color = "red") +
  geom_ribbon(aes(ymin = pred_temp[, "lwr"], ymax = pred_temp[, "upr"]), alpha = 0.3) +
  labs(title = "Body Temperature vs Age with 95% CI", x = "Age", y = "BodyTemp")

ggsave("BodyTemp_vs_Age.png", width = 7, height = 5)
shell.exec("BodyTemp_vs_Age.png")

# HeartRate vs Age
lm_hr <- lm(HeartRate ~ Age, data = maternal_dt)
pred_hr <- predict(lm_hr, interval = "confidence")
summary(lm_hr)

ggplot(maternal_dt, aes(x = Age, y = HeartRate)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_hr[, "fit"]), color = "red") +
  geom_ribbon(aes(ymin = pred_hr[, "lwr"], ymax = pred_hr[, "upr"]), alpha = 0.3) +
  labs(title = "Heart Rate vs Age with 95% CI", x = "Age", y = "HeartRate")

ggsave("HeartRate_vs_Age.png", width = 7, height = 5)
shell.exec("HeartRate_vs_Age.png")

