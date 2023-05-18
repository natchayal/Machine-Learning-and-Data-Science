---
# title: "Predictors of arsenic content in California's groundwater"
# Description: "Perform statistical analysis on Arsenic data for Dr. Lu CGN 6311 Intro to Data Science Project."
# author: "Natchaya Luangphairin, Shelby Rocha, Mark Vicciardo"
# date last revised: "2/27/2023"
# output: R Script
---

# Libraries
  # install package using install.packages(" ") or development version from Github use devtools::instal_github("tidyverse/readr")
  # load the packages using library()
    # libraries
  # List out packages
    packages = c("ggplot2", "plyr","dplyr","shiny","statsr","plotly","grid","gridExtra",
      "readxl","readr","ggpubr","RColorBrewer","scales","naniar","tidyr","stringr","ggpubr",
      "ggthemes","janitor","binr","mltools","gtools","formattable","foreign","utils","lubridate",
      "data.table","hrbrthemes")


  # Now load, or install & load, all
    package.check <- lapply(
      packages,
      FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
          install.packages(x, dependencies = TRUE)
          library(x, character.only = TRUE)
        }
      }
    )


    https://github.com/DanielMartinAlarcon/arsenic-in-durango/blob/master/Arsenic1_simple_models.ipynb

##Predictors of arsenic content in goundwater
	#The State of California, in the United States, is one of those affected with significant natural presence of arsenic in groundwater 
	#(for a national analysis, see this previous post). One of the problems with studying arsenic is that it's expensive to measure it accurately. 
	#But what if we could reliably predict the arsenic content of a water sample, based on other characteristics of the water that are easier to determine?
	#I used a dataset of water samples from Durango, for which the levels of several common ions had been quantified. I trained several regression models on these data and found that potassium (K) is by far the factor that is most closely associated with arsenic presence in this dataset.
	#https://www.waterqualitydata.us/#statecode=US%3A06&siteType=Well&startDateLo=01-01-2015&mimeType=csv&sorted=no&providers=NWIS&providers=STEWARDS&providers=STORET

### Data cleanup ###
raw <- read.csv("C:/Users/nluan/Box Sync/CGN6311 Data Science Project/data/raw/CA_ARSENIC/resultphyschem.csv") # 336801 obs of 81 variables
df <- subset(raw, select = c("ActivityStartDate","CharacteristicName","ResultMeasureValue", "ResultMeasure.MeasureUnitCode","ResultStatusIdentifier")) 
# Assuming your data frame is named "df"
accepted_data <- df[df$ResultStatusIdentifier == "Accepted",] # 245848 of 5 variables

library(reshape2)
# Convert the ResultMeasureValue column to numeric
accepted_data$ResultMeasureValue <- as.numeric(accepted_data$ResultMeasureValue)

# Pivot the data to wide format with separate columns for each characteristic and units
wide_data <- dcast(accepted_data, ActivityStartDate ~ paste0(CharacteristicName, ".", ResultMeasure.MeasureUnitCode), value.var = "ResultMeasureValue", fun.aggregate = sum) # 707 obs. of 680 variables


# View the resulting data frame
head(wide_data)
# That's a lot of chemical data, we will only look at the common ones As, pH, conductivity, hardness, F, Na, K, Ca, Mg, nitrate, Cl, carbonate, bicarbonate, total alkalinity, sulfate
As <-  subset(wide_data, select = c("ActivityStartDate","Arsenic ion (3+).","Arsenic ion (3+).ug/l","Arsenic, Inorganic.","Arsenic, Inorganic.ug/l","Arsenic.","Arsenic.mg/L","Arsenic.ug/l","Arsenic.ug/L"))

# Subset the columns of interest
As <- subset(wide_data, select = c("ActivityStartDate","Arsenic ion (3+).","Arsenic ion (3+).ug/l","Arsenic, Inorganic.","Arsenic, Inorganic.ug/l","Arsenic.","Arsenic.mg/L","Arsenic.ug/l","Arsenic.ug/L"))
# Multiply mg/L values by 1000
mgL_cols <- grep("mg[/L]", colnames(As), value = TRUE)
As[, mgL_cols] <- As[, mgL_cols] * 1000
# Calculate the maximum value from the four columns and store it in As_ug.l
As_ug_cols <- c("Arsenic ion (3+).ug/l", "Arsenic, Inorganic.ug/l", "Arsenic.ug/l", "Arsenic.ug/L")
As$As_ug.L <- apply(As[, As_ug_cols], 1, function(x) ifelse(length(nz <- x[x != 0]) > 0, max(nz), 0))
As <- As[, c(1, ncol(As))]

pH <- subset(wide_data, select = c("ActivityStartDate","pH.None","pH.std units"))
pH_max <- do.call(pmax, c(pH[,c("pH.None", "pH.std units")], na.rm=TRUE))
pH$pH <- ifelse(pH_max > 0, pH_max, 0)
pH <- pH[, c(1, ncol(pH))]

conductivity <- subset(wide_data, select = c("ActivityStartDate","Specific conductance.mS/cm", "Specific conductance.uS/cm @25C"))
# Multiply mg/L values by 1000
mScm_cols <- grep("mS[/cm]", colnames(conductivity), value = TRUE)
conductivity[, mScm_cols] <- conductivity[, mScm_cols] * 1000
# Calculate the maximum value from the four columns and store it in As_ug.l
conductivity_uScm_cols <- c("Specific conductance.mS/cm", "Specific conductance.uS/cm @25C")
conductivity$conductivity_uS.cm <- apply(conductivity[, conductivity_uScm_cols], 1, function(x) ifelse(length(nz <- x[x != 0]) > 0, max(nz), 0))
conductivity <- conductivity[, c(1, ncol(conductivity))]

hardness <- subset(wide_data, select = c("ActivityStartDate","Hardness, Ca, Mg.","Hardness, Ca, Mg.mg/l CaCO3","Hardness, non-carbonate.mg/l CaCO3"))
hardness_max <- do.call(pmax, c(hardness[,c("Hardness, Ca, Mg.","Hardness, Ca, Mg.mg/l CaCO3","Hardness, non-carbonate.mg/l CaCO3")], na.rm=TRUE))
hardness$hardness <- ifelse(hardness_max > 0, hardness_max, 0)
hardness <- hardness[, c(1, ncol(hardness))]

F <- subset(wide_data, select = c("ActivityStartDate","Fluoride.","Fluoride.mg/l","Fluoride.mg/L"))
F_max <- do.call(pmax, c(F[,c("Fluoride.","Fluoride.mg/l","Fluoride.mg/L")], na.rm=TRUE))
F$F_mg.L <- ifelse(F_max > 0, F_max, 0)
F <- F[, c(1, ncol(F))]

Na <- subset(wide_data, select = c("ActivityStartDate","Sodium.","Sodium.mg/l","Sodium.mg/L"))
Na_max <- do.call(pmax, c(Na[,c("Sodium.","Sodium.mg/l","Sodium.mg/L")], na.rm=TRUE))
Na$Na_mg.L <- ifelse(Na_max > 0, Na_max, 0)
Na <- Na[, c(1, ncol(Na))]

K <- subset(wide_data, select = c("ActivityStartDate","Potassium.","Potassium.mg/l" ,"Potassium.mg/L"))
K_max <- do.call(pmax, c(K[,c("Potassium.","Potassium.mg/l" ,"Potassium.mg/L")], na.rm=TRUE))
K$K_mg.L <- ifelse(K_max > 0, K_max, 0)
K <- K[, c(1, ncol(K))]

Ca <- subset(wide_data, select = c("ActivityStartDate","Calcium.mg/l","Calcium.mg/L"))
Ca_max <- do.call(pmax, c(Ca[,c("Calcium.mg/l","Calcium.mg/L")], na.rm=TRUE))
Ca$Ca_mg.L <- ifelse(Ca_max > 0, Ca_max, 0)
Ca <- Ca[, c(1, ncol(Ca))]

Mg <- subset(wide_data, select = c("ActivityStartDate","Magnesium.","Magnesium.mg/l","Magnesium.mg/L"))
Mg_max <- do.call(pmax, c(Mg[,c("Magnesium.","Magnesium.mg/l","Magnesium.mg/L")], na.rm=TRUE))
Mg$Mg_mg.L <- ifelse(Mg_max > 0, Mg_max, 0)
Mg <- Mg[, c(1, ncol(Mg))]

nitrate <- subset(wide_data, select = c("ActivityStartDate","Nitrate.","Nitrate.mg/L","Nitrate.mg/l as N","Nitrate.mg/l asNO3"))
nitrate_max <- do.call(pmax, c(nitrate[,c("Nitrate.","Nitrate.mg/L","Nitrate.mg/l as N","Nitrate.mg/l asNO3")], na.rm=TRUE))
nitrate$nitrate_mg.L <- ifelse(nitrate_max > 0, nitrate_max, 0)
nitrate <- nitrate[, c(1, ncol(nitrate))]

Cl <- subset(wide_data, select = c("ActivityStartDate","Chloride.mg/l","Chloride.mg/L"))
Cl_max <- do.call(pmax, c(Cl[,c("Chloride.mg/l","Chloride.mg/L")], na.rm=TRUE))
Cl$Cl_mg.L <- ifelse(Cl_max > 0, Cl_max, 0)
Cl <- Cl[, c(1, ncol(Cl))]

carbonate <- subset(wide_data, select = c("ActivityStartDate","Carbonate.","Carbonate.mg/l","Carbonate.mg/l CaCO3**"))
carbonate_max <- do.call(pmax, c(carbonate[,c("Carbonate.","Carbonate.mg/l","Carbonate.mg/l CaCO3**")], na.rm=TRUE))
carbonate$carbonate_mg.L <- ifelse(carbonate_max > 0, carbonate_max, 0)
carbonate <- carbonate[, c(1, ncol(carbonate))]

bicarbonate <- subset(wide_data, select = c("ActivityStartDate","Bicarbonate.","Bicarbonate.mg/l","Bicarbonate.mg/l CaCO3**")) 
bicarbonate_max <- do.call(pmax, c(bicarbonate[,c("Bicarbonate.","Bicarbonate.mg/l","Bicarbonate.mg/l CaCO3**")], na.rm=TRUE))
bicarbonate$bicarbonate_mg.L <- ifelse(bicarbonate_max > 0, bicarbonate_max, 0)
bicarbonate <- bicarbonate[, c(1, ncol(bicarbonate))]

total_alkalinity <- subset(wide_data, select = c("ActivityStartDate","Alkalinity, bicarbonate.mg/l CaCO3**","Alkalinity, carbonate.mg/l CaCO3**" ,"Alkalinity, Hydroxide.mg/l CaCO3**","Alkalinity, total.mg/l CaCO3**","Alkalinity.","Alkalinity.mg/l CaCO3"))
total_alkalinity_max <- do.call(pmax, c(total_alkalinity[,c("Alkalinity, bicarbonate.mg/l CaCO3**","Alkalinity, carbonate.mg/l CaCO3**" ,"Alkalinity, Hydroxide.mg/l CaCO3**","Alkalinity, total.mg/l CaCO3**","Alkalinity.","Alkalinity.mg/l CaCO3")], na.rm=TRUE))
total_alkalinity$total_alkalinity_mg.L <- ifelse(total_alkalinity_max > 0, total_alkalinity_max, 0)
total_alkalinity <- total_alkalinity[, c(1, ncol(total_alkalinity))]

sulfate <- subset(wide_data, select = c("ActivityStartDate","Sulfate.","Sulfate.mg/l","Sulfate.mg/L"))
sulfate_max <- do.call(pmax, c(sulfate[,c("Sulfate.","Sulfate.mg/l","Sulfate.mg/L")], na.rm=TRUE))
sulfate$sulfate_mg.L <- ifelse(sulfate_max > 0, sulfate_max, 0)
sulfate <- sulfate[, c(1, ncol(sulfate))]


# create a list of dataframes that contain only the columns you want to merge
df_list <- list(As[c("ActivityStartDate", "As_ug.L")],
                pH[c("ActivityStartDate", "pH")],
                conductivity[c("ActivityStartDate", "conductivity_uS.cm")],
                hardness[c("ActivityStartDate", "hardness")],
                F[c("ActivityStartDate", "F_mg.L")],
                Na[c("ActivityStartDate", "Na_mg.L")],
                K[c("ActivityStartDate", "K_mg.L")],
                Ca[c("ActivityStartDate", "Ca_mg.L")],
                Mg[c("ActivityStartDate", "Mg_mg.L")],
                nitrate[c("ActivityStartDate", "nitrate_mg.L")],
                Cl[c("ActivityStartDate", "Cl_mg.L")],
                carbonate[c("ActivityStartDate", "carbonate_mg.L")],
                bicarbonate[c("ActivityStartDate", "bicarbonate_mg.L")],
                total_alkalinity[c("ActivityStartDate", "total_alkalinity_mg.L")],
                sulfate[c("ActivityStartDate", "sulfate_mg.L")])

# merge the dataframes in df_list by ActivityStartDate
merged_df <- Reduce(function(x, y) merge(x, y, by = "ActivityStartDate", all = TRUE), df_list)
merged_df <- rename(merged_df, Date = ActivityStartDate)
# convert the "Date" column to the date format
merged_df$Date <- as.Date(merged_df$Date)

write.csv(merged_df,"C:/Users/nluan/Box Sync/CGN6311 Data Science Project/data/processed/CA_water_samples_JNL.csv", row.names = FALSE)



######################################################################################################################################
df <- read.csv("C:/Users/nluan/Box Sync/CGN6311 Data Science Project/data/processed/CA_water_samples_JNL.csv") # 707 of 17 variables

# I assume that the sampling date is not important.
df = df[-1]
# Brief look at the clean dataframe
head(df)
# Remove any rows with missing values
df <- df[complete.cases(df),]


######## Data Exploration #########
# investigate association between arsenic and other features
# install.packages("corrplot")
# install.packages("ggcorrplot")
## use correlation plot to highlight the most correlated variables

# Original data
	# Non-log transform
	# Calculate mean and standard deviation of As_ug.L column in training data
	mean_As <- mean(df$As_ug.L)
	sd_As <- sd(df$As_ug.L)

	# Plot histogram of As_ug.L column with mean line
	hist(df$As_ug.L, breaks = 20, col = "steelblue",
	     main = "Histogram: Arsenic in the original data", xlab = expression(paste("Arsenic concentration (", mu, "g/L)")), ylab = "Number of samples with this value")
	abline(v = mean(df$As_ug.L), col = "red", lty = 2)
	legend("topright", legend = c("mean", "arsenic"), col = c("red", "steelblue"), lty = c(2,1), cex = 0.8)


	# Plot histogram of As_ug.L column with mean line
	library(scales)
	hist(log(df$As_ug.L), breaks = 20, col = "steelblue",
	     main = "Histogram: Arsenic in the original data (log-normal distribution)", 
	     xlab = expression(paste("Log-transformed arsenic concentration (", mu, "g/L)")),
	     ylab = "Number of samples with this value")
	abline(v = mean(log(df$As_ug.L)), col = "red", lty = 2)
	legend("topright", legend = c("mean", "log-arsenic"), col = c("red", "steelblue"), lty = c(2,1), cex = 0.8)
	# Add logarithmic x-axis with 10^x labels
	scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
	              labels = trans_format("log10", math_format(10^.x)))


	library(corrplot)
	corrplot::corrplot.mixed(cor(df[, -15]), upper="circle")
	library(ggcorrplot)
	ggcorrplot(cor(df[, -15]), hc.order = TRUE, type = "lower", lab = TRUE)

	# to get the top 5 cor with arsenic
	# calculate correlation matrix
	cor_matrix <- cor(df)
	# sort correlations in decreasing order and select top 5
	top_correlations <- sort(cor_matrix[,"As_ug.L"], decreasing = TRUE)[1:6]
	# print top 5 correlations
	print(top_correlations)

	# select top 5 being the following ions:  "bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L".


 #### Train/test sets ####
	#Throughout this notebook, I used cross-validation to test my models on the training dataset and then tested the final results on holdout test data. 
	#I really don't have enough values for the test data to be reliable, so I'll have to take those results with a grain of salt.

	#Set the seed to ensure that the results are reproducible:
	set.seed(1)
	#Split the data into training and testing sets
	train <- sample(1:dim(df)[1], dim(df)[1]*.75, replace=FALSE)
	test <- -train
	training.data <- df[train, ]
	testing.data <- df[test, ]
	#Check the dimensions of the training and testing datasets to ensure they were split correctly:
	dim(training.data)
	dim(testing.data)
	#Use the cor() function to compute the correlation between each pair of variables in the training dataset:
	cor(training.data)
	pairs(training.data)
	summary(df$As_ug.L)

	# Calculate mean and standard deviation of As_ug.L column in training data
	mean_As <- mean(training.data$As_ug.L)
	sd_As <- sd(training.data$As_ug.L)

	# Plot histogram of As_ug.L column with mean line
	hist(training.data$As_ug.L, breaks = 20, col = "steelblue",
	     main = "Histogram: Arsenic in the training data", xlab = expression(paste("Arsenic concentration (", mu, "g/L)")), ylab = "Number of samples with this value")
	abline(v = mean(training.data$As_ug.L), col = "red", lty = 2)
	legend("topright", legend = c("mean", "arsenic"), col = c("red", "steelblue"), lty = c(2,1), cex = 0.8)


	# Plot histogram of As_ug.L column with mean line
	library(scales)
	hist(log(training.data$A_ug.L), breaks = 20, col = "steelblue",
	     main = "Histogram: Arsenic in the training data (log-normal distribution)", 
	     xlab = expression(paste("Log-transformed arsenic concentration (", mu, "g/L)")),
	     ylab = "Number of samples with this value")
	abline(v = mean(log(training.data$As_ug.L)), col = "red", lty = 2)
	legend("topright", legend = c("mean", "log-arsenic"), col = c("red", "steelblue"), lty = c(2,1), cex = 0.8)
	# Add logarithmic x-axis with 10^x labels
	scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
	              labels = trans_format("log10", math_format(10^.x)))


 #### Train/test sets TOP 5 ####
	#Throughout this notebook, I used cross-validation to test my models on the training dataset and then tested the final results on holdout test data. 
	#I really don't have enough values for the test data to be reliable, so I'll have to take those results with a grain of salt.

	#Set the seed to ensure that the results are reproducible:
	set.seed(1)
	#Split the data into training and testing sets: split the data into 75% train and 25% test.
	train <- sample(1:dim(df)[1], dim(df)[1]*.75, replace=FALSE)
	test <- -train
	training.data <- df[train, c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")]
	testing.data <- df[test, c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")]

	#Check the dimensions of the training and testing datasets to ensure they were split correctly:
	dim(training.data)
	dim(testing.data)
	#Use the cor() function to compute the correlation between each pair of variables in the training dataset:
	cor(training.data)
	pairs(training.data)
	summary(df$As)


  ######## Baseline Model #######
	# Calculate the mean of the arsenic concentrations in the training set
	mean_as_training <- mean(training.data$As_ug.L)
	# Create a vector of predicted arsenic concentrations for the test set, using the mean of the training set
	pred_as_test <- rep(mean_as_training, length(testing.data$As_ug.L))
	# Calculate the RMSE and R-squared of the baseline model
	baseline_rmse <- sqrt(mean((pred_as_test - testing.data$As_ug.L)^2))
	baseline_rsq <- 1 - sum((testing.data$As_ug.L - pred_as_test)^2)/sum((testing.data$As_ug.L - mean(testing.data$As_ug.L))^2)
	# Create a table of the baseline model results
	baseline_table <- data.frame(Model = "Baseline", RMSE = baseline_rmse, R_squared = baseline_rsq)
	# Print the table
	print(baseline_table)

  #Perform LDA on the training data in order to predict mpg01 using the variables that 
	#seemed most associated with As_ug.L which is # select top 5 being the following ions:  Magnesium (Mg), Sodium (Na), Calcium (Ca), Potassium (K), and Chloride (Cl). 
	# What is the test error of the model obtained?
	library(MASS)
	lda.fit <- lda(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data)
	lda.fit
	plot(lda.fit)



  # Fit multiple linear regression model
	# Fit the model
	library(dplyr)
	# create a data frame to store the model results
	model_results <- data.frame(model = character(),
	                            R2 = numeric(),
	                            MSE = numeric(),
	                            stringsAsFactors = FALSE)
	# fit the model and calculate R^2 and MSE
	lm.fit <- lm(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data)
	R2 <- summary(lm.fit)$r.squared
	MSE <- mean(resid(lm.fit)^2)
	# add the results to the data frame
	model_results <- model_results %>%
	  add_row(model = "lm", R2 = R2, MSE = MSE)
	# view the table
	model_results

	install.packages("lme4")
	library(lme4)

  # multiple linear regression model
	# Fit initial model with all predictor variables
	lm.full <- lm(As_ug.L ~ ., data = training.data)
	# Perform stepwise selection to find best model
	lm.step <- step(lm.full, direction = "both")
	# View summary of best model
	summary(lm.step)
	# Compare full and best models
	anova(lm.full, lm.step)




	# Plot model 1 and model 2
	library(ggplot2)
	# MODEL 1: Fit the multiple linear regression model
	lm.fit <- lm(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data)
	# Make predictions using the model
	predictions <- predict(lm.fit, newdata = testing.data)
	# Calculate R-squared and MSE
	rsq <- summary(lm.fit)$r.squared
	mse <- mean((testing.data$As_ug.L - predictions)^2)
	# Plot the scatter plot with the regression line
	plot_data <- data.frame(As_ug.L = testing.data$As_ug.L, K_mg.L = testing.data$K_mg.L, predicted = predictions)

	ggplot(plot_data, aes(x = K_mg.L, y = As_ug.L)) +
	  geom_point(aes(color = "Actual data"), show.legend = TRUE) + 
	  geom_line(aes(y = predicted, color = "Modelled data"), linetype = "dashed", show.legend = TRUE) +
	  scale_color_manual(name = "Data type",
	                     values = c("Actual data" = "black", "Modelled data" = "red"),
	                     labels = c("Actual data", "Modelled data")) +
	  labs(title = "Model 1: Multiple Linear Regression Model of Arsenic and Potassium",
	       x = "Potassium concentration (mg/L)",
	       y = "Arsenic concentration (ug/L)",
	       caption = paste0("R-squared = ", round(rsq, 2), ", MSE = ", round(mse, 2))) +
	  theme_bw() +
	  #ylim(c(0,500)) +
	  theme(plot.title = element_text(hjust = 0.5),
	        legend.position = "bottom")



	# MODEL 2: Fit the single linear regression model
	lm.fit <- lm(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L, data = training.data)

	# Make predictions using the model
	predictions <- predict(lm.fit, newdata = testing.data)

	# Calculate R-squared and MSE
	rsq <- summary(lm.fit)$r.squared
	mse <- mean((testing.data$As_ug.L - predictions)^2)

	# Plot the scatter plot with the regression line
	plot_data <- data.frame(As_ug.L = testing.data$As_ug.L, K_mg.L = testing.data$K_mg.L, predicted = predictions)

	ggplot(plot_data, aes(x = K_mg.L, y = As_ug.L)) +
	  geom_point(aes(color = "Actual data"), show.legend = TRUE) + 
	  geom_line(aes(y = predicted, color = "Modelled data"), linetype = "dashed", show.legend = TRUE) +
	  scale_color_manual(name = "Data type",
	                     values = c("Actual data" = "black", "Modelled data" = "red"),
	                     labels = c("Actual data", "Modelled data")) +
	  labs(title = "Model 2: Multiple Linear Regression Model of Arsenic and Potassium",
	       x = "Potassium concentration (mg/L)",
	       y = "Arsenic concentration (ug/L)",
	       caption = paste0("R-squared = ", round(rsq, 2), ", MSE = ", round(mse, 2))) +
	  theme_bw() +
	  #ylim(c(0,500)) +
	  theme(plot.title = element_text(hjust = 0.5),
	        legend.position = "bottom")



	# perform cross validation

	library(caret)
	# MODEL 1
	# Define the training control
	train_control <- trainControl(method = "cv", number = 10)
	# Train the model using 10-fold cross-validation
	lm.fit <- train(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data, method = "lm", trControl = train_control)
	# Get the cross-validation results
	cv_results <- lm.fit$results
	cv_results



	# MODEL 2
	# Set up cross-validation
	train_control <- trainControl(method = "cv", number = 10)
	# Train the model using 10-fold cross-validation
	lm.fit <- train(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L, data = training.data, method = "lm", trControl = train_control)
	# Get the cross-validation results
	cv_results <- lm.fit$results
	# View the results
	cv_results






	## KNN take the training response variable separately so create train.arsenic data
	train.arsenic = df$As_ug.L[train]
	## we also need to have the testing separately for assessing the model later 
	test.arsenic= df$As_ug.L[test]


	# Using for loop to find he optimum K values
	library(class)
	set.seed(1)
	# try different values of k and store the error rate for each k
	error_rates <- c()
	for (k in 1:20) {
	  knn.pred.y <- knn(training.data, testing.data, train.arsenic, k = k)
	  error_rate <- mean(knn.pred.y != test.arsenic)
	  error_rates <- c(error_rates, error_rate)
	}

	# find the value of k that gives the lowest error rate
	optimal_k <- which.min(error_rates)

	# print the optimal k value and the corresponding error rate
	cat("Optimal k value:", optimal_k, "\n")
	cat("Error rate at optimal k:", error_rates[optimal_k], "\n")

	# plot error rate against k values
	plot(1:20, error_rates, type="b", pch=19, frame=FALSE, 
	     xlab="Number of Nearest Neighbors (k)", ylab="Error Rate")


	# Once the optimal value of k is selected using cross-validation, you can use the selected k to fit your KNN model on the training data. 
	library(ggplot2)
	# Fit KNN model with k=1
	knn.fit <- knn(train = training.data[, -1], test = testing.data[, -1], cl = training.data$As_ug.L, k = 1)
	# Plot the predicted vs. actual values
	plot(testing.data$As_ug.L, knn.fit, col = "blue",
	     main = "Predicted vs. Actual Values", xlab = "Actual values", ylab = "Predicted values")
	abline(0, 1, col = "red")

	# Create a data frame for the actual and predicted values
	results <- data.frame(actual = testing.data$As_ug.L, predicted = knn.fit)
	# Create a scatter plot with the actual values on the x-axis and predicted values on the y-axis
	ggplot(results, aes(x = actual, y = predicted)) +
	  geom_point(color = "black") +
	  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
	  labs(title = "K-Nearest Neighbors (K = 1) Predicted vs. Actual Values of Arsenic", x = "Actual values", y = "Predicted values") +
	  theme_bw()






	# Decision Trees

	install.packages("rpart.plot")
	library(rpart)
	library(rpart.plot)
	dt.fit <- rpart(As_ug.L ~ ., data = training.data, method = "anova")
	predictions <- predict(dt.fit, testing.data)
	mse <- mean((testing.data$As_ug.L - predictions)^2)
	rpart.plot(dt.fit, main = "Decision Tree for Arsenic Concentration", extra = 101)


	# Make predictions on the test data
	predictions <- predict(dt.fit, newdata = testing.data)
	# Calculate R-squared and MSE
	rsq <- cor(testing.data$As_ug.L, predictions)^2
	mse <- mean((testing.data$As_ug.L - predictions)^2)
	# Plot the actual vs. predicted values
	plot(testing.data$As_ug.L, predictions,
	     xlab = "Actual As_ug.L", ylab = "Predicted As_ug.L",
	     main = paste0("Decision Tree Model: R-squared = ", round(rsq, 2), ", MSE = ", round(mse, 2)))
	abline(0, 1, col = "red")



	# log-normal distribution
	training.data$log_As_ug.L <- log(training.data$As_ug.L)
	# Fit the model
	lm.fit <- lm(log_As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data)

	# Make predictions using the model
	predictions <- exp(predict(lm.fit, newdata = testing.data))

	# Calculate R-squared and MSE on original scale
	rsq <- summary(lm.fit)$r.squared
	mse <- mean((testing.data$As_ug.L - predictions)^2)

	# Plot the scatter plot with the regression line on original scale
	plot_data <- data.frame(As_ug.L = testing.data$As_ug.L, K_mg.L = testing.data$K_mg.L, predicted = predictions)

	ggplot(plot_data, aes(x = K_mg.L, y = As_ug.L)) +
	  geom_point(aes(color = "Actual data"), show.legend = TRUE) + 
	  geom_line(aes(y = predicted, color = "Modelled data"), linetype = "dashed", show.legend = TRUE) +
	  scale_color_manual(name = "Data type",
	                     values = c("Actual data" = "black", "Modelled data" = "red"),
	                     labels = c("Actual data", "Modelled data")) +
	  labs(title = "Model 1: Multiple Linear Regression Model of Arsenic and Potassium",
	       x = "Potassium concentration (mg/L)",
	       y = "Arsenic concentration (ug/L)",
	       caption = paste0("R-squared = ", round(rsq, 2), ", MSE = ", round(mse, 2))) +
	  theme_bw() +
	  theme(plot.title = element_text(hjust = 0.5),
	        legend.position = "bottom")










#################################################################################################################
df <- read.csv("C:/Users/nluan/Box Sync/CGN6311 Data Science Project/data/processed/CA_water_samples_JNL.csv") # 707 of 17 variables

# I assume that the sampling date is not important.
df = df[-1]
# Brief look at the clean dataframe
head(df)
# Remove any rows with missing values
df <- df[complete.cases(df),]

# Split the data into training and testing sets
set.seed(1)
#Split the data into training and testing sets: split the data into 75% train and 25% test.
train <- sample(1:dim(df)[1], dim(df)[1]*.75, replace=FALSE)
test <- -train
training.data <- df[train, c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")]
testing.data <- df[test, c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")]

	# to get the top 5 cor with arsenic
	# calculate correlation matrix
	cor_matrix <- cor(df)
	# sort correlations in decreasing order and select top 5
	top_correlations <- sort(cor_matrix[,"As_ug.L"], decreasing = TRUE)[1:6]
	# print top 5 correlations
	print(top_correlations)

# original data use the non-tranformed df
	# Set up panel for multiple plots
	par(mfrow=c(2,3))
	# Iterate over variable names and create histograms
	for (var in c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")) {
	  # Create histogram for variable
	  hist(df[,var], breaks = 20, col = "steelblue",
	       main = paste("Histogram: ", var), 
	       xlab = paste(var, "concentration"),
	       ylab = "Number of samples with this value")
	  # Add mean line
	  abline(v = mean(df[,var]), col = "red", lty = 2)
	  # Add legend
	  legend("topright", legend = c("mean", var), col = c("red", "steelblue"), lty = c(2,1), cex = 0.8)
	}

	#End previous plot and reset graphics settings
		dev.off()

	# log-normal plot for all variables
	# Create a list of variable names
	vars <- c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")
	# Set up the plot grid
	par(mfrow=c(2,3))
	# Loop through each variable
	for (i in 1:length(vars)) {
	  # Plot histogram of variable with mean line
	  hist(log(df[,vars[i]]), breaks = 20, col = "steelblue",
	       main = paste("Histogram:", vars[i]), 
	       xlab = paste("Log-transformed", vars[i]),
	       ylab = "Number of samples with this value")
	  abline(v = mean(log(df[,vars[i]])), col = "red", lty = 2)
	  legend("topright", legend = c("mean", paste("log-",vars[i])), col = c("red", "steelblue"), lty = c(2,1), cex = 0.8)
	  
	  # Add logarithmic x-axis with 10^x labels
	  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
	                labels = trans_format("log10", math_format(10^.x)))
	}

	#End previous plot and reset graphics settings
	dev.off()


# on training and testing data for top 5
	# distribution plot for all variables
	# Set up panel for multiple plots
	par(mfrow=c(2,3))
	# Iterate over variable names and create histograms
	for (var in c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")) {
	  # Create histogram for variable
	  hist(training.data[,var], breaks = 20, col = "steelblue",
	       main = paste("Histogram: ", var), 
	       xlab = paste(var, "concentration"),
	       ylab = "Number of samples with this value")
	  # Add mean line
	  abline(v = mean(training.data[,var]), col = "red", lty = 2)
	  # Add legend
	  legend("topright", legend = c("mean", var), col = c("red", "steelblue"), lty = c(2,1), cex = 0.8)
	}

	#End previous plot and reset graphics settings
	dev.off()

	# log-normal plot for all variables
	# Create a list of variable names
	vars <- c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")
	# Set up the plot grid
	par(mfrow=c(2,3))
	# Loop through each variable
	for (i in 1:length(vars)) {
	  # Plot histogram of variable with mean line
	  hist(log(training.data[,vars[i]]), breaks = 20, col = "steelblue",
	       main = paste("Histogram:", vars[i], "(train)"), 
	       xlab = paste("Log-transformed", vars[i]),
	       ylab = "Number of samples with this value")
	  abline(v = mean(log(training.data[,vars[i]])), col = "red", lty = 2)
	  legend("topright", legend = c("mean", paste("log-",vars[i])), col = c("red", "steelblue"), lty = c(2,1), cex = 0.8)
	  
	  # Add logarithmic x-axis with 10^x labels
	  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
	                labels = trans_format("log10", math_format(10^.x)))
	}
	#End previous plot and reset graphics settings
	dev.off()



df <- read.csv("C:/Users/nluan/Box Sync/CGN6311 Data Science Project/data/processed/CA_water_samples_JNL.csv") # 707 of 17 variables
# I assume that the sampling date is not important.
df = df[-1]
# Brief look at the clean dataframe
head(df)
# Remove any rows with missing values
df <- df[complete.cases(df),]

# Log transform all variables in the data
	df$As_ug.L <- log(df$As_ug.L)
	df$bicarbonate_mg.L <- log(df$bicarbonate_mg.L)
	df$total_alkalinity_mg.L <- log(df$total_alkalinity_mg.L)
	df$F_mg.L <- log(df$F_mg.L)
	df$K_mg.L <- log(df$K_mg.L)
	df$carbonate_mg.L <- log(df$carbonate_mg.L)

	# Filter out rows with -Inf values after log transformation
	df <- as.data.frame(df)
	# Replace -Inf values with a small number
	df[df == -Inf] <- 1e-6

#df <- subset(df, select = c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L"))
	library(corrplot)
	corrplot::corrplot.mixed(cor(df[, -15]), upper="circle")
	library(ggcorrplot)
	ggcorrplot(cor(df[, -15]), hc.order = TRUE, type = "lower", lab = TRUE)

	# to get the top 5 cor with arsenic
	# calculate correlation matrix
	cor_matrix <- cor(df)
	# sort correlations in decreasing order and select top 5
	top_correlations <- sort(cor_matrix[,"As_ug.L"], decreasing = TRUE)[1:6]
	# print top 5 correlations
	print(top_correlations)

	# select top 5 being the following ions:  "bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L".


	# Split the data into training and testing sets
	set.seed(1)
	#Split the data into training and testing sets: split the data into 75% train and 25% test.
	train <- sample(1:dim(df)[1], dim(df)[1]*.75, replace=FALSE)
	test <- -train
	training.data <- df[train, c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")]
	testing.data <- df[test, c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")]


	#Check the dimensions of the training and testing datasets to ensure they were split correctly:
	dim(training.data)
	dim(testing.data)
	#Use the cor() function to compute the correlation between each pair of variables in the training dataset:
	cor(training.data)
	pairs(training.data)
	summary(df$As_ug.L)

######## Baseline Model #######
	# Calculate the mean of the arsenic concentrations in the training set
	mean_as_training <- mean(training.data$As_ug.L)
	# Create a vector of predicted arsenic concentrations for the test set, using the mean of the training set
	pred_as_test <- rep(mean_as_training, length(testing.data$As_ug.L))
	# Calculate the RMSE and R-squared of the baseline model
	baseline_rmse <- sqrt(mean((pred_as_test - testing.data$As_ug.L)^2))
	baseline_rsq <- 1 - sum((testing.data$As_ug.L - pred_as_test)^2)/sum((testing.data$As_ug.L - mean(testing.data$As_ug.L))^2)
	# Create a table of the baseline model results
	baseline_table <- data.frame(Model = "Baseline", RMSE = baseline_rmse, R_squared = baseline_rsq)
	# Print the table
	print(baseline_table)

  #Perform LDA on the training data in order to predict mpg01 using the variables that 
	#seemed most associated with As_ug.L which is # select top 5 being the following ions:  Magnesium (Mg), Sodium (Na), Calcium (Ca), Potassium (K), and Chloride (Cl). 
	# What is the test error of the model obtained?
	library(MASS)
	lda.fit <- lda(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data)
	lda.fit
	plot(lda.fit)



  # Fit multiple linear regression model
	# Fit the model
	library(dplyr)
	# create a data frame to store the model results
	model_results <- data.frame(model = character(),
	                            R2 = numeric(),
	                            MSE = numeric(),
	                            stringsAsFactors = FALSE)
	# fit the model and calculate R^2 and MSE
	lm.fit <- lm(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data)
	R2 <- summary(lm.fit)$r.squared
	MSE <- mean(resid(lm.fit)^2)
	# add the results to the data frame
	model_results <- model_results %>%
	  add_row(model = "lm", R2 = R2, MSE = MSE)
	# view the table
	model_results

	install.packages("lme4")
	library(lme4)

  # multiple linear regression model
	# Fit initial model with all predictor variables
	lm.full <- lm(As_ug.L ~ ., data = training.data)
	# Perform stepwise selection to find best model
	lm.step <- step(lm.full, direction = "both")
	# View summary of best model
	summary(lm.step)
	# Compare full and best models
	anova(lm.full, lm.step)




	# Plot model 1 and model 2
	library(ggplot2)
	# MODEL 1: Fit the multiple linear regression model
	lm.fit <- lm(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data)
	# Make predictions using the model
	predictions <- predict(lm.fit, newdata = testing.data)
	# Calculate R-squared and MSE
	rsq <- summary(lm.fit)$r.squared
	mse <- mean((testing.data$As_ug.L - predictions)^2)
	# Plot the scatter plot with the regression line
	plot_data <- data.frame(As_ug.L = testing.data$As_ug.L, K_mg.L = testing.data$K_mg.L, predicted = predictions)

	ggplot(plot_data, aes(x = K_mg.L, y = As_ug.L)) +
	  geom_point(aes(color = "Actual data"), show.legend = TRUE) + 
	  geom_line(aes(y = predicted, color = "Modelled data"), linetype = "dashed", show.legend = TRUE) +
	  scale_color_manual(name = "Data type",
	                     values = c("Actual data" = "black", "Modelled data" = "red"),
	                     labels = c("Actual data", "Modelled data")) +
	  labs(title = "Model 1: Multiple Linear Regression Model of Arsenic and Potassium",
	       x = "Log Potassium concentration (mg/L)",
	       y = "Log Arsenic concentration (ug/L)",
	       caption = paste0("R-squared = ", round(rsq, 2), ", MSE = ", round(mse, 2))) +
	  theme_bw() +
	  #ylim(c(0,500)) +
	  theme(plot.title = element_text(hjust = 0.5),
	        legend.position = "bottom")



	# MODEL 2: Fit the single linear regression model
	lm.fit <- lm(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L, data = training.data)

	# Make predictions using the model
	predictions <- predict(lm.fit, newdata = testing.data)

	# Calculate R-squared and MSE
	rsq <- summary(lm.fit)$r.squared
	mse <- mean((testing.data$As_ug.L - predictions)^2)

	# Plot the scatter plot with the regression line
	plot_data <- data.frame(As_ug.L = testing.data$As_ug.L, K_mg.L = testing.data$K_mg.L, predicted = predictions)

	ggplot(plot_data, aes(x = K_mg.L, y = As_ug.L)) +
	  geom_point(aes(color = "Actual data"), show.legend = TRUE) + 
	  geom_line(aes(y = predicted, color = "Modelled data"), linetype = "dashed", show.legend = TRUE) +
	  scale_color_manual(name = "Data type",
	                     values = c("Actual data" = "black", "Modelled data" = "red"),
	                     labels = c("Actual data", "Modelled data")) +
	  labs(title = "Model 2: Multiple Linear Regression Model of Arsenic and Potassium",
	       x = "Log Potassium concentration (mg/L)",
	       y = "Log Arsenic concentration (ug/L)",
	       caption = paste0("R-squared = ", round(rsq, 2), ", MSE = ", round(mse, 2))) +
	  theme_bw() +
	  #ylim(c(0,500)) +
	  theme(plot.title = element_text(hjust = 0.5),
	        legend.position = "bottom")



	# perform cross validation

	library(caret)
	# MODEL 1
	# Define the training control
	train_control <- trainControl(method = "cv", number = 10)
	# Train the model using 10-fold cross-validation
	lm.fit <- train(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = training.data, method = "lm", trControl = train_control)
	# Get the cross-validation results
	cv_results <- lm.fit$results
	cv_results



	# MODEL 2
	# Set up cross-validation
	train_control <- trainControl(method = "cv", number = 10)
	# Train the model using 10-fold cross-validation
	lm.fit <- train(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L, data = training.data, method = "lm", trControl = train_control)
	# Get the cross-validation results
	cv_results <- lm.fit$results
	# View the results
	cv_results






	## KNN take the training response variable separately so create train.arsenic data
	train.arsenic = df$As_ug.L[train]
	## we also need to have the testing separately for assessing the model later 
	test.arsenic= df$As_ug.L[test]


	# Using for loop to find he optimum K values
	library(class)
	set.seed(1)
	# try different values of k and store the error rate for each k
	error_rates <- c()
	for (k in 1:20) {
	  knn.pred.y <- knn(training.data, testing.data, train.arsenic, k = k)
	  error_rate <- mean(knn.pred.y != test.arsenic)
	  error_rates <- c(error_rates, error_rate)
	}

	# find the value of k that gives the lowest error rate
	optimal_k <- which.min(error_rates)

	# print the optimal k value and the corresponding error rate
	cat("Optimal k value:", optimal_k, "\n")
	cat("Error rate at optimal k:", error_rates[optimal_k], "\n")

	# plot error rate against k values
	plot(1:20, error_rates, type="b", pch=19, frame=FALSE, 
	     xlab="Number of Nearest Neighbors (k)", ylab="Error Rate")


	# Once the optimal value of k is selected using cross-validation, you can use the selected k to fit your KNN model on the training data. 
	library(ggplot2)
	# Fit KNN model with k=1
	knn.fit <- knn(train = training.data[, -1], test = testing.data[, -1], cl = training.data$As_ug.L, k = 1)
	# Plot the predicted vs. actual values
	plot(testing.data$As_ug.L, knn.fit, col = "blue",
	     main = "Predicted vs. Actual Values", xlab = "Actual values", ylab = "Predicted values")
	abline(0, 1, col = "red")

	# Create a data frame for the actual and predicted values
	results <- data.frame(actual = testing.data$As_ug.L, predicted = knn.fit)
	# Create a scatter plot with the actual values on the x-axis and predicted values on the y-axis
	ggplot(results, aes(x = actual, y = predicted)) +
	  geom_point(color = "black") +
	  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
	  labs(title = "K-Nearest Neighbors (K = 1) Predicted vs. Actual Values of Arsenic", x = "Actual values", y = "Predicted values") +
	  theme_bw()






	# Decision Trees

	install.packages("rpart.plot")
	library(rpart)
	library(rpart.plot)
	dt.fit <- rpart(As_ug.L ~ ., data = training.data, method = "anova")
	predictions <- predict(dt.fit, testing.data)
	mse <- mean((testing.data$As_ug.L - predictions)^2)
	rpart.plot(dt.fit, main = "Decision Tree for Log Arsenic Concentration", extra = 101)


	# Make predictions on the test data
	predictions <- predict(dt.fit, newdata = testing.data)
	# Calculate R-squared and MSE
	rsq <- cor(testing.data$As_ug.L, predictions)^2
	mse <- mean((testing.data$As_ug.L - predictions)^2)
	# Plot the actual vs. predicted values
	plot(testing.data$As_ug.L, predictions,
	     xlab = "Actual Log (As_ug.L)", ylab = "Predicted Log (As_ug.L)",
	     main = paste0("Decision Tree Model: R-squared = ", round(rsq, 2), ", MSE = ", round(mse, 2)))
	abline(0, 1, col = "red")




# building a random forest model and plotting the variable importance:
library(randomForest)

# Build random forest model and Train random forest model
set.seed(1)
rf_model <- randomForest(As_ug.L ~ bicarbonate_mg.L + total_alkalinity_mg.L + F_mg.L + K_mg.L + carbonate_mg.L, data = df, ntree = 500, mtry = 3)
# Print model summary
print(rf_model)
# Plot variable importance
varImpPlot(rf_model, type = 2, main = "Variable Importance Plot")



library(ggplot2)
library(gridExtra)
library(cowplot)
# Predict arsenic concentration on test data
test_pred <- predict(rf_model, newdata=testing.data)
# Create data frame with actual and predicted values
test_results <- data.frame(Actual=testing.data$As_ug.L, Predicted=test_pred)

# Calculate R-squared and RMSE
library(DescTools)
# Calculate R-squared and MSE
r_squared <- R2(test_pred, testing.data$As_ug.L)
mse <- mean((test_pred - testing.data$As_ug.L)^2)

# Plot predicted versus actual with R-squared and RMSE
p1 <- ggplot(test_results, aes(x=Actual, y=Predicted)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  xlab("Actual Log-transformed As_ug.L") + ylab("Predicted Log-transformed As_ug.L") +
  ggtitle("Random Forest Model Predictions vs. Actual Log Arsenic Concentration Data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom")

p2 <- grid.arrange(
  tableGrob(data.frame(R_squared = round(r_squared, 3), MSE = round(mse, 3)), rows = NULL),
  ncol = 2, widths = c(0.5, 0.5), bottom = 0.2)

plot_grid(p1, p2, ncol = 1, align = "v", axis = "lr", rel_heights = c(0.8, 0.2))




########## 
#perform cross validation to compare the performance of multiple linear regression, decision tree, 
#and random forest using cross-validation and displaying the results in a nice table

library(caret)
library(plyr)
library(dplyr)
library(kableExtra)

set.seed(1)

# Split the data into training and testing sets: split the data into 75% train and 25% test.
train <- sample(1:dim(df)[1], dim(df)[1]*.75, replace=FALSE)
test <- -train
training.data <- df[train, c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")]
testing.data <- df[test, c("As_ug.L","bicarbonate_mg.L","total_alkalinity_mg.L","F_mg.L","K_mg.L","carbonate_mg.L")]

# Define models
lm_model <- train(As_ug.L ~ ., data = training.data, method = "lm", trControl = trainControl(method = "cv", number = 5))
dt_model <- train(As_ug.L ~ ., data = training.data, method = "rpart", trControl = trainControl(method = "cv", number = 5))
rf_model <- train(As_ug.L ~ ., data = training.data, method = "rf", trControl = trainControl(method = "cv", number = 5))

# Collect performance metrics
lm_metrics <- data.frame(
  Method = "Multiple Linear Regression",
  R_squared = lm_model$results$Rsquared,
  RMSE = lm_model$results$RMSE
)
dt_metrics <- data.frame(
  Method = "Decision Tree",
  R_squared = dt_model$results$Rsquared,
  RMSE = dt_model$results$RMSE
)
rf_metrics <- data.frame(
  Method = "Random Forest",
  R_squared = rf_model$results$Rsquared,
  RMSE = rf_model$results$RMSE
)

# Combine results into a table
results_table <- rbind(lm_metrics, dt_metrics, rf_metrics) %>%
  arrange(case_when(Method == "Multiple Linear Regression" ~ 1,
                    Method == "Decision Tree" ~ 2,
                    Method == "Random Forest" ~ 3),
          R_squared, desc(RMSE)) %>%
  mutate(R_squared = round(R_squared, 3),
         RMSE = round(RMSE, 3))

# Display table using kableExtra
results_table %>%
  kable("html", align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


# Find optimal mtry to use for decision tree and random forest
library(randomForest)

# Load data
data(iris)

# Split data into training and testing sets
trainIndex <- sample(1:nrow(iris), round(nrow(iris)*0.8))
trainData <- iris[trainIndex,]
testData <- iris[-trainIndex,]

# Tune mtry value
set.seed(123)
tuneResult <- tuneRF(trainData[,1:4], trainData[,5], ntree=500, stepFactor=1.5, trace=TRUE, plot=TRUE)

# Print results
print(tuneResult)

# Build final model using optimal mtry value
finalModel <- randomForest(Species ~ ., data=trainData, mtry=tuneResult[["bestMtry"]], ntree=500)
