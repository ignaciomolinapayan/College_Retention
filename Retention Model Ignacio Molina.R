
library(tidyverse)
library(caret)
library(sassy)
library(knitr)
library(kableExtra)
library(pandoc)
library("pROC")
##########################################################
# Import file to be used for the retention model
##########################################################

Retention <- read.csv("~/Hardvard X/Capstone/Retention/Retention.csv")

Retrain <- Retention %>% filter(AcadYr > 2019 & AcadYr <2025)

#Code 1 Converting Our Dependent Variable into Numeric Value
Retrain$rsem <- ifelse(Retrain$NxtSem == "Y",1,0)
Retrain$pel <- ifelse(Retrain$Pell == "1", 1,0)

#Descriptive Statistics for Average Attempted Hours
Athrs <-Retrain %>% group_by(rsem) %>% proc_means(var = AvAthr, class = rsem) %>% mutate(Retention = ifelse(CLASS ==1, "Retained","Not Retained")) %>% mutate(Variable = "Attempted Horus")

#Descriptive Statistics for Age
Age <- Retrain %>% group_by(rsem) %>% proc_means(var = Age, class = rsem) %>% 
  mutate(Retention = ifelse(CLASS ==1, "Retained","Not Retained")) %>%
  mutate(Variable = "Age")

#Descriptive Statistics for GPA
Gpa <- Retrain %>% group_by(rsem) %>% proc_means(var = gpa, class = rsem) %>% 
  mutate(Retention = ifelse(CLASS ==1, "Retained","Not Retained")) %>%
  mutate(Variable = "GPA")

#Append or stack all Descriptive Statistics
Descriptive <- rbind(Athrs, Age, Gpa)

#Reorder and round variables  to the second decimal place 
Descriptive <- subset(Descriptive, select = -c( CLASS, VAR)) %>% 
  mutate(across(c('MEAN', 'STD', 'MIN'), round,2))  #this help us round to a particular decimal place 

#Remove Missing Observations
Descriptive <- Descriptive[!is.na(Descriptive$Retention),]
#Reorder Variables 
Descriptive <- Descriptive %>% select(Variable, Retention, N, MEAN, STD, MIN, MAX) 

# Retrieve Frequencies for Categorical Variable: Pell
nopel <- Retrain %>% filter(pel==0)%>%proc_freq( tables = rsem)
pel <- Retrain %>% filter(pel==1) %>%proc_freq(tables =  rsem)
pells <- rbind(nopel,pel) 

as.numeric(pells$PCT)

a <- pells[1:4, "PCT"]

#Generate a plot for Pell Student Retention
barplot(a, names.arg = c("No Pell-Dropped", "No Pell-Retained", "Pell-Dropped", "Pell-Retained"),
        cex.name = .8,
        border = 'white',
        main = "Retention Patterns for Pell Elegibility",
        ylab = "Percent of Students"
) 

#Generate Frequencies for Non credit retention
nonon <- Retrain %>% filter(non==0)  %>%proc_freq(tables = rsem)
non <- Retrain %>% filter(non==1) %>% proc_freq(tables = rsem)
nons <- rbind(nonon, non)

b <- nons[1:4, "PCT"]

#Generate a plot for Pell Student Retention
barplot(b, names.arg = c("Cred-Drop", "Cred-Retained", "No Cred-Drop", "No Cred-Retained"),
        cex.names = 0.8,
        border = "white",
        main = "Retention Paterns for Non Credited Programs",
        ylab = "Percent of Students") 

#Generate Frequencies for Female Retention
Femno <- Retrain %>% filter(Female==0)%>%proc_freq(tables = rsem)
Fem <- Retrain %>% filter(Female==1) %>% proc_freq(tables = rsem)
Females <- rbind(Femno, Fem)
c <- Females[1:4, "PCT"]

#Generate a plot for Female Retention
barplot(c, names.arg = c("No Female-Drop", "No Fem-Retained", "Female-Drop", "Female-Retained"),
        cex.names = 0.8,
        border = "white",
        main = "Retention Patterns for Gender")

#Generate Frequencies for Certificate Retention
cclno <- Retrain %>% filter(ccl == 0) %>%proc_freq(tables = rsem)
cc <- Retrain %>% filter(ccl == 1) %>% proc_freq(tables = rsem)
ccl <- rbind(cclno,cc)

d <- ccl[1:4, "PCT"]

#Generate a plot for Certificate Retention
barplot(d, names.arg = c("No Cert Drop", "No Cert Retained", "Cert Drop", "Cert Retained"),
        cex.names = 0.8,
        border = "white",
        main = "Retention Patterns for Certificates",
        ylab = "Percent of Students")

#Generate Frequencies for Associate's Degree Retention
Ascono <- Retrain %>% filter(aas==0) %>% proc_freq(tables = rsem)
Asoc <- Retrain %>% filter(aas==1) %>% proc_freq(tables =  rsem)
aas <- rbind(Ascono,Asoc)

e <- aas[1:4, "PCT"]

#Generate a plot for Associates Degree Retention
barplot(e, names.arg = c("No Asoc-Drop", "No Asoc-Ret", "Asoc Drop", "Asoc-Ret"),
        cex.names = 0.8,
        border = "white",
        main = "Retention Patterns for Associates",
        ylab = "Percent of Students")

# Generate the Statistical Model for Retention
Ret_Model <- glm(rsem ~ AvAthr + Age + gpa + pel + non + Female + ccl + aas,
                 data = Retrain,
                 family = "binomial",
                 na.action = na.exclude)

##obtain Coefficients from the model summary
Coef <- as.data.frame(Ret_Model$coefficients)

# Extract the summary of the model
summary_model <- summary(Ret_Model)  
# Extract the coefficients table
coefficients_table <- coef(summary_model)
# Get the variable names (excluding the intercept if present)
variable_names <-rownames(coefficients_table)
# Extract the coefficient values
coefficients <-coefficients_table[,"Estimate"]
# Extract the z-values (or t-values) - the column name might vary
z_values <- coefficients_table[, grep("z value|t value", colnames(coefficients_table))]
# Create the dataframe
results_df <- data.frame(
  Variable = variable_names,
  Coefficient = coefficients,
  Chi_Square = z_values
)

results_df <- results_df %>% mutate(Point_Estimate = exp(Coefficient)) %>%
  select(Variable, Coefficient, Point_Estimate, Chi_Square)

results_df <- results_df %>% mutate(across(c("Coefficient", "Point_Estimate", "Chi_Square"), round,2))

#Retrieving a Prediction and ROC
predicted_probs <- predict(Ret_Model)

roc(Retrain$rsem, predicted_probs, plot=TRUE, legacy.axes=TRUE,
    percent = TRUE, xlab = "Percent False Positives",
    ylab = "Percent True Positives", col = "blue", lwd=2,
    print.auc=TRUE)

#Identify threshold maximizing sensitivity and specificity
roc_curve <- roc(Retrain$rsem, predicted_probs)

#Generating a Probability Threshold
coords <- coords(roc_curve, "best", best.method = "closest.topleft")

#Creating a dummy for ROC Treshold
predicted_class <- ifelse(predicted_probs >= coords$threshold,1,0)
predicted_class <- as.data.frame(predicted_class)

#Merging threshold dummy and the complete data
Retrain <- merge(Retrain, predicted_class$predicted_class, by= 'row.names', all = TRUE ) 

#Creating Cassifications for the final retention
Retrain <- Retrain %>% mutate(Desision =
                                ifelse(rsem == 1 & y == 1, "Retained",
                                       ifelse(rsem == 1 & y == 0, "False Positive",
                                              ifelse(rsem == 0 & y == 0, "Drop",
                                                     ifelse(rsem == 0 & y == 1, "False Negative", "na")))))

#preping the data for table display
Decision <-proc_freq(data = Retrain, tables = Desision)
Decision <- subset(Decision, select= -c(VAR))%>%
  mutate(across(c("N", "CNT", "PCT"), round,2)) 
Decision <-  rename(Decision,
                    Retention_Categories = "CAT",
                    Observations = "N",
                    Expected_Count = "CNT",
                    Percent = "PCT")



######################################
######Testing Phase ##################

#Code 5: Model Implementation in Testing phase
Tse_coef <- as.data.frame(t(Coef))
Tse_coef <- rename(Tse_coef,
                   Intercept = "(Intercept)")

#Filter to 2025 cohort
Retest <- Retention %>% filter( AcadYr == 2025) 
Retest$rsem <- ifelse(Retest$NxtSem == "Y",1,0) 
Retest$pel <- ifelse(Retest$Pell == "1", 1,0)


#Apply the statistical model 
Retest <- Retest %>% mutate(
  log_odds = Tse_coef$Intercept + 
    (Tse_coef$AvAthr*AvAthr) +
    (Tse_coef$Age*Age) +
    (Tse_coef$gpa*gpa) + 
    (Tse_coef$pel*pel)+
    (Tse_coef$non*non)+
    (Tse_coef$Female*Female)+
    (Tse_coef$ccl*ccl)+
    (Tse_coef$aas*aas)
) %>%
  mutate(Probability = 1 / (1+ exp(-log_odds)))

#Code 6: Reclassifying Probabilities in the Testing Phase.
Retest <- Retest %>% mutate(Decision =
                              ifelse(Probability > coords$threshold & Probability < 0.5, "False Positive",
                                     ifelse(Probability >0.5, "Retained",   
                                            ifelse(Probability < coords$threshold & pel == 0 & non == 1 & ccl == 0 & aas == 0, "False Negative",
                                                   ifelse(Probability < coords$threshold, "Drop","na"  )))))



#Preping Retention Distributions for testing data
Decision25 <-proc_freq(data = Retest, tables = Decision)
Decision25 <- subset(Decision25, select= -c(VAR))%>%
  mutate(across(c("N", "CNT", "PCT"), round,2))


Decision25 <-  rename(Decision25,
                      Retention_Categories = "CAT",
                      Observations = "N",
                      Expected_Count = "CNT",
                      Percent = "PCT")


#Gran Finale
#Se Frego!!



