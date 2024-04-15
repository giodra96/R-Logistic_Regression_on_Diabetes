set.seed(123)

################################################# RIDUZIONE ################################################# 

# RIMOZIONE (RIDUZIONE DEL DATASET, esclusivamente delle osservazioni di NON diabetici)

data0 <- data %>% filter(Outcome==0) # teniamo da parte solo i NON diabetici, sui quali ridurremo il sample

indici_da_rimuovere <- sample(1:468, 100) 
data0 <- data0[-indici_da_rimuovere, ] # in data rimangono solo 300 osservazioni di pazienti NON affetti dal diabete

data1 <- data %>% filter(Outcome==1) # dataset coi soli diabetici
data2 <- rbind(data0, data1)
table(data2$Outcome)

data2$Outcome <- as.factor(data2$Outcome)

# correlazioni

encode_labels <- function(column) {
  unique_labels <- unique(column)
  labels_to_numbers <- as.numeric(factor(column, levels = unique_labels))
  return(labels_to_numbers)
}

# correlazioni con Outcome

data_temp2 <- data2 %>% mutate_if(is.logical, encode_labels)
data_temp2 <- data_temp2 %>% mutate_if(is.factor, encode_labels)
coroutcome2 <- cor(data_temp2$Outcome, data_temp2)
print(coroutcome2[,1:7])

################################################### RIDUZIONE: MODELLO FULL ######################################################
mod_full_reduced <-glm(Outcome ~ ., binomial(link=logit), data2)
summary(mod_full_reduced) #il modello ci suggerisce dfi elimninare blood pressure e age in quanto presentano un pvalue che conferma HO (Beta = 0)

# confusion matrix

previsioni <- predict(mod_full_reduced, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), data2$Outcome)
print(matrice_confusione)

################################################### RIDUZIONE: MODELLO AGE^2 - BP ######################################################

mod_age2_reduced <-glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + I(scale(Age)) + I(scale(Age)^2), binomial(link=logit), data2)
summary(mod_age2_reduced)

# confusion matrix

previsioni <- predict(mod_age2_reduced, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), data2$Outcome)
print(matrice_confusione) # questo facciamolo stampare nell'Rmarkdown perché si veda l'accuratezza (spoiler = è più alta se rimuoviamo le gravidanze)

# il modello suggerisce ancora una volta di rimuovere il numero di gravidanze. Si intenta nuovamente visto che il dataset è cambiato per verificare se l'accuratezza migliora stavolta

mod_age2_reduced <-glm(Outcome ~ Glucose + BMI + DiabetesPedigreeFunction + I(scale(Age)) + I(scale(Age)^2), binomial(link=logit), data2)
summary(mod_age2_reduced)

# confusion matrix

previsioni <- predict(mod_age2_reduced, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), data2$Outcome)
print(matrice_confusione) # visto?

# il modello è più accurato senza le gravidanze nel dataset

# si fa un ulteriore tentativo invece aumentando il numero dei dati, anzichè diminuirli 

################################################# AUMENTO ################################################# 

data0 <- data %>% filter(Outcome==0) # teniamo da parte solo i NON diabetici, sui quali ridurremo il sample
data1 <- data %>% filter(Outcome==1) # dataset coi soli diabetici

dup <- sample(1:238, 100, replace = TRUE)
data3 <- rbind(data0, data1, data1[dup, ])
table(data3$Outcome)

data3$Outcome <- as.factor(data3$Outcome)

# correlazioni

encode_labels <- function(column) {
  unique_labels <- unique(column)
  labels_to_numbers <- as.numeric(factor(column, levels = unique_labels))
  return(labels_to_numbers)
}

# correlazioni con Outcome

data_temp3 <- data3 %>% mutate_if(is.logical, encode_labels)
data_temp3 <- data_temp3 %>% mutate_if(is.factor, encode_labels)
coroutcome3 <- cor(data_temp3$Outcome, data_temp3)
print(coroutcome3[,1:7])

################################################### RIDUZIONE: MODELLO FULL ######################################################
mod_full_increased <-glm(Outcome ~ ., binomial(link=logit), data3)
summary(mod_full_increased) #il modello ci suggerisce dfi elimninare blood pressure e age in quanto presentano un pvalue che conferma HO (Beta = 0)

# confusion matrix

previsioni <- predict(mod_full_increased, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), data3$Outcome)
print(matrice_confusione)

################################################### AUMENTO: MODELLO AGE^2 - BP ######################################################

mod_age2_increased <-glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + I(scale(Age)) + I(scale(Age)^2), binomial(link=logit), data3)
summary(mod_age2_increased)

# il modello suggerisce di rimuovere l'informazione relativa al numero di gravidanze. in questo caso ad ogni modo l'accuratezza peggiorerebbe se le rimuovessimo.

# confusion matrix

previsioni <- predict(mod_age2_increased, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), data3$Outcome)
print(matrice_confusione)

################################################# FRANKENSTEIN'S MOSTER ################################################# 

# HYBRID APPROACH

data0 <- data %>% filter(Outcome==0) # teniamo da parte solo i NON diabetici, sui quali ridurremo il sample
data1 <- data %>% filter(Outcome==1) # dataset coi soli diabetici

dup <- sample(1:238, 30, replace = TRUE)
indici_da_rimuovere <- sample(1:468, 30) 
data0 <- data0[-indici_da_rimuovere, ] # in data rimangono solo 300 osservazioni di pazienti NON affetti dal diabete

dataH <- rbind(data0, data1, data1[dup, ])
table(dataH$Outcome)

options(contrasts = c("contr.treatment", "contr.poly"))
attach(dataH)

################################################### RIDUZIONE: MODELLO FULL ######################################################
mod_full_H <-glm(Outcome ~ ., binomial(link=logit), dataH)
summary(mod_full_H) #il modello ci suggerisce dfi elimninare blood pressure e age in quanto presentano un pvalue che conferma HO (Beta = 0)

# confusion matrix

previsioni <- predict(mod_full_H, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), dataH$Outcome)
print(matrice_confusione)

################################################### HYBRID: MODELLO AGE^2 - BP ######################################################

mod_age2H <-glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + I(scale(Age)) + I(scale(Age)^2), binomial(link=logit), dataH)
summary(mod_age2H)

# il modello suggerisce di rimuovere l'informazione relativa al numero di gravidanze. in questo caso ad ogni modo l'accuratezza peggiorerebbe se le rimuovessimo.

# confusion matrix

previsioni <- predict(mod_age2H, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), dataH$Outcome)
print(matrice_confusione)

#################################################### CONFRONTO TRA I TRE MODELLI #####################################

# il confronto tra i tre modelli con l'AIC non si puo' fare perché sono dataset diversi di dimensioni diverse. quindi lo facciamo a mano:

# reduced
previsioni <- predict(mod_age2_reduced, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), data2$Outcome)
print(matrice_confusione)

# increased
previsioni <- predict(mod_age2_increased, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), data3$Outcome)
print(matrice_confusione)

# hybrid
previsioni <- predict(mod_age2H, type = "response")
classi_predette <- ifelse(previsioni > 0.5, 1, 0)
matrice_confusione <- confusionMatrix(as.factor(classi_predette), dataH$Outcome)
print(matrice_confusione)

# il più accurato è senza dubbio il primo, con un ridotto numero di pazenti non diabetici dal dataset.

