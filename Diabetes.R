install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("car")
install.packages("caret")
install.packages("epiDisplay")
install.packages("sjPlot")
install.packages("pROC")
install.packages("ROSE")

library(dplyr)
library(ggplot2)
library(plotly)
library(caret)
library(car)
library(pROC)
library(MASS)
#library(sjPlot)
library(ROSE)

source <- read.csv("diabetes2.csv")

########################################################## DATA CLEANING ####################################################
#Na elimination

valori_null <- is.na(source)

source_null <- source[rowSums(valori_null)>0, ] #non vengono rilevati NA 

data <- source

#Eliminazione dei duplicati

duplicati <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ] #non ci sono righe duplicate all'interno del dataset

data_backup <- data #creiamo un backup

#puliamo l'environment
rm(duplicati)
rm(source_null)
rm(valori_null)

###################################################### OUTLIER ANALYSIS ####################################################

#PREGNANCIES

box_pregnancies <- plot_ly(data, y = ~Pregnancies,
                  type = "box", line = list(color="black"),
                  marker = list(color = "black")) %>%
  layout(title = "Gravidanza",
         yaxis = list(title = "Mesi"),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         margin = list(l = 110, r = 70, b = 100, t = 100)
  )

box_pregnancies #eliminiamo gli outlier rilevati da 14 a salire

data <- data %>% filter (Pregnancies < 14)

bp_pregnancies <- plot_ly(data = as.data.frame(table(data$Pregnancies)),
                            x = ~Var1, y = ~Freq,
                            type = "bar",
                            marker = list(color = ~Freq, colorscale = list(c(0, 1), c("#0088ff", "#120a8f")))) %>%
  layout(
    title = list(text = "Gravidanza", font = list(color = 'black', family = 'Times New Roman', size = 26, weight = 'bold')),
    xaxis = list(title = list(text = ""), standoff = 20,
                 tickfont = list(color = 'black', family = 'Times New Roman', size = 16, weight = 'bold'), ticklen = 10),
    yaxis = list(title = list(text = "Osservazioni", font = list(color = 'black', family = 'Times New Roman', size = 20, weight = 'bold'), standoff = 20),
                 tickfont = list(color = 'black', family = 'Times New Roman', size = 16, weight = 'bold'), ticklen = 10, gridcolor = '#a0a0a0'),
    paper_bgcolor = 'white',
    plot_bgcolor = 'white',
    margin = list(l = 110, r = 70, b = 100, t = 100)
  )

bp_pregnancies

#GLUCOSE

box_glucose <- plot_ly(data, y = ~Glucose,
                           type = "box", line = list(color="black"),
                           marker = list(color = "black")) %>%
  layout(title = "Glucosio",
         yaxis = list(title = "Valore"),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         margin = list(l = 110, r = 70, b = 100, t = 100)
  )

box_glucose #eliminiamo gli outlier presenti sullo 0

data <- data %>% filter (Glucose > 0)

#BLOOD PRESSURE

box_BloodPressure <- plot_ly(data, y = ~BloodPressure,
                       type = "box", line = list(color="black"),
                       marker = list(color = "black")) %>%
  layout(title = "Pressione del Sangue",
         yaxis = list(title = "Valore"),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         margin = list(l = 110, r = 70, b = 100, t = 100)
  )

box_BloodPressure 

data <- data %>% filter (BloodPressure > 0)#eliminiamo gli outlier presenti sullo 0

#SKINTHICKNESS

box_SkinThickness <- plot_ly(data, y = ~SkinThickness,
                             type = "box", line = list(color="black"),
                             marker = list(color = "black")) %>%
  layout(title = "Spessore della pelle",
         yaxis = list(title = "Valore"),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         margin = list(l = 110, r = 70, b = 100, t = 100)
  )

box_SkinThickness #eliminiamo l'outlier a 99

data <- data %>% filter (SkinThickness < 99)

#INSULIN

box_Insulin <- plot_ly(data, y = ~Insulin,
                             type = "box", line = list(color="black"),
                             marker = list(color = "black")) %>%
  layout(title = "Insulina",
         yaxis = list(title = "Valore"),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         margin = list(l = 110, r = 70, b = 100, t = 100)
  )

box_Insulin #tagliamo gli outlier sopra i 650

data <- data %>% filter (Insulin < 650)

#BMI

box_BMI <- plot_ly(data, y = ~BMI,
                       type = "box", line = list(color="black"),
                       marker = list(color = "black")) %>%
  layout(title = "IMC",
         yaxis = list(title = "Valore"),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         margin = list(l = 110, r = 70, b = 100, t = 100)
  )

box_BMI #tagliamo i valori pari a 0 e sopra 50

data <- data %>% filter (BMI < 60)
data <- data %>% filter (BMI > 0)

#DIABETES PEDIGREE FUNCTION

box_DiabetesPedigreeFunction <- plot_ly(data, y = ~DiabetesPedigreeFunction,
                   type = "box", line = list(color="black"),
                   marker = list(color = "black")) %>%
  layout(title = "Diabetes Pedigree",
         yaxis = list(title = "Valore"),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         margin = list(l = 110, r = 70, b = 100, t = 100)
  )

box_DiabetesPedigreeFunction #tagliamo sopra 1.5

data <- data %>% filter (DiabetesPedigreeFunction < 1.5)

#AGE

box_Age <- plot_ly(data, y = ~Age,
           type = "box", line = list(color="black"),
           marker = list(color = "black")) %>%
  layout(title = "Age",
         yaxis = list(title = "Valore"),
         paper_bgcolor = 'white',
         plot_bgcolor = 'white',
         margin = list(l = 110, r = 70, b = 100, t = 100)
  )

box_Age #tagliamo l'ottantenne

data <- data %>% filter (Age < 80)

#OUTCOME

bar_Outcome <- plot_ly(data = as.data.frame(table(data$Outcome)),
                            x = ~Var1, y = ~Freq,
                            type = "bar",
                            marker = list(color = ~Freq, colorscale = list(c(0, 1), c("#0088ff", "#120a8f")))) %>%
  layout(
    title = list(text = "Pazienti con il Diabete", font = list(color = 'black', family = 'Times New Roman', size = 26, weight = 'bold')),
    xaxis = list(title = list(text = ""), standoff = 20,
                 tickfont = list(color = 'black', family = 'Times New Roman', size = 16, weight = 'bold'), ticklen = 10),
    yaxis = list(title = list(text = "Osservazioni", font = list(color = 'black', family = 'Times New Roman', size = 20, weight = 'bold'), standoff = 20),
                 tickfont = list(color = 'black', family = 'Times New Roman', size = 16, weight = 'bold'), ticklen = 10, gridcolor = '#a0a0a0'),
    paper_bgcolor = 'white',
    plot_bgcolor = 'white',
    margin = list(l = 110, r = 70, b = 100, t = 100)
  )

bar_Outcome #i diabetici risultano il 32% del dataset finale mentre i non diabetici risultano il 68%.
#Risulta leggermente sbilanciato ma proseguiamo comunque l'analisi per vedere come si adatta il modello 

data$Outcome <- as.factor(data$Outcome)

############################################################## CORRELATION ANALYSIS #######################################################################

encode_labels <- function(column) {
  unique_labels <- unique(column)
  labels_to_numbers <- as.numeric(factor(column, levels = unique_labels))
  return(labels_to_numbers)
}

data_temp <- data %>% mutate_if(is.logical, encode_labels)
data_temp <- data_temp %>% mutate_if(is.factor, encode_labels)

cormatrix <- cor(data_temp)

heatmap(cormatrix, 
        symm = TRUE,  # Usa la simmetria
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Scala dei colori
        main = "Heatmap della Matrice di Correlazione")

#notiamo come l'outcome sembri correlato significativamente con alcune variabili, diamo un'occhiata più nel dettaglio

coroutcome <- cor(data_temp$Outcome, data_temp)

print(coroutcome[,1:8])

#dai risultati andiamo ad escludere le variabili relative ad insulina, spessore della pelle per bassa correlazione. 
#Insulina, risulta anche modestamente correlata con il glucosio (0.32) per cui si decide di tenere solo la variabile glucosio
#che pertanto è anche maggiormente correlata con l'outcome.
#Pregnancies e age risultano correlate, tuttavia avendo esse una correlazione con l'outcome in maniera simile le teniamo per poterle analizzare più in profondità
#Analogo ragionamento con blood pressure e age.

data <- data[,-c(4,5)]

data_backup <- data

################################################### MODELLI #################################################################

#  l'opzione per indicare che la prima categoria è di riferimento
options(contrasts = c("contr.treatment", "contr.poly"))

attach(data)

#################################################### MODELLO FULL ###########################################################

mod_full <-glm(Outcome ~ ., binomial(link=logit), data)
summary(mod_full) #il modello ci suggerisce dfi elimninare blood pressure e age in quanto presentano un pvalue che conferma HO (Beta = 0)

exp(cbind(coef(mod_full), confint.default(mod_full, level=0.99))) #la propensione a sviluppare il diabete (o la possibilità di essere già affetto) aumenta o diminuisce all'aumentare o diminuire del valore della variabili

#mostriamo gli intervalli di confidenza
plot_model(mod_full, sort.est = TRUE)

#vediamo come si distribuisce la probabilità in relazione alle singole variabili del modello

plot(Pregnancies, mod_full$fitted, ylab = "Probabilità di avere il diabete", xlab = "Numero di gravidanze")
plot(Glucose, mod_full$fitted, ylab = "Probabilità di avere il diabete", xlab = "Glucosio")
plot(BloodPressure, mod_full$fitted, ylab = "Probabilità di avere il diabete", xlab = "Pressione del sangue")
plot(BMI, mod_full$fitted, ylab = "Probabilità di avere il diabete", xlab = "BMI")
plot(DiabetesPedigreeFunction, mod_full$fitted, ylab = "Probabilità di avere il diabete", xlab = "Diabetes Pedigree")
plot(Age, mod_full$fitted, ylab = "Probabilità di avere il diabete", xlab = "Età")

pchisq(mod_full$deviance, mod_full$df.residual, lower.tail = F)

# questa linea di codice calcola la probabilità della coda superiore della distribuzione del chi-quadro,
# data la devianza del modello e il numero di gradi di libertà residui.
# Questo può essere utilizzato per testare l'ipotesi nulla che il modello sia corretto, un buon fit,
# poiché un valore basso di probabilità indicherebbe che la devianza osservata è significativamente
# più grande di quanto ci si aspetterebbe casualmente sotto l'ipotesi nulla.

# confusion matrix

previsioni <- predict(mod_full, type = "response")

classi_predette <- ifelse(previsioni > 0.5, 1, 0)

matrice_confusione <- confusionMatrix(as.factor(classi_predette), data$Outcome)

print(matrice_confusione)

# curva ROC

roc(Outcome,previsioni)
rocplot<-roc(Outcome~previsioni, col = "red", plot=TRUE, print.auc=T, legacy.axes = T,
             main = "ROC curve")

################################################ MODELLO SENZA BLOOD PRESSURE #############################################

mod_nobp <-glm(Outcome ~. -BloodPressure, binomial(link=logit), data)
summary(mod_nobp)

exp(cbind(coef(mod_nobp), confint.default(mod_nobp, level=0.99))) # livello di conf 0.99

pchisq(mod_nobp$deviance, mod_nobp$df.residual, lower.tail = F)

# nel nostro caso, l'ipotesi nulla era che il modello ridotto senza blood pressure fosse un buon modello per l'analisi,
# visto il valore alto 0.9 di chi quadro, NON rifiutiamo l'ipotesi nulla e confermiamo che si tratti di un buon fit.

# confusion matrix

previsioni <- predict(mod_nobp, type = "response")

classi_predette <- ifelse(previsioni > 0.5, 1, 0)

matrice_confusione <- confusionMatrix(as.factor(classi_predette), data$Outcome)

print(matrice_confusione)

# curva ROC
roc(Outcome,previsioni)
rocplot<-roc(Outcome~previsioni, col = "red", plot=TRUE, print.auc=T, legacy.axes = T,
             main = "ROC curve")

################################################### MODELLO CON BP CATEGORIZZATA ################################################

hist(BloodPressure)
data <- within(data, {
  BloodPressure.cat <- NA
  BloodPressure.cat[BloodPressure < 60] <- "BP low"
  BloodPressure.cat[BloodPressure >= 60 & BloodPressure <80] <- "BP normal"
  BloodPressure.cat[BloodPressure >= 80] <- "BP high"
})

mod_bpcat <-glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + BloodPressure.cat + Age, binomial(link=logit), data)
summary(mod_bpcat)

exp(cbind(coef(mod_bpcat), confint.default(mod_bpcat, level=0.99))) #il modello non sembra dare risultati rilevanti usando congiuntamente queste due variabili

pchisq(mod_bpcat$deviance, mod_bpcat$df.residual, lower.tail = F) 

data <- data[,-8]

################################################ MODELLO SENZA AGE & PRESSURE #############################################

mod_nobpnoage <-glm(Outcome ~. -BloodPressure -Age, binomial(link=logit), data)
summary(mod_nobpnoage)

exp(cbind(coef(mod_nobpnoage), confint.default(mod_nobpnoage, level=0.99)))

pchisq(mod_nobpnoage$deviance, mod_nobpnoage$df.residual, lower.tail = F)

# confusion matrix

previsioni <- predict(mod_nobpnoage, type = "response")

classi_predette <- ifelse(previsioni > 0.5, 1, 0)

matrice_confusione <- confusionMatrix(as.factor(classi_predette), data$Outcome)

print(matrice_confusione)

# curva ROC
roc(Outcome,previsioni)
rocplot<-roc(Outcome~previsioni, col = "red", plot=TRUE, print.auc=T, legacy.axes = T,
             main = "ROC curve")

################################################### MODELLO CON AGE ^ 2 ######################################################

mod_age2 <-glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + I(scale(Age)) + I(scale(Age)^2), binomial(link=logit), data)
summary(mod_age2)

# il modello suggerisce di rimuovere pregnancies. Provato a seguire

exp(cbind(coef(mod_age2), confint.default(mod_age2, level=0.99))) #il modello sembra migliorare con l'età al quadrato

pchisq(mod_age2$deviance, mod_age2$df.residual, lower.tail = F) 

# confusion matrix

previsioni <- predict(mod_age2, type = "response")

classi_predette <- ifelse(previsioni > 0.5, 1, 0)

matrice_confusione <- confusionMatrix(as.factor(classi_predette), data$Outcome)

print(matrice_confusione)

# curva ROC

roc(Outcome,previsioni)
rocplot<-roc(Outcome~previsioni, col = "red", plot=TRUE, print.auc=T, legacy.axes = T,
             main = "ROC curve")

################################################### MODELLO CON AGE 2 - PREGNANCIES ################################################

mod_age2preg <-glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + I(scale(Age)) + I(scale(Age)^2), binomial(link=logit), data)
summary(mod_age2preg)

exp(cbind(coef(mod_age2preg), confint.default(mod_age2preg, level=0.99))) #il modello sembra migliorare con l'età al quadrato

pchisq(mod_age2preg$deviance, mod_age2preg$df.residual, lower.tail = F) 

# confusion matrix

previsioni <- predict(mod_age2preg, type = "response")

classi_predette <- ifelse(previsioni > 0.5, 1, 0)

matrice_confusione <- confusionMatrix(as.factor(classi_predette), data$Outcome)

print(matrice_confusione)

# curva ROC

roc(Outcome,previsioni)
rocplot<-roc(Outcome~previsioni, col = "red", plot=TRUE, print.auc=T, legacy.axes = T,
             main = "ROC curve")

# in realtà i valori sia di accuratezza che di curva ROC sono peggiorati un po' per cui il precendete era migliore

################################################### MODELLO CON AGE CAT ###########################################################

hist(Age)
data <- within(data, {
  Age.cat <- NA
  Age.cat[Age < 26] <- "Very Young"
  Age.cat[Age >= 26 & Age <36] <- "Young"
  Age.cat[Age >= 36 & Age <51] <- "Adult"
  Age.cat[Age >=51] <- "Senior"
})

mod_agecat <-glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + Age.cat, binomial(link=logit), data)
summary(mod_agecat)

exp(cbind(coef(mod_agecat), confint.default(mod_agecat, level=0.99))) #il modello non sembra dare risultati rilevanti usando congiuntamente queste due variabili

pchisq(mod_agecat$deviance, mod_agecat$df.residual, lower.tail = F) 

# confusion matrix

previsioni <- predict(mod_agecat, type = "response")

classi_predette <- ifelse(previsioni > 0.5, 1, 0)

matrice_confusione <- confusionMatrix(as.factor(classi_predette), data$Outcome)

print(matrice_confusione)

# curva ROC

roc(Outcome,previsioni)
rocplot<-roc(Outcome~previsioni, col = "red", plot=TRUE, print.auc=T, legacy.axes = T,
             main = "ROC curve")

data <- data[,-8]

######################################### CONFRONTIAMO I MODELLI ##########################################################

AIC(mod_full, mod_nobp, mod_bpcat, mod_nobpnoage, mod_age2, mod_agecat, mod_agebp)

#anova(mod_noedu, mod_full, test='Chisq')
#Anova(mod_full)

#AIC significa "Criterio di Informazione di Akaike". È una misura utilizzata in statistica per valutare la qualità relativa di modelli statistici per un determinato insieme di dati. L'AIC quantifica il bilancio tra la bontà di adattamento del modello e la complessità del modello, penalizzando i modelli troppo complessi. Valori più bassi di AIC indicano un migliore compromesso tra adattamento del modello e complessità.

#il modello con AIC minore è anche quello che coincide con i riusultati più accurati in termini di ROC e confuson matrix

########################################## SELEZIONE AUTOMATICA (BACKWARD) ###################################################

stepAIC(mod_full, direction="backward", data=data) #anche utilizzando la selezione automatica viene suggerita l'eliminazione della sola var bp
# tuttavia come abbiamo visto prima con l'AIC il modello contenente age trasformata risulta essere un modello migliore

############################################ RESIDUI E PREVISIONE #################################################

# I "residui di Pearson" sono una misura della discrepanza tra i valori osservati e quelli predetti da un modello statistico, espressi in termini di deviazioni standard. Sono utilizzati principalmente nei modelli di regressione per valutare la bontà di adattamento del modello.

# I residui di Pearson sono calcolati come la differenza tra il valore osservato e il valore predetto dal modello, diviso per la radice della varianza dei valori predetti.

# I residui di Pearson sono utilizzati per diagnosticare la presenza di punti dati influenti o outlier, nonché per valutare se la varianza dei valori predetti varia con il livello della variabile indipendente. Se i residui di Pearson mostrano una struttura o un pattern evidente, questo può indicare violazioni delle assunzioni del modello, come l'omoschedasticità, che può richiedere correzioni o modelli più appropriati.

residualPlots(mod_age2)