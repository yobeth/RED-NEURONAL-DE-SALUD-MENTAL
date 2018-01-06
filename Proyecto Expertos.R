
library(neuralnet)
setwd("C:\\Users\\HP\\Downloads\\mental-health-in-tech-survey")
mental1 = read.csv("salud1.csv", header=T)
str(mental1)
summary(mental1)
head(mental1)
mental1
ment=model.matrix(~treatment +Age + Gender + family_history + work_interfere, d=mental1)
ment1=model.frame(ment)

fold.test <- sample(nrow(ment), nrow(ment) / 3)
test <- ment[fold.test, ]
train <- ment[-fold.test, ]

nuevo<-neuralnet(treatmentYes ~ Age + GenderFemale + GenderMale +
                  family_historyYes + work_interfereOften +
                  work_interfereRarely+work_interfereSometimes,d=train, hidden = 10,threshold = 0.01)

plot(nuevo)


output <- compute(nuevo, test[ , c("Age", "GenderFemale", 
                                 "GenderMale", "family_historyYes","work_interfereOften"
                                 ,"work_interfereRarely","work_interfereSometimes")])

salida=data.frame(predict(ment,test),treatmentYes=predict(ment,test))

result <- data.frame(
  Real = test1$treatmentYes, 
  Predicted = levels(ment$treatmentYes)[round(output$net.result)])
result

# Tabla de confusión
table(result$Predicted, result$Real)




