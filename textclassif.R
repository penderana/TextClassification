#install.packages("e1071")
#install.packages("readr")
#install.packages("mldr")
#install.packages("remotes")
#install.packages("devtools")
#install.packages("caret")
#install.packages("NLP")
#install.packages("modeldata")
#install.packages('Rmisc', dependencies = TRUE) CRAN - step dummy
#install.packages("recipes")


library(e1071)
library("readr")
#library("mldr")
library("remotes")
#library("devtools")
library(caret) #one hot encoding
library(stringr)
library(mlr)

library(dplyr)
library(janeaustenr)
library(tidytext)
library(text2vec)
library(NLP)
library(superml)
library(readxl)
library(tidyverse)
library(recipes)
library(mltools)
library(encoder)
library(data.table)

library(RWeka)
library(mda)
library(modeltools)

library(splitstackshape)

library(tm)

library(RSNNS)
require(caret)



#CARGAMOS LOS DATOS
#########################################################
#etiquetas_excel2 <- read_excel("etiquetas_excel.xlsx")
#etiquetas_excel <- read_excel("etiquetas_excel_final.xlsx", 
#                              col_types = c("text", "text", "text", 
#                                            "text", "text", "text", "text", "text", 
#                                            "text", "text"))

etiquetas_excel <- read_excel("etiquetas_excel_final.xlsx")
x <- read_delim("datos_stemmed.txt",delim="\n",col_names = FALSE)
#print(etiquetas_excel)
#########################################################

#HACEMOS ONE HOT ENCODING DE LAS ETIQUETAS
#########################################################



binarizer <- function(levels){
        f = function(v){
                m = matrix(0, nrow=length(v), ncol=length(levels))
                vf = as.numeric(factor(v, levels=levels))
                m[cbind(1:length(v),vf)]=1
                colnames(m)=levels
                m
        }
        f
}

nrows = nrow(as.data.frame(x))
#print(nrows)
## 75% of the sample size
smp_size <- floor(0.7 * nrows)

## set the seed to make your partition reproducible
set.seed(22)
#train.index <- createDataPartition(Data$Class, p = .7, list = FALSE)
train_ind <- sample(seq_len(nrows), size = smp_size)


etiquetas_train <- etiquetas_excel[train_ind, ]
etiquetas_test <- etiquetas_excel[-train_ind, ]






distintos <- data.frame(unique(etiquetas_train$Col1))
colnames(distintos) <- "uno"

distintos2 <- data.frame(unique(etiquetas_train$Col2))
colnames(distintos2) <- "uno"

distintos3 <- data.frame(unique(etiquetas_train$Col3))
colnames(distintos3) <- "uno"

distintos4 <- data.frame(unique(etiquetas_train$Col4))
colnames(distintos4) <- "uno"

distintos5 <- data.frame(unique(etiquetas_train$Col5))
colnames(distintos5) <- "uno"

distintos6 <- data.frame(unique(etiquetas_train$Col6))
colnames(distintos6) <- "uno"

distintos7 <- data.frame(unique(etiquetas_train$Col7))
colnames(distintos7) <- "uno"

distintos8 <- data.frame(unique(etiquetas_train$Col8))
colnames(distintos8) <- "uno"

distintos9 <- data.frame(unique(etiquetas_train$Col9))
colnames(distintos9) <- "uno"

distintos10 <- data.frame(unique(etiquetas_train$Col10))
colnames(distintos10) <- "uno"

distintos11 <- data.frame(unique(etiquetas_train$Col11))
colnames(distintos11) <- "uno"

distintos12 <- data.frame(unique(etiquetas_train$Col12))
colnames(distintos12) <- "uno"

distintos13 <- data.frame(unique(etiquetas_train$Col13))
colnames(distintos13) <- "uno"

distintos14 <- data.frame(unique(etiquetas_train$Col14))
colnames(distintos14) <- "uno"

distintos15 <- data.frame(unique(etiquetas_train$Col15))
colnames(distintos15) <- "uno"

distintos16 <- data.frame(unique(etiquetas_train$Col16))
colnames(distintos16) <- "uno"

distintos17 <- data.frame(unique(etiquetas_train$Col17))
colnames(distintos17) <- "uno"

distintos18 <- data.frame(unique(etiquetas_train$Col18))
colnames(distintos18) <- "uno"

distintos19 <- data.frame(unique(etiquetas_train$Col19))
colnames(distintos19) <- "uno"

distintos20 <- data.frame(unique(etiquetas_train$Col20))
colnames(distintos20) <- "uno"

distintos <- rbind(distintos,distintos2)
distintos2 <- rbind(distintos3,distintos4)
distintos3 <- rbind(distintos5,distintos6)
distintos4 <- rbind(distintos7,distintos8)
distintos5 <- rbind(distintos9,distintos10)
distintos6 <- rbind(distintos11,distintos12)
distintos7 <- rbind(distintos13,distintos14)
distintos8 <- rbind(distintos15,distintos16)
distintos9 <- rbind(distintos17,distintos18)
distintos10 <- rbind(distintos19,distintos20)


distintos <- rbind(distintos,distintos2)
distintos2 <- rbind(distintos3,distintos4)
distintos3 <- rbind(distintos5,distintos6)
distintos4 <- rbind(distintos7,distintos8)
distintos5 <- rbind(distintos9,distintos10)


distintos <- rbind(distintos,distintos2)
distintos2 <- rbind(distintos3,distintos4)
distintos2 <- rbind(distintos2,distintos5)

distintos <- rbind(distintos,distintos2)


distintos <- distintos[!is.na(distintos),]




final <- unique(distintos)


  

ab = binarizer(final)



#PARTE TRAIN


solo_uno <- ab(etiquetas_train[1,])





total <- solo_uno[1,]






for (i in 1:19)
{
  j <- i + 1

    total <- total + solo_uno[j,]
}




N = length(final)

#total <- as.logical(total)
matriz <- matrix(total,ncol=N)
df <- as.data.frame(matriz)

for (i in 2:nrow(etiquetas_train))
{
  solo_uno <- ab(etiquetas_train[i,])
  
  aux <- solo_uno[1,]
  
  for (k in 1:19)
  {
    j <- k + 1
    
    aux <- aux + solo_uno[j,]
  }
  
  #aux <- as.logical(aux)
  df <- rbind(df,aux)
}
'

for(i in 1:nrow(df))
{
  
  if (length(which(df[i,]==1)) == 1)
  {
    print(i)
  }
}
  




'
#########################################################

#PARTE TEST

solo_uno <- ab(etiquetas_test[1,])


total <- solo_uno[1,]

for (i in 1:19)
{
  j <- i + 1
  
  total <- total + solo_uno[j,]
}

#N = length(final)

total <- as.logical(total)
matriz <- matrix(total,ncol=N)
df_t <- as.data.frame(matriz)

for (i in 2:nrow(etiquetas_test))
{
  solo_uno <- ab(etiquetas_test[i,])
  
  aux <- solo_uno[1,]
  
  for (k in 1:19)
  {
    j <- k + 1
    
    aux <- aux + solo_uno[j,]
  }
  
  aux <- as.logical(aux)
  df_t <- rbind(df_t,aux)
}
#########################################################
    

#CONVERTIMOS EL TEXTO A VECTORES DE DOUBLE (TF IDF)
#########################################################


datos_train = as.data.frame(x$X1)[train_ind,]
datos_test = as.data.frame(x$X1)[-train_ind,]

dtm1 <- DocumentTermMatrix(Corpus(VectorSource(datos_train)), control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), stopwords = TRUE))
dtm1 <- as.data.frame(as.matrix(dtm1))


colnames(dtm1) <- make.names(colnames(dtm1),unique = T)

dtm2 <- DocumentTermMatrix(Corpus(VectorSource(datos_test)), control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), stopwords = TRUE))
dtm2 <- as.data.frame(as.matrix(dtm2))


colnames(dtm2) <- make.names(colnames(dtm2),unique = T)

print("Hecho")


#NOS QUEDAMOS CON EL NOMBRE DE LAS VARIABLES A PREDECIR Y
#AÑADIMOS EL TEXTO AL DATAFRAME CON LAS ETIQUETAS
#########################################################
labels <- colnames(df)
#df[,476] <- x

N <- ncol(df)
M <- N+1
C <- M+ncol(datos_train)

print(ncol(df))


df[,M:C] <- datos_train
df_t[,M:C] <- datos_test

print(ncol(df))



#########################################################

#print(paste("Original: ",toString(which(df[2,]==TRUE))))

#CREAMOS LA TAREA MULTIETIQUETA
#########################################################
task <- makeMultilabelTask(id = "multi", data = df, target = labels)
task_test <- makeMultilabelTask(id = "multi", data = df_t, target = labels)
#########################################################



#CREAMOS CLASIFICADOR Y ENTRENAMOS
#########################################################

#DECISION TREE

#lrn.br = makeLearner("classif.rpart", predict.type = "prob") 


#SVM  

lrn.br = makeLearner("classif.svm", predict.type = "prob")

#MLP

#lrn.br = makeLearner("classif.mlp", predict.type = "prob") 

#NAIVE BAYES

#lrn.br = makeLearner("classif.naiveBayes", predict.type = "prob") 


lrn.br = makeMultilabelBinaryRelevanceWrapper(lrn.br)


# train, predict and evaluate
mod = mlr::train(lrn.br, task)

#########################################################
#PREDECIMOS
#########################################################
#pred = predict(mod, task = task, subset = 1:length(x$X1))
#pred = predict(mod, task = task, subset = i)



thresh <- 0.05




pred = predict(mod, task = task_test)
pred = setThreshold(pred, threshold = thresh)

resultados <- pred$data
ver <- resultados[,2756:4132]
df_1 <- df_t[1:1378]

TOTAL <- 5

for(i in 1:TOTAL)
{
  
  #resultados2 <- resultados[,1380:2756]
  #ver <- resultados2[i,]
  
  ver <- ver[i,]
  
  
  print(paste("Original: ",toString(which(df_1[i,]==TRUE))))
  
  #print(paste("Prediccion: ",toString(which(ver>0.45 & ver<0.83))))
  print(paste("Prediccion: ",toString(which(ver==TRUE))))
  print("")
}




#########################################################
#########################################################

# RESULTADOS

#########################################################

#print(listMeasures("multilabel"))
print(performance(pred, measures = list(multilabel.hamloss, multilabel.acc, multilabel.f1,multilabel.tpr)))


print(paste("Original:  63, 135, 235, 470, 580, 1092"))
print(paste("Prediccion: 61, 115, 235, 470"))
print(paste(""))
print(paste("Original:  201, 231, 432, 511"))
print(paste("Prediccion: 201, 432, 730, 771, 1171"))
print(paste(""))
print(paste("Original:  11, 201"))
print(paste("Prediccion: 8, 11, 300"))
print(paste(""))
print(paste( "Original:  109, 163, 598, 790, 1083"))
print(paste("Prediccion: 111, 837, 1083"))

