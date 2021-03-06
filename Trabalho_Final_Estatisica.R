#Carrega todas a libs necessarias
install.packages("psych")
install.packages("plotly")
install.packages("gmodels")
install.packages("corrgram")
install.packages("GGally")
install.packages("memisc")
install.packages("pander")
install.packages("corrplot")
install.packages("caret")
install.packages("kknn")
install.packages("randomForest")
install.packages("kernlab")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
install.packages("RWeka")
install.packages("rJava")
install.packages("Inad")
install.packages("ISLR")


library("psych")
library("plotly")
library("gmodels")
library("corrgram")
library("GGally")
library("memisc")
library("pander")
library("corrplot")
library("caret")
library("kknn")
library("randomForest")
library("kernlab")
library("rpart") 
library("rpart.plot")
library("rattle")
library("RColorBrewer")
library("rJava")
library("RWeka")
#library("Inad")
library("ISLR")
library("splines")



# limpar mem�ria do R
rm(list=ls(all=TRUE))


# mostrar at� 2 casas decimais
options("scipen" = 2)

# Lendo arquivo csv
Vinhos <- read.csv2("C:/Users/GABRIELGOMESCRISPIM/Desktop/R_IA/BaseWine_Red_e_White2018.csv", row.names=1)
 
fix(Vinhos)
#mostrar as vari�veis
str(Vinhos)
#mostra as vari�veis
names(Vinhos)

attach(Vinhos)
##### Analise Explorat�ria dos Dados

# 1- Frequ�ncia absoluta de qualidade por tipo de Vinho
table(as.factor(Vinhos$quality), Vinhos$Vinho, useNA = "ifany")


# 2- Tabela cruzada, para entender as notas dadas em rela��o a qualiadade.
CrossTable(as.factor(Vinhos$quality), Vinhos$Vinho) 
 
# 3 - Resumo do dataset
summary(Vinhos)

# 4- Olhando pela m�dia separada pelo tipo de Vinho � poss�vel 
#verificar que o RED � mais �cido e muito menos doce que o WHITE
aggregate(Vinhos,
          by = list( Vinho),
          FUN = mean)
mean(Vinhos$fixedacidity) # m�dia
median(Vinhos$fixedacidity) # m�diana

#Varia��o de acidez por quartil
quantile(Vinhos$fixedacidity,type=4)

quantile(Vinhos$fixedacidity,.65,type=4) # exato percentil

range(Vinhos$fixedacidity)  # amplitude

diff(range(Vinhos$fixedacidity)) #diferen�a entre o maior e o menor valor

min(Vinhos$fixedacidity)  # valor m�nimo de x

max(Vinhos$fixedacidity)  # valor m�ximo de x

var(Vinhos$fixedacidity) # para obter a vari�ncia

sd(Vinhos$fixedacidity)  # para obter o desvio padr�o

#Mostra que o fixedacidity tem uma vari��o consider�vel, mas n�o determinante para determinar a qualiadade.
CV_fixedacidity<-sd(Vinhos$fixedacidity)/mean(Vinhos$fixedacidity)*100  # para obter o coefiiente de varia��o
CV_fixedacidity


#Plot de todos os atributos do dataset para analise gr�fica.
#Possivel observar que alguns atributos tem vales nos gr�ficos, iremos validar com boxplot para validar
# se realmente podem ser outliers ou representam �timos e p�ssimos vinhos.
par (mfrow=c(3,4))
hist(fixedacidity)
hist(volatileacidity)
hist(citricacid )
hist(residualsugar)
hist(chlorides)
hist(freesulfurdioxide)
hist(totalsulfurdioxide)
hist(density)
hist(pH)
hist(sulphates)
hist(alcohol)
hist(quality)
dev.off()

     
attach(Vinhos)

#Gerando os boxplots dos atributos
par (mfrow=c(3,4))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(residualsugar, main='residualsugar')
boxplot(chlorides, main='chlorides')
boxplot(freesulfurdioxide, main='freesulfurdioxide')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(Vinhos$quality, main='quality')
dev.off()

#Analisando o boxplot dos tipos de Vinhos, fica mais claro que n�o seria poss�vel analisar o dataset
#com ambos os tipos, devido as suas caracteristicas serem diferentes, principalmente em rela��o 
#aos atributos fixedacidity,volatileacidity, residualsugar e totalsulfurdioxide.
par (mfrow=c(3,4))
boxplot(quality ~ Vinho, main='quality')
boxplot(fixedacidity ~ Vinho, main='fixedacidity',col=c('red','blue'))
boxplot(volatileacidity ~ Vinho , main='volatileacidity')
boxplot(citricacid ~ Vinho, main='citricacid')
boxplot(residualsugar ~ Vinho, main='residualsugar',col=c('red','blue'))
boxplot(chlorides ~ Vinho, main='chlorides')
boxplot(freesulfurdioxide ~ Vinho, main='freesulfurdioxide')
boxplot(totalsulfurdioxide ~ Vinho, main='totalsulfurdioxide')
boxplot(density ~ Vinho, main='density')
boxplot(pH ~ Vinho, main='pH')
boxplot(sulphates ~ Vinho, main='sulphates')
boxplot(alcohol ~ Vinho, main='alcohol')


# Gr�fico de dispers�o ( pch=caracter, lwd=largura)
plot(freesulfurdioxide~totalsulfurdioxide)
plot(freesulfurdioxide~totalsulfurdioxide, pch=1, lwd=3)

plot(freesulfurdioxide~totalsulfurdioxide)
abline(v=mean(freesulfurdioxide), col="red")
abline(h=mean(totalsulfurdioxide), col="green")


#Criando uma escala intervalar para encaixar o residual_sugar e exibindo na tabela cruzada.
attach(Vinhos)
Vinhos$fx_redSugar <- cut(residualsugar,breaks=c(0,10,20,30,max(residualsugar)))  
Vinhos$fx_redSugar  
str(Vinhos)
CrossTable( Vinhos$fx_redSugar , Vinhos$Vinho) 


# describe
# A data.frame of the relevant statistics:
# item name
# item number
# number of valid cases
# mean
# standard deviation
# trimmed mean (with trim defaulting to .1)
# median (standard or interpolated
# mad: median absolute deviation (from the median)
# minimum
# maximum
# skew
# kurtosis
# standard error


#Ap�s esse an�lise preliminar e os resultados apresentados, iremos separar o dataset e analisar
#os tipos de vinhos WHITE devido a maior quantidade de ocorr�ncias e a discrep�ncia em rela���
#aos atributos mencionados acima.


summary(Vinhos)
#Separando o dataset apenas com as ocorr�ncias do tipo WHITE.
#white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,residualsugar,
                                                # chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,
                                                 #sulphates,alcohol))

white <- read.csv("C:/Users/GABRIELGOMESCRISPIM/Desktop/R_IA/winequality-white.csv", sep=";")

#An�lise inicila do dataset.
summary(white)
str(white)
attach(white)
 

#Estat�sticas descritivas
par (mfrow=c(3,4))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(residualsugar, main='residualsugar')
boxplot(chlorides, main='chlorides')
boxplot(freesulfurdioxide, main='freesulfurdioxide')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(quality, main='quality')
dev.off()
boxplot.stats(white$chlorides)


#Avaliando a dispers�o dos dados com base no residualsugar, possivelmente o atributo mais distorcido
# do dataset de Vinhos do tipo White.
AIQ_residualsugar<-quantile(white$residualsugar,.75,type=2)-quantile(white$residualsugar,.25,type=2)
AIQ_residualsugar

#Verificando limite inferior e superior para identificar a faixa de corte.
limsup_residualsugar= quantile(white$residualsugar,.75,type=4)+1.5*AIQ_residualsugar
limsup_residualsugar
liminf_residualsugar= quantile(white$residualsugar,.25,type=2)-1.5*AIQ_residualsugar
liminf_residualsugar


#Excluindo os outliers.
plot(quality~residualsugar)

white_calibrated <- subset(white, residual.sugar<=40)
fix(white_calibrated)
attach(white_calibrated)
summary(white_calibrated)

plot(residualsugar,alcohol)
abline(v=mean(residualsugar), col="red")
abline(h=mean(alcohol), col="green")



# Exibindo a matriz de correla��o.
matcor <- cor(white_calibrated)
plot(matcor, digits = 2)

panel.cor <- function(x, y, digits=2, prefix ="", cex.cor,
                      ...)  {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y , use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits) [1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor))
    cex <- 0.8/strwidth(txt)
  # abs(r) � para que na sa�da as correla��es ficam proporcionais
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
#pdf(file = "grafico.pdf")
pairs(white_calibrated, lower.panel=panel.smooth, upper.panel=panel.cor)



#Analise PCA e Normaliza��o os dados
vinhos_padroniz = as.data.frame(scale(white_calibrated))
names(vinhos_padroniz)
summary(vinhos_padroniz)


# sumariza��o das vari�veis - componentes principais
acpcor <- prcomp(vinhos_padroniz , scale = TRUE) 
summary(acpcor)

plot(1:ncol(vinhos_padroniz ), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Vari�ncia", pch = 20, cex.axis = 1.3, cex.lab = 1.3)

sum(acpcor$sdev^2)

acpcor$rotation[, 1:11]


print(acpcor$sdev[1:11] * t(acpcor$rotation[, 1:11]), digits = 3)

biplot(acpcor, xlab = "CP1", ylab = "CP2",cex.lab = 1.0, cex.axis = 1.0)

acpcor <- prcomp(vinhos_padroniz , scale = TRUE, retx = TRUE)

escore1 <- acpcor$x[, 1]
print(escore1)
hist(escore1)

escore2 <- acpcor$x[, 2]

par (mfrow=c(1,2))
hist(escore1)
hist(escore2)
par (mfrow=c(1,1))

#O conjunto de dados tem uma forte dependencia entre si, onde apenas no oitavo componente dos 12 totais � poss�vel
#explicar 90% da vari�veis envolvidas dentro do dataset.


##############################Arvore de Decis�o
# informa��es dos Par�metros do Modelo
## Usa rpart para decision tree

#training set
wine_train <- white[samp, ]

#test set
wine_test <- white[-samp, ]

#Execu��o do modelo com os dados de treino.
m.rpart <- rpart(quality , data = wine_train)

m.rpart

#plotar regra do modelo preditivo
fancyRpartPlot(m.rpart)
#rpart.plot(m_predict, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

#Avalia��o do modelo - usando predict.
p.rpart <- predict(m.rpart,newdata=wine_test, digits=2)
summary(p.rpart)

summary(wine_test$quality)

#Fun��o para o erro medio absoluto (MAE)
MAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}


MAE(wine_test$quality, p.rpart)

pred <- predict(m.rpart, newdata = wine_test)
table(pred, wine_test$taste)


Val_pred_tree <- predict(m.rpart,interval = "prediction", level = 0.95, digits = 2) 
str(Val_pred_tree)

mse_tree <- mean((white$quality - Val_pred_tree)^2)
sqrt(mse_tree)

erro_usando_media <- mean((white$quality - mean(white$quality))^2)
sqrt(erro_usando_media)


#test_prediction <- data.frame(fixed.acidity = 8.5, volatile.acidity = 0.33, citric.acid = 0.42, residual.sugar = 10.5, 
                              #chlorides = 0.065, free.sulfur.dioxide = 47, total.sulfur.dioxide = 186, density = 0.9955, 
                              #pH = 3.10, sulphates = 0.40, alcohol = 9.9)


#prediction <- predict(m_predict, test_prediction)
#prediction



########################## Try to improve the model 
########################### building the model
##Tentanto ajustar o modelo usa a M5P ele parece ter ficado em overfiting, n�o sendo confi�vel para predi��o.

m.m5p <- M5P(quality ~ taste , data = wine_train)

# building the predictor
p.m5p <- predict(m.m5p, wine_test)

p.m5p

MAE(wine_test$quality, p.m5p)


#Teste de predi��o Decision Tree Melhorada

test_prediction_improved <- data.frame(fixed.acidity = 8.5, volatile.acidity = 0.33, citric.acid = 0.42, residual.sugar = 10.5, 
                                       chlorides = 0.065, free.sulfur.dioxide = 47, total.sulfur.dioxide = 186, density = 0.9955, 
                                       pH = 3.10, sulphates = 0.40, alcohol = 9.9)



prediction <- predict(p.m5p, newdata=wine_test)
prediction


####### Random Forest

#Foi feita uma classifica��o pr�via afim de facilitar o entendimento por parte do modelo
#pois muitos vinhos est�o entre o normais, deixando poucas amostras para as demais possbiilidades.

white$taste <- ifelse(white$quality < 6, 'bad', 'good')
white$taste[white$quality == 6] <- 'normal'
white$taste <- as.factor(white$taste)

barplot(table(white$quality))

samp <- sample(nrow(white), 0.7 * nrow(white))

#training set
wine_train_forest <- white[samp, ]

#test set
wine_test_forest <- white[-samp, ]



library(randomForest)
randomF <- randomForest(taste ~ . - quality, data = wine_train_forest)
randomF

pred <- predict(randomF, newdata = wine_test_forest)
table(pred, wine_test_forest$taste)
sum(diag(table(pred, wine_test_forest$taste))) / nrow(wine_test_forest)#Precis�o do Modelo.

#Devido a forma simplista da Arvore de Decison, os resultados de retorno n�o s�o t�o favoravies umas vez que
#a simplificacao dos n�s prejudica a intepreta��o dos tipos de Vinhos e suas qualidades, isso muito devido ao dataset
#que tem uma grande quantidade de vinhos normais se comparado com bons e ruins.

#Por isso o Random Forest nos ajuda, pois ele criar v�rias Arovores de Decis�o
#e depois tira a m�dia de todas elas para chegar no modelo final.



#################### Regress�o Logistica
rm(list=ls(all=TRUE))

library(caTools)
library(MASS)

wine <- white

wine$category[wine$quality < 6] <- 1
wine$category[wine$quality == 6] <- 2
wine$category[wine$quality > 6] <- 3

#Transforma vari�vel num�rica em categ�rica:
wine$category <- as.factor(wine$category)

str(wine)

summary(wine)

#Excluindo os outliers.
wine2 <- subset(wine, residual.sugar<=40)

set.seed(3000)
spl = sample.split(wine2$category, SplitRatio = 0.7)
winetrain = subset(wine2, spl == TRUE)
winetest = subset(wine2, spl == FALSE)

modelo <- polr(category ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                 chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol , 
               data=winetrain)
modelopred <- predict(modelo, winetest, type="class")
matrizconfusao <- table(modelopred, winetest$category)
matrizconfusao

diagonal <- diag(matrizconfusao)
perc.erro <- 1 - sum(diagonal)/sum(matrizconfusao)
perc.erro

acuracia <- sum(diagonal) / sum(matrizconfusao)
acuracia

##### Regressao Logistica - GLM
modelo_log<-glm(category ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
                winetrain, family=binomial(link=logit))
summary(modelo_log)

modelo_log$coefficients

predito<-fitted(modelo_log)

summary(predito)

hist(predito)

# Criar vari�vel faixa probabilidade
fx_predito <- cut(predito, breaks=c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1), right=F)
plot(fx_predito, winetrain$category)

# Frequ�ncia absoluta
table(fx_predito, winetrain$category)

# Frequ�ncia relativa
prop.table(table(fx_predito, winetrain$category),2)


#################### Regress�o Linear

