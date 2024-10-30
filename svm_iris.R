data("iris")

summary(iris)

head(iris)

str(iris)

names(iris)

library(lattice)

cor = cor(iris[,1:4])

rgb.palette = colorRampPalette(c("blue", "red"), space = "rgb") 

install.packages("scatterplot3d") 

library(scatterplot3d) 

colors = c("#e41a1c", "#377eb8", "#4daf4a") 

colors = colors[as.numeric(iris$Species)] 

scatterplot3d(iris[,1:3], pch = 15, color=colors)

dim(iris)

treino = sample(1:150,0.7*150)

teste = setdiff(1:150, treino)

iris_treino = iris[treino,]

iris_teste = iris[teste,]

install.packages("e1071") #biblioteca do modelo SVM

library(e1071)

modelo1 = svm(Species ~ . , data = iris_treino) #a coluna espécie é o alvo sobre as outras colunas(~)
summary(modelo1)

# Parameters:
  #SVM-Type:  C-classification 
  #SVM-Kernel:  radial
  #cost:  1 

preditos1 =predict(modelo1, iris_teste)
table(preditos1, iris_teste$Species)

#preditos1    setosa versicolor virginica
#setosa         15          0         0
#versicolor      0         11         0
#virginica       0          3        16


modelo2 = svm(Species ~ . ,kernel = "linear",  data = iris_treino)
summary(modelo2)

#Parameters:
  #SVM-Type:  C-classification 
  #SVM-Kernel:  linear 
  #cost:  1 

preditos2 =predict(modelo2, iris_teste)
table(preditos2, iris_teste$Species)

#preditos2    setosa versicolor virginica
#setosa         15          0         0
#versicolor      0         11         0
#virginica       0          3        16
> 

modelo3 = svm(Species ~ . ,kernel = "polynomial",  data = iris_treino)
summary(modelo3)

#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  1 
#degree:  3 
#coef.0:  0 

preditos3 =predict(modelo3, iris_teste)
table(preditos3, iris_teste$Species)

#preditos3    setosa versicolor virginica
#setosa         15          0         0
#versicolor      0         14         2
#virginica       0          0        14    (deu melhor resultado)

modelo4 = svm(Species ~ . ,kernel = "sigmoid",  data = iris_treino)
summary(modelo4)

#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  sigmoid 
#cost:  1 
#coef.0:  0 


preditos4 =predict(modelo4, iris_teste)
table(preditos4, iris_teste$Species)

#preditos4    setosa versicolor virginica
#setosa         15          0         0
#versicolor      0          9         0
#virginica       0          5        16

  