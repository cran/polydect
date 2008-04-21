library(polydect)
############ checking Hausdorff distance calculation################# 
X<-c(1,2,3)
Y<-c(2,3,4,9)
HD(X,Y)

# If one of the dataset is an empty set
X<-c()
Y<-c(1,2,3,4)
HD(X,Y)