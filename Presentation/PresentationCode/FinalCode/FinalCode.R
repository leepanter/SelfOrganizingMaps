####	Kohonen Self Organizin Map Example	 ####
####	Script Name: PresentationCodeV1.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will generate a Kohonen Self Organizing Map (KSOM) on the iris example data.  The goals of the analysis conducted in this script are to:
#   - us the KSOM to artificially generate a supervised variable using an un-supervised learning process.
#   - evaluate the accuracy of that learning process' classification based upon the "true" outcome.


####	Script Dependencies	 ####
set.seed(123)

####  ## Package Dependencies:
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
# install.packages(packageurl, repos = NULL, type = "source")
library(kohonen)
library(class)
library(ggplot2)
library(dplyr)
library(gridExtra)

####  ## Set Working Directory
WD="/Users/lee/Documents/GitHub/SelfOrganizingMaps/Presentation/PresentationCode/FinalCode" # <-- Replace ____ with working directory
setwd(WD)

####  Data Dependencies:

##### Import & attach mtcars data set
data("iris")
attach(iris)

#### Re-structure data variables:

## Outcome classification variable
levels(Species) = c("S", "Ve", "Vi")
iris$Species = Species
dat=iris
#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

# Data subsets for summaries
dat.s =dat[which(dat$Species=="S"),]
dat.ve=dat[which(dat$Species=="Ve"),]
dat.vi=dat[which(dat$Species=="Vi"),]

# Create Meta-variables that will be used for ranking clusters
dat$SumOfAll=Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
dat$ProdOfAll=Sepal.Length*Sepal.Width*Petal.Length*Petal.Width

# S.index =which(dat$Species=="S")
# Ve.index=which(dat$Species=="Ve")
# Vi.index=which(dat$Species=="Vi")
#

# Meta-variable plots used in paper
# p1sum=ggplot(dat, aes(x=Petal.Length, y=SumOfAll, color=Species))+
#   geom_point()+
#   xlab("Petal Length")+
#   ylab("Sum of All Variables")+
#   theme(legend.position = "none")
# p2sum=ggplot(dat, aes(x=Petal.Width, y=SumOfAll, color=Species))+
#   geom_point()+
#   xlab("Petal Width")+
#   ylab("Sum of All Variables")+
#   theme(legend.position = "none")
# p3sum=ggplot(dat, aes(x=Sepal.Length, y=SumOfAll, color=Species))+
#   geom_point()+
#   xlab("Sepal Length")+
#   ylab("Sum of All Variables")+
#   theme(legend.position = "none")
# p4sum=ggplot(dat, aes(x=Sepal.Width, y=SumOfAll, color=Species))+
#   geom_point()+
#   xlab("Sepal Width")+
#   ylab("Sum of All Variables")+
#   theme(legend.position = "none")
# grid.arrange(p1sum, p2sum,p3sum, p4sum, ncol=2)
#
# p1prod=ggplot(dat, aes(x=Petal.Length, y=ProdOfAll, color=Species))+
#   geom_point()+
#   xlab("Petal Length")+
#   ylab("Product of All Variables")+
#   theme(legend.position = "none")
# p2prod=ggplot(dat, aes(x=Petal.Width, y=ProdOfAll, color=Species))+
#   geom_point()+
#   xlab("Petal Width")+
#   ylab("Product of All Variables")+
#   theme(legend.position = "none")
# p3prod=ggplot(dat, aes(x=Sepal.Length, y=ProdOfAll, color=Species))+
#   geom_point()+
#   xlab("Sepal Length")+
#   ylab("Product of All Variables")+
#   theme(legend.position = "none")
# p4prod=ggplot(dat, aes(x=Sepal.Width, y=ProdOfAll, color=Species))+
#   geom_point()+
#   xlab("Sepal Width")+
#   ylab("Product of All Variables")+
#   theme(legend.position = "none")
# grid.arrange(p1prod, p2prod,p3prod, p4prod, ncol=2)
#
#
# p=ggplot(dat, aes(x=SumOfAll, y=ProdOfAll, color=Species))+
#   geom_point()+
#   xlab("Sum of All Variables")+
#   ylab("Product of All Variables")
# p

# Create training and test data
len.train=floor(150*0.75)

index.dat=1:150
train.index=sample(index.dat, len.train, replace = F)
test.index=index.dat[-train.index]

dat.in = iris[,1:4]

dat.train=dat.in[train.index,]
dat.train=scale(dat.train)
df.dat.train=data.frame(dat.train)

dat.test=dat.in[test.index,]
dat.test=scale(dat.test,
               center = attr(dat.train, "scaled:center"),
               scale = attr(dat.train, "scaled:scale"))
df.dat.test=data.frame(dat.test)

dat.full.train=dat[train.index,]
dat.full.test=dat[test.index,]


### Unsupervised learning

# Specify Outcome Mapping
out.grid = somgrid(xdim=4, ydim = 4, topo = "hexagonal")

# Create Clustering
som.out = som(dat.train, grid=out.grid, rlen = 400)


# Create Convergence Plot
plot(som.out, type = 'changes')

# Possible Plots
plot(som.out)
plot(som.out, type = 'count')
plot(som.out, type = 'dist.neighbours')
plot(som.out, type = 'codes')
plot(som.out, type = 'quality')

# Store SOM Cluster Assignments
somOut.16=as.factor(som.out$unit.classif)
df.dat.train$somOut.16=somOut.16

# Assign Hierarchical Clustering Values to SOM Clusters
hclust(dist(som.out$codes))
plot(hclust(dist(som.out$codes)))
class.cuts=cutree(hclust(dist(som.out$codes)),3)
plot(som.out, type = 'codes', bgcol = rainbow(3)[class.cuts])
add.cluster.boundaries(som.out, class.cuts)
ones=c(16,12,11,15)
twos=c(2,3,4,7,8)
somOut.3=c()
df.dat.train$somOut.16=as.factor(df.dat.train$somOut.16)
for(i in 1:112){
  if(df.dat.train$somOut.16[i] %in% ones){
    somOut.3[i]=1
  }
  else if(df.dat.train$somOut.16[i] %in% twos){
    somOut.3[i]=2
  } else {somOut.3[i]=3}
}

df.dat.train$somOut.3=as.factor(somOut.3)

df.dat.train$ProdOfAll=df.dat.train$Sepal.Length*df.dat.train$Sepal.Width*df.dat.train$Petal.Length*df.dat.train$Petal.Width
df.dat.train$SumOfAll =df.dat.train$Sepal.Length+df.dat.train$Sepal.Width+df.dat.train$Petal.Length+df.dat.train$Petal.Width

df.dat.train.table.summary.som3.Prod = df.dat.train %>% group_by(somOut.3) %>% summarise(mean(ProdOfAll))
df.dat.train.table.summary.som3.Prod

df.dat.train.table.summary.som3.Sum  = df.dat.train %>% group_by(somOut.3) %>% summarise(mean(SumOfAll))
df.dat.train.table.summary.som3.Sum

df.dat.train.table.summary.som3.Sum  = df.dat.train %>% group_by(somOut.3) %>% summarise(mean(Sepal.Length))
df.dat.train.table.summary.som3.Sum

df.dat.train.table.summary.som3.Sum  = df.dat.train %>% group_by(somOut.3) %>% summarise(mean(Sepal.Width))
df.dat.train.table.summary.som3.Sum

df.dat.train.table.summary.som3.Sum  = df.dat.train %>% group_by(somOut.3) %>% summarise(mean(Petal.Length))
df.dat.train.table.summary.som3.Sum

df.dat.train.table.summary.som3.Sum  = df.dat.train %>% group_by(somOut.3) %>% summarise(mean(Petal.Width))
df.dat.train.table.summary.som3.Sum

somMap.train=c()
for(i in 1:112){
  if(df.dat.train$somOut.3[i]==1){
    somMap.train[i]="Vi"
  }
  else if(df.dat.train$somOut.3[i]==2){
    somMap.train[i]="S"
  } else {somMap.train[i]="Ve"}
}

df.dat.train$somMap.train=somMap.train

length(which(dat.full.train$Species == df.dat.train$somMap.train))/112

####  Supervised
som.out.sup.1=predict(som.out,
                    newdata = dat.test,
                    trainX = dat.train,
                    trainY = dat.full.train$Species)
acc.trainY.truth=length(which(dat.full.test$Species == som.out.sup.1$prediction))/38
acc.trainY.truth
#-------------------------------------------------------------------------#
####	End Script	 ####
#-------------------------------------------------------------------------#





#-------------------------------------------------------------------------#
#####	Post-Script	#####

####  Notes:

####  Compilation Errors:

####  Execution Errors:

####  Next Scripts to Consider:

#-------------------------------------------------------------------------#
