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
library(kohonen)
library(class)

####  ## Set Working Directory
WD="/Users/lee/Documents/GitHub/SelfOrganizingMaps/Presentation/PresentationCode" # <-- Replace ____ with working directory
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

#calculate summary stat data frame
dat.s=subset(dat, Species=="S")
dat.ve=subset(dat, Species=="Ve")
dat.vi=subset(dat, Species=="Vi")


min.val.Sepal.Length = c(min(dat.s$Sepal.Length), min(dat.ve$Sepal.Length), min(dat.vi$Sepal.Length))
min.val.Sepal.Width  = c(min(dat.s$Sepal.Width),  min(dat.ve$Sepal.Width),  min(dat.vi$Sepal.Width))
min.val.Petal.Length = c(min(dat.s$Petal.Length), min(dat.ve$Petal.Length), min(dat.vi$Petal.Length))
min.val.Petal.Width  = c(min(dat.s$Petal.Width),  min(dat.ve$Petal.Width),  min(dat.vi$Petal.Width))

mean.val.Sepal.Length =c(mean(dat.s$Sepal.Length), mean(dat.ve$Sepal.Length), mean(dat.vi$Sepal.Length))
mean.val.Sepal.Width  =c(mean(dat.s$Sepal.Width),  mean(dat.ve$Sepal.Width),  mean(dat.vi$Sepal.Width))
mean.val.Petal.Length =c(mean(dat.s$Petal.Length), mean(dat.ve$Petal.Length), mean(dat.vi$Petal.Length))
mean.val.Petal.Width  =c(mean(dat.s$Petal.Width),  mean(dat.ve$Petal.Width),  mean(dat.vi$Petal.Width))

max.val.Sepal.Length =c(max(dat.s$Sepal.Length), max(dat.ve$Sepal.Length), max(dat.vi$Sepal.Length))
max.val.Sepal.Width  =c(max(dat.s$Sepal.Width),  max(dat.ve$Sepal.Width),  max(dat.vi$Sepal.Width))
max.val.Petal.Length =c(max(dat.s$Petal.Length), max(dat.ve$Petal.Length), max(dat.vi$Petal.Length))
max.val.Petal.Width  =c(max(dat.s$Petal.Width),  max(dat.ve$Petal.Width),  max(dat.vi$Petal.Width))


df.Sepal.Length=rbind(min.val.Sepal.Length,
                      mean.val.Sepal.Length,
                      max.val.Sepal.Length)
row.names(df.Sepal.Length)=c("min-SepalLen", "mean-SepalLen", "max-SepalLen")

df.Sepal.Width=rbind(  min.val.Sepal.Width,
                      mean.val.Sepal.Width,
                       max.val.Sepal.Width)
row.names(df.Sepal.Width)=c("min-SepalWid", "mean-SepalWid", "max-SepalWid")

df.Petal.Length=rbind( min.val.Petal.Length,
                      mean.val.Petal.Length,
                       max.val.Petal.Length)
row.names(df.Petal.Length)=c("min-PetalLen", "mean-PetalLen", "max-PetalLen")

df.Petal.Width=rbind(  min.val.Petal.Width,
                      mean.val.Petal.Width,
                       max.val.Petal.Width)
row.names(df.Petal.Width)=c("min-PetalWid", "mean-PetalWid", "max-PetalWid")

df.sum=rbind(df.Sepal.Length,
             df.Sepal.Width,
             df.Petal.Length,
             df.Petal.Width)

# Un-comment the line below to get an Xcell/csv version of the statistical summaries
# write.csv(df.sum, file = "summaryDF.csv")

dat.in = iris[,-5]
dat.in=scale(dat.in)


# Remove Data-processing variables
rm(WD)


#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

### Unsupervised learning

# Specify Outcome Mapping
out.grid = somgrid(xdim=4, ydim = 4, topo = "hexagonal")


# Create Clustering
som.out = som(dat.in, grid=out.grid)


# Create Convergence Plot
plot(som.out, type = 'changes')

# Possible Plots
plot(som.out)
plot(som.out, type = 'mapping')
plot(som.out, type = 'count')
plot(som.out, type = 'dist.neighbours')
plot(som.out, type = 'codes')
plot(som.out, type = 'quality')


dat$somOut.16=som.out$unit.classif
somOut.16=som.out$unit.classif

### Specify Outcome Based Uppon number of Clusters
# The code below uses a heirarchical clustering algorithm to find three discrete clusters within the output space of the KSOM.  The code will allow us to create a mapping to the output classes defined by the flower types.

hclust(dist(som.out$codes[[1]]))
class.cuts=cutree(hclust(dist(som.out$codes[[1]])),3)
plot(som.out, type = 'codes', bgcol = rainbow(3)[class.cuts])
add.cluster.boundaries(som.out, class.cuts)

# Based on the clustering, we will assign the following "group-mappings"
# 1,2,3,4,7,8 --> "Vi"
# 5,6,9,13 --> "Ve"
# others --> "S"

# The above information is based upon conclusions drawn by comparing the statistics calculated from the to the groups in the KSOM

somOut=c()
for(i in 1:150){
  if(somOut.16[i]==1){
    somOut[i]="Vi"
  } else if(somOut.16[i]==2){
    somOut[i]="Vi"
  } else if(somOut.16[i]==3){
    somOut[i]="Vi"
  } else if(somOut.16[i]==4){
    somOut[i]="Vi"
  } else if(somOut.16[i]==7){
    somOut[i]="Vi"
  } else if(somOut.16[i]==8){
    somOut[i]="Vi"
  } else if(somOut.16[i]==5){
    somOut[i]="Ve"
  } else if(somOut.16[i]==6){
    somOut[i]="Ve"
  } else if(somOut.16[i]==9){
    somOut[i]="Ve"
  } else if(somOut.16[i]==13){
    somOut[i]="Ve"
  } else if(somOut.16[i]==14){
    somOut[i]="Ve"
  } else {
    somOut[i]="S"
  }
}

dat$somOut=somOut

length(which(dat$Species != dat$somOut))/150

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
