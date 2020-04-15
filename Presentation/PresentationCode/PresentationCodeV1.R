####	Kohonen Self Organizin Map Example	 ####
####	Script Name: PresentationCodeV1.R

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will generate a Kohonen Self Organizing Map (KSOM) on the mtcars example data.  The goals of the analysis conducted in this script are to:
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
data("mtcars")
attach(mtcars)

data("wines")
#### Re-structure data variables:

## Outcome classification variable
mtcars$cyl.factor=factor(cyl,
                         ordered = T,
                         levels = c(4,6,8),
                         labels = c("four", "six", "eight"))

## V/inline engine
mtcars$vs.factor=factor(vs, levels = c(0,1),
                        labels = c("V", "S"))

## Transmission Type: Auto/Manual
mtcars$am.factor=factor(am, levels = c(0,1),
                        labels = c("A", "M"))

## Number of Forward Gears: 3,4,5
mtcars$gear.factor=factor(gear, levels = c(3,4,5),
                        labels = c("gear.three",
                                   "gear.four",
                                   "gear.five"))

## Number of Carburators: 1,2,3,4,5,6,7,8
mtcars$carb.factor=factor(carb, levels = c(1,2,3,4,5,6,7,8),
                          labels = c("carb.one",
                                     "carb.two",
                                     "carb.three",
                                     "carb.four",
                                     "carb.five",
                                     "carb.six",
                                     "carb.seven",
                                     "carb.eight"))


## Create Working Data Set:

# normalize numerical variables
dat.num=data.frame(mpg, disp, hp, drat, wt, qsec)
dat.num=scale(dat.num)

# Re-combine with factor variables
dat=data.frame(mtcars$cyl.factor,
               dat.num,
               mtcars$vs.factor,
               mtcars$am.factor,
               mtcars$gear.factor,
               mtcars$carb.factor)

# Rename columns for easier workin
colnames(dat)=c("cyl", "mpg", "disp", "hp", "drat",
                "wt", "qsec", "vs", "am", "gear",
                "carb")

# Add previous row names as column
dat$car.names=row.names(mtcars)

# Remove Data-processing variables
rm(dat.num)
rm(WD)
rm(mtcars)

#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#

### Unsupervised learning

# Remove CYL & car name variables
dat.un=scale(dat[,-c(1,5,7,8,9,10,11,12)])

# Create KSOM grid (output layer)
out.grid=kohonen::somgrid(xdim=4, ydim = 4)

som.wines <- som(wines, grid = somgrid(5, 5, "hexagonal"))

# Specify Mapping
map.un=som(data=dat.un, grid=somgrid(5, 5, "hexagonal") )


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
