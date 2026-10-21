# Dogwood IPM
# Oct 21, 2026
# Sophie Wieland
getwd()
library(tidyverse)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Dogwood IPM")

# Create a 5D array of all treatments (maybe 6?)
# FRI: 1, 2, 3, 4, 20/unburned = 5
# Grazer: B, C, Un = 3
        # Bison, Cattle, Ungrazed
# Browser: G, E, L, A = 4
        # Goat, Early clip, Late clip, Ambient (deer, but no purposeful browsing)
# Fire Season?: Sp, Su = 2

# + dimension for FruitProb, FruitNum, Germ, Surv = 4
TreatmentArray <- array(data = NA, dim = c(5, 3, 4, 4))

# do this multiplication in the IPM loop so I can change the bin size

transition from small adult to 1yo = probability of fruiting as a function of area * mean area of small adults * fruit number per area
* germination * survival from white pasture experiment

# something to look at: maybe I can name and label the dimensions
I know other, more knowledgeable, people
have replied to Christophe's question, but
perhaps the List would be interested to know
that zero-extent arrays are useful (to me at least)
  because although such an array  has no content, the
dimname are nevertheless retained:


 > a <- array(0,dim=c(0,3))
 > dimnames(a) <- list(size=c() , fish=c("cod","skate","crab"))
 > b <- array(0,dim=c(2,0))
 > dimnames(b) <- list(size=c("huge","small"),depth=c())
 >


We can attach these arrays ---both of  which are of length 0---using  
adiag():

 > library(magic)
 > adiag(a,b)
        fish
size    cod skate crab
   huge    0     0    0
   small   0     0    0
 >


Note how the dimnames of "a" and "b" are retained in the output.
The contents of this array are the default "pad" value of adiag().

This is terribly useful in the humble workaday world
of high-dimensional magic hypercubes.