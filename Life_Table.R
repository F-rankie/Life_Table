# Life_Table.R
#
# MSE10 / IMBEI, Vorlesung "Demographic Methods", Frank Wiaczny, Federal Institute for Population Research, Germany
# see Slide #28
#
# this code written by Frank M. Berger, Feb 2015
#
# Reference: "Adapted from Weeks, John R. 1981. Population. Belmont, California: Wadsworth. p. 373.
#
# LT = Life Table
#
setwd("R:/MyProjects/Life_Table")

col.Age = c(0,1,seq(5, 85, by=5))
ncols = length(col.Age)

LT <- data.frame(Age = col.Age,
                 n   = integer(length=ncols),
                 nMx = numeric(ncols),
                 nqx = numeric(ncols),
                 npx = numeric(ncols),
                 Ix  = numeric(ncols),            # integer would force rounding and introduce errors
                 ndx = integer(ncols),
                 nLx = integer(ncols),
                 Tx  = integer(ncols),
                 ex  = numeric(ncols))

# define two scopes (row selectors):
# scope - all but last row (85 to inf)
# scope2 - all but first and last row
scope <- 1:(ncols-1)
scope2 <- 2:(ncols-1)

# Step 1: calculate n: number of years in the age interval x
#
#   Method 1: for loop
#
for(i in scope) {
    LT[i,"n"] <- LT[i+1,"Age"] - LT[i,"Age"]
}
LT[ncols, "n"] <- Inf

# Step 2: nMx (as observed): age specific death rates (per 1,000)

LT$nMx <- c(15.307, 0.652, 0.303, 0.282, 0.561, 0.679, 0.785, 1.035,
             0.863, 2.506, 3.834, 5.626, 8.447, 12.667, 18.233, 30.737, 51.098, 80.850, 153.876)


# Step 3: nqx: the probability of dying between two exact ages
# 
# take formula for nqx from Excel example and accompanying text
#
LT[scope,]$nqx <- (2 * LT[scope,]$n * LT[scope,]$nMx/1000) / (2 + LT[scope,]$n*LT[scope,]$nMx/1000)
LT[ncols,]$nqx <- 1.0


# Step 4: npx: the probability of surviving between two exact ages (= 1-(nqx: probability of dying))
#
LT$npx <- 1.0 - LT$nqx


# Step 5: Ix: the number of survivors at exact age x
#
LT[1,]$Ix <- 100000
for(i in 2:ncols) {
    LT[i,]$Ix <- LT[i-1,]$Ix*LT[i-1,]$npx
}

# Step 6: ndx: the number of deaths between two exact ages, x and x+n
#
LT$ndx <- LT$Ix * LT$nqx


# Step 7: nLx: the average number of years lived between ages x and x+n
# specific formulas for first and last row
#
# first row
LT$nLx[1] <- 0.3*LT$Ix[1] + 0.7*LT$Ix[2]
#
# middle rows
LT[scope2,]$nLx <- LT[scope2,]$n * (LT[scope2,]$Ix - 0.5*LT[scope2,]$ndx)
#
# last row
LT$nLx[ncols] <- LT$Ix[ncols]/(LT$nMx[ncols]/1000)

# Step 8: Tx: total number of person years remaining to be lived aged x and over
#         ex: Life expectancy at age x (= Tx / Ix)
#
# Start value (age=0): Total sum of nLx
#
LT$Tx[1] <- sum(LT$nLx)
LT$ex[1] <- LT$Tx[1]/LT$Ix[1]
#
for(i in 2:ncols) {
    LT$Tx[i] <- LT$Tx[i-1] - LT$nLx[i-1]
    LT$ex[i] <- LT$Tx[i] / LT$Ix[i]
}


######## calculation done, now output ########


# round values prior to output
LT$Ix <- round(LT$Ix)
LT$ndx <- round(LT$ndx)
LT$nLx <- round(LT$nLx)
LT$Tx <- round(LT$Tx)
LT$ex <- round(LT$ex, 2)

# output
LT

plot(LT$Age, LT$Ix)
