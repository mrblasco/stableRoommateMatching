## Stable Roommate Problem
## contact: ablasco@fas.harvard.edu
## First version: 17 Aug, 2014
##--------------------------------------------##
rm( list=ls() )
setwd('~/Documents/stableRoomateMatching/')
source('stableRoommateFunctions.R')

## Example from Wikipedia (6 agents)
persons = matrix(
  c(
  3,4,2,6,5,  
  6,5,4,1,3,  
  2,4,5,1,6,  
  5,2,3,6,1, 
  3,1,2,4,6,
  5,1,3,4,2), 
  nrow=6, byrow=T);
persons = cbind(persons, 1:nrow(persons))
table(persons) ## check


## Run the algorithm
outI <- phaseI(persons)
phaseII(outI)


## Irving's example (8 agents)
persons = matrix(
  c(
    2,5,4,6,7,8,3,
    3,6,1,7,8,5,4,
    4,7,2,8,5,6,1,
    1,8,3,5,6,7,2,
    6,1,8,2,3,4,7,
    7,2,5,3,4,1,8,
    8,3,6,4,1,2,5,
    5,4,7,1,2,3,6),
  nrow=8, byrow=T)
persons = cbind(persons, 1:nrow(persons))
table(persons) ## check

## Run the algorithm
outI <- phaseI(persons)
phaseII(outI)


## Additional example (8 agents - no solution found)
persons = matrix(
  c(2, 3, 5, 6, 7, 4, 8,
    6, 3, 1, 4, 8, 7, 5,
    1, 7, 2, 5, 8, 4, 6,
    2, 5, 8, 3, 7, 6, 1,
    1, 6, 4, 3, 2, 8, 7,
    1, 7, 5, 2, 3, 4, 8,
    8, 6, 3, 4, 1, 5, 2,
    4, 2, 7, 1, 5, 3, 6),
  nrow=8, byrow=T)
persons = cbind(persons, 1:nrow(persons))
table(persons) ## check

## Run the algorithm
outI <- phaseI(persons)
phaseII(outI)



## Additional examples from data 
## 1429
persons <- as.matrix(read.table('data/example1429.txt')[, -1] + 1)
ranks <- makeRanks(persons)
n <- nrow(persons)
stable1429 <- c(9, 8, 4, 6, 2, 7, 3, 5, 1, 0) + 1

phaseII(phaseI())
print(stable1429)

## 9774
persons <- as.matrix(read.table('data/example9774.txt')[, -1] + 1)
ranks <- makeRanks(persons)
n <- nrow(persons)
stable9774 <- c(1, 0, 6, 4, 3, 9, 2, 8, 7, 5)+1
phaseII(phaseI())
print(stable9774)

## 3084
persons <- as.matrix(read.table('data/example3084.txt')[, -1] + 1)
ranks <- makeRanks(persons)
n <- nrow(persons)
stable3084 <- c(7, 6, 5, 9, 8, 2, 1, 0, 4, 3) +1
phaseII(phaseI())
print(stable3084)

## 9045
persons <- as.matrix(read.table('data/example9045.txt')[, -1] + 1)
ranks <- makeRanks(persons)
n <- nrow(persons)
stable9045 <- c(6,2,1,5,9,3,0,8,7,4) + 1
phaseII(phaseI())
print(stable9045)
