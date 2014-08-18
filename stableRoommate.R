## Stable Roommate Problem
## contact: ablasco@fas.harvard.edu
## First version: 17 Aug, 2014
##--------------------------------------------##
rm(list=ls())
setwd('~/Documents/stableRoomateMatching/')
source('stableRoommateFunctions.R')

## Example from Wikipedia
n = 6;

## Preferences - persons
persons = matrix(
  c(
  3,4,2,6,5,  
  6,5,4,1,3,  
  2,4,5,1,6,  
  5,2,3,6,1, 
  3,1,2,4,6,
  5,1,3,4,2), 
  nrow=n, byrow=T);
persons = cbind(persons, 1:n)

## Preferences - ranks 
ranks = makeRanks()

## Run the algorithm
phaseI()
phaseII()
## Display the results
matrix(c(1:n,rightperson), ncol=2)

## Irving's example
n = 8
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
  nrow=n, byrow=T)
persons = cbind(persons, 1:n)
ranks = makeRanks()

### Get the matchings
phaseI()
phaseII()
## Display results
matrix(c(1:n,rightperson), ncol=2)




