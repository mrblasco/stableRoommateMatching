## Stable Roommate Problem
## contact: ablasco@fas.harvard.edu
## First version: 17 Aug, 2014
##--------------------------------------------##


## Organize preferences by persons and ranks
## Input: NxN matrix of persons id so that (i, j) = id ranked jth by i
## Output: NxN matrix of ranks so that (i, j) = rank of person j by i
makeRanks = function(pers) {
  if (ncol(pers)>nrow(pers)) {stop('check input matrix dimensions!')}
  n <- nrow(pers)
  ranks = matrix(nrow=n, ncol=n) ## init
  for(i in 1:n) {
    for (j in 1:n) { ranks[i, j] = which(pers[i, ]==j) } 
  }  
  return(ranks)
}

## FUNCTION to get preference
## INPUT: (1x1 person id, 1x1 position p, NxN matrix of id, NxN matrix of ranks)
## OUTPUT: list(1x1 new person id ranked in position p by person id, 1x1 position of person id ranked by new person id)
getPreferences = function(x, i, pers, ranks) {
  y = pers[x, i] ## i-th ranked person by x
  r = ranks[y, x] ## rank of x in i-th person's preference list
  ##return(list(nextPerson = y, nextPersonRank = r))
  return(c(y, r))
}


## Irving's Phase I of the algorithm
## INPUT: NxN matrix of person id
## OUTPUT: either false or list()
phaseI = function(persons) {
  n <- nrow(persons); ranks = makeRanks(persons);
  ## Initialize vars
  holds_proposal <- rep(FALSE, n);
  rightperson <- persons[, n];  ## the last column
  rightrank <- rep(n, n); ## worst
  leftrank <- rep(1, n); ## hasn't been rejected
  leftperson <- persons[, 1]; ## 
  ## 
  for (i in 1:n) {
    proposer = i;
    test_until = FALSE;
    while ( test_until==FALSE ) {
      out = getPreferences(proposer, leftrank[proposer], persons, ranks) 
      while( out[2] > rightrank[out[1]] ) { ## keep searching 
        leftrank[proposer] <- leftrank[proposer] + 1;
        out = getPreferences(proposer, leftrank[proposer], persons, ranks)        
      }
      previous = rightperson[out[1]] ## previous
      rightrank[out[1]] <- out[2] ## update rank of receiver
      rightperson[out[1]] <- proposer ## update rightperson of receiver
      leftperson[proposer] <- out[1] ## update left person of proponent
      proposer = previous ## previous becomes a proponent
      if (holds_proposal[out[1]]==FALSE) {test_until = TRUE;} 
    }
    holds_proposal[out[1]] <- TRUE;
    if (leftrank[proposer]==n) {
      return(FALSE);
    }
  }
  return(list(leftrank = leftrank, leftperson=leftperson, rightrank= rightrank, rightperson = rightperson, holds_proposal=holds_proposal, persons=persons));
}


## Irving's Second phase 
## Input from phaseI
phaseII = function(outPhaseI) {
  n <- nrow(outPhaseI$persons); persons<-outPhaseI$persons;
  ## Init Solutions
  solution_possible = TRUE;
  solution_found = FALSE;
  ## Initialize New vars
  newOut <- list(secondperson = persons[, n], secondrank = rep(n, n), secondrightrank = rep(NA, n) )
  while ( solution_possible==TRUE & solution_found==FALSE) {
    out = seekCycle(outPhaseI, newOut) 
    if ( !any(!is.na(out$mycycle)) ) { 
      solution_found=TRUE
    } else {    
      for (i in out$mycycle[!is.na(out$mycycle)]) { 
        outPhaseI$leftrank[i] <- out$secondrank[i];
        outPhaseI$leftperson[i] <- out$secondperson[i];
        outPhaseI$rightrank[outPhaseI$leftperson[i]] <- out$secondrightrank[i]
        outPhaseI$rightperson[outPhaseI$leftperson[i]] <- i
      }
      if ( any(outPhaseI$leftrank > outPhaseI$rightrank) ) {solution_possible=FALSE} 
    }
  }
  return(list(solution_found=solution_found, rightperson=outPhaseI$rightperson))
}


### all-or-nothing cycles
seekCycle = function(outPhaseI, newOut) { 
  n <- nrow(outPhaseI$persons); ranks <- makeRanks(outPhaseI$persons)
  ## initialize
  mycycle = rep(NA, n);
  for (i in 1:n) {
    if (outPhaseI$leftrank[i] < outPhaseI$rightrank[i] ) {break}
  }
  if ( outPhaseI$leftrank[i] >= outPhaseI$rightrank[i] ) {
    return( list(mycycle=NA) ) ## return empty cycle
  } else { 
    last = 1;
    test_until = FALSE
    while ( test_until == FALSE ) { 
      mycycle[last] = i;
      last = last + 1;
      p = outPhaseI$leftrank[i];
      test_until_2 = FALSE;
      while ( test_until_2 == FALSE ) {
        p = p + 1;
        out = getPreferences(i, p, persons, ranks);
        if ( out[2] <= outPhaseI$rightrank[out[1]] ) {test_until_2 = TRUE}
      }
      newOut$secondrank[i] <- p;
      newOut$secondperson[i] <- out[1];
      newOut$secondrightrank[i] <- out[2];
      i = outPhaseI$rightperson[newOut$secondperson[i]]
      if ( i %in% mycycle ) {test_until = TRUE} 
    }
    last = last - 1;
    first = last - 1;
    while ( mycycle[first] != i ) {first = first -1; }
    return(list(mycycle=mycycle, secondrank = newOut$secondrank, secondperson=newOut$secondperson, secondrightrank = newOut$secondrightrank));
  }
}
