## Stable Roommate Problem
## contact: ablasco@fas.harvard.edu
## First version: 17 Aug, 2014
##--------------------------------------------##


## Organize preferences by persons and ranks
## Input: NxN matrix of persons id so that (i, j) = id ranked jth by i
## Output: NxN matrix of ranks so that (i, j) = rank of person j by i
makeRanks = function(pers) {
  if (ncol(pers)!=nrow(pers)) {stop('check input matrix dimensions!')}
  n <- nrow(pers)
  ranks = matrix(nrow=n, ncol=n) ## init
  for(i in 1:n) {
    for (j in 1:n) { ranks[i, j] = which(pers[i, ]==j) } 
  }  
  return(ranks)
}

## FUNCTION to get preference
## INPUT: 1x1 person id, 1x1 position p, NxN matrix of id, NxN matrix of ranks
## OUTPUT: list(1x1 new person id ranked in position p by person id, 1x1 position of person id ranked by new person id)
getPreferences = function(x, i) {
  y = persons[x, i] ## i-th ranked person by x
  r = ranks[y, x] ## rank of x in i-th person's preference list
  ##return(list(nextPerson = y, nextPersonRank = r))
  return(c(y, r))
}


## Irving's Phase I of the algorithm
## INPUT: NxN matrix of person id
## OUTPUT: either false or list()
phaseI = function() {  
  ## Initialize vars
  rightperson <- persons[, n]; ## get a proposal from myself
  leftperson <- persons[, 1]; ## I am proposing to best option 
  rightrank <- rep(n, n); ## myself is worst
  leftrank <- rep(1, n); ## 
  holds_proposal <- rep(FALSE, n);
  ## 
  for (i in 1:n) {
    proposer = i;
    test_until = FALSE; ## Repeat until find (hold proposal == FALSE)
    while ( test_until==FALSE ) {
      out = getPreferences(proposer, leftrank[proposer]) 
      while( out[2] > rightrank[out[1]] ) {## keep searching 
        leftrank[proposer] <- leftrank[proposer] + 1;
        out = getPreferences(proposer, leftrank[proposer])        
      }
      previous <- rightperson[out[1]] ## previous at the right of
      rightrank[out[1]] <- out[2] ## update rank of receiver
      rightperson[out[1]] <- proposer ## update rightperson of receiver
      leftperson[proposer] <- out[1] ## update left person of proponent
      proposer <- previous ## previous becomes a proponent
      if (holds_proposal[out[1]]==FALSE) {test_until = TRUE;} 
    }
    holds_proposal[out[1]] <- TRUE;
    if (leftrank[proposer]==n) { return(FALSE); }
  }
  return(list(leftrank = leftrank, leftperson=leftperson, rightrank=rightrank, rightperson=rightperson));
}


## Irving's Second phase 
## Input from phaseI
phaseII = function(outPhaseI) {
  solution_possible = TRUE; solution_found = FALSE; ## Repeat until 
  while ( solution_possible==TRUE & solution_found==FALSE) {
    out <- seekCycle(outPhaseI)
    if ( !any(!is.na(out$mycycle)) ) { 
      solution_found=TRUE
    } else { 
      for (i in out$mycycle[out$first:out$last]) { 
        outPhaseI$leftrank[i] <- out$secondrank[i];
        outPhaseI$leftperson[i] <- out$secondperson[i];
        outPhaseI$rightrank[out$secondperson[i]] <- out$secondrightrank[i]
        outPhaseI$rightperson[out$secondperson[i]] <- i
      }
      if ( any(outPhaseI$leftrank > outPhaseI$rightrank) ) {solution_possible=FALSE} 
    }
  }
  return(list(solution_found=solution_found, rightperson=outPhaseI$rightperson))
}


### all-or-nothing cycles
seekCycle = function(outPhaseI) { 
  ## initialize
  secondrank <- rep(NA, n);
  secondperson <- rep(NA, n);
#  secondrightrank <- rep(NA, n);
  secondrightrank <- outPhaseI$rightrank
  mycycle = rep(NA, n);
  for (i in 1:n) {
    if (outPhaseI$leftrank[i] < outPhaseI$rightrank[i] ) {break}
  }
  if ( outPhaseI$leftrank[i] >= outPhaseI$rightrank[i] ) {
    return( list(mycycle=NA) ) ## return empty cycle
  } else { 
    last = 1;
    test_until = FALSE ## repeat until
    while ( test_until == FALSE ) { 
      mycycle[last] <- i;
      last <- last + 1; 
      p <- outPhaseI$leftrank[i];
      test_until_2 = FALSE; ## repeat until
      while ( test_until_2 == FALSE ) {
        p = p + 1;
        out = getPreferences(i, p);
        if ( out[2] <= secondrightrank[out[1]] ) {test_until_2 = TRUE}
#        if ( out[2] <= outPhaseI$rightrank[out[1]] ) {test_until_2 = TRUE}
      }
      secondrank[i] <- p;
      secondperson[i] <- out[1];
      secondrightrank[i] <- out[2];
      i <- outPhaseI$rightperson[secondperson[i]] ## a new one is up
      if ( i %in% mycycle ) {test_until = TRUE} 
    }
    last <- last - 1;
    first <- last - 1;
    while ( mycycle[first] != i ) {first <- first - 1 }
    return(list(first=first, last=last, mycycle=mycycle, secondrank=secondrank, secondperson=secondperson, secondrightrank=secondrightrank));
  }
}
