## Stable Roommate Problem
## contact: ablasco@fas.harvard.edu
## First version: 17 Aug, 2014
##--------------------------------------------##


## Organize preferences by persons and ranks
makeRanks = function() {
  ranks = matrix(nrow=n, ncol=n)
  for(i in 1:n) {
    for (j in 1:n) {
      ranks[i, j] = which(persons[i, ]==j)
    } 
  }  
  return(ranks)
}

## Preferences
getPreferences = function(x, i) {
  y = persons[x, i] ## i-th ranked person by x
  r = ranks[y, x] ## rank of x in i-th person's preference list
  ##return(list(nextPerson = y, nextPersonRank = r))
  return(c(y, r))
}

## Irving's Phase I of the algorithm
phaseI = function() {
  ## Initialize (global vars)
  holds_proposal <<- rep(FALSE, n);
  rightperson <<- persons[, n];  ## the last column
  rightrank <<- rep(n, n); ## worst
  leftrank <<- rep(1, n); ## hasn't been rejected
  leftperson <<- persons[, 1]; ## 
  ## 
  for (i in 1:n) {
    proposer = i;
    test_until = FALSE;
    while ( test_until==FALSE ) {
      out = getPreferences(proposer, leftrank[proposer]) 
      ## out[1] := next person; out[2] := rank of i of next person
      while( out[2]  > rightrank[out[1]] ) { ## if true, keep searching 
        leftrank[proposer] <<- leftrank[proposer] + 1;
        out = getPreferences(proposer, leftrank[proposer])        
      }
      #cat(proposer,'propose to', out[1],'\n')
      previous = rightperson[out[1]] ## previous
      rightrank[out[1]] <<- out[2] ## update rank of receiver
      rightperson[out[1]] <<- proposer ## update rightperson of receiver
      leftperson[proposer] <<- out[1] ## update left person of proponent
      proposer = previous ## previous becomes a proponent
      if (holds_proposal[out[1]]==FALSE) {test_until = TRUE;} 
    }
    holds_proposal[out[1]] <<- TRUE;
    if (leftrank[proposer]==n) {
      return(FALSE);
    }
  }
  return(TRUE);
}

### all-or-nothing cycles
seekCycle = function() { 
  ## initialize
  mycycle = rep(NA, n);
  for (i in 1:n) {
    if ( leftrank[i] < rightrank[i] ) {break}
  }
  if ( leftrank[i] >= rightrank[i] ) {
    return( list(first=1,last=1,mycycle=NA) ) ## return empty cycle
  } else { 
    last = 1;
    test_until = FALSE
    while ( test_until == FALSE ) { 
      mycycle[last] = i;
      last = last + 1;
      p = leftrank[i];
      test_until_2 = FALSE;
      while ( test_until_2 == FALSE ) {
        p = p + 1;
        out = getPreferences(i, p);
        if ( out[2] <= rightrank[out[1]] ) {test_until_2 = TRUE}
      }
      secondrank[i] <<- p;
      secondperson[i] <<- out[1];
      secondrightrank[i] <<- out[2];
      i = rightperson[secondperson[i]]
      if ( i %in% mycycle ) {test_until = TRUE} 
    }
    last = last - 1;
    first = last - 1;
    while ( mycycle[first] != i ) {first = first -1; }
    return(list(first=first,last=last,mycycle=mycycle));
  }
}

## Second phase 
phaseII = function() { 
  solution_possible = TRUE;
  solution_found = FALSE;
  ## Initialize
  secondperson <<- persons[, n];  ## the last column
  secondrank <<- rep(n, n); ## worst
  secondrightrank <<- rep(NA, n);
  while ( solution_possible==TRUE & solution_found==FALSE) {
    out = seekCycle() 
    if ( !any(!is.na(out$mycycle)) ) { 
      solution_found=TRUE
    } else {
      for (i in out$mycycle[!is.na(out$mycycle)]) { 
        leftrank[i] <<- secondrank[i];
        leftperson[i] <<- secondperson[i];
        rightrank[leftperson[i]] <<- secondrightrank[i]
        rightperson[leftperson[i]] <<- i
      }
      if ( any(leftrank > rightrank) ) {solution_possible=FALSE} 
    }
  }
  return(solution_found)
}
