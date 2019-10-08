#Trying to stitch together rda eye sample data and line clears
#Ideally, we want to be able to identify all the line clears that are not tetrises
#TODO: Design a flow that makes sense so I don't pull parts of RDA out a million times

#First, find the non-tetris line clears in the rda
#Then, take those timestamps
#For each such timestamp:
# search [event_type=="EYE_SAMP"] in the rda and take the one with the closest timestamp
# index of one with closest timestamp is going to match diameters

#Find indices of closest timestamps
#Generally want A to be shorter and/or more selective
#Input: 2 vectors of timestamps
#Output: Vector of indices of close timestamps of A in B
nearIndices <- function(A, B){
  #scan forward technique, since both sets are always increasing
  #For each entry in A,
  #Scan through B
  #Start at ind of B closest to most recent A
  #End when ts differences begin increasing
  #Record nearest B ind
  if(length(A) == 0){
    return(A)
  }
  nearInds = rep(0,length(A))
  BInd = 1
  for(AInd in 1:length(A)){
    tsa = A[AInd]
    
    decreasing = TRUE
    lastDiff = 100000
    nextDiff = 100000
    while(BInd <= length(B) & decreasing){
      lastDiff = nextDiff
      nextDiff = abs(tsa-B[BInd])
      if(lastDiff < nextDiff){
        decreasing = FALSE
      } else {
        BInd = BInd + 1 
      }
      
    }
    nearInds[AInd] = BInd-1
  }
  #only returning unique ones because af there are repeats, then that means eye data was too sparse
  return(unique(nearInds)) 
}

chooseMome <- function(rda,target) {
  switch(target,
         tetris = tetrisClearsTs(rda),
         nontetris = nonTetrisClearsTs(rda),
         newZoid = newZoid(rda),
         iZoid = IPieceAppears(rda),
         press = firstPress(rda),
         start = starts(rda))
}

#Get the non-tetris line clears from GAME_EVENT rda, excluding any moments which include NAs
#Input: rda type session data with: 
#        lines_cleared
#        ts (timestamp)
#Output: Vector of timestamps where non tetrises occurred
nonTetrisClearsTs <- function(rda){
  clearTss = c()
  clears = rda$lines_cleared
  diams = rda$diams
  for(i in 1:(length(clears)-1)){
    #Increment by 4 indicates tetris occurred on last tick
    if(! ( is.na(clears[[i]]) | is.na(clears[[i+1]])) ){
      if (clears[[i]]!=clears[[i+1]] & clears[[i+1]]!=clears[[i]]+4 & clears[[i+1]]!=0){
        clearTss = c(clearTss,i)
        
      }  
    }
    
  }
  return(rda$ts[clearTss])
}

#Get the tetris line clears from GAME_EVENT rda
#Input: rda type session data with: 
#        GAME_EVENT data
#        lines_cleared
#        ts (timestamp)
#Output: Vector of timestamps where such tetrises occurred
tetrisClearsTs <- function(rda){
  clearTss = c()
  clears = rda$lines_cleared
  diams = rda$diams
  for(i in 1:(length(clears)-1)){
    #Increment by 4 indicates tetris occurred on last tick
    if(! ( is.na(clears[[i]]) | is.na(clears[[i+1]])) ){
      if (clears[[i+1]]==clears[[i]]+4){
        clearTss = c(clearTss,i)
        
      }  
    }
    
  }
  return(rda$ts[clearTss])
}
 
#Get when new zoids appear
#Input: rda type session data with: 
#        GAME_EVENT data
#        ts (timestamp)
#Output: Vector of timestamps where pieces appear
#TODO: Deal with the case where we see double I's
newZoid <- function(rda){
  newZoids = c()
  zoids = rda$curr_zoid
  for(i in 1:(length(zoids)-1)){
    if (! ( is.na(zoids[[i]]) | is.na(zoids[[i+1]]) )){ 
      if (zoids[[i]]!=zoids[[i+1]]){
        newZoids = c(newZoids,i)
      }
    }
  }
  return(rda$ts[newZoids])
}

#Returns the indices of all the first keypresses in episodes
#input: rda-type session data containing game_number
#output: Vector of timestamps where new games begin
firstPress <- function(rda){
  presses = c()
  zoids = rda$curr_zoid
  evts = rda$evt_id
  i = 1
  while (i<length(zoids)){ #find zoid appear
    if (! ( is.na(zoids[[i]]) | is.na(zoids[[i+1]]) )){ 
      if (zoids[[i]]!=zoids[[i+1]]){
        #Then we iterate until we find the first press
        while(i<length(zoids)){
          #print(evts[i])
          if(!is.na(evts[i])){
            if(evts[i]=="KEYPRESS"){
              presses = c(presses,i)
              break
            }
          }
          i = i+1
        }
      }
    }
    i = i+1
  }
 
  return(rda$ts[presses])
}

#Get the I-piece appears from GAME_EVENT rda
#Input: rda type session data with: 
#        GAME_EVENT data
#        ts (timestamp)
#Output: Vector of timestamps where I-pieces appear
#TODO: Deal with the case where we see double I's
IPieceAppears <- function(rda){
  newZoids = c()
  zoids = rda$curr_zoid
  for(i in 1:(length(zoids)-1)){
    if (! ( is.na(zoids[[i]]) | is.na(zoids[[i+1]]) ))
      if (zoids[[i]]!=zoids[[i+1]] & zoids[[i+1]]=="I"){
        
        newZoids = c(newZoids,i)
      }
    }
  return(rda$ts[newZoids])
}

#Returns the indices of all the game starts in a given session
#TODO: Is this starts or ends?
#input: rda-type session data containing game_number
#output: Vector of timestamps where new games begin
starts <- function(rda){
  gameStarts = 0
  gameNums = rda$game_number
  diams = rda$diams
  for(i in 1:(length(gameNums)-1)){
    if(! (is.na(gameNums[[i]]) | is.na(gameNums[[i+1]]))){
      if (gameNums[[i]]!=gameNums[[i+1]]){
          gameStarts = c(gameStarts,i)
      }
    }
  }
  return(rda$ts[gameStarts])
}



