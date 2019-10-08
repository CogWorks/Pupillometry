#Correct Luminance, then Blink, then Baseline

#First, we get the list of all games in a session
#For each game:
#     t0 is 50 frames after first zoid appears
#     t1 is 10 frames before second zoid appears
#     baseline is mean( diams[t0:t1] )

#Game number starts at 0 for pre-game
#Game 1 is what we want to start with
#Episode number starts at 0 for when the first zoid is falling
#Episode 0 is what we want to start with

#TODO: efficientize the fuck out of this
#Preprocesses a session
# First NA all blinks and set sample rate to 250Hz
# Then separates by game and do baseline 
#Puts everything back together to return
#Input: rda-type sessions data containing:
#     episonde_number
#     game_number
#     all 4 smi diameters
#Output: returns diameters baseline corrected
subBaseCorrect <- function(rda, samplesFromBegin = 50, samplesFromEnd = 10){
  #newDiams = lumCorrect(rda)
  #newDiams = NAblinks(newDiams)
  newDiams = NAblinks(rda[,smi_diam_x_l])
  gNumbers = rda$game_number
  
  #Commented out first-zoid baseline correction 
  # eNumbers = rda$episode_number
  # for (g in unique(gNumbers)) { 
  #   gInds = which(gNumbers == g) #TODO: Is this too slow? I can write a function to scan instead
  #   gDiams = newDiams[gInds]
  #   gEpisodes = eNumbers[gInds]
  #   
  #   
  #   #and grab the middle of the first episode
  #   ep0Inds = which(gEpisodes==0)
  #   ep0Diams = gDiams[ep0Inds]
  #   n = length(ep0Diams)
  #   basesWeWant = ep0Diams[samplesFromBegin:(n-samplesFromEnd)]
  #   
  #   gDiams = gDiams - mean(basesWeWant, na.rm = T)
  #   newDiams[gInds] = gDiams
  # }
  
  #Commented out luminance correction and whole-game baseline
  #Currently using whole-game baseline correction
  # for (g in unique(gNumbers)) {
  #   gInds = which(gNumbers == g) #TODO: Is this too slow? I can write a function to scan instead
  #   gDiams = newDiams[gInds]
  # 
  #   gDiams = gDiams - mean(gDiams, na.rm = T)
  #   newDiams[gInds] = gDiams
  # }
  
  return(newDiams)
}


#Correct for luminance using constants and Crawford and Bouma
#Interpolate between 5 luminance values for the setup
#First create a lookup table: zoidFullness -> luminance
#Build lookup table for luminance correction
#Hardcoded luminance values from setup of Cogworks Lab Popluation Study 2014-2019
C = c(20.61, 20.90, 21.19, 21.48, 21.77) #candela
  #c(35.5,36,36.5,37,37.5) #lux at 30cm

Z = c(0,50,100,150,200) #onscreen zoids
a = 30 #degrees

lut = approx(Z,C, xout = 0:200)$y #These are currently luminance values
lut = lut*a/846^.41
lut = 7.75 - 5.75 * lut/(lut+2) #And now they are D values 

#Hardcoded. Derived from idf file. Couldn't figure out a better way to do this. Maybe call smi??
mmConversionFactor = 146.6758714
#Then all we have to do is calculate fullness and use the table to correct
#Input: rda object containing
#         board state
#         diameters
#Output: luminance-corrected diameters
#TODO: Separate this out. Right now I am also applying correction to get mm from pixel values
lumCorrect <- function(rda){
  xls = rda[,smi_diam_x_l]
  yrs = rda[,smi_diam_y_r] 
  yls = rda[,smi_diam_y_l] 
  xrs = rda[,smi_diam_x_r]
  boardStates = rda$board_rep
  diams = rep(0,length(xls))
  for (i in 1:length(diams)){
    xl = xls[i]
    yl = yls[i]
    xr = xrs[i]
    yr = yrs[i]
    cubes = boardSum(boardStates[i])
    ds = c(xl,yl,xr,yr)
    NA_or_0 = min(ds)
    if(is.na(NA_or_0) | NA_or_0<=0){
      diams[i] = NA
    } else{
      diams[i] = mean(ds)/mmConversionFactor - lut[cubes+1]
      if(is.na(diams[i])){
        print("calculation for luminance correction produced an NA")
      }
    }
  }
  return(diams)
}

#Helper function for lumCorrect. Just sums a board
boardSum <- function(boardString){
  if(is.na(boardString)){
    return(0)
  }
  
  for (c in strsplit(boardString,split="")){
    b = as.numeric(c)
  }
  b[which(is.na(b))] = 0
  return(length(which(b!=0)))
}

#Returns diameter measurements after we have called blinks NA
#Switching to using this method because our technique is too blink-sensitive to interpolate across blinks
#Input: vector of diameters
#       optional: samples to lose off end of blinks
#       optional: length of an interruption that we consider a blink
#Output: Vector of new diameters
NAblinks <- function(oldDiams,samplesToKill=50,notAblink = 10){
  n = length(oldDiams)
  newDiams = oldDiams 
  s = 1
  while(s<n){
    if(is.na(oldDiams[s])){
      t0 = max(1,s-samplesToKill)
      t1 = s
      while(is.na(oldDiams[t1]) & t1<n){
        t1 = t1 + 1
      }
      if((t1-s)<=notAblink & s>1){ #if the number of na'ed samples is small, then it's not a real blink and we impute (assuming there is something to impute)
        newDiams[s:t1] = oldDiams[s-1]
      } else {
        t1 = min(n,t1+samplesToKill-1)
        newDiams[t0:t1] = NA
      }
    
      s = t1
    }
    s = s + 1
  }
  
  return(newDiams)
}

  