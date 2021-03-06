Hz = 250

#For each moment in each bin
# For each Subject
#  grab all moments
#  calculate all the features for each moment
#  average all these feaures
#  average all these moments
# Average all features in bin
# Average all moments in bin
#Compare to other bins

#TODO: pass the data table around instead of parameter columns
#back in the day when I started writing this, it was data.frame and passing columns was more efficient

#TODO: I fucked up because I didn't consider that game events don't have eye samples
#I mistakenly sorta correct this in NABlinks by imputing across nonblink NA events
#But the right way is to not impute across nonblinks events at all (or at least until later)
#And then pull ONLY the eye samples when doing this analysis stuff



#Get all the windows for a given participant
momentWindows <- function(rda,target,before=10,after=100){
  diams = subset(rda,event_type=="EYE_SAMP",diams)$diams
  diamTs = subset(rda,event_type=="EYE_SAMP",ts)$ts
  
  momeTs = chooseMome(rda,target)
  momeInds = nearIndices(momeTs,diamTs)
  momeRs = c() #We use this to count only the ones we care about for tracking level
  
  momes = data.frame(matrix(ncol=before+after+1,nrow=0))
  if(length(momeInds)<=1){
    print("no moments")
  } else {
    
    n = length(momeInds)
    for (i in 1:n){
      mw = momentWindow(diams, diamTs, momeInds[i], before, after)
      mw = mw-mean(mw)  #TODO: I'm also doing another correction  here, which I'm not sure is necessary   
      if(anyNA(mw)){
        #print("sad")
        # We are sad because we are losing data
      } else{
        momes = rbind(momes,mw)      #Such that any column of momes is the same length as momeRs
        momeRs = c(momeRs,momeTs[i]) #momeRs is the timestamps of the moments we keep
      }
    }
  }
  levels = subset(rda,event_type=="GAME_EVENT",c(level,ts))
  levelTs = levels$ts
  levels = levels$level
  levels = levels[nearIndices(momeRs,levelTs)]
  
  crit = rda$criterion[1]
  momes = cbind(momes,levels)
  momes = cbind(momes,crit=rep(crit,length(levels)))
  
  return(momes)
}


#Same as below but takes an RDA. currently not in use
momeAvgRDA <- function(rda,target,before=10,after=100){
  diams = subset(rda,event_type=="EYE_SAMP",diams)$diams
  diamTs = subset(rda,event_type=="EYE_SAMP",ts)$ts
  momeTS = chooseMome(rda,target)
  momeInds = nearIndices(momeTS,diamTs)
  
  return(momentAverages(diams,diamTs, momeInds, before=before,after=after))
}


#Grab a moment window and force it to be n samples of 250hz
#Input: vector of diameters (ONLY EyE SAMPLES)
#       vector of eye timestamps
#       number of moment occurrence
#       optional: number of samples before moment that will be in window
#       number of samples after moment that will be in window
#Output: vector of diameters
momentWindow <- function(diams, diamTs, ind, before, after){
  #TODO: Handle moments where the window extends beyond samples
  #      We just want to exclude these moments entirely
  secPart = 1/Hz
  
  #Ts is in seconds, so t0 is in seconds (at before=10, after=100, t1-t0 = 0.44)
  t0 = diamTs[ind]-before*secPart
  t1 = diamTs[ind]+after*secPart
  
  #Get all samples in that time range
  ind0 = max(which(diamTs<= t0))
  ind1 = max(which(diamTs<= t1))+1 #adding one so that last number is not NA when interping
  
  #now force to 250Hz using piecewise linear interpolation
  if(anyNA(diams[ind0:ind1])){
    return(NA)
  } else {
    
    at250 = approx(x=diamTs[ind0:ind1], y=diams[ind0:ind1], xout = seq(t0,t1,by=secPart))
    return(at250$y)
  }
}


#TODO: Write this in a meaningful way and choose features
#Return features of a moment window
#Input: diameter measures of a moment window
#       cutoff for before-stimulus data
#Return: ordered vector of features
#           number of samples in latency between onset and peak
#           peak value
features <- function(mwDiams, cutoff){
  diams = mwDiams[cutoff:length(mwDiams)]
  peak = max(diams)
  latency = match(peak,diams)
  return(c(peak,latency))
}





