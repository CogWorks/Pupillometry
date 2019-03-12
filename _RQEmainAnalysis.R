#This is main file for Chris RQE
#Goal of this overall is to compare expertise bins and pupil dilation patterns in selected moments
#It draws on functions from getTetrisMomets.R
#and from cleaning files (getGameBaselines/linfix blinks/others)
#
#TODO: Decide if I wanna combine cleaning files

#Actual technique: 
# get full list of participants we will use
# For each participant
#   Run through and create/save a new data object
#   Data object has bin and eye samples and indices of moments we care about
#   Does NOT have much of the original data provided in rda or complete file
#TODO: Make this true. Right now we're looking at doing a single feature at a time
# Get list of all generated files
# Separate participants by bins
# For each expertise bin:
#  For each participant:
#   For each moment:
#    Calculate ERP features from data
#  Include participant moment averages to bin averages
#TODO: Make this true, Right now we're not doing any of this lmao


remote_run = parsed = F
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("MyrdaTetrisLog2.R")
source("getGameBaselines.R")
require(ggplot2)


inFolder = "D:/_School/RQE/DataandAnalysis"
new_data <- paste(inFolder,"QuickParseOuts1",sep="/")
data_folder <- paste(inFolder,"PopStudyData",sep="/")

######## Bin stuff is far below

#Locate SIDs, folders, and data files
allFolders = list.files(data_folder)
nS = length(allFolders)
bins = rep(0,nS)
ids = rep(0,nS)
rdsFiles = rep(0,nS)
for (i in 1:nS){
  f = allFolders[i] #The folder containing 5 files
  #Stupid trick where I take everything before the second-to-last "_" and hope that's the subject id
  uScors = gregexpr('_',f)[[1]]
  second2 = uScors[length(uScors)-1]
  
  subject.id = substr(f,1,second2-1)
  ids[i] = subject.id
}


#make all the RDS so I can read right into RDA later
for (i in 1:nS){
  subject.id = ids[i]
  rdsFile = file.path(new_data, "rds", sprintf("%s.a1.rds", subject.id)) #TODO: I fucked w/ some settings and may need to fix this
  rdsFiles[i] = rdsFile
  okay = NA #bs variable I have to make because commands in error section do not run
  if (!file.exists(rdsFile)){ #If rds file does not exist, we need to make it
    fi = allFolders[i]
    
    #File naming is consistent, so I just paste "complete"
    filename = paste(data_folder,"/", fi,"/complete_",fi,".tsv", sep="")
    tryCatch({
      oldw <- getOption("warn")
      options(warn = -1)
      
      rdaTetrisLog2(sid=subject.id, f=filename, meta2=F)
      print(paste("Rds file for",subject.id,"has been created.",nS-i,"remain"))
    
    }, error = function(e) {
      if(e=="subscript out of bounds"){
        print(paste("Complete reads not possible on",subject.id,nS-i,"remain"))
      } else {
        print(paste("Unkown error on",subject.id,nS-i,"remain"))
      }
      #Assignments here will not run. Only prints, it seems
      #bins[i] <- NA
    
    }, finally = {
      options(warn = oldw)
    })
    
  } else{
    print(paste(subject.id,"RDS already exists"))
  }
  
}


#At this point, all corrections have been completed and now we want to reduce data to easily handle-able objects
#So we compute average shit for each subject, and we can do this one feature at a time
source("summarizeFeature.R")
source("getTetrisMoments.R")
#tetris = tetrisClearsTs(rda),
#nontetris = nonTetrisClearsTs(rda),
#newZoid = newZoid(rda),
#iZoid = IPieceAppears(rda),
#start = starts(rda))
# target = 'tetris'
# target = 'nontetris'
#target = 'newZoid'
# target = 'iZoid'
#target = 'start'
 target = 'press'


 
#Run this beeboop to generate rds of moments
 #with entries of all the moments in it
 #entries contain:
 #  all of window
 #  SID
 #  criterion
{dataOuts = paste(inFolder,"moments",target,sep="/")
  before=100
  after=100
  momeLen = before + after + 1
   for (i in 1:nS){
     rdaLoc = rdsFiles[i]
     subject.id = ids[i]
     toLoc = paste(dataOuts, basename(rdaLoc),sep="/")
     if(file.exists(rdaLoc)){
       if(!file.exists(toLoc)){
         rda = readRDS(rdaLoc)
         
         subject.windows = momentWindows(rda,target,before=before,after=after)
         
         print(paste(subject.id,"saved",i/nS))
         saveRDS(subject.windows,file=toLoc)
       } else {
         print(paste(toLoc, "already exists"))
       }
     }
   }
 }
 
 ####Bin and save in folder w/moment data
 #TODO: make this happen by level
{
# binEdges = #c(0,7400,13700,21600,31000,50000,70000)
binEdges = c(0,15000,21600,31000,70000)
nBins = length(binEdges)-1
dtn = c(-before:after,"bin")
momes = data.frame(matrix(ncol=momeLen+1,nrow=0))
names(momes) = dtn
compositeLoc = paste(dataOuts,"composite",sep="/")
if(file.exists(compositeLoc)) {
  print(paste("Composite file for",target,"already exists. Now loading"))
  momes = readRDS(compositeLoc)
  } else {
   for (i in 1:nS){
     rdaLoc = rdsFiles[i]
     subject.id = ids[i]
     momeLoc = paste(dataOuts, basename(rdaLoc),sep="/")
     if(file.exists(rdaLoc)){
       if(file.exists(momeLoc)){
         rda = readRDS(rdaLoc)
         bin = max(which(binEdges<rda$criterion[1]))
         
         #Now either impute bin to all members and make a super long thingy or make separate structs for all the bins
         windows = readRDS(momeLoc)
         
         if(dim(windows)!=0){
           windows$bin = bin
           names(windows) = dtn
           
           momes = rbind(momes,windows)
         }
        
         print(paste("added",subject.id,"to the list"))
         
         
       } else {
         print(paste(toLoc, "does not exist!"))
       }
     }
   }
   saveRDS(momes,file=compositeLoc)
   print(paste(target,"Saved successfully"))
 }
}


#Real dumb. Just gonna try to make a long list of all the things to ggplot
momes = momes[1:1000]
 momeSplit = split(momes,f=momes$bin)
 


means = lapply(momeSplit,FUN = function(x){lapply(x[,1:momeLen],FUN = function(y)mean(y))})
sds = lapply(momeSplit,FUN = function(x){lapply(x[,1:momeLen],FUN = function(y)sd(y))})
conds = c()
for (b in 1:nBins){
  conds = c(conds, rep(b,momeLen))
}

momeDF <- data.frame(xs = rep((1:momeLen-before),nBins)*4,
                     means = unlist(means,use.names = F),
                     sds = unlist(sds,use.names = F),
                     cond = conds)

cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=nBins))

(momePlot = ggplot(data=momeDF, aes(x=xs,y=means,color=factor(cond)))
  + geom_point() + scale_color_manual(values = cc)
  + labs(title=target,x="ms after stimulus onset",y="difference from baseline pupil diameter (mm)",color="Expertise \nLevel"))

(momePlot = ggplot(data=momeDF, aes(x=xs,y=means,color=factor(cond)))
  + geom_point() + scale_color_manual(values = cc)
  + geom_errorbar(aes(ymin=means-sds,ymax=means+sds))
  + labs(title=target,x="ms after stimulus onset",y="difference from baseline pupil diameter (mm)",color="Expertise \nLevel"))


 {#Running this beeboop to generate plotting data
#Now read them all in again into one big structure so we can do some ez statistics
#names = c(-10:100,"peak","latency","bin","n")

#Now that it's all read into dt, we can do math and plot things
#mean of moments, peak, and latency, sorted by bin #
mems = dt[, lapply(.SD,FUN = function(x){ mean(x,na.rm = TRUE)}), by=bin, .SDcols=1:113]
setkey(mems,bin)
mems = mems[sort(bin)]
#number of occurences of each event, sorted by bin #
eachN = dt[, lapply(.SD, sum),by=bin, .SDcols=115]
setkey(eachN,bin)
eachN = eachN[sort(bin)]


}#Running this beeboop to generate plotting data
{
colors = c("red", "orange", "yellow","green","blue", "black")
plot(range(1:111)/250, range(-.07,.4), type='n')
for(b in 1:nrow(mems)){
  ones = c(t(mems[b]))[-1]
  lines(((1:111)-10)/250, ones[1:111], type='p',col=colors[b])
  print(paste("mean peak:",ones[112],"mean latency:",ones[113]))
  Sys.sleep(.7)
}
}

#Try putting these on different graphs
# try looking at first keystroke moment

#try limiting everyone to the same levels that the red novices play on (e.g. only look at levels 1-3)
 
#Try to check the variability of individuals in their moments
 #Is variability of moment predicted by level of expertise?
 
#Check replication of Gray and Lindstedt finding to look at zoid recognition differences for skill
 

 #look at subsets of subjects to see who looks at the preview box from the last event
 #Sort the subjects based on the different strategies that we observe
#Check what these graphs look like if the subjects aren't looking at the top