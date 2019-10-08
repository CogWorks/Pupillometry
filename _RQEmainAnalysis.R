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

source("MyrdaTetrisLog2.R") #may have to run this twice (dependencies are weird)
source("getGameBaselines.R")
req.load("ggplot2")
req.load('svglite')


inFolder = dirname(getwd())
new_data <- paste(inFolder,"QuickParseOuts1",sep="/")
data_folder <- paste(inFolder,"PopStudyData",sep="/")

######## Bin stuff is far below

#Locate SIDs, folders, and data files
#Skip these if I already have the data in RDAs
{
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
  
  rdsFile = file.path(new_data, "rds", sprintf("%s.a1.rds", subject.id))
  rdsFiles[i] = rdsFile
}
}
#make all the RDS so I can read right into RDA later
for (i in 1:nS){
  
  rdsFile = rdsFiles[i]
  subject.id = ids[i]
  
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
# target = 'tetris'
# target = 'nontetris'
# target = 'iZoid'
#target = 'start'
#target = 'newZoid'
target = 'press'

graphOne <- function(momeWindows,lmax=30,SID){
  momeWindows = subset(momeWindows,levels<=lmax)
  means = lapply(momeWindows[,1:momeLen],FUN = function(x){mean(x)})
  sds = lapply(momeWindows[,1:momeLen],FUN = function(x){sd(x)})
  momeDF <- data.frame(xs = (1:momeLen-before)*4,
                       means = unlist(means,use.names = F),
                       sds = unlist(sds,use.names = F))
  bin = max(which(momeWindows$crit[1]>=binEdges))
  #momePlot = ggplot(data=momeDF, aes(x=xs,y=means)) + geom_point() + labs(title=paste(target,SID,"level <=",lmax,"n =",length(momeWindows$levels),"bin = ",bin),x="ms after stimulus onset",y="difference from baseline pupil diameter (mm)",color="Expertise \nLevel") + geom_errorbar(aes(ymin=means-sds,ymax=means+sds))
  momePlot = ggplot(data=momeDF, aes(x=xs,y=means)) + geom_point() + labs(title=paste(target,SID,"level <=",lmax,"n =",length(momeWindows$levels),"bin = ",bin),x="ms after stimulus onset",y="Pupil diameter (a.u.)",color="Expertise \nLevel") + geom_errorbar(aes(ymin=means-sds,ymax=means+sds)) + ylim(min=-100,max=20)
  #add parentheses around momePlot to get the image to print
  
  ggsave(filename=paste(paste(inFolder,"pictures",SID,sep="/"),".svg",sep=""), plot = momePlot)
  
}
lmax=30

binEdges = c(0,15000,21600,31000,70000) # binEdges = #c(0,7400,13700,21600,31000,50000,70000) #hardcoded options
# {k = 4
# cd = classIntervals(sort(crits),k,style="jenks")
# binEdges = cd$brks
# }
nBins = length(binEdges)-1
before=100
after=100
dtn = c(-before:after,"level","bin") #TODO: this dtn stuff should be in the function for making the window (esp the part in the for loop)

#Run this beeboop to generate rds of moments
#with entries of all the moments in it
#entries contain:
#  all of window
#  SID
#  criterion
#  level
# has a very nice and interesting graph
#Also make pictures
{dataOuts = paste(inFolder,"moments",target,sep="/")
  momeLen = before + after + 1
  for (i in 1:nS){
    rdaLoc = rdsFiles[i]
    subject.id = ids[i]
    toLoc = paste(dataOuts, basename(rdaLoc),sep="/")
    if(file.exists(rdaLoc)){
      if(!file.exists(toLoc)){
        rda = readRDS(rdaLoc)
        
        momeWindows = momentWindows(rda,target,before=before,after=after)
                  
        saveRDS(momeWindows,file=toLoc)
        print(paste(subject.id,"saved",i/nS))
      } else {
        momeWindows = readRDS(toLoc)
        graphOne(momeWindows,lmax=lmax,SID=subject.id)
        
        print(paste(toLoc, "already exists"))
      }
    }
  }
}




####Bin and save in folder w/moment data
{
  momes = data.frame(matrix(ncol=momeLen+2,nrow=0))#momeLen +1 for bin +1 for level
  names(momes) = dtn
  compositeLoc = paste(dataOuts,"composite",sep="/")
  if(file.exists(compositeLoc)) {
    print(paste("Composite file for",target,"already exists. Now loading"))
    momes = readRDS(compositeLoc)
  } else {
    for (i in 1:nS){
      rdaLoc = rdsFiles[i]
      subject.id = ids[i] #TODO: clean up rdaLoc and extra statements here
      momeLoc = paste(dataOuts, basename(rdaLoc),sep="/")
      if(file.exists(rdaLoc)){
        if(file.exists(momeLoc)){
          
          windows = readRDS(momeLoc)

          
          if(dim(windows)!=0){ #TODO: this check works, but yields a warning
            bin = max(which(windows$crit[1]>=binEdges))
            windows$crit = bin #TODO: this is kinda a lie 
            names(windows) = dtn
            momes = rbind(momes,windows)
          }
          
          #print(paste("added",subject.id,"to the list"))
          
          
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
#run averages and make graph
{
momeSplit = subset(momes,level<=lmax)
momeSplit = split(momeSplit,f=momeSplit$bin) 
#momeSplit = split(momes,f=momes$level)
nConds = length(momeSplit)

means = lapply(momeSplit,FUN = function(x){lapply(x[,1:momeLen],FUN = function(y)mean(y))})
sds = lapply(momeSplit,FUN = function(x){lapply(x[,1:momeLen],FUN = function(y)sd(y))})
conds = c()
for (b in 1:nConds){
  conds = c(conds, rep(b,momeLen))
}

momeDF <- data.frame(xs = rep((1:momeLen-before),nConds)*4,
                     means = unlist(means,use.names = F),
                     sds = unlist(sds,use.names = F),
                     cond = conds)

cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=nConds))

(momePlot = ggplot(data=momeDF, aes(x=xs,y=means,color=factor(cond)))
  + geom_point() + scale_color_manual(values = cc)
  + labs(title=paste(target,"where level <=",lmax),x="ms after key press",y="Pupil diameter (a.u.)",color="Expertise \nLevel"))
} 

(momePlot = ggplot(data=momeDF, aes(x=xs,y=means,color=factor(cond)))
  + geom_point() + scale_color_manual(values = cc)
  + geom_errorbar(aes(ymin=means-sds,ymax=means+sds))
  + labs(title=paste(target,"where level <=",lmax),x="ms after key press",y="Pupil diameter (a.u.)",color="Expertise \nLevel"))






#look at lots of graphs for individuals
#Is variability of moment predicted by level of expertise?

#Check replication of Gray and Lindstedt finding to look at zoid recognition differences for skill


#look at subsets of subjects to see who looks at the preview box from the last event
#Sort the subjects based on the different strategies that we observe
#Check what these graphs look like if the subjects aren't looking at the 

#individuals have frequency (Roussel idea)

#check newZoid - iZoid

#Check RTs and check IIT of RTs
#reduce the number of corrections I'm making