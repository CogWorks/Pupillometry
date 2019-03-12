require("classInt")
require(data.table)

data_folder <- "D:/_School/RQE/DataandAnalysis/PopStudyData/"


#Locate SIDs, folders, and data files
#Weird substring stuff because I didn't want to change file names or Catherine-provided binning data
allFolders = list.files(data_folder)
nS = length(allFolders)
crits = c()
for (i in 1:nS){
  f = allFolders[i] #The folder containing 5 files
  gamesF = paste(data_folder, f,"/games_",f,".tsv", sep="")
  if(file.exists(gamesF)){
    games = fread(gamesF, sep="\t")
    tryCatch({
      
        crits = c(crits,mean(tail(sort(games$score),4)))
    }) #Hardcoded definition of criterion score)
  }
  
  
}

k = 4
classIntervals(sort(crits),k,style="jenks")
hist(crits)
length(crits)
