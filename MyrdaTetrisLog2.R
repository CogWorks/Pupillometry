#This is the file that takes "complete_" data - right in the format from the lab PCs
#And saves them as rds in the form that gazetools likes and that is appropriate for pupil analysis

#What I did: 
#deleted else statements that didn't matter to my base case
#deleted columns I didn't need
#added aggregate column for board state (just lum correction number)
#added columns for 



##UPDATED AS OF APRIL 11, 2018
req.load <- function(x){
  if(eval(parse(text= paste(c("!require(",x,")"),collapse="")))){
    eval(parse(text = paste(c("install.packages('",x,"')"),collapse="")))
  }
}

if(!exists("remote_run")){
  remote_run = F
} 
if(remote_run==T){
  wd <- getwd()
  setwd(dirname(sys.frame(1)$ofile))
}
req.load("devtools")

## GET GAZETOOL
# devtools::install_github("ryanhope/gazetools")
##load_all("/home/hoper2/gazetools")

if(!require(gazetools)){
  devtools::install_github("CogWorks/gazetools")
} 

source("getGameBaselines.R")

req.load("spatstat")
req.load("ggplot2")
req.load("sp")
req.load("png")
req.load("jsonlite")
req.load("data.table")
#req.load("alphahull")
req.load("data.table")
req.load("plyr")
req.load("stringr")
req.load("Rmisc")
#Parallel

req.load("foreach")
req.load("parallel")
req.load("doParallel")
req.load("bit64")
req.load("zoo")
library(zoo)
library(jsonlite)
library(data.table)
req.load("qdapRegex")
req.load("manipulateWidget")
req.load("animation")
req.load("plotly")

if(remote_run==TRUE){
  setwd(wd)
}
options(datatable.integer64="integer64")


fromBrokenJSON <- function(txt) fromJSON(sprintf("[%s]", str_replace_all(txt,"\\(|\\)","")))


empty_board <- paste("'[",paste(rep(paste("[",paste(rep(0,10),collapse=", "),"]", sep=""),20),collapse=", "),"]'",sep="")


##Changed for popStudy
rdaTetrisLog2 <- function(sid=NULL,assessment=1, debug=FALSE, f=NULL,meta2=F, screen.res.width=NULL, screen.res.height=NULL, data.file=NULL) {
  
  if(is.null(data.file)){
      
      try(d <- fread(f,sep="\t", sep2=",", header = T, showProgress = F, na.strings=c("NA","", "None"), fill=TRUE), silent = TRUE) #TODO: determine if "Fill" screws things up
      if(!"ts" %in% names(d)){ #TODO: figure out why we are reading twice
        d <- fread(f, sep2=",", header = T, showProgress = F, na.strings=c("NA","", "None"),fill=TRUE)
      }
      #fill up shit that might have NAs
      #d[,c("smi_ts", "level", "score") := list(na.locf(na.fill(smi_ts, c("extend", NA))), na.locf(na.fill(level, c("extend", NA))), na.locf(na.fill(score, c("extend", NA))))]
      #d[,c("curr_zoid", "next_zoid") := list(na.locf(na.fill(curr_zoid, c("extend",NA))),na.locf(na.fill(next_zoid, c("extend",NA))) )]
      
    
    #Grab beginning and end frames of each game
    b <- cbind(d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="BEGIN",which=T],
               d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="END",which=T]) + 1
  }
  
  d[,game_number:=0L]
  
  
  #Fill in game and episode numbers
  for (i in 1L:nrow(b)) {
    d[b[i,1]:b[i,2], game_number:=i]
    if(i == 1L){
      d[,episode_number:=0L]
    }
    d[game_number==i,episode_number:=NA]
    epi <- c(d[game_number==i & evt_id=="GAME" & evt_data1=="BEGIN", which=T],d[game_number==i & evt_id=="EPISODE" & evt_data1=="BEGIN", which=T]-1)
    d[epi[order(epi)], episode_number:=c(0L:(.N-1))]
    d[b[i,2], episode_number:=(length(epi))]
    d[game_number==i, episode_number:=as.integer(na.locf(na.fill(d[game_number==i]$episode_number, c("extend", NA)),fromLast = T))]
  }
  d[game_number==0, episode_number:=0]
  
  
  #zero things that should be 0 instead of na
  zeroCols <- c("height","level","score","ridge_len","avgheight","roughness","filled_rows_covered","ridge_len_sqr","lines_cleared","tetris_progress")
  falseCols <- c("danger_mode","tetris_available")
  
  zeroCols <- zeroCols[which(zeroCols %in% names(d))]
  falseCols <- falseCols[which(falseCols %in% names(d))]
  
  if(length(zeroCols)>0){
    d[game_number==1 & episode_number==0 & is.na(score), (zeroCols):=0L]
  }
  if(length(falseCols)>0){
    d[game_number==1 & episode_number==0 & is.na(score), (falseCols):=FALSE]
  }
  for (i in 2:max(d[,game_number])) {
    d[game_number==i & episode_number==0 & score==d[game_number==(i-1),.SD[.N,score]], (zeroCols):=0L]
    d[game_number==i & episode_number==0 & score==d[game_number==(i-1),.SD[.N,score]], (falseCols):=FALSE]
  }
  
  ############################## BEGIN IMPORTANT CHRIS SHIT ########################
  
  
  #Do baseline corrections. Check getGameBaselines
  options( warn = -1)
  d$diams = subBaseCorrect(d)
  options(warn=0)
  
  d$criterion = criterionScore(gsub("complete", "games", f))
  
  
  ############################## END IMPORTANT CHRIS SHIT ########################
  
  
  
  #Null all the shit we don't need because they take up a lot of space on my hard drive
  d[,ECID:=NULL]
  
  d[,game_duration:=NULL]
  d[,avg_ep_duration:=NULL]
  d[,zoid_sequence:=NULL]
  d[,evt_data1:=NULL]
  d[,danger_mode:=NULL]
  d[,evt_sequence:=NULL]
  d[,rots:=NULL]
  d[,trans:=NULL]
  d[,path_length:=NULL]
  d[,min_rots:=NULL]
  d[,min_trans:=NULL]
  d[,min_path:=NULL]
  d[,min_rots_diff:=NULL]
  d[,min_trans_diff:=NULL]
  d[,min_path_diff:=NULL]
  d[,u_drops:=NULL]
  d[,s_drops:=NULL]
  d[,prop_u_drops:=NULL]
  d[,initial_lat:=NULL]
  d[,drop_lat:=NULL]
  d[,avg_lat:=NULL]
  d[,tetrises_game:=NULL]
  d[,tetrises_level:=NULL]
  d[,agree:=NULL]
  d[,delaying:=NULL]
  d[,dropping:=NULL]
  d[,zoid_rot:=NULL]
  d[,zoid_col:=NULL]
  d[,zoid_row:=NULL]
  
  d[,zoid_rep:=NULL]
  d[,smi_ts:=NULL]
  d[,smi_eyes:=NULL]
  d[,smi_samp_x_l:=NULL]
  d[,smi_samp_x_r:=NULL]
  d[,smi_samp_y_l:=NULL]
  d[,smi_samp_y_r:=NULL]
  
  d[,smi_eye_z_l:=NULL]
  d[,smi_eye_z_r:=NULL]
  d[,fix_x:=NULL]
  d[,fix_y:=NULL]
  d[,all_diffs:=NULL]
  d[,all_ht:=NULL]
  d[,all_trans:=NULL]
  d[,cd_1:=NULL]
  d[,cd_2:=NULL]
  d[,cd_3:=NULL]
  d[,cd_4:=NULL]
  d[,cd_5:=NULL]
  d[,cd_6:=NULL]
  d[,cd_7:=NULL]
  d[,cd_8:=NULL]
  d[,cd_9:=NULL]
  d[,cleared:=NULL]
  d[,col_trans:=NULL]
  d[,column_9:=NULL]
  d[,cuml_cleared:=NULL]
  d[,cuml_eroded:=NULL]
  d[,cuml_wells:=NULL]
  d[,d_all_ht:=NULL]
  d[,d_max_ht:=NULL]
  d[,d_mean_ht:=NULL]
  d[,d_pits:=NULL]
  d[,deep_wells:=NULL]
  d[,eroded_cells:=NULL]
  d[,full_cells:=NULL]
  d[,jaggedness:=NULL]
  d[,landing_height:=NULL]
  d[,lumped_pits:=NULL]
  d[,matches:=NULL]
  d[,max_diffs:=NULL]
  d[,max_ht:=NULL]
  d[,max_ht_diff:=NULL]
  d[,max_well:=NULL]
  d[,mean_ht:=NULL]
  d[,mean_pit_depth:=NULL]
  d[,min_ht:=NULL]
  d[,min_ht_diff:=NULL]
  d[,move_score:=NULL]
  d[,nine_filled:=NULL]
  d[,pattern_div:=NULL]
  d[,pit_depth:=NULL]
  d[,pit_rows:=NULL]
  d[,pits:=NULL]
  d[,row_trans:=NULL]
  d[,tetris:=NULL]
  d[,tetris_progress:=NULL]
  d[,weighted_cells:=NULL]
  d[,wells:=NULL]
  d[,score:=NULL]
  d[,SID:=NULL]
  d[,evt_data2:=NULL]
  d[,completed:=NULL]
  d[,session:=NULL]
  d[,game_type:=NULL]
  
  #Commented out so I can do some gaze things
  # d[,smi_eye_x_l:=NULL]
  # d[,smi_eye_x_r:=NULL]
  # d[,smi_eye_y_l:=NULL]
  # d[,smi_eye_y_r:=NULL]
  
  #Commented out so I can see keypress
  #d[,evt_id:=NULL]
  
  #can null these things now that we have done the corrections
  d[,board_rep:=NULL]
  d[,smi_diam_x_l:=NULL]
  d[,smi_diam_y_l:=NULL]
  d[,smi_diam_x_r:=NULL]
  d[,smi_diam_y_r:=NULL]
  
  assessment = 1
  if(is.null(data.file)){
    saveRDS(d, file=file.path(new_data, "rds", sprintf("%s.a%d.rds", sid, assessment)), compress=TRUE)
    invisible(d)
  }else{
    return(d)
  }
}



criterionScore <- function(filename){ #A little irritated cuz I could get it from the complete file, but ehhhhhh
  crit = NA
  tryCatch({
    games = fread(filename, sep="\t")
    crit = mean(tail(sort(games$score),4))
  }) #Hardcoded definition of criterion score)
  return(crit)
}


jankRatio <- function(diams){
  jank = (length(which(is.na(diams)))+length(which(diams==0)))/length(diams)
  return(jank)
}

