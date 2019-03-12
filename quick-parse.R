remote_run = parsed = F

### the only variables you should have to change are 
## data_folder --- this is where the data is located
## new_data --- this is where the output files will be, you need a folder called "rds" and one called "rois" in this folder
## subject.id --- whats the id for this subject
## filename --- what's the raw file you're going to work on.
##### lastly, in the section on gazetools  you will have to update the settings for the project you are working on.

## location of raw input files
data_folder <- "D:/_School/RQE/DataandAnalysis/PopStudyData/18S002_2018-1-26_12-48-6/"

## location of to-be created output files
## make sure this folder contains a folder called "rds" and one called "rois"
new_data <- "D:/_School/RQE/DataandAnalysis/QuickParseOuts1"



#source("parse-popStudy.R")
require(ggplot2)


############### step 1 ############### 
######## create RDA
subject.id = "18S002"
filename = "complete_18S002_2018-1-26_12-48-6.tsv"
filename = paste(data_folder, filename, sep="")

##this function creates an rds with the data in the proper format
rdaTetrisLog2(sid=subject.id, f=filename, meta2=F)


############### step 2 ############### 
####### load RDA/RDS
rda <- readRDS("D:/_School/RQE/DataandAnalysis/QuickParseOuts1/rds/18S002.a1.rds")
#"D:/_School/RQE/DataandAnalysis/QuickParseOuts1/rds/18S002.a1.rds"


############### step 3 ############### 
####### create ROI file
## this function creates rois, and takes a while to run. 
write.DynamicROI(getROIs(rda, 1), file.path(new_data, "rois", sprintf("%s.a1.rois.txt", subject.id)))


############### step 4 ############### 
####### load ROI
rois <- read.DynamicROI(file.path(new_data, "rois", sprintf("%s.a1.rois.txt", subject.id)))




############### step 5 ############### 
####### use gazetool
rda = rda[event_type=="EYE_SAMP"]

##make sure to adjust the parameters for your experiment. See ?pva for documentation.
g = rda[game_number > 0, gazetools(x=smi_samp_x_l, y=smi_samp_y_l, samplerate=500, rx=1680, ry=1050, sw=473.76, sh=296.1, ez=smi_eye_z_l,ex = smi_eye_x_l, ey= smi_eye_y_l, blinks=(smi_samp_x_l==0 & smi_samp_y_l==0), timestamp=smi_ts)]
g$data = cbind(g$data, rda[game_number >0, list(game_number,episode_number,curr_zoid,next_zoid,level,score)])
g$data[is.na(score),score:=0]
g$data[is.na(level),level:=0]

## check ?classify for documentation
g$classify()



############### step 6 ############### 
####### now you can plot and manipulate data (these are all do-able without an ROI file)

##for plot methods  ---  you can set filter to anything you would subset a data.table by

##filter by time window
g$plot(filter = (time>10 & time <20))

##look at episodes in a game
g$plot(filter = (episode_number %in% c(1:5) & game_number==1))


#Additionally, the default plot style is "timeseries", there are other styles. For example, here is the "spatial-raw" style:
##This is a location based scatterplot of the fixations in the subset you select
##raw samples classified
g$plot(style="spatial-raw", filter= (time < 10))



##Fixation durations, and locations. 
g$plot(style="spatial-class", filter = (time > 10 & time < 20))



## if you would like the tetris screen underlayed under the plot
tetris_bg <- as.raster(readPNG("/location/of/tetris_bg.png"))


## raw samples classified, but with tetris background
g$plot(style="spatial-raw", game_number == 4 & episode_number %in% c(1:20),  background = tetris_bg)


## there is an issue with the "spatial-class" plot function such that it plots transparent gray circles that are hard to see over the background
## the following function provides a workaround that lets you set the color of the circles and the lines
g.spatplot(g, pointcolor="blue", linecolor="red", filter = (game_number == 4 & episode_number %in% c(1:20)), background = tetris_bg)




############### step 7 ###############
##### look at data WITH ROI file (requires ROI file)

## play a single episode, all fixations as slider.
playEpisode(.g = g, .rois = rois, .game = 4, .episode = 2)


## play a single game, all episodes as slider.
playGame(.g=g, .rois = rois, .game = 4)


## you can also play over tetris board too: -- this will not be visible
playGame(.g=g, .rois = rois, .game = 4, .background = tetris_bg)

## further you can set the episode window, such that you can look at all fixations for a number of episodes in a row
## this lets you look at fixations for the current episode and previous 2. 
playGame(.g=g, .rois = rois, .game = 4, .background = tetris_bg, .episodeback = 2)

## lastly, you can change the color of the dashed lines connecting the fixations, and also the previous fixation bubble colors
playGame(.g=g, .rois = rois, .game = 4, .background = tetris_bg, linecolor = "lightblue", pointcolor = "lightblue")

## you can also change these colors without needing a background. -- note, if you only set one it autosets the other to the color you set
playGame(.g=g, .rois = rois, .game = 4,  linecolor = "lightblue", pointcolor = "lightblue")





## record a single game, all episodes in mp4. --- just fixations
recordGame(.g=g, .rois = rois, .game = 4,.interval = 1.25, .type="fix")


## record a single game, all episodes in mp4. --- allsamples
recordGame(.g=g, .rois = rois, .game = 4,.interval = .2, .type="all")

## lastly, you can make it real time at the set interval
recordGame(.g=g, .rois = rois, .game = 1,.interval = .1, .type="all", realTime = T)

## note that you can change the ratio of the plots by setting .ratio to some value y/x



############### step 8 ###############
##### look at data WITH ROI file (requires ROI file)

sp <- g$scanpath(game_number > 0, rois = rois)