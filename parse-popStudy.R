##setwd("~/cogback/cogback/Projects/Tetris/Rational Tetris/Analyses/Hope_2014")
# setwd("~/cogback/cogback/Projects/Tetris/Time Pressure Tetris/Sangster_2015")


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
##source("rc.R")
source("getROIs.R")

req.load("spatstat")
req.load("ggplot2")
req.load("sp")
req.load("png")
#req.load("alphahull")
req.load("data.table")
req.load("plyr")
req.load("stringr")
req.load("jsonlite")
req.load("Rmisc")
#Parallel

req.load("foreach")
req.load("parallel")
req.load("doParallel")
req.load("bit64")
req.load("zoo")
req.load("qdapRegex")
req.load("manipulateWidget")
req.load("animation")
req.load("plotly")
if(remote_run==TRUE){
  setwd(wd)
}
options(datatable.integer64="integer64")


if(!exists("data_folder")){
  data_folder <- "../Data"
}
if(!exists("bad")){
  bad <- NULL
}
# bad2 <- c("EXP")
##204 games 4 and 5
##222 games 6,7,8,9

fromBrokenJSON <- function(txt) fromJSON(sprintf("[%s]", str_replace_all(txt,"\\(|\\)","")))

getEpisodeTimes <- function(rda, game, episode) range(rda[game_number==game & episode_number==episode, smi_ts])

highersize <-   function(x){x$vals[which(x$size==max(x$size))]}

p <- sid <- 201
assessment=1

##Changed for popStudy:
getAssessments <- function(exclude=NULL, debug=FALSE, meta2=F) {
  if(debug==TRUE){
    print("getAssessments")
  }
  if(meta2==F){
    if(parsed==T){
      a1 <- list.files("../Data/d1/assessment1/")
      a2 <- list.files("../Data/d4/assessment2/")
      a1 <- a1[which(unname(sapply(a1, function(x) file.info(sprintf("../Data/d1/assessment1/%s/games.txt",x))$size>0)))]
      a2 <- a2[which(unname(sapply(a2, function(x) file.info(sprintf("../Data/d4/assessment2/%s/games.txt",x))$size>0)))]
      a1.ids <- unname(sapply(a1, substr, 1, 3))
      a2.ids <- unname(sapply(a2, substr, 1, 3))
      ids.common <- intersect(a1.ids, a2.ids)
      a1 <- a1[sapply(a1, function(x) substr(x,1,3) %in% ids.common)]
      a2 <- a2[sapply(a2, function(x) substr(x,1,3) %in% ids.common)]
      a <- mapply(c, a1, a2, SIMPLIFY=FALSE)
      names(a) <- ids.common
      a[which(names(a) %in% exclude)] <- NULL
    }else{
      a <- list.files(sprintf(data_folder))
      a <- a[which(unname(sapply(a, function(x) file.info(sprintf("%s/%s/games_%s.tsv",data_folder,x,x))$size>0)))]
      if(length(unique(unname(sapply(a,substr,1,4)))) > (0.5 * NROW(a))){
        a.ids <- unname(sapply(a, substr, 1, 4))
        a <- mapply(c,a, SIMPLIFY=FALSE)
        names(a) <- a.ids
        a[exclude] <- NULL
      }else{
        if(sum(grepl("15_s3_",a))>1){
          a.ids <- unname(sapply(a, rm_between, "15_[sS]3_", "_2015-", extract=TRUE,fixed=F))
        }else if(sum(grepl("16_s1",a))>1){
          a.ids <- unname(sapply(a, rm_between, "16_[sS]1_", "_2016-", extract=TRUE,fixed=F))
        }else{
          file.year <- c(2016:format(Sys.Date(), "%Y"))[which(sapply(c(2016:format(Sys.Date(), "%Y")), function(x) sum(grepl(x,a)))!=0)]
          file.start <- gsub("s","[sS]",substr(a[which(grepl(file.year,a))[1]],1,3), ignore.case = T)
          a.ids <- unname(sapply(a, rm_between, file.start, paste("_",file.year,"-",sep=""),extract=TRUE, fixed=F))
        }
        a <- mapply(c,a, SIMPLIFY=FALSE) 
        names(a) <- a.ids 
        a[which(names(a) %in% exclude)] <- NULL 
        a <- a[!is.na(names(a))]
      }
      if(length(names(a)) > length(unique(names(a)))){
        print("Non-unique Names, only using longest files.")
        ad <- data.table(names=names(a), vals=a)[, size:= sapply(vals, function(x) file.info(sprintf("%s/%s/games_%s.tsv",data_folder,x,x))$size)]
        ad <- unique(ad[, truval := highersize(.SD), by=names][,list(names,truval)])
        a <-  as.list(ad$truval)
        names(a) <- ad$names
        
      }
    }
  }else{
    a <- list.files(sprintf(data_folder))
    a.ids <- unname(sapply(a,rm_between,"_",".",extract=TRUE ))
    a <- mapply(c,a, SIMPLIFY=FALSE)
    names(a) <- a.ids
    a[which(names(a) %in% exclude)] <- NULL
    a <- a[!is.na(names(a))]
  }
  a
}

playEpisode <- function(.g, .rois, .game, .episode, .background = NULL, .start=1, debug=FALSE) {
  if(debug==TRUE){
    print("playEpisode")
  }
  if(require(manipulate)){
    .call <- as.call(quote(.g$fixations()))
    .call[[2]] <- as.quoted(sprintf("game_number==%d & episode_number==%d", .game, .episode))[[1]]
    .fix <- eval(.call)
    .call <- as.call(quote(.g$plot()))
    .call[[2]] <- as.quoted(sprintf("game_number==%d & episode_number==%d", .game, .episode))[[1]]
    .call$style <- "spatial-class"
    if(!is.null(.background)){
      .call$background <- .background
    }
    p <- eval(.call)
    
    manipulate(p + geom_path(aes(x=x,y=y,color=parent,group=id),data=.fix[fixation,.rois[start<timestamp.begin & timestamp.begin<=end]][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id]) + geom_point(aes(x=fixation.x,y=fixation.y),size=4,color="red",data=.fix[fixation]),fixation=slider(1,nrow(.fix),.start))
  }
}

playGame <- function(.g, .rois, .game, .background = NULL, .episodeback = 0, .start=1, pointcolor = NULL, linecolor=NULL, debug=FALSE ) {
  if(debug==TRUE){
    print("playGame")
  }
  if(!is.null(.background)){
    if(is.null(pointcolor)){
      if(!is.null(linecolor)){
        pointcolor = linecolor
      } else{
        pointcolor = "blue"
      }
    }
    if(is.null(linecolor)){
      if(!is.null(pointcolor)){
        linecolor = pointcolor
      }else{
        linecolor = "blue"
      }
    }
  } else {
    ##if one color is not null, set the other to it
    if(!is.null(linecolor)){
      pointcolor = linecolor
    }
    if(!is.null(pointcolor)){
      linecolor = pointcolor
    }
  }
  .call <- as.call(quote(.g$fixations()))
  .call[[2]] <- as.quoted(sprintf("game_number==%d", .game ))[[1]]
  .fix <- eval(.call)
  
  if(require(manipulateWidget)){
    if(is.null(linecolor)){
      
      .call <- as.call(quote(.g$plot()))
      .call[[2]] <- as.quoted(sprintf("game_number==%d", .game))[[1]]
      .call$style <- "spatial-class"
      
      p <- eval(.call)
    }else{
      if(is.null(.background)){
        p.raw <- g.spatplot(.g, pointcolor = pointcolor, linecolor = linecolor, filter = (game_number==.game))
        p <- copy(p.raw)
      }else{
        p.raw <- g.spatplot(.g, pointcolor = pointcolor, linecolor = linecolor, filter = (game_number==.game), .background = .background)
        p <- copy(p.raw)
      }
    }
    
    remap.p <- function( end, start=0){
      
      newp <- copy(p)
      if(!is.null(.background)){
        newp$layers[[2]]$data = .fix[fixation_ids %in% .get.fix(end=end,start=start)]
        newp$layers[[3]]$data = .fix[fixation_ids %in% .get.fix(end=end,start=start)]
        # whichpoint <- which(lapply(p$layers, function(x) "GeomPoint" %in% class(x$geom))==T)
        # whichline <- which(lapply(p$layers, function(x) "GeomPath" %in% class(x$geom))==T)
        # newp$layers[[whichpoint]]$data = p$layers[[whichpoint]]$data[which(as.numeric(p$layers[[whichpoint]]$data$fixation_ids) %in% .get.fix(p=p,end=end,start=start))]
        # newp$layers[[whichline]]$data = p$layers[[whichline]]$data[which(as.numeric(p$layers[[whichpoint]]$data$fixation_ids) %in% .get.fix(p=p,end=end,start=start))]
        # newp$layers[[whichpoint]]$aes_params$colour = pointcolor
        # newp$layers[[whichline]]$aes_params$colour = linecolor
        # 
        
      }else{
        newp$data <- p$data[which(as.numeric(p$data$fixation_ids) %in% .get.fix(end=end,start=start))]
        
      }
      newp
      
    }
    
    
    
    .get.fix <- function(end, start = 0){
      as.numeric(unlist(unique(.g$data[game_number == .game & episode_number %in% c(start:end)][, list(fixation_ids)])))
    }
    
    
    # game.plot(episode=1,fixation=1)
    # ggplotly(game.plot(episode=1,fixation=1))
    manipulateWidget(
      ggplotly(remap.p( end=episode, start=episode - .episodeback) + geom_path(aes(x=x,y=y,color=parent,group=id),data=.fix[fixation_ids %in% .get.fix(end=episode, start=episode - .episodeback)][min(length(which(.get.fix(end=episode, start=episode - .episodeback) != 0)),fixation)][ ,.rois[start<timestamp.begin & timestamp.begin<=end]][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id]) + 
                 geom_point(aes(x=fixation.x,y=fixation.y),size=4,color="red",data=.fix[fixation_ids %in% .get.fix(end=episode, start=episode - .episodeback)][min(length(which(.get.fix(end=episode, start=episode - .episodeback) != 0)),fixation)])
      ),
      episode = mwSlider(min=0,max=.g$data[game_number == .game, length(unique(episode_number))] ,value = .start, step=1),
      fixation = mwSlider(min=1, max=length(which(.get.fix(end=episode,start=episode - .episodeback)!=0))  , value=1, step = 1 )
    )
    
  }
}


recordGame <- function(.g, .rois, .game,.ratio=NULL, .interval=NULL, .type = "fix", .background = NULL, .episodeback = 0, .start=1, pointcolor = NULL, linecolor=NULL, debug=FALSE, realTime=FALSE, suffix=NULL ){
  if(debug==TRUE){
    print("playGame")
  }
  if(is.null(.ratio)){
    .ratio = .g$ry/.g$rx
  }
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  all_cols <- c(gg_color_hue(length(unique(.rois$parent))), gg_color_hue(length(unique(g$data$class))))
  names(all_cols) <- c(unique(.rois$parent)[order(unique(.rois$parent))],as.character(unique(g$data$class))[order(as.character(unique(g$data$class)))])
  
  if(.type=="fix"){
    if(realTime){
      warning("Real Time only implemented for All Samples")
    }
    if(is.null(.interval)){
      .interval = 1.25
    }
    if(!is.null(.background)){
      if(is.null(pointcolor)){
        if(!is.null(linecolor)){
          pointcolor = linecolor
        } else{
          pointcolor = "blue"
        }
      }
      if(is.null(linecolor)){
        if(!is.null(pointcolor)){
          linecolor = pointcolor
        }else{
          linecolor = "blue"
        }
      }
    } else {
      ##if one color is not null, set the other to it
      if(!is.null(linecolor)){
        pointcolor = linecolor
      }
      if(!is.null(pointcolor)){
        linecolor = pointcolor
      }
    }
    .call <- as.call(quote(.g$fixations()))
    .call[[2]] <- as.quoted(sprintf("game_number==%d", .game ))[[1]]
    .fix <- eval(.call)
    if (require(animation)) {
      if(is.null(linecolor)){
        
        .call <- as.call(quote(.g$plot()))
        .call[[2]] <- as.quoted(sprintf("game_number==%d", .game))[[1]]
        .call$style <- "spatial-class"
        
        p <- eval(.call)
      } else{
        if(is.null(.background)){
          p.raw <- g.spatplot(.g, pointcolor = pointcolor, linecolor = linecolor, filter = (game_number == .game))
          p <- copy(p.raw)
        } else{
          p.raw <- g.spatplot(.g, pointcolor = pointcolor, linecolor = linecolor, filter = (game_number == .game), .background = .background)
          p <- copy(p.raw)
        }
      }
      
      remap.p <- function( end, start=0){
        
        newp <- copy(p)
        if(!is.null(.background)){
          newp$layers[[2]]$data = .fix[fixation_ids %in% .get.fix(end=end,start=start)]
          newp$layers[[3]]$data = .fix[fixation_ids %in% .get.fix(end=end,start=start)]
          
          
        }else{
          
          newp$data <- p$data[which(as.numeric(p$data$fixation_ids) %in% .get.fix(end=end,start=start))]
          
        }
        newp
        
      }
      
      
      
      .get.fix <- function(end, start = 0){
        as.numeric(unlist(unique(.g$data[game_number == .game & episode_number %in% c(start:end)][, list(fixation_ids)])))
      }
      
      game.plot <- function(episode, fixation){
        g <- remap.p( end=episode, start=episode - .episodeback) + geom_path(aes(x=x,y=y,color=parent,group=id),data=.fix[fixation_ids %in% .get.fix(end=episode, start=episode - .episodeback)][min(length(which(.get.fix(end=episode, start=episode - .episodeback) != 0)),fixation)][ ,.rois[start<timestamp.begin & timestamp.begin<=end]][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id]) + 
          geom_point(aes(x=fixation.x,y=fixation.y),size=4,color="red",data=.fix[fixation_ids %in% .get.fix(end=episode, start=episode - .episodeback)][min(length(which(.get.fix(end=episode, start=episode - .episodeback) != 0)),fixation)])
        g
      }
      
      justAWrap <- function(){
        for(episode in 0:.g$data[game_number == .game, length(unique(episode_number))]){
          for(fixation in 1:length(which(.get.fix(end=episode,start=episode - .episodeback)!=0))){
            
            g<-  remap.p( end=episode, start=episode - .episodeback) + geom_path(aes(x=x,y=y,color=parent,group=id),data=.fix[fixation_ids %in% .get.fix(end=episode, start=episode - .episodeback)][min(length(which(.get.fix(end=episode, start=episode - .episodeback) != 0)),fixation)][ ,.rois[start<timestamp.begin & timestamp.begin<=end]][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id]) + 
              geom_point(aes(x=fixation.x,y=fixation.y),size=4,color="red",data=.fix[fixation_ids %in% .get.fix(end=episode, start=episode - .episodeback)][min(length(which(.get.fix(end=episode, start=episode - .episodeback) != 0)),fixation)])+
              guides(color=FALSE, size=FALSE)+ggtitle(sprintf("Game #%s: Episode #%s",.game,episode))
            print(g)
            
          }
        }
      }
      
      
    } }else{
      if(is.null(.interval)){
        .interval = .25
      }
      
      .event <- .g$data[game_number == .game, list(x=mean(x), y=mean(y), ts.start=min(timestamp),ts.end=max(timestamp), class=unique(class)[1]), by="event_ids,episode_number,game_number"]
      justAWrap <- function(){
        for(episode in 0:.g$data[game_number == .game, length(unique(episode_number))]){
          for(event_id in as.numeric(unique(.event[episode_number==episode]$event_ids))){
            
            
            cur = .event[episode_number==episode & event_ids==event_id]
            currois <- unique(cur[,.rois[start<=ts.start & ts.start <=end]][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id])
            
            plt <- ggplot(cur, aes(x=x, y=y, color=class)) + geom_point() + geom_path(aes(x=x,y=y,color=parent,group=id),data=currois) + scale_colour_manual(name="ROIs",values=all_cols)+
              guides(color=FALSE, size=FALSE)+ggtitle(sprintf("Game #%s: Episode #%s",.game,episode)) + coord_fixed(ratio = .ratio)
            print(plt)
            # ggsave("temp.png",plt, width=12, height = 8)
            
          }
        }
      }
      
    }
  
  if(realTime){
    if(is.null(.interval)){
      .interval <- .02
    }
    .event <- .g$data[game_number == .game, list(x=mean(x), y=mean(y), ts.start=min(timestamp),ts.end=max(timestamp), class=unique(class)[1], curr_zoid= curr_zoid[1], next_zoid = next_zoid[1]), by="event_ids,episode_number,game_number"]
    .starttime <- .event$ts.start[1]
    
    
    justAWrap <- function(){
      for(curTime in seq.default(from = min(.event$ts.start), to = max(.event$ts.end), by = .interval*1000)){
        cur = .event[ts.start <= curTime  & ts.end >= curTime]
        if(NROW(cur)==0){
          cur = .event[ts.start > curTime & ts.start <= curTime+.interval*1000]
        }
        if(NROW(cur)>1){
          ## if two events occured in same interval, play just first one
          cur<- cur[1]
        }
        # print(curTime)
        currois <- unique(cur[,.rois[start<=ts.start & ts.start <=end]][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id])
        cz_y <- max(currois[parent=="worldsurf"]$y)*.8
        cz_x <- min(currois[parent=="gamesurf"]$x)*.5
        
        t_y <- max(currois[parent=="worldsurf"]$y)*.6
        
        nz_x <- mean(currois[parent=="nextsurf"]$x)
        nz_y <- mean(currois[parent=="nextsurf"]$y)
        
        
        plt <- ggplot(cur, aes(x=x, y=y, color=class)) + geom_point() + geom_path(aes(x=x,y=y,color=parent,group=id),data=currois) + scale_colour_manual(name="color",values=all_cols, breaks = as.character(unique(g$data$class))[order(as.character(unique(g$data$class)))]) +
          guides(color=FALSE, size=FALSE)+ggtitle(sprintf("Game #%s: Episode #%s",.game,cur$episode_number)) + coord_fixed(ratio = .ratio, xlim = c(0, .g$rx), ylim=c(0,.g$ry)) + 
          annotate("text", x=cz_x, y=cz_y, label= cur$curr_zoid)+ annotate("text", x=nz_x, y=nz_y, label= cur$next_zoid) + theme(legend.position="bottom") #+ 
        #annotate("text", x=nz_x, y=t_y, label= paste("Time:",(curTime-.starttime)/1000))
        print(plt)
        # ggsave("temp.png",plt, width=12, height = 8)
      }
    }
    
    .type = "RTall"
    
  }
  if(is.null(suffix)){
    saveVideo(justAWrap(), video.name = sprintf("%s_%s_%s.mp4",.type,.game,format(Sys.Date(), "%Y%m%d")), interval = .interval, ani.width=.g$rx, ani.height=.g$ry)
  }else{
    saveVideo(justAWrap(), video.name = sprintf("%s_%s_%s.mp4",.type,.game,suffix), interval = .interval, ani.width=.g$rx, ani.height=.g$ry)
    
  }
}


recordZoidGame <- function(.g, .rois, .game, .rda, .ratio=NULL, .interval=NULL,  pointcolor = NULL, linecolor=NULL, debug=FALSE, suffix=NULL){
  ## eventually we should implement the ability to add a background, but it seems unnecessary at this point
  .background = NULL
  
  if(debug==TRUE){
    print("recordZoidGame")
  }
  if(require(animation)){
    if(is.null(.ratio)){
      .ratio = .g$ry/.g$rx
    }
    if(is.null(.interval)){
      .interval <- mean(as.numeric(.rda[,list(diff = (ts[-1]-ts[-.N]))][diff!=0,diff]) ) * 5
      # .interval <- .005
    }
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    all_cols <- c(gg_color_hue(length(unique(.rois$parent))), gg_color_hue(length(unique(g$data$class))))
    names(all_cols) <- c(unique(.rois$parent)[order(unique(.rois$parent))],as.character(unique(g$data$class))[order(as.character(unique(g$data$class)))])
    
    
    zcol <- c(gg_color_hue(7))
    names(zcol) <- c("1", "2", "3", "4", "5", "6", "7")
    
    
    if(debug==T){
      
      print(sprintf("Making DT. Started at %s", Sys.time()))
    }
    remap.p <- function( end, start=0){
      
      newp <- copy(p)
      if(!is.null(.background)){
        newp$layers[[2]]$data = .fix[fixation_ids %in% .get.fix(end=end,start=start)]
        newp$layers[[3]]$data = .fix[fixation_ids %in% .get.fix(end=end,start=start)]
        
        
      }else{
        
        newp$data <- p$data[which(as.numeric(p$data$fixation_ids) %in% .get.fix(end=end,start=start))]
        
      }
      newp
      
    }
    
    .get.fix <- function(end, start = 0){
      as.numeric(unlist(unique(.g$data[game_number == .game & episode_number %in% c(start:end)][, list(fixation_ids)])))
    }
    
    game.plot <- function(episode, fixation, frame=NULL){
      if(!is.null(frame)){
        frame.range <- eval(parse(text=sub("\\(","c(",sub("\\]",")",sub("\\[", "(", frame)))))
        frame.range <- c(.rda[ts >= (frame.range[1]) & ts <= (frame.range[2]), list(min(smi_ts),max(smi_ts))])
        gf <- remap.p( end=episode, start=episode)
        gunder <- ggplot()+ geom_path(aes(x=x,y=y,color=parent,group=id),data=.rois[start<frame.range[[2]] & frame.range[[2]]<=end][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id])
        gf$layers <- c(gunder$layers, gf$layers)
        gf <- gf + geom_point(aes(x=fixation.x,y=fixation.y),size=4,color="red",data=.fix[fixation_ids %in% .get.fix(end=episode, start=episode)][min(length(which(.get.fix(end=episode, start=episode ) != 0)),fixation)])
        gf
      }else{
        
        gf <- remap.p( end=episode, start=episode) + 
          gunder <- ggplot() + geom_path(aes(x=x,y=y,color=parent,group=id),data=.fix[fixation_ids %in% .get.fix( end=episode, start=episode)][min(length(which(.get.fix(end=episode, start=episode ) != 0)),fixation)][ ,.rois[start<timestamp.begin & timestamp.begin<=end]][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id])
          gf$layers <- c(gunder$layers, gf$layers)
          gf <- gf + geom_point(aes(x=fixation.x,y=fixation.y),size=4,color="red",data=.fix[fixation_ids %in% .get.fix(end=episode, start=episode)][min(length(which(.get.fix(end=episode, start=episode ) != 0)),fixation)])
          gf
          
      }
    }
    
    
    .call <- as.call(quote(.g$fixations()))
    .call[[2]] <- as.quoted(sprintf("game_number==%d", .game ))[[1]]
    .fix <- eval(.call)
    if(is.null(linecolor)){
      
      .call <- as.call(quote(.g$plot()))
      .call[[2]] <- as.quoted(sprintf("game_number==%d", .game))[[1]]
      .call$style <- "spatial-class"
      
      p <- eval(.call)
    } else{
      if(is.null(.background)){
        p.raw <- g.spatplot(.g, pointcolor = pointcolor, linecolor = linecolor, filter = (game_number == .game))
        p <- copy(p.raw)
      } else{
        p.raw <- g.spatplot(.g, pointcolor = pointcolor, linecolor = linecolor, filter = (game_number == .game), .background = .background)
        p <- copy(p.raw)
      }
    }
    
    ep.fixnumbers <- .g$data[game_number==.game & class=="Fixation",list(fix_id = unique(fixation_ids)),by=episode_number][,list(fix_id = fix_id, fix_number=1:.N),by=episode_number]
    
    
    
    full.dt <- rbindlist(lapply(c(1:NROW(.fix)), function(itime){
      fix.begin = .fix$timestamp.begin[itime]
      fix.end = .fix$timestamp.end[itime]
      
      currda = .rda[smi_ts <= fix.end & smi_ts >= fix.begin]
      frames = round((currda$ts[NROW(currda)] - currda$ts[1])/.interval)
      currda$frame <- cut_number(currda$ts,frames)
      frame_uid <- currda[,list(zoid_uid = unique(zoid_uid), start = min(smi_ts), stop=max(smi_ts)),by="frame,episode_number"][,list(zoid_uid=zoid_uid[.N], f.start = min(start), f.stop=max(stop)),by="frame,episode_number"]
      
      frame_uid <- frame_uid[unique(currda[,list(zoid_uid,board_uid,frame,episode_number)]), board_uid:=board_uid, on=c(zoid_uid="zoid_uid", frame="frame", episode_number="episode_number")]
      
      
      board_zoid_reps <- rbindlist(apply(frame_uid, 1, function(ze){
        if(is.null(attr(.rda,"board_rep")[as.numeric(ze[["board_uid"]])])){
          test1 <- list()
        }else{
          test1 <- getBlocks(fromJSON(attr(.rda,"board_rep")[as.numeric(ze[["board_uid"]])]), .rda)
        }
        if(is.null(attr(.rda,"zoid_rep")[as.numeric(ze[["zoid_uid"]])])){
          test2 <- list()
        }else{
          test2 <- getBlocks(fromJSON(attr(.rda,"zoid_rep")[as.numeric(ze[["zoid_uid"]])]), .rda)
        }
        rbindlist(list(test2, test1))[,c("frame","episode_number"):=list(ze[["frame"]], ze[["episode_number"]])]
      }))
      
      
      
      
      copy(board_zoid_reps)[, id:= 1:.N][, list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)), zoid = v, frame=frame, episode_number = episode_number, fix=.fix$fixation_ids[itime]), by = id]
      
      
      
    }))
    
    # ep.fixtotals <- .g$data[game_number==.game & class=="Fixation",unique(fixation_ids),by=episode_number][,.N,by=episode_number]
    
    
    
    justAWrap <- function(){
      ## first layer we cycle through all unique fixations in unique episodes. 
      lapply(1:NROW(ep.fixnumbers), function(cur.fix){
        epi = ep.fixnumbers[cur.fix]$episode_number
        fixnum =  ep.fixnumbers[cur.fix]$fix_number
        cur.dt <- full.dt[episode_number==epi & fix==ep.fixnumbers[cur.fix]$fix_id ]
        
        ## then we cycle through all unique frames for that fixation/episode combination
        lapply(unique(cur.dt$frame), function(cur.frame){
          
          finalg <- game.plot(episode = epi,fixation = fixnum, frame=cur.frame) 
          underg <-ggplot()+ geom_polygon(data=cur.dt[frame==cur.frame], aes(x=x, y=y, fill = factor(zoid), group = id, color = NULL))+
            scale_fill_manual(name="Zoid", labels=list("1"="I", "2"="O", "3" = "T", "4" = "S", "5"="Z", "6"="J", "7"= "L"), values= zcol)+
            guides(color=FALSE, size=FALSE, fill =FALSE) + ggtitle(sprintf("Game #%s: Episode #%s -- Frame = %s",.game, epi, cur.frame))
          finalg$layers <- c(underg$layers, finalg$layers)
          print(finalg)
        })
      })
    }  
    
    if(debug==T){
      print(sprintf("Plotting %s frames. Started at %s", NROW(full.dt), Sys.time()))
    }
    
    .type = "zoidFancyFix"
    
    if(is.null(suffix)){
      saveVideo(justAWrap(), video.name = sprintf("%s_%s_%s.mp4",.type,.game,format(Sys.Date(), "%Y%m%d")), interval = .interval, ani.width=.g$rx, ani.height=.g$ry)
    }else{
      saveVideo(justAWrap(), video.name = sprintf("%s_%s_%s.mp4",.type,.game,suffix), interval = .interval, ani.width=.g$rx, ani.height=.g$ry)  
    }
    if(debug==T){
      print(sprintf("Finished at %s", Sys.time()))
    }
  }else{
    print("Animation Package Failed to Load")
  }
}




##THIS TAKES AN OBSCENE AMOUNT OF TIME
recordAllSampleZoidGame <- function(.g, .rois, .game, .rda, .ratio=NULL, .interval=NULL, .background = NULL,  pointcolor = NULL, linecolor=NULL, debug=FALSE, suffix=NULL ){
  
  
  if(debug==TRUE){
    print("playGame")
  }
  if(require(animation)){
    if(is.null(.ratio)){
      .ratio = .g$ry/.g$rx
    }
    if(is.null(.interval)){
      .interval <- .02
    }
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    all_cols <- c(gg_color_hue(length(unique(.rois$parent))), gg_color_hue(length(unique(g$data$class))))
    names(all_cols) <- c(unique(.rois$parent)[order(unique(.rois$parent))],as.character(unique(g$data$class))[order(as.character(unique(g$data$class)))])
    
    
    zcol <- c(gg_color_hue(7))
    names(zcol) <- c("1", "2", "3", "4", "5", "6", "7")
    
    
    .event <- .g$data[, list(x=mean(x), y=mean(y), ts.start=min(timestamp),ts.end=max(timestamp), class=unique(class)[1], curr_zoid= curr_zoid[1], next_zoid = next_zoid[1]), by="event_ids,episode_number,game_number"]
    .starttime <- .event[game_number==.game,ts.start[1]][1]
    # .rda <- copy(.rda)[smi_ts >= min(.event$ts.start)]
    
    gaze.breaks <- as.character(unique(g$data$class))[order(as.character(unique(g$data$class)))]
    
    
    basic.rois <- rois[parent %in% c("worldsurf", "gamesurf","nextsurf")][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id]
    
    cz_y <- max(basic.rois[parent=="worldsurf"]$y)*.8
    cz_x <- min(basic.rois[parent=="gamesurf"]$x)*.5
    t_y <- max(basic.rois[parent=="worldsurf"]$y)*.6
    nz_x <- mean(basic.rois[parent=="nextsurf"]$x)
    nz_y <- mean(basic.rois[parent=="nextsurf"]$y)
    
    if(debug==T){
      
      print(sprintf("Making DT. Started at %s", Sys.time()))
    }
    full.dt <- rbindlist(lapply(c(1:length(unique(.rda[game_number==.game & !zoid_uid %in% which(is.na(attr(.rda, "zoid_rep")))]$smi_ts))), function(itime){
      curTime = unique(.rda[game_number==.game & !zoid_uid %in% which(is.na(attr(.rda, "zoid_rep")))]$smi_ts)[itime]
      cur = .event[ts.start <= curTime  & ts.end >= curTime]
      currda = .rda[smi_ts <= curTime & (smi_ts+(.interval*1000)) >= curTime]
      if(NROW(cur)==0){
        cur = .event[ts.start > curTime & ts.start <= curTime+.interval*1000]
      }
      if(NROW(cur)>1){
        ## if two events occured in same interval, play just first one
        cur<- cur[1]
      }
      if(NROW(cur)>0){
        zoid = unique(currda$zoid_uid)
        zoid = zoid[which(zoid!=1)][1]
        episode =unique(currda[zoid_uid == zoid, board_uid])[1]
        
        
        dur <- currda[zoid_uid == zoid, max(ts)-min(ts)]
        # print(dur)
        
        if(is.null(attr(.rda,"board_rep")[episode])){
          test1 <- list()
        }else{
          test1 <- getBlocks(fromJSON(attr(.rda,"board_rep")[episode]), .rda)
        }
        if(is.null(attr(.rda,"zoid_rep")[zoid])){
          test2 <- list()
        }else{
          test2 <- getBlocks(fromJSON(attr(.rda,"zoid_rep")[zoid]), .rda)
        }
        test <- rbindlist(list(test2, test1))
        
        # print(curTime)
        currois <- cur[,.rois[start<=ts.start & ts.start <=end]][,list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)),parent=parent),by=id]
        
        
        
        zb <- copy(test)[, id:= 1:.N][, list(x=as.numeric(unlist(x)),y=as.numeric(unlist(y)), zoid = v), by = id]
        
        frame.dt <- data.table(id = NA, x=NA, y=NA, zb.x=NA, zb.y=NA,gaze.x=cur$x, gaze.y=cur$y, zoid=NA, curr_zoid=cur$curr_zoid, next_zoid=cur$next_zoid, episode_number=cur$episode_number, class=cur$class, parent=NA, c.frame=curTime, cz_x=cz_x, cz_y=cz_y,nz_x=nz_x, nz_y=nz_y)
        frame.dt <- rbindlist(list(frame.dt,
                                   data.table(id = zb$id,x=NA,y=NA, zb.x=zb$x, zb.y=zb$y, gaze.x=NA, gaze.y=NA, zoid=zb$zoid, curr_zoid=NA, next_zoid=NA, episode_number=cur$episode_number, class=NA, parent=NA, c.frame=curTime, cz_x=NA, cz_y=NA,nz_x=NA, nz_y=NA)))
        frame.dt <- rbindlist(list(frame.dt,
                                   data.table(id = currois$id, x=currois$x, y=currois$y,zb.x=NA,zb.y=NA,gaze.x=NA, gaze.y=NA,  zoid=NA, curr_zoid=NA, next_zoid=NA, episode_number=cur$episode_number, class=NA, parent=currois$parent, c.frame=curTime, cz_x=NA, cz_y=NA, nz_x=NA, nz_y=NA)))
        
        
      }
    }))
    if(debug==T){
      print(sprintf("Plotting %s frames. Started at %s", NROW(full.dt), Sys.time()))
    }
    justAWrap <- function() {lapply(unique(.rda[game_number==.game & !zoid_uid %in% which(is.na(attr(.rda, "zoid_rep")))]$smi_ts), function(x) {
      if(NROW(full.dt[c.frame==x])>0){
        g <- ggplot(full.dt[c.frame == x]) + geom_polygon( aes(x=zb.x, y=zb.y, fill = factor(zoid), group = id, color = NULL),   na.rm=T) +  
          scale_fill_manual(name="Zoid", labels=list("1"="I", "2"="O", "3" = "T", "4" = "S", "5"="Z", "6"="J", "7"= "L"), values= zcol) +
          geom_path(aes(x=x,y=y,color=parent,group=id),   na.rm=T) + scale_colour_manual(name="color",values=all_cols, breaks = gaze.breaks) +
          geom_text(aes(x=cz_x, y=cz_y,label=curr_zoid),   na.rm=T) + geom_text(aes(x=nz_x, y=nz_y,label=next_zoid),   na.rm=T)+
          geom_point(size=4,aes(x=gaze.x,y=gaze.y,color=class, fill=NULL),   na.rm=TRUE) + xlab(label = "Pixel X") + xlab(label = "Pixel Y") +
          guides(color=FALSE, size=FALSE, fill =FALSE)+ggtitle(sprintf("Game #%s",.game)) + coord_fixed(ratio = .ratio, xlim = c(0, .g$rx), ylim=c(0,.g$ry)) + 
          theme(legend.position="bottom") #+
        print(g)
      }}, full.dt = full.dt)}
    
    
    .type = "zoidAllSamples"
    
    if(is.null(suffix)){
      saveVideo(justAWrap(), video.name = sprintf("%s_%s_%s.mp4",.type,.game,format(Sys.Date(), "%Y%m%d")), interval = .interval, ani.width=.g$rx, ani.height=.g$ry)
    }else{
      saveVideo(justAWrap(), video.name = sprintf("%s_%s_%s.mp4",.type,.game,suffix), interval = .interval, ani.width=.g$rx, ani.height=.g$ry)
      
    }
    if(debug==T){
      print(sprintf("Finished at %s", Sys.time()))
    }
  }else{
    print("Animation Package Failed to Load")
  }
}

getGameBoundaries <- function(data, debug=FALSE) {
  if(debug==TRUE){
    print("getGameBoundaries")
  }
  data.frame(begin=as.numeric(rownames(subset(data, event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="BEGIN"))),
             end=as.numeric(rownames(subset(data, event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="END"))))
}

empty_board <- paste("'[",paste(rep(paste("[",paste(rep(0,10),collapse=", "),"]", sep=""),20),collapse=", "),"]'",sep="")

#'
#' TODOs:
#'  * delete first 1
#'  * finish moving stuff redundant stuff to attrs and drop cols
#'

##Changed for popStudy
rdaTetrisLog2 <- function(sid=NULL,assessment=1, debug=FALSE, f=NULL,meta2=F, screen.res.width=NULL, screen.res.height=NULL, data.file=NULL) {

  if(is.null(data.file)){
    if(!meta2){
      if(parsed==T){
        ##get list of all pairs of assessments
        a <- getAssessments(debug=debug)
        ##grab the assessments for "sid" and grab the 1st or 2nd of that pair (assessment)
        pid <- a[[as.character(sid)]][[assessment]]
        ##print(pid)
        f <- sprintf("%s/%s.tsv.rmh.a%s.gz", new_data,pid, assessment)
        d <- fread(sprintf("zcat %s", f), sep="\t", header=TRUE, showProgress=F)
      } else {
        if(is.null(f)){
          ##get list of all pairs of assessments
          a <- getAssessments(debug=debug)
          ##grab the assessments for "sid" and grab the 1st or 2nd of that pair (assessment)
          pid <- a[[as.character(sid)]][[assessment]]
          ##print(pid)
          f <- sprintf("%s/%s/complete_%s.tsv", data_folder,pid,pid)
        }else {
          pid = sid
        }
        try(d <- fread(f,sep="\t", sep2=",", header = T, showProgress = F, na.strings=c("NA","", "None")), silent = TRUE)
        if(!exists(x = "d")){
          d <- fread(f,sep="\t", sep2=",", header = F, showProgress = F, na.strings=c("NA","","None"), skip="CALIBRATION\tStart")
          cs <- sapply(d,FUN = class)
          attr_raw <- read.csv(f, sep="\t", stringsAsFactors = F, nrows = 1000,colClasses = cs, na.strings = c("NA","","None"))
          dattr <- data.table(dattr_raw)[1:(which(evt_id=="CALIBRATION")+1)]
          names(d) <- names(dattr)
          
          dattr$smi_ts <- as.character(dattr$smi_ts)
          d$smi_ts <- as.character(d$smi_ts)
          
          d <- rbindlist(list(dattr,d), use.names = T)
          d$smi_ts <- as.integer64(d$smi_ts)
          if(NROW(d[,.N, by=ts][N>1])){
            stop("REPEAT TS")
            ##use d[,.N, by=ts][N>1] to figure out which ts
          }
        }
        if(!"ts" %in% names(d)){
          d <- fread(f, sep2=",", header = T, showProgress = F, na.strings=c("NA","", "None"))
        }
        d[,c("smi_ts", "level", "score") := list(na.locf(na.fill(smi_ts, c("extend", NA))), na.locf(na.fill(level, c("extend", NA))), na.locf(na.fill(score, c("extend", NA))))]
        d[,c("curr_zoid", "next_zoid") := list(na.locf(na.fill(curr_zoid, c("extend",NA))),na.locf(na.fill(next_zoid, c("extend",NA))) )]
        
      }
      b <- cbind(d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="BEGIN",which=T],
                 d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="END",which=T]) + 1
    }else{
      if(is.null(f)){
        ##get list of all pairs of assessments
        a <- getAssessments(debug=debug, meta2=T)
        ##grab the assessments for "sid" and grab the 1st or 2nd of that pair (assessment)
        pid <- a[[as.character(sid)]][[assessment]]
        f <- sprintf("%s/complete_%s.tsv", data_folder,sid)
        d <- fread(f,sep="\t", sep2=",", header = T, showProgress = F, na.strings=c("NA","", "None"))
        
      }else{
        d <- fread(f, sep="\t", sep2=",", header = T, showProgress = F, na.strings=c("NA","", "None"))
      }
      d[,c("smi_ts", "level", "score") := list(na.locf(na.fill(smi_ts, c("extend", NA))), na.locf(na.fill(level, c("extend", NA))), na.locf(na.fill(score, c("extend", NA))))]
      d[,c("curr_zoid", "next_zoid", "episode_number") := list(na.locf(na.fill(curr_zoid, c("extend",NA))),na.locf(na.fill(next_zoid, c("extend",NA))), na.locf(na.fill(episode_number, c("extend",NA))) )]
      # print(d[,length(is.na(curr_zoid)),by=game_number])
      # pid <- strsplit(f, "/")
      # pid <- strsplit(pid[[1]][length(pid[[1]])], "\\.")[[1]][1]
      pid <- as.character(rm_between(f, "complete_", ".tsv", extract=T))
      b <- cbind(d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="BEGIN",which=T],
                 d[event_type=="GAME_SUMM",which=T]) + 1
    }
  }else{
    nafill.cols <- c("smi_ts","level","score","curr_zoid","next_zoid","episode_number")
    nafill.cols <- nafill.cols[which(nafill.cols %in% names(data.file))]
    
    d <- copy(data.file)[,c(nafill.cols):=lapply(nafill.cols, function(x) na.locf(na.fill(get(x), c("extend", NA))))]
    
    
    
    if(NROW(d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="END",which=T])>0){
      if(debug){print("Detected as Meta-T")}
      b <- cbind(d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="BEGIN",which=T],
                 d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="END",which=T]) + 1
    }else{
      if(debug){print("Detected as Meta-TWO_js")}
      meta2=T
      b <- cbind(d[event_type=="GAME_EVENT" & evt_id=="GAME" & evt_data1=="BEGIN",which=T],
                 d[event_type=="GAME_SUMM",which=T]) + 1
    }
    
  }
  
  d[,game_number:=0L]
  
  
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
  
  d<-d[zoid_rep=="", zoid_rep:=NA][board_rep=="", board_rep:=NA][zoid_sequence=="", zoid_sequence:=NA]
  d[!is.na(board_rep),board_uid:=ruidvec(d[!is.na(board_rep),board_rep])][,board_uid:=na.locf(na.fill(board_uid, c("extend", NA)))]
  d[!is.na(zoid_rep),zoid_uid:=ruidvec(d[!is.na(zoid_rep),zoid_rep])][,zoid_uid:=na.locf(na.fill(zoid_uid, c("extend", NA)))]
  
  zoid_rep <- d[,list(zoid_rep=str_replace_all(zoid_rep[1],"'","")),by=zoid_uid][,zoid_rep]
  board_rep <- d[,list(board_rep=str_replace_all(board_rep[1],"'","")),by=board_uid][,board_rep]
  
  d[,board_rep:=NULL]
  d[,zoid_rep:=NULL]
  
  attr(d, "board_rep") <- board_rep
  attr(d, "zoid_rep") <- zoid_rep
  
  
  #simplifying. if issues arise in the future, uncomment
  # if(meta2){
  tempstr <- gsub("([\\])","",na.exclude(unique(d[,zoid_sequence])))
  attr(d, "zoid_sequence") <- sapply(tempstr,function(x) {fromJSON(str_replace_all(x,"'",""))})
  
  # }else{
  #   attr(d, "zoid_sequence") <- sapply(na.exclude(unique(d[,zoid_sequence])),function(x) {fromJSON(str_replace_all(x,"'",""))})
  # }
  
  names(attr(d, "zoid_sequence")) <- NULL
  
  if(parsed == T){
    
    attr(d,"Screen Y") <- d[.N,`Screen Y`]
    attr(d,"Screen X") <- d[.N,`Screen X`]
    
    attr(d,"nextsurf_rect.height") <- d[.N,nextsurf_rect.height]
    attr(d,"nextsurf_rect.width") <- d[.N,nextsurf_rect.width]
    attr(d,"nextsurf_rect.top") <- d[.N,nextsurf_rect.top]
    attr(d,"nextsurf_rect.left") <- d[.N,nextsurf_rect.left]
    
    attr(d,"gamesurf_rect.height") <- d[.N,gamesurf_rect.height]
    attr(d,"gamesurf_rect.width") <- d[.N,gamesurf_rect.width]
    attr(d,"gamesurf_rect.top") <- d[.N,gamesurf_rect.top]
    attr(d,"gamesurf_rect.left") <- d[.N,gamesurf_rect.left]
    
    attr(d,"worldsurf_rect.height") <- d[.N,worldsurf_rect.height]
    attr(d,"worldsurf_rect.width") <- d[.N,worldsurf_rect.width]
    
    attr(d,"score_lab_left") <- fromBrokenJSON(d[.N,score_lab_left])
    attr(d,"score_left") <- fromBrokenJSON(d[.N,score_left])
    attr(d,"lines_lab_left") <- fromBrokenJSON(d[.N,lines_lab_left])
    attr(d,"lines_left") <- fromBrokenJSON(d[.N,lines_left])
    attr(d,"level_lab_left") <- fromBrokenJSON(d[.N,level_lab_left])
    attr(d,"level_left") <- fromBrokenJSON(d[.N,level_left])
    
    attr(d,"intervals") <- fromJSON(d[.N,intervals])
    attr(d,"scoring") <- fromJSON(d[.N,scoring])
    attr(d,"random_seeds") <- fromJSON(d[.N,random_seeds])
    attr(d,"dimtris_alphas") <- fromJSON(d[.N,dimtris_alphas])
    
    
    attr(d,"das_chargeable") <- d[.N,das_chargeable]
    attr(d,"SID") <- d[.N,SID]
    attr(d,"far_next") <- d[.N,far_next]
    attr(d,"next_dim") <- d[.N,next_dim]
    attr(d,"look_ahead") <- d[.N,look_ahead]
    attr(d,"RIN") <- d[.N,RIN]
    attr(d,"Eyetracker") <- d[.N,Eyetracker]
    attr(d,"lc_delay") <- d[.N,lc_delay]
    attr(d,"kept_bgc") <- d[.N,kept_bgc]
    attr(d,"continues") <- d[.N,continues]
    attr(d,"bg_color") <- d[.N,bg_color]
    attr(d,"gridlines_color") <- d[.N,gridlines_color]
    attr(d,"ghost_zoid") <- d[.N,ghost_zoid]
    attr(d,"das_delay") <- d[.N,das_delay]
    attr(d,"gridlines_y") <- d[.N,gridlines_y]
    attr(d,"gridlines_x") <- d[.N,gridlines_x]
    attr(d,"session") <- d[.N,session] ##capitalized now the capitalized version is there too... 
    attr(d,"Verbose_logging") <- d[.N,Verbose_logging] ## not here
    attr(d,"inverted") <- d[.N,inverted]
    attr(d,"board_mask") <- d[.N,board_mask]
    attr(d,"Log-Version") <- d[.N,`Log-Version`]
    attr(d,"dimtris") <- d[.N,dimtris]
    attr(d,"fullscreen") <- d[.N,fullscreen]
    attr(d,"pause_enabled") <- d[.N,pause_enabled]
    attr(d,"tetris_zoids") <- d[.N,tetris_zoids]
    attr(d,"das_reversible") <- d[.N,das_reversible]
    attr(d,"fps") <- d[.N,fps]
    attr(d,"border_color") <- d[.N,border_color]
    attr(d,"undo") <- d[.N,undo]
    attr(d,"pentix_zoids") <- d[.N,pentix_zoids]
    attr(d,"lines_per_lvl") <- d[.N,lines_per_lvl]
    attr(d,"next_mask") <- d[.N,next_mask]
    attr(d,"grace_period") <- d[.N,lines_per_lvl]
    attr(d,"tetrises_covered") <- d[.N,tetrises_covered] ## missing
    attr(d,"drop_bonus") <- d[.N,drop_bonus]
    attr(d,"board_echo_lc") <- d[.N,board_echo_lc]
    attr(d,"Session") <- d[.N,Session]  ##what?
    attr(d,"zoid_slam") <- d[.N,zoid_slam]
    attr(d,"seven_bag_switch") <- d[.N,seven_bag_switch]
    attr(d,"keep_zoid") <- d[.N,keep_zoid]
    attr(d,"wall_kicking") <- d[.N,wall_kicking]
    attr(d,"feedback_mode") <- d[.N,feedback_mode]
    attr(d,"board_echo_placed") <- d[.N,board_echo_placed]
    attr(d,"are_delay") <- d[.N,are_delay]
    attr(d,"Start time") <- d[.N,`Start time`]
    attr(d,"next_dim_alpha") <- d[.N,next_dim_alpha]
    attr(d,"das_repeat") <- d[.N,das_repeat]
    attr(d,"game_wd") <- d[.N,game_wd]
    attr(d,"game_ht") <- d[.N,game_ht]
    attr(d,"game_seed") <- d[.N,game_seed]   ##missing -- random seeds
    attr(d,"tiny_zoids") <- d[.N,tiny_zoids]
    attr(d,"game_type") <- d[.N,game_type]
    attr(d,"visible_board") <- d[.N,visible_board]
    attr(d,"visible_zoid") <- d[.N,visible_zoid]
    attr(d,"gravity") <- d[.N,gravity]
    attr(d,"grace_refresh") <- d[.N,grace_refresh]
    attr(d,"tps") <- d[.N,tps]
  } else {
    if(meta2){
      d[,episode_number := as.integer(episode_number)]
      d[game_number==0, episode_number:=0]
      d[is.na(event_type), event_type := "EYE_SAMP"]
      
      
      ### gazepoint -- 1439 x 1079
      ### 1.79875
      
      disp.scale = 1080/600 
      
      
      if(!is.null(screen.res.height)){
        attr(d,"Screen Y") <- screen.res.height #1080
        y.offset = 4
      }else{
        attr(d,"Screen Y") <-1080 #1080
        y.offset = 4
      }
      disp.scale = attr(d, "Screen Y")/600 ## true screen y / display y --- game scales based on y.
      if(!is.null(screen.res.width)){
        attr(d,"Screen X") <- screen.res.width #1920
        
      }else{
        attr(d,"Screen X") <- 1920 #1920
      }
      x.offset = ((attr(d,"Screen X")/2) - ((800 * disp.scale)/2))
      
      attr(d,"nextsurf_rect.height") <- 120 * disp.scale
      attr(d,"nextsurf_rect.width") <- 120* disp.scale
      attr(d,"nextsurf_rect.top") <- y.offset + 72* disp.scale
      attr(d,"nextsurf_rect.left") <- x.offset + 595* disp.scale
      
      attr(d,"gamesurf_rect.height") <- 503* disp.scale
      attr(d,"gamesurf_rect.width") <- 252* disp.scale
      attr(d,"gamesurf_rect.top") <- y.offset + 72* disp.scale
      attr(d,"gamesurf_rect.left") <- x.offset + 275* disp.scale
      
      attr(d,"worldsurf_rect.height") <- attr(d,"Screen Y")
      attr(d,"worldsurf_rect.width") <- attr(d,"Screen X")
      
      attr(d,"score_lab_left") <- c(580, 300)* disp.scale + c(x.offset, y.offset)
      attr(d,"score_left") <- c(714, 300)* disp.scale + c(x.offset, y.offset)
      attr(d,"lines_lab_left") <- c(584, 354)* disp.scale + c(x.offset, y.offset)
      attr(d,"lines_left") <- c(714, 354)* disp.scale + c(x.offset, y.offset)
      attr(d,"level_lab_left") <- c(584, 408)* disp.scale + c(x.offset, y.offset)
      attr(d,"level_left") <- c(714, 408)* disp.scale + c(x.offset, y.offset)
      
      attr(d,"SID") <- unique(d[!is.na(SID),list(SID)])[1]
      if("Session" %in% colnames(d)){
        attr(d,"Session") <- unique(d[!is.na(Session), list(Session)])[1]
        
        d[, -c("SID","Session"), with=F]
      } else{
        d[, -c("SID"), with=F]
      }
    } else {
      
      attr(d,"Screen Y") <- as.numeric(d[evt_id=="Screen Y"]$evt_data1)
      attr(d,"Screen X") <- as.numeric(d[evt_id=="Screen X"]$evt_data1)
      
      attr(d,"nextsurf_rect.height") <- as.numeric(d[evt_id=="nextsurf_rect.height"]$evt_data1)
      attr(d,"nextsurf_rect.width") <- as.numeric(d[evt_id=="nextsurf_rect.width"]$evt_data1)
      attr(d,"nextsurf_rect.top") <- as.numeric(d[evt_id=="nextsurf_rect.top"]$evt_data1)
      attr(d,"nextsurf_rect.left") <- as.numeric(d[evt_id=="nextsurf_rect.left"]$evt_data1)
      
      attr(d,"gamesurf_rect.height") <- as.numeric(d[evt_id=="gamesurf_rect.height"]$evt_data1)
      attr(d,"gamesurf_rect.width") <- as.numeric(d[evt_id=="gamesurf_rect.width"]$evt_data1)
      attr(d,"gamesurf_rect.top") <- as.numeric(d[evt_id=="gamesurf_rect.top"]$evt_data1)
      attr(d,"gamesurf_rect.left") <- as.numeric(d[evt_id=="gamesurf_rect.left"]$evt_data1)
      
      attr(d,"worldsurf_rect.height") <- as.numeric(d[evt_id=="worldsurf_rect.height"]$evt_data1)
      attr(d,"worldsurf_rect.width") <- as.numeric(d[evt_id=="worldsurf_rect.width"]$evt_data1)
      
      attr(d,"score_lab_left") <- fromBrokenJSON(d[evt_id=="score_lab_left"]$evt_data1)
      attr(d,"score_left") <- fromBrokenJSON(d[evt_id=="score_left"]$evt_data1)
      attr(d,"lines_lab_left") <- fromBrokenJSON(d[evt_id=="lines_lab_left"]$evt_data1)
      attr(d,"lines_left") <- fromBrokenJSON(d[evt_id=="lines_left"]$evt_data1)
      attr(d,"level_lab_left") <- fromBrokenJSON(d[evt_id=="level_lab_left"]$evt_data1)
      attr(d,"level_left") <- fromBrokenJSON(d[evt_id=="level_left"]$evt_data1)
      
      attr(d,"intervals") <- fromJSON(d[evt_id=="intervals"]$evt_data1)
      attr(d,"scoring") <- fromJSON(d[evt_id=="scoring"]$evt_data1)
      attr(d,"random_seeds") <- fromJSON(d[evt_id=="random_seeds"]$evt_data1)
      attr(d,"dimtris_alphas") <- fromJSON(d[evt_id=="dimtris_alphas"]$evt_data1)
      
      
      attr(d,"das_chargeable") <- d[evt_id=="das_chargeable"]$evt_data1
      attr(d,"SID") <- d[evt_id=="SID"]$evt_data1
      attr(d,"far_next") <- d[evt_id=="far_next"]$evt_data1
      attr(d,"next_dim") <- d[evt_id=="next_dim"]$evt_data1
      attr(d,"look_ahead") <- d[evt_id=="look_ahead"]$evt_data1
      attr(d,"RIN") <- d[evt_id=="RIN"]$evt_data1
      attr(d,"Eyetracker") <- d[evt_id=="Eyetracker"]$evt_data1
      attr(d,"lc_delay") <- d[evt_id=="lc_delay"]$evt_data1
      attr(d,"kept_bgc") <- d[evt_id=="kept_bgc"]$evt_data1
      attr(d,"continues") <- d[evt_id=="continues"]$evt_data1
      attr(d,"bg_color") <- d[evt_id=="bg_color"]$evt_data1
      attr(d,"gridlines_color") <- d[evt_id=="gridlines_color"]$evt_data1
      attr(d,"ghost_zoid") <- d[evt_id=="ghost_zoid"]$evt_data1
      attr(d,"das_delay") <- d[evt_id=="das_delay"]$evt_data1
      attr(d,"gridlines_y") <- d[evt_id=="gridlines_y"]$evt_data1
      attr(d,"gridlines_x") <- d[evt_id=="gridlines_x"]$evt_data1
      attr(d,"session") <- d[.N,session] 
      #attr(d,"Verbose_logging") <- d[evt_id=="Verbose_logging"]$evt_data1 ## not here
      attr(d,"inverted") <- d[evt_id=="inverted"]$evt_data1
      attr(d,"board_mask") <- d[evt_id=="board_mask"]$evt_data1
      attr(d,"Log-Version") <- d[evt_id=="`Log-Version`"]$evt_data1
      attr(d,"dimtris") <- d[evt_id=="dimtris"]$evt_data1
      attr(d,"fullscreen") <- d[evt_id=="fullscreen"]$evt_data1
      attr(d,"pause_enabled") <- d[evt_id=="pause_enabled"]$evt_data1
      attr(d,"tetris_zoids") <- d[evt_id=="tetris_zoids"]$evt_data1
      attr(d,"das_reversible") <- d[evt_id=="das_reversible"]$evt_data1
      attr(d,"fps") <- d[evt_id=="fps"]$evt_data1
      attr(d,"border_color") <- d[evt_id=="border_color"]$evt_data1
      attr(d,"undo") <- d[evt_id=="undo"]$evt_data1
      attr(d,"pentix_zoids") <- d[evt_id=="pentix_zoids"]$evt_data1
      attr(d,"lines_per_lvl") <- d[evt_id=="lines_per_lvl"]$evt_data1
      attr(d,"next_mask") <- d[evt_id=="next_mask"]$evt_data1
      attr(d,"grace_period") <- d[evt_id=="lines_per_lvl"]$evt_data1
      #attr(d,"tetrises_covered") <- d[evt_id=="tetrises_covered"]$evt_data1 ## missing
      attr(d,"drop_bonus") <- d[evt_id=="drop_bonus"]$evt_data1
      attr(d,"board_echo_lc") <- d[evt_id=="board_echo_lc"]$evt_data1
      attr(d,"Session") <- d[evt_id=="Session"]$evt_data1  
      attr(d,"zoid_slam") <- d[evt_id=="zoid_slam"]$evt_data1
      attr(d,"seven_bag_switch") <- d[evt_id=="seven_bag_switch"]$evt_data1
      attr(d,"keep_zoid") <- d[evt_id=="keep_zoid"]$evt_data1
      attr(d,"wall_kicking") <- d[evt_id=="wall_kicking"]$evt_data1
      attr(d,"feedback_mode") <- d[evt_id=="feedback_mode"]$evt_data1
      attr(d,"board_echo_placed") <- d[evt_id=="board_echo_placed"]$evt_data1
      attr(d,"are_delay") <- d[evt_id=="are_delay"]$evt_data1
      attr(d,"Start time") <- d[evt_id=="`Start time`"]$evt_data1
      attr(d,"next_dim_alpha") <- d[evt_id=="next_dim_alpha"]$evt_data1
      attr(d,"das_repeat") <- d[evt_id=="das_repeat"]$evt_data1
      attr(d,"game_wd") <- d[evt_id=="game_wd"]$evt_data1
      attr(d,"game_ht") <- d[evt_id=="game_ht"]$evt_data1
      #attr(d,"game_seed") <- d[evt_id=="game_seed"]$evt_data1   ##missing -- random seeds
      attr(d,"tiny_zoids") <- d[evt_id=="tiny_zoids"]$evt_data1
      attr(d,"game_type") <- d[evt_id=="game_type"]$evt_data1
      attr(d,"visible_board") <- d[evt_id=="visible_board"]$evt_data1
      attr(d,"visible_zoid") <- d[evt_id=="visible_zoid"]$evt_data1
      attr(d,"gravity") <- d[evt_id=="gravity"]$evt_data1
      attr(d,"grace_refresh") <- d[evt_id=="grace_refresh"]$evt_data1
      attr(d,"tps") <- d[evt_id=="tps"]$evt_data1
    }
  }
  
  attr(d,"worldsurf_rect.top") <- 0
  attr(d,"worldsurf_rect.left") <- 0
  
  
  ##what is side? --new files
  
  
  set(d, j=which(colnames(d) %in% names(attributes(d))), value=NULL)
  if(is.null(data.file)){
    saveRDS(d, file=file.path(new_data, "rds", sprintf("%s.a%d.rds", pid, assessment)), compress=TRUE)
    invisible(d)
  }else{
    return(d)
  }
}

write.DynamicROI <- function(x, file, debug=FALSE) {
  if(debug==TRUE){
    print("write.DynamicROI")
  }
  fileConn <-file(file)
  writeLines(c(paste(colnames(x),collapse="\t"),
               x[,paste(id,parent,layer,start,end,static,sapply(x,function(o) paste(o,collapse=",")),sapply(y,function(o) paste(o,collapse=",")),sep="\t")]
  ), fileConn)
  close(fileConn)
}

read.DynamicROI <- function(file, ...) fread(file,sep = "\t",colClasses =  list(character = c("start","end")),...)[,c("x","y"):=list(x=lapply(str_split(x,","),as.numeric),y=lapply(str_split(y,","),as.numeric))]

##Changed for popStudy
loadTetrisRDS <- loadTetrisRDA <- function(pid, assessment=1, debug=FALSE, meta2=F) {
  if(debug==TRUE){
    print("loadTetrisRDS")
  }
  a <- getAssessments(debug=debug, meta2=meta2)
  
  if(!meta2){
    pid <- a[[as.character(pid)]][[assessment]]
  }
  rda <- readRDS(sprintf("%s/rds/%s.a%d.rds", new_data, pid, assessment))
  #   g1idx <- rda[game_number==1,.I,by=game_number][1,I]
  #   n <- names(rda)
  #   for (i in which(is.na(rda[g1idx]))) {
  #     set(rda,g1idx,i,rda[(g1idx+1),n[i],with=F])
  #   }
  rda
}

##Changed for popStudy
loadTetrisROIs <- function(pid, assessment=1, debug=FALSE, meta2=F) {
  if(debug==TRUE){
    print("loadTetrisROIs")
  }
  a <- getAssessments(debug=debug, meta2=meta2)
  if(!meta2){
    pid <- a[[as.character(pid)]][[assessment]]
  }
  rois <- read.DynamicROI(sprintf("%s/rois/%s.rois.a%d.txt", new_data, pid, assessment))
  
  ##workaround for integer64
  if(as.numeric(rois$start[1])!= -Inf){
    rois$start <- as.integer64(rois$start)
    rois$end <- as.integer64(rois$end)
  } else {
    rois$start <- as.numeric(rois$start)
    rois$end <- as.numeric(rois$end)
  }
  rois
}

summarizeEyeData <- function(obj, debug=FALSE) {
  if(debug==TRUE){
    print("summarizeEyeData")
  }
  gametot <- tail(unique(obj$game_number), n=1)
  s <- ddply(subset(obj, event_type=="EYE_SAMP"), .(game_number), function(d) {
    ##check to make sure that there are at least some fixations
    if(length(which(d$smi_samp_x_l!=0 & d$evt_id!="CALIBRATION"))/length(d$smi_samp_x_l) > .01){
      
      ##pupil=smi_diam_y_l is depricated 
      ##d.pva <- with(d, pva(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l))
      ##d.c <- classify(d.pva$v, e=d.pva$blinks, samplerate=attr(d.pva,"samplerate"), minsac=7*(1/attr(d.pva, "samplerate")), sigma=4)
      ##d.pva <- with(d, pva(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l, blinks=(smi_samp_x_l==0 & smi_samp_y_l==0) ))
      
      d.g <- with(d, gazetools(smi_samp_x_l, smi_samp_y_l, 500, rx=1680, ry=1050, sw=473.76, sh=296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l,blinks=(smi_samp_x_l==0 & smi_samp_y_l==0), timestamp=smi_ts, minsac=7*(1/500)))
      if(debug==TRUE){
        print(sprintf("classify game %s of %s", unique(d$game_number), gametot))
      }
      d.c <- d.g$classify(sigma=4)
      ##print("classify")
      ## d.c <- d.g$classify()
      
      d.f <- d.g$fixations()
      
      ##f <- summarySE(subset(fixations(d.c,d.pva),quality==1), "fixation.duration")
      f <- summarySE(subset(d.f,quality==1), "fixation.duration")
      f <- f[,3:6]
      colnames(f) <- c("fix","fix.sd","fix.se","fix.ci")
      f} else {
        if(debug==TRUE){
          print(sprintf("classify game %s of %s. Less than 1%% non-zero fixations", unique(d$game_number), gametot))
        }
        f <- data.table(fix=0,fix.sd=0,fix.se=0,fix.ci=0)
        f}
  })
  
  s$id <- attr(obj,"SID")
  s
}

getCriterionScore <- function(obj, debug=FALSE) {
  if(debug==TRUE){
    print("getCriterionScore")
  }
  data.frame(id=attr(obj,"SID"),
             criterion=mean(tail(sort(unlist(dlply(obj, .(game_number), function(x) tail(x$score,1)))),4)))
}


getAllSaccades <- function(obj, debug=FALSE) {
  if(debug==TRUE){
    print("getAllSaccades")
  }
  gametot <- tail(unique(obj$game_number), n=1)
  s <- ddply(subset(obj, event_type=="EYE_SAMP"), .(game_number), function(d) {
    d[which(is.na(d$level)),"level"] <- 0
    if(debug==TRUE){
      print(sprintf("Saccades for game %s of %s", unique(d$game_number), gametot))
    }
    
    ##d.pva <- with(d, pva(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l, window=21, blinkFUN="detect_blinks.PV", timestamp=smi_ts))
    d.g <- with(d, gazetools(smi_samp_x_l, smi_samp_y_l, 500, rx=1680, ry=1050, sw=473.76, sh=296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l, timestamp=smi_ts, minsac=7*(1/500)))
    
    ##d.c <- classify(d.pva@v, blinks=d.pva@blinks, min.sac=7*(1/d.pva@samplerate), sigma=4)
    d.c <- d.g$classify(sigma=4)
    
    d.s <- d.g$saccades()
    ##m <- cbind(d,as.data.frame(d.pva),as.data.frame(d.c))
    m <- cbind(d,as.data.frame(d.g$data),as.data.frame(d.c))
    
    ##cbind(getSaccades(d.c,d.pva,drop=F),
    cbind(d.s,
          ddply(subset(m,saccade_ids>0), .(saccade_ids), function(x) {
            data.frame(curr_zoid=head(x$curr_zoid,1),
                       level=head(x$level,1),
                       ridge_len=head(x$ridge_len,1),
                       avgheight=head(x$avgheight,1),
                       pits=head(x$pits,1),
                       roughness=head(x$roughness,1),
                       filled_rows_covered=head(x$filled_rows_covered,1),
                       tetris_progress=head(x$tetris_progress,1),
                       ridge_len_sqr=head(x$ridge_len_sqr,1),
                       lines_cleared=head(x$lines_cleared,1)
            )}))
  })
  s$id <- attr(obj,"SID")
  s
}

getAllFixations <- function(obj, debug=FALSE) {
  if(debug==TRUE){
    print("getAllFixations")
  }
  gametot <- tail(unique(obj$game_number), n=1)
  s <- ddply(subset(obj, event_type=="EYE_SAMP"), .(game_number), function(d) {
    d[which(is.na(d$level)),"level"] <- 0
    if(debug==TRUE){
      print(sprintf("Fixations for game %s of %s", unique(d$game_number), gametot))
    }
    
    ##d.pva <- with(d, pva(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l, window=21, blinkFUN="detect_blinks.PV", timestamp=smi_ts))
    d.g <- with(d, gazetools(smi_samp_x_l, smi_samp_y_l, 500, rx=1680, ry=1050, sw=473.76, sh=296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l, timestamp=smi_ts, minsac=7*(1/500)))
    
    ##d.c <- classify(d.pva@v, blinks=d.pva@blinks, min.sac=7*(1/d.pva@samplerate), sigma=4)
    d.c <- d.g$classify(sigma=4)
    
    d.f <-d.g$fixations()
    
    #m <- cbind(d,as.data.frame(d.pva),as.data.frame(d.c))
    m <- cbind(d,as.data.frame(d.g$data),as.data.frame(d.c))
    
    ##cbind(getFixations(d.c,d.pva,drop=F),
    cbind(d.f,
          ddply(subset(m,fixation_ids>0), .(fixation_ids), function(x) {
            data.frame(curr_zoid=head(x$curr_zoid,1),
                       level=head(x$level,1),
                       ridge_len=head(x$ridge_len,1),
                       avgheight=head(x$avgheight,1),
                       pits=head(x$pits,1),
                       roughness=head(x$roughness,1),
                       filled_rows_covered=head(x$filled_rows_covered,1),
                       tetris_progress=head(x$tetris_progress,1),
                       ridge_len_sqr=head(x$ridge_len_sqr,1),
                       lines_cleared=head(x$lines_cleared,1)
            )}))
  })
  s$id <- attr(obj,"SID")
  s
}

getSP <- function(p, assessment=1, debug=FALSE,paramlist=NULL, meta2=F) {
  if(debug==TRUE){
    print("getSP")
  }
  
  ##write.table(sprintf("Subject: %s", p), file="getsperrorlog.csv", append=TRUE, col.names=F, row.names=F)
  rda = loadTetrisRDA(as.character(p), assessment, debug=debug, meta2=meta2)
  top4 = rda[game_number>0,list(score=.SD[.N,score]),by=game_number][order(-score)][1:4,game_number]
  if(sum(is.na(top4))>0){
    top = 4-sum(is.na(top4))
    top4 = rda[game_number>0,list(score=.SD[.N,score]),by=game_number][order(-score)][1:top,game_number]
  }
  rois = loadTetrisROIs(as.character(p), assessment, debug=debug, meta2=meta2)
  rois[parent=="nextsurf",layer:=1]
  ##print("rbind")
  print(sprintf("Games: %s",top4))
  
  game_samplerates <- rda[,list(game_number=game_number[1],duration = (ts[.N]-ts[1]),samps = rda[game_number==.BY & event_type=="EYE_SAMP",.N]),by=game_number][game_number!=0,samps/duration]
  
  missing.eyes <- which(game_samplerates < game_samplerates[which(game_samplerates==max(game_samplerates))]*.75 | game_samplerates==0)
  if(length(missing.eyes)>0){
    print(sprintf("Less than 75%s typical sample rate: %s.", paste("%"), paste(sprintf("(game %s samples)",paste(missing.eyes, round(game_samplerates[missing.eyes]), sep="; ")),collapse=", ")))
    
    rda <- rda[!game_number %in% missing.eyes]
    
  }
  rbindlist(
    lapply(top4, function(i,rda,rois) {
      print(sprintf("Started Game %s at %s", i, Sys.time()))
      ##write.table(sprintf("Started Game %s of 4: %s",which(top4==i) ,i), "getsperrorlog.csv", append=TRUE, col.names=F, row.names=F)
      ##print("gazetools")
      if(NROW(rda[game_number==i])>0){
        
        #rda$smi_ts <- as.character(rda$smi_ts)
        if(!is.null(paramlist)){
          necessary <- c("samplerate","rx","ry","sw","sh")
          if(length(which(!necessary %in% names(paramlist)))>0){
            stop(sprintf("Missing the following parameters: %s", paste(necessary[which(!necessary %in% names(paramlist))], collapse=",")))
          }else{
            g = rda[game_number==i, gazetools(smi_samp_x_l, smi_samp_y_l, samplerate=paramlist$samplerate, rx=paramlist$rx, ry=paramlist$ry, sw=paramlist$sw, sh=paramlist$sh, smi_eye_z_l, blinks=(smi_samp_x_l==0 | smi_samp_y_l==0), timestamp=smi_ts)]
          }
        }else{
          g = rda[game_number==i, gazetools(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, blinks=(smi_samp_x_l==0 | smi_samp_y_l==0), timestamp=smi_ts)]
        }
        g$data = cbind(g$data, rda[game_number==i, list(game_number,episode_number,curr_zoid,next_zoid,level,score)])
        g$data[is.na(score),score:=0]
        g$data[is.na(level),level:=0]
        ##print("classify")
        g$classify(idbase=i*100000)
        ##print("scanpath")
        #sp = g$scanpath(game_number>0,rois,by=c("game_number","episode_number"))
        sp = g$scanpath(game_number>0,rois,by=c("game_number"))
        ##print("finished scanpath")
        setkey(sp,fixation_ids)
        
        sp = sp[g$data[fixation_ids>0,.SD[1,list(curr_zoid,next_zoid,level,score)],by=fixation_ids]]
        sp[,sid:=as.character(p)]
        rm(g);gc();
        
        rda$smi_ts <- as.integer64(rda$smi_ts)
        
        ##write.table(sp, "allScanPaths.dat", append=TRUE, col.names=F, row.names=F)
        
        sp
      } else {
        NULL
      }
      ##write.table(sprintf("Finished Game %s of 4: %s",which(top4==i) ,i), "getsperrorlog.csv", append=TRUE, col.names=F, row.names=F)
      
    }, rda=rda[event_type=="EYE_SAMP"], rois=rois)
    ##, mc.cores=4, mc.preschedule=FALSE)
  )
}

getSP2 <- function(rda, roi, debug=FALSE) {
  if(debug==TRUE){
    print("getSP")
  }
  
  ##write.table(sprintf("Subject: %s", p), file="getsperrorlog.csv", append=TRUE, col.names=F, row.names=F)
  rda = rda
  top4 = rda[game_number>0,list(score=.SD[.N,score]),by=game_number][order(-score)][1:4,game_number]
  if(sum(is.na(top4))>0){
    top = 4-sum(is.na(top4))
    top4 = rda[game_number>0,list(score=.SD[.N,score]),by=game_number][order(-score)][1:top,game_number]
  }
  
  rois = roi
  rois[parent=="nextsurf",layer:=1]
  ##print("rbind")
  print(sprintf("Games: %s",top4))
  rbindlist(
    lapply(top4, function(i,rda,rois) {
      print(sprintf("Started Game %s at %s", i, Sys.time()))
      ##write.table(sprintf("Started Game %s of 4: %s",which(top4==i) ,i), "getsperrorlog.csv", append=TRUE, col.names=F, row.names=F)
      ##print("gazetools")
      g = rda[game_number==i, gazetools(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, blinks=(smi_samp_x_l==0 & smi_samp_y_l==0), timestamp=smi_ts)]
      g$data = cbind(g$data, rda[game_number==i, list(game_number,episode_number,curr_zoid,next_zoid,level,score)])
      g$data[is.na(score),score:=0]
      g$data[is.na(level),level:=0]
      ##print("classify")
      g$classify(idbase=i*100000)
      ##print("scanpath")
      #sp = g$scanpath(game_number>0,rois,by=c("game_number","episode_number"))
      sp = g$scanpath(game_number>0,rois,by=c("game_number"))
      ##print("finished scanpath")
      setkey(sp,fixation_ids)
      
      sp = sp[g$data[fixation_ids>0,.SD[1,list(curr_zoid,next_zoid,level,score)],by=fixation_ids]]
      sp[,sid:=as.character(p)]
      rm(g);gc();
      
      ##write.table(sp, "allScanPaths.dat", append=TRUE, col.names=F, row.names=F)
      
      sp
      ##write.table(sprintf("Finished Game %s of 4: %s",which(top4==i) ,i), "getsperrorlog.csv", append=TRUE, col.names=F, row.names=F)
      
    }, rda=rda[event_type=="EYE_SAMP"], rois=rois)
    ##, mc.cores=4, mc.preschedule=FALSE)
  )
  
  
}

getMeta <- function(p, debug=FALSE) {
  if(debug==TRUE){
    print("getMeta")
  }
  rda = loadTetrisRDA(as.character(p), 1, debug=debug)
  top4 = rda[game_number>0,list(score=.SD[.N,score]),by=game_number][order(-score)][1:4,game_number]
  rbindlist(mclapply(top4, function(i,rda) {
    g = rda[game_number==i, gazetools(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, blinks=(smi_samp_x_l==0 & smi_samp_y_l==0), timestamp=smi_ts)]
    g$data = cbind(g$data, rda[game_number==i, list(game_number,episode_number,curr_zoid,next_zoid,level,score)])
    g$data[is.na(score),score:=0]
    g$data[is.na(level),level:=0]
    g$classify()
    m = g$data[fixation_ids>0,.SD[1,list(curr_zoid,next_zoid,level,score)],by=c("game_number","fixation_ids")]
    rm(g);gc();
    m
  }, rda=rda, mc.cores=4, mc.preschedule=FALSE))
}

getSummary <- function(p, debug=FALSE) {
  if(debug==TRUE){
    print("getSummary")
  }
  rda = loadTetrisRDA(as.character(p), 1, debug=debug)
  top4 = rda[game_number>0,list(score=.SD[.N,score]),by=game_number][order(-score)][1:4,game_number]
  rbindlist(mclapply(top4, function(i,rda,rois) {
    g = with(rda[game_number==i], gazetools(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, blinks=(smi_samp_x_l==0 & smi_samp_y_l==0), timestamp=smi_ts))
    g$data[,c("game_number","episode_number","curr_zoid","next_zoid","level","score"):=rda[game_number==i,list(game_number,episode_number,curr_zoid,next_zoid,level,score)]]
    g$classify()
    s = g$summary()
    s[,`:=`(sid=rda[1,SID],game=i)]
    rm(g);gc();
    setcolorder(s, c("sid", "game", "measure", "value", "sd", "N"))
    s
  }, rda=rda, mc.cores=4, mc.preschedule=FALSE))
}


getCalibration <- function(p, debug=FALSE){
  if(debug==TRUE){
    print("getCalibration")
  }
  data = data.table()
  for(i in p){
    if(file.exists(sprintf("%s/rds/%s.a%d.rds", new_data, getAssessments()[i][[1]][assessment], assessment))){
      rda = loadTetrisRDA(as.character(i), 1, debug=debug)
      cur_calib = unique(rda[evt_id=="CALIBRATION" & evt_data1 == "Complete"]$evt_data2)
      data <- rbindlist(list(data, data.table(sid=i,calibration=cur_calib)), use.names=T)
    }
  }
  data
}

g.spatplot <- function(x,pointcolor = "blue", linecolor = "red", filter=NULL, .background=NULL){
  ##this function is almost exclusively for recoloring the "spatial-class" plot stuff
  .call <- as.call(quote(x$plot()))
  .call[[2]] <- match.call()$filter
  .call$style <- "spatial-class"
  if(!is.null(.background)){
    .call$background <- .background
  }
  plt <- eval(.call)
  
  # if(!is.null(background)){
  #   
  #   
  #   plt <- x$plot(style="spatial-class", filter = (filter), background=background)
  # }else{
  #   plt <- x$plot(style="spatial-class", filter = (filter))
  # }
  whichpoint <- which(lapply(plt$layers, function(x) "GeomPoint" %in% class(x$geom))==T)
  whichline <- which(lapply(plt$layers, function(x) "GeomPath" %in% class(x$geom))==T)
  plt$layers[[whichpoint]]$aes_params$colour = pointcolor
  plt$layers[[whichline]]$aes_params$colour = linecolor
  plt
  
}




if (F) {
  #make sure you run everything that comes before this to get all of it in your environment
  
  ##ASSESSMENT 1
  cl <- makeCluster(4)
  registerDoParallel(cl)
  
  ## All the same (pick one)
  mclapply(as.numeric(names(getAssessments(exclude=bad))), rdaTetrisLog2, 1, mc.cores=2)
  foreach (p=as.numeric(names(getAssessments(exclude=bad)))[1], .packages=c("data.table")) %do% rdaTetrisLog2(p, 1, debug=TRUE)
  for (p in as.numeric(names(getAssessments(exclude=bad)))) rdaTetrisLog2(p,1)
  
  ##ROIS
  
  ##skipped 213 and 214.. Error in rbindlist(list(rois, zoid_final, zoid_future, zoid_n_board, piles)) : 
  ###Class attributes at column 4 of input list at position 2 does not match with column 4 of input list at position 1. Coercion of objects of class 'factor' alone is handled internally by rbind/rbindlist at the moment.
  
  for (p in  names(getAssessments(exclude=bad))) write.DynamicROI(getROIs(loadTetrisRDA(as.character(p), 1), 1), sprintf("../Data_Sangster_2015/rois/%s.rois.a1.txt", getAssessments()[[p]][1]))
  
  ##do this using reparse-caller.py (you should've already done it)
  ##foreach (p=sapply(getAssessments(),function(x)x[2])) %do% system(sprintf("python reparse.py -i ../Data/d4/assessment2/%s/%s.tsv -o ../Data_Sangster_2015/%s.tsv.rmh.a2",p,p,p))
  
  ##ASSESSMENT 2
  cl <- makeCluster(4)
  registerDoParallel(cl)
  
  
  ##all the same (run only one)
  mclapply(as.numeric(names(getAssessments(exclude=bad))), rdaTetrisLog2, 2, mc.cores=2)
  foreach (p=as.numeric(names(getAssessments(exclude=bad)))[-1], .packages=c("data.table")) %do% rdaTetrisLog2(p, 2, debug=TRUE)
  for (p in as.numeric(names(getAssessments(exclude=bad)))) rdaTetrisLog2(p,2, debug=TRUE)
  
  ## ROIS
  for (p in names(getAssessments(exclude=bad))) write.DynamicROI(getROIs(loadTetrisRDA(as.character(p), 2), 1), sprintf("../Data_Sangster_2015/rois/%s.rois.a2.txt", getAssessments(exclude=bad)[[p]][2]))
  
  
  
  ##OTHER ANALYSIS
  .summary <- rbindlist(mclapply(names(getAssessments(exclude=bad)), getSummary, mc.cores=3, mc.preschedule=FALSE, mc.allow.recursive=TRUE))
  .meta <- rbindlist(mclapply(names(getAssessments(exclude=bad)), getMeta, mc.cores=4, mc.preschedule=FALSE, mc.allow.recursive=TRUE))
  ##use next if function for this instead
  #.scanpaths <- rbindlist(mclapply(names(getAssessments(exclude=bad)), getSP, mc.cores=2, mc.preschedule=FALSE, mc.allow.recursive=TRUE))
}



##this was al just debugging stuff
#getAllFixations2 <- function(obj) {
#  d <- d[d$event_type=="EYE_SAMP",]
#  d[which(is.na(d$level)),"level"] <- 0
#  d.pva <- with(d, pva(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l, pupil=smi_diam_y_l, window=21, blinkFUN="detect_blinks.PV"))
#  d.c <- classify.VI(d.pva@v, blinks=d.pva@blinks, min.sac=7*(1/d.pva@samplerate), sigma=4)
#  m <- cbind(d,as.data.frame(d.pva),as.data.frame(d.c))
#  s <- ddply(m[m$fixation_ids>0,], .(game_number, fixation_ids), function(d,ts) {
#    data.frame(fixation.duration=nrow(d)*ts,
#               level=head(d$level,1),
#               curr_zoid=head(d$curr_zoid,1))
#  },ts=1/d.pva@samplerate)
#  s$id <- attr(obj,"SID")
#  s
#}


##TESTING


###END TESTING





if (F) {
  
  # you should just be able to run all this at once just read through text comments first
  #any comment code is just other ways of achieving the same goals, or troubleshooting
  
  
  ##  z <- mclapply(names(getAssessments()), function(x) {
  ##  print(x)
  ##  summarizeEyeData(loadTetrisRDA(x,1))
  ##}, mc.cores=8)
  
  z <- lapply(names(getAssessments(exclude=bad, debug=TRUE)), function(x) {
    print(x)
    summarizeEyeData(loadTetrisRDA(x,1, debug=TRUE), debug=TRUE)
  })
  
  
  ##  fx <- mclapply(names(getAssessments()), function(x) {
  ##    getAllFixations(loadTetrisRDA(x,1))
  ##  }, mc.cores=16)
  
  ##list("224","225","226")
  ## names(getAssessments(exclude=bad, debug=TRUE))
  
  
  fx <- lapply(names(getAssessments(exclude=bad, debug=TRUE)), function(x) {
    print(x)
    getAllFixations(loadTetrisRDA(x,1, debug=TRUE), debug=TRUE)
  })
  fx <- do.call("rbind",fx)
  write.table(fx, "allFixations.dat", col.names=T, row.names=F)
  
  ##  sc <- mclapply(names(getAssessments()), function(x) {
  ##    getAllSaccades(loadTetrisRDA(x,1))
  ##  }, mc.cores=16)
  sc <- lapply(names(getAssessments(exclude=bad, debug=TRUE)), function(x) {
    print(x)
    getAllSaccades(loadTetrisRDA(x,1, debug=TRUE), debug=TRUE)
  })
  sc <- do.call("rbind",sc)
  write.table(sc, "allSaccades.dat8", col.names=T, row.names=F)
  
  ##  cr <- mclapply(names(getAssessments()), function(x) {
  ##    getCriterionScore(loadTetrisRDA(x,1))
  ##  }, mc.cores=16)
  cr <- lapply(names(getAssessments(exclude=bad, debug=TRUE)), function(x) {
    getCriterionScore(loadTetrisRDA(x,1, debug=TRUE), debug=TRUE)
  })
  cr <- do.call("rbind",cr)
  cr <- cr[rev(order(cr$criterion)),]
  row.names(cr) <- NULL
  write.table(cr, "criterion.dat", col.names=T, row.names=F)
  ##if you have errors, uncomment this, but make sure you're not parallel processing
  ##write.table("", "getsperrorlog.csv")
  
  
  for(i in names(getAssessments(exclude=bad))){
    print(sprintf("Started %s at %s", i, Sys.time()))
    
    s_raw <- getSP(i)
    
    print(sprintf("Finished %s at %s", i, Sys.time()))
    print(unique(s_raw$game_number))
    s1 <- s_raw[, lapply(.SD, as.character)]
    
    if(i == names(getAssessments(exclude=bad))[1]){
      write.table(as.matrix(s1), "allScanPaths_20160309.dat", col.names=T, row.names=F,quote = T)
    }else {
      write.table(as.matrix(s1), "allScanPaths_20160309.dat", append=TRUE, col.names=F, row.names=F, quote=T)
    }
    print(sprintf("Finished writing %s at %s", i, Sys.time()))
    rm(s1,s_raw)
  }
  
  ##sp <- lapply(names(getAssessments(exclude=bad)), function(x) {
  ##getSP(x)
  ##})
  ##sp <- do.call("rbind",sp)
  ##write.table(sp, "allScanPath.dat", col.names=T, row.names=F)
  
  
  cl <- makeCluster(16)
  registerDoParallel(cl)
  
  cr <-foreach(id=names(getAssessments(exclude=bad, debug=TRUE)), .combine=rbind, .packages=c("plyr","stringr","rjson","gazetools","Rmisc")) %dopar%
    getCriterionScore(loadTetrisRDA(id,1, debug=TRUE),debug=TRUE)
}

if(F){
  #you should just be able to run this as is. if it hangs on one for longer than about 6 hours terminate run and skip it 
  #by changing the for(i in names(getAssessments())) to for(i in names(getAssessments())[-1:x]) where x is however many you successfully passed
  
  ##don't run this if the previous if command was successful as this code is included in the previous call -- this is also the old method, be careful.
  
  for(i in names(getAssessments(exclude=bad))){
    print(sprintf("Started %s at %s", i, Sys.time()))
    s <- getSP(i)
    print(sprintf("Finished %s at %s", i, Sys.time()))
    print(unique(s$game_number))
    
    if(i == names(getAssessments(exclude=bad))[1]){
      write.table(as.matrix(s), "allScanPaths.dat", col.names=T, row.names=F, quote=T)
    }else {
      write.table(as.matrix(s), "allScanPaths.dat", append=TRUE, col.names=F, row.names=F, quote=T)
    }
    print(sprintf("Finished writing %s at %s", i, Sys.time()))
    rm(s)
  }
}

if(F){
  ##this is if we accidentally make allScanPaths.dat without quoting the list columns
  ##change stuff as you see needed
  
  test2 <- fread("allScanPathsTEST.dat",sep = " ", header = F, )
  
  test3 <- apply( test2, 1 , function(x) toString(x[18:22]))
  test3 <- lapply(test3, function(x) gsub(",,",",",x))
  test4 <- apply( test2, 1 , function(x) toString(x[23:27]))
  test4 <- lapply(test4, function(x) gsub(",,",",",x))
  test2$V5
  new.test <- data.table("game_number"= test2$V1, 
                         "fixation_ids" = test2$V2, 
                         "time.begin" = test2$V3, 
                         "time.end" = test2$V4,
                         "timestamp.begin" = test2$V5, 
                         "timestamp.end"  = test2$V6,
                         "fixation.x"  = test2$V7,
                         "fixation.y"  = test2$V8,
                         "fixation.duration"  = test2$V9,
                         "fixation.velocity"  = test2$V10,
                         "ex"  = test2$V11,
                         "ey"  = test2$V12,
                         "ez"  = test2$V13,
                         "quality"  = test2$V14,
                         "id"  = test2$V15,
                         "parent"  = test2$V16,
                         "roi.layer"  = test2$V17,
                         "roi.x"  = unlist(test3),
                         "roi.y"  = unlist(test4),
                         "size"  = test2$V28,
                         "dist"  = test2$V29,
                         "pip"  = test2$V30,
                         "curr_zoid"  = test2$V31,
                         "next_zoid"  = test2$V32,
                         "level"  = test2$V33,
                         "score"  = test2$V34,
                         "sid" = test2$V35)
  
  write.table(new.test, "allScanPathsTEST.dat",col.names = T, row.names = F, quote = T)
  
  
  ##this was to consolidate multiple allScanPaths files, became an issue because of timestamps larger than 32 bit
  
  test <- read.table("allScanPaths_150613.dat", header = T )
  test2 <- read.table("allScanPaths_216-223-225.dat",header = T)
  test3 <- read.table("allScanPaths_213-214-222-236-237-238.dat", header = T)
  
  test3
  test4 <- read.table("allScanPathsTEST.dat", header = T)
  unique(new.test$sid)
  new.test <- rbindlist(list(test3, test2, test, test4))
  ## 213, 214, 222, 237, 238, 241
  new.test[which(new.test$sid==21)]
  
  write.table(new.test, "allScanPaths_150811.dat",col.names = T, row.names = F, quote = T)
  which(gray.test$timestamp.begin==min(gray.test$timestamp.begin))
  
  
  test <- test[which(test$sid!=214),]
  
  
  gray.test <- new.test[which(new.test$sid=="213"|new.test$sid=="214"|new.test$sid=="222"|new.test$sid=="237"|new.test$sid=="238"|new.test$sid=="241"),]
  
  write.table(gray.test, "allScanPaths_Requested_150811.dat", col.names = T, row.names = F, quote = T)
  
  new.test <- rbind(test3, test2, test, test4)
  
  
  
  test <- read.table("allScanPaths_150803.dat", header = T)
}

if(F){
  
  ##used to check the quality of eyedata for a subject
  
  
  d <- loadTetrisRDA("119",1)
  ##pupil=smi_diam_y_l, is depricated
  ##d.pva <- with(d, pva(smi_samp_x_l, smi_samp_y_l, 500, 1680, 1050, 473.76, 296.1, smi_eye_z_l, smi_eye_x_l, smi_eye_y_l, window=21, blinkFUN="detect_blinks.SW"))
  ##classify.VI(blinks=.., min.sac=..) is now just classify(e=.., minsac=..) 
  d.c <- classify(d.pva@v, e=(d.pva@blinks), minsac=7*(1/d.pva@samplerate), sigma=4)
  p <- plot(d.pva,d.c,40)
}
