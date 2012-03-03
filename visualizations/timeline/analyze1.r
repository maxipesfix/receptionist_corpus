#################################################
#
# Copyright (c) Maxim Makatchev, 2012
# 
#################################################

initialize_ds = function(filepath)
{
  op <- options(digits.secs=3)
  y = read.delim(filepath,header=TRUE,sep="\t")

  time1 <- strptime(paste("1/1/2000", y$Begin),  "%m/%d/%Y %H:%M:%OS")
  time2 <- strptime(paste("1/1/2000", y$End),  "%m/%d/%Y %H:%M:%OS")
  y = cbind(y, time1=time1)
  y = cbind(y, time2=time2)
  y =data.frame(y)
  return(y)
}


#########
# extract the data that overlaps with the time segment 
#########
extractSegment = function(req_start, req_end, ds) {
  return( ds[ ((ds$time1<req_end)&(ds$time2>req_start)), ] )
}

getSegmentsDuration = function(ds, label1) {
  return( sum(as.numeric(difftime(ds$time2[ds$Type == label1], ds$time1[ds$Type == label1], units='secs'))) )
}


getSegmentsOverlapDuration = function(ds, label1, label2) {

  overlap = 0
  done_i = FALSE
  done_j = FALSE

  indexes1 = which(ds$Type==label1)
  indexes2 = which(ds$Type==label2)
  
  if ((length(indexes1) == 0)||(length(indexes2) == 0)) {
     return(0)
  }

  i = 1
  j = 1

  while (!(done_i && done_j)) { 

    tj1 = ds$time1[indexes2[j]]
    tj2 = ds$time2[indexes2[j]]
    ti1 = ds$time1[indexes1[i]]
    ti2 = ds$time2[indexes1[i]]
    #print(paste("i=", i , " j=",j))
    #print(paste("ti1=",ti1, ", ti2=", ti2, ", tj1=", tj1, ", tj2=", tj2))

    # checking 4 different overlaps
    if ((tj2 <= ti2 ) && (tj2 > ti1 ) && (tj1 <= ti1)) {  
      # overlap type 1: tj1 ti1 tj2 ti2
        overlap = overlap + as.numeric(difftime(tj2, ti1, units='secs'))
        #print(paste("additive1=", as.numeric(difftime(tj2, ti1, units='secs'))))

    } else if ((tj2 >= ti2 ) && (tj1 <= ti1)) {  
      # overlap type 2: tj1 ti1 ti2 tj2
        overlap = overlap + as.numeric(difftime(ti2, ti1, units='secs'))
        #print(paste("additive2=", as.numeric(difftime(ti2, ti1, units='secs'))))


    } else if ((tj1 < ti2) && (tj1 >= ti1) && (tj2 >= ti2 )) {  
      # overlap type 3: ti1 tj1 ti2 tj2
        overlap = overlap + as.numeric(difftime(ti2, tj1, units='secs'))
        #print(paste("additive3=", as.numeric(difftime(ti2, tj1, units='secs'))))

    } else if ((tj1 >= ti1) && (tj2 <= ti2)) {  
      # overlap type 4: ti1 tj1 tj2 ti2
        overlap = overlap + as.numeric(difftime(tj2, tj1, units='secs'))
        #print(paste("additive4=", as.numeric(difftime(tj2, tj1, units='secs'))))

    } 
    # otherwise no overlap to account for
  
    if (ti2 >= tj2) {
      if (j < length(indexes2)) {
        j = j + 1
      } else if (i < length(indexes1)) {
        i = i + 1
        done_j = TRUE
      } else {
        done_j = TRUE
        done_i = TRUE
      }
    } else {
      if (i < length(indexes1)) {
        i = i + 1
      } else if (j < length(indexes2)) {
        j = j + 1
        done_i = TRUE
      } else {
        done_j = TRUE
        done_i = TRUE
      }
    }

  }
  
  return(overlap)
}



#########
# word wrap
#########
wordwrap = function(x,len) paste(strwrap(x,width=len),collapse="\n") 

#########
# plot the data between the interval
#########
plot_timeline = function(req_start, req_end, ds, title="Timeline") {
  # text.width.adjust=20
  max_y  = 15

  ds = extractSegment(req_start, req_end, ds)

  # extract relevant modalities
  ds = ds[(ds$Type == "u:text")|(ds$Type == "r:text")|(ds$Type == "r:gaze_on_user")|(ds$Type == "u:gaze_on_r")|(ds$Type == "r:gaze_point")|(ds$Type == "u:gaze_point_r"), ]
  #ds = ds[(ds$Type == "u:text")|(ds$Type == "r:text"), ]
  min_time = min(ds$time1)
  max_time = max(ds$time2) 


  text.left = ds$Content[ds$Type == "u:text"]
  text.right = ds$Content[ds$Type == "r:text"]
  
  ## remove text= prefix
  text.left = gsub(pattern="text=", replacement="", text.left)
  text.right = gsub(pattern="text=", replacement="", text.right)

  ## Keep the old par() setting around so that we can reset them at the end
  #par.default = par(no.readonly=TRUE)


  pdf(file=paste("timeline_", title, ".pdf", sep=""), height=8, width=6)

  plot(1, 1, xlim=c(-3,3), ylim=c(0, max_y), type="n", axes=FALSE, ann=FALSE, xaxs="i", yaxs="i")
  par(oma=c(0, 0, 0, 0))
  par(ps=6)  
	
  max_char = floor(3/strwidth("w"))
  text.left = sapply(text.left, wordwrap, len=max_char)
  text.right = sapply(text.right, wordwrap, len=max_char)


	## Define area for drawing the graph (Text is not part of the graph)
	## par(mar=c(bottom, left, top, right))

  par(mar=c(0, 0, 2, 0))
  # par(xaxp=c(-2,2,3), yaxp = c(0,1,1))

  duration = as.numeric(difftime(max_time, min_time, units='secs'))

  left_start_y = max_y - max_y * as.numeric(difftime(ds$time1[ds$Type == "u:text"], min_time, units='secs'))/duration
  left_end_y = max_y - max_y * as.numeric(difftime(ds$time2[ds$Type == "u:text"], min_time, units='secs'))/duration
  left_text_y = (left_start_y + left_end_y)/2
 
  right_start_y = max_y - max_y * as.numeric(difftime(ds$time1[ds$Type == "r:text"], min_time, units='secs'))/duration
  right_end_y = max_y - max_y * as.numeric(difftime(ds$time2[ds$Type == "r:text"], min_time, units='secs'))/duration
  right_text_y = (right_start_y + right_end_y)/2

  

 
	## Display Text on the left
	#mtext(text.left, at=left_text_y, adj=1, side=2, las=2)
	text(-1, left_text_y, text.left, pos=2)
	
  ## Display Text on the right
	#mtext(text.right, at=right_text_y, adj=0, side=4, las=2)

  text(1, right_text_y, text.right, pos=4)


  ## draw the segments

  rect(-1, left_end_y, 0, left_start_y, col="cadetblue2")
  rect(0, right_end_y, 1, right_start_y, col="coral")

  ##
  ## Compute mutual gaze segments
  ##
  u_gaze_on_r_frac = getSegmentsDuration(ds, "u:gaze_on_r")/duration
  r_gaze_on_u_frac = getSegmentsDuration(ds, "r:gaze_on_user")/duration

  u_gaze_on_r_over_r_speech = getSegmentsOverlapDuration(ds, "u:gaze_on_r", "r:text")/getSegmentsDuration(ds, "r:text")
  u_gaze_on_r_over_u_speech = getSegmentsOverlapDuration(ds, "u:gaze_on_r", "u:text")/getSegmentsDuration(ds, "u:text")

  r_gaze_on_u_over_r_speech = getSegmentsOverlapDuration(ds, "r:gaze_on_user", "r:text")/getSegmentsDuration(ds, "r:text")
  r_gaze_on_u_over_u_speech = getSegmentsOverlapDuration(ds, "r:gaze_on_user", "u:text")/getSegmentsDuration(ds, "u:text")

  left_start_y = max_y - max_y * as.numeric(difftime(ds$time1[ds$Type == "u:gaze_on_r"], min_time, units='secs'))/duration
  left_end_y = max_y - max_y * as.numeric(difftime(ds$time2[ds$Type == "u:gaze_on_r"], min_time, units='secs'))/duration
  left_text_y = (left_start_y + left_end_y)/2
 
  right_start_y = max_y - max_y * as.numeric(difftime(ds$time1[ds$Type == "r:gaze_on_user"], min_time, units='secs'))/duration
  right_end_y = max_y - max_y * as.numeric(difftime(ds$time2[ds$Type == "r:gaze_on_user"], min_time, units='secs'))/duration
  right_text_y = (right_start_y + right_end_y)/2

  if ( length(left_start_y) > 0 ) { 
    rect(0.3, left_end_y, 0.45, left_start_y, col="blue")
    text(0.25, -0.1, sprintf("%.2f", u_gaze_on_r_frac), pos = 1) 
    text(0.25, -0.4, sprintf("%.2f", u_gaze_on_r_over_r_speech), pos=1)
    text(0.25, -0.7, sprintf("%.2f", u_gaze_on_r_over_u_speech), pos=1)
  }
  if ( length(right_start_y) > 0 ) { 
    rect(0.45, right_end_y, 0.6, right_start_y, col="red")
    text(0.6, -0.1, sprintf("%.2f", r_gaze_on_u_frac), pos = 1) 
    text(0.6, -0.4, sprintf("%.2f", r_gaze_on_u_over_r_speech), pos=1)
    text(0.6, -0.7, sprintf("%.2f", r_gaze_on_u_over_u_speech), pos=1)
  }


  ##
  ## Compute shared gaze segments
  ##
  u_gaze_point_r_frac = getSegmentsDuration(ds, "u:gaze_point_r")/duration
  r_gaze_point_frac = getSegmentsDuration(ds, "r:gaze_point")/duration

  u_gaze_point_r_over_r_speech = getSegmentsOverlapDuration(ds, "u:gaze_point_r", "r:text")/getSegmentsDuration(ds, "r:text")
  u_gaze_point_r_over_u_speech = getSegmentsOverlapDuration(ds, "u:gaze_point_r", "u:text")/getSegmentsDuration(ds, "u:text")

  r_gaze_point_over_r_speech = getSegmentsOverlapDuration(ds, "r:gaze_point", "r:text")/getSegmentsDuration(ds, "r:text")
  r_gaze_point_over_u_speech = getSegmentsOverlapDuration(ds, "r:gaze_point", "u:text")/getSegmentsDuration(ds, "u:text")


  left_start_y = max_y - max_y * as.numeric(difftime(ds$time1[ds$Type == "u:gaze_point_r"], min_time, units='secs'))/duration
  left_end_y = max_y - max_y * as.numeric(difftime(ds$time2[ds$Type == "u:gaze_point_r"], min_time, units='secs'))/duration
  left_text_y = (left_start_y + left_end_y)/2
 
  right_start_y = max_y - max_y * as.numeric(difftime(ds$time1[ds$Type == "r:gaze_point"], min_time, units='secs'))/duration
  right_end_y = max_y - max_y * as.numeric(difftime(ds$time2[ds$Type == "r:gaze_point"], min_time, units='secs'))/duration
  right_text_y = (right_start_y + right_end_y)/2

  if ( length(left_start_y) > 0 ) { 
    rect(-0.6, left_end_y, -0.45, left_start_y, col="darkorchid")
    text(-0.6, -0.1, sprintf("%.2f", u_gaze_point_r_frac), pos = 1)
    text(-0.6, -0.4, sprintf("%.2f", u_gaze_point_r_over_r_speech), pos=1)
    text(-0.6, -0.7, sprintf("%.2f", u_gaze_point_r_over_u_speech), pos=1)
  }
  if ( length(right_start_y) > 0 ) { 
    rect(-0.45, right_end_y, -0.3, right_start_y, col="darkorange1")
    text(-0.25, -0.1, sprintf("%.2f", r_gaze_point_frac), pos = 1) 
    text(-0.25, -0.4, sprintf("%.2f", r_gaze_point_over_r_speech), pos=1)
    text(-0.25, -0.7, sprintf("%.2f", r_gaze_point_over_u_speech), pos=1)
  }

    text(-1, -0.41, "Fraction of entire interaction:", pos = 2) 
    text(-1, -0.71, "Fraction of receptionist's speech:", pos=2)
    text(-1, -1.01, "Fraction of visitor's speech:", pos=2)


  #axis(1, pos=0) 
  done = FALSE
  tick_times = c(0)
  tick_time = 0
  tick_inc = 5
  while (!done) {
    tick_time = tick_time + tick_inc
    if (tick_time > (duration - 1) ) {
      tick_times = c(tick_times, duration)
      done = TRUE
    } else {
      tick_times = c(tick_times, tick_time)
    }
  }
  #tick_times = c(0, 5, 10, 15, 20, 25, 30)
  tick_y = max_y - max_y * tick_times/duration
  tick_time_labels = tick_times;
  if (floor(tick_times[length(tick_times)]) == tick_times[length(tick_times)]) {   
    tick_time_labels[length(tick_time_labels)] = sprintf("%.0f", tick_time_labels[length(tick_time_labels)])   
  } else {
    tick_time_labels[length(tick_time_labels)] = sprintf("%.1f", tick_time_labels[length(tick_time_labels)]) 
  }

  axis(2, pos=-3, at = tick_y, labels = tick_time_labels)


  #text(-3.2, 7.5, "Time (seconds)")
  mtext("Time (seconds)", side=2, line=-2, cex=1.2, las=0)

  ## no title
  mtext(title, side=3, line=0.5)
  dev.off()


}


plot_file_timelines = function(filename) {
  x = initialize_ds(filename)

  for (i in which(x$Type=="interaction")) {
    req_start = x$time1[i]
    req_end = x$time2[i]
    title= gsub(pattern=" ", replacement="_", x$Content[i])
    title= gsub(pattern="id=", replacement="", title)
    plot_timeline(req_start, req_end, x, title=title)
  }
}


videodial_dir = "~/h/robocept2/data/videodial/"
#videodial_dir = "~/h/robocept3/robocept2/data/videodial/"

plot_file_timelines(paste(videodial_dir, "/qatar/viddial0614_2/front/v35.tsv", sep=""))
plot_file_timelines(paste(videodial_dir, "/qatar/viddial0614_2/front/v37.tsv", sep=""))
plot_file_timelines(paste(videodial_dir, "/qatar/viddial0614_2/front/v38.tsv", sep=""))
plot_file_timelines(paste(videodial_dir, "/qatar/viddial0614_2/front/v39.tsv", sep=""))


plot_file_timelines(paste(videodial_dir, "/qatar/viddial0609/frontright/v107.tsv", sep=""))
plot_file_timelines(paste(videodial_dir, "/qatar/viddial0609/frontright/v108.tsv", sep=""))
plot_file_timelines(paste(videodial_dir, "/qatar/viddial0609/frontright/v114.tsv", sep=""))


#req_start <- strptime(paste("1/1/2000", "00:05:00.000"),  "%m/%d/%Y %H:%M:%OS")
#req_end <- strptime(paste("1/1/2000", "00:8:00.000"),  "%m/%d/%Y %H:%M:%OS")


