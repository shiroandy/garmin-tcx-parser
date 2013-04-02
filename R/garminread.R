# -----------------------------------------------------------------------------
# garminread.r - Dieter Kleinboehl and Andrew White 
# -----------------------------------------------------------------------------
#
# Read Garmin Pulse Watch datafiles .TCX, which is an XML file, and export to:
#
# datsout1	<- "datagarminout.txt"		# Output file ASCII
# datsout2	<- "datagarminout.rdat"		# Output file R data frame
# -----------------------------------------------------------------------------

rm(list=ls(all=TRUE))                   # Delete all objects
graphics.off()                          # close all open graphics windows

library(rChoiceDialogs)

a <- rchoose.dir(default = getwd(),caption = "Select Directory")
setwd(a)

datagarmin <- rchoose.files(default = getwd(), caption = "Select GARMIN file/s", multi = TRUE, filters="*.tcx")

# loop over each garmin file
for(i in 1:length(datagarmin)) {

# Grab filename from path
datafilesplit <- basename(datagarmin[i])

filesplit <- strsplit(datafilesplit,split = ".", fixed = T)

garmindatsout1  <- paste("garmindats_",filesplit[[1]][1],".txt",sep='')		# Output file ASCII
garmindatsout2	<- paste("garmindats_",filesplit[[1]][1],".rdata", sep='')		# Output file R data frame

if(file.exists(garmindatsout1)) 
	file.remove(garmindatsout1)

if(file.exists(garmindatsout2)) 
	file.remove(garmindatsout2)


# READ GARMIN DATA.TCX ----------------------------------------------------

datsgar <- read.table(file=datagarmin[i], strip.white=T,
           blank.lines.skip=FALSE, header=TRUE,sep=';',dec='.')

# get pointer to relevant data rows - exlude Distance after Lap
ilap 	<- which(substr(datsgar[,],1,14) == '<Lap StartTime')
datsgar[ilap+2,] <- NA

#get pointer to time tags - exclude the last tag since this refers to software version
itime 	<- which(substr(datsgar[,],1,6)  == '<Time>')
itime   = itime[1:length(itime)-1]
ihrbpm 	<- which(substr(datsgar[,],1,7)  == '<Value>')
ilat 	<- which(substr(datsgar[,],1,17) == '<LatitudeDegrees>')
ilon	<- which(substr(datsgar[,],1,18) == '<LongitudeDegrees>')
ialt	<- which(substr(datsgar[,],1,16) == '<AltitudeMeters>')
idist	<- which(substr(datsgar[,],1,16) == '<DistanceMeters>')


# take all indices together, sort them and extract rows
ind 	<- c(ilap,itime,ihrbpm,ilat,ilon,ialt,idist)
index 	<- sort(ind)
rows 	<- datsgar[index,]
Nrows	<- length(rows)

# create data structure with N time points, preset with NA
N	 <- length(itime)
d0   <- cbind(seq(1,N,1))
dats <- data.frame(cbind(d0,d0,d0,d0,d0,d0,d0,d0,d0,d0,d0,d0,d0,d0,d0))
dats[,] = NA
colnames(dats) <- c('lap','time','UNIXtime','hrbpm','hrbpmx','lat','latx','lon','lonx','alt','dist','dist_interpol','p2p_dist','tot_dist','speed')

# function to cut up strings between < >
strcut <- function(txt)
{		tx1 <- unlist(strsplit(txt,    ">", fixed = TRUE))
		tx2 <- unlist(strsplit(tx1[2], "<", fixed = TRUE))
		txt <- tx2[1]
}

# function to cut up time strings to POSIX format
timecut <- function(txt)
{		tx1 <- unlist(strsplit(txt, "Z", fixed = TRUE))
		tx2 <- unlist(strsplit(tx1, "T", fixed = TRUE))
		txt <- paste(tx2[1],tx2[2])
}

# fill data structure dats successively -----------------------------------

cnt=0
flag=0

for(i in 1:Nrows){

	txt <- as.character(rows[i])

	if(substr(txt,1,4) == '<Lap')				# set lap tag with next time
		flag=1

	if(substr(txt,1,4) == '<Tim')				# time is the synchronizer
	{	cnt = cnt+1
		txt1 <- strcut(txt)
		dats$time[cnt] = timecut(txt1)

		if(flag==1)
		{	dats$lap[cnt]  = 1	 	
			flag=0
		}
	}

	if(substr(txt,1,4) == '<Lat')
	 	dats$lat[cnt]  = as.numeric(strcut(txt))

	if(substr(txt,1,4) == '<Lon')
		dats$lon[cnt]  = as.numeric(strcut(txt))

	if(substr(txt,1,4) == '<Alt')
		dats$alt[cnt]  = as.numeric(strcut(txt))

	if(substr(txt,1,4) == '<Dis') 
		dats$dist[cnt] = as.numeric(strcut(txt))
			
	if(substr(txt,1,4) == '<Val')
		dats$hrbpm[cnt] = as.numeric(strcut(txt))

}	

# Interpolate Heartrate, longitude and latitude  --------------------------

dats$hrbpmx <- dats$hrbpm
hr = 0

for(i in 1:N){
	
	if(is.na(dats$hrbpm[i]) == F) 
		hr <- dats$hrbpm[i]
	

	if(is.na(dats$hrbpm[i]) == T)
		dats$hrbpmx[i] <- hr
	}
  
dats$lonx <- dats$lon
long = 0

for (i in 1:N){
  if(is.na(dats$lon[i]) == F) 
  	long <- dats$lon[i]

  if(is.na(dats$lon[i]) == T) 
  	dats$lonx[i] <- long
	}

dats$latx <- dats$lat
lati = 0

for (i in 1:N){  
  if(is.na(dats$lat[i]) == F) 
	  lati <- dats$lat[i]

  if(is.na(dats$lat[i]) == T) 
	  dats$latx[i] <- lati
  }


# UNIX time creation - using N number of rows in already created data.frame
for (i in 1:N){
                 datetime <- strptime(dats$time[i], "%Y-%m-%d %H:%M:%S")	     
        dats$UNIXtime[i]  <- as.POSIXct(datetime)
}


# Interpolate distance measures in order to create cumulative distance - METERS

#dist

dats$dist_interpol <- dats$dist
dis = 0

for(i in 1:N){
  
	if(is.na(dats$dist[i]) == F) 
		dis <- dats$dist[i]
	
	if(is.na(dats$dist[i]) == T)
		dats$dist_interpol[i] <- dis
	}

#p2p_dist
for (i in 1:N){  
  if(is.na(dats$p2p_dist[i]) == T) 
	  dats$p2p_dist[i] = 0
  }


# CALCULATE DISTANCE ------------------------------------------------------


dats$p2p_dist[1] = 0

for (i in 2:N) {


dats$p2p_dist[i] <- (dats$dist_interpol[i] - dats$dist_interpol[i-1]) 
}



# Total distance = cumulative sum of p2p_dist

dats$tot_dist <- cumsum(dats$p2p_dist)

  

# Calculate speed - first converting distance to km -----------------------

dats$speed[1] = 0
for (i in 2:N) {
	d <-difftime(as.POSIXct(dats$UNIXtime[i],origin="1970-01-01",tz="UTC"),as.POSIXct(dats$UNIXtime[(i-1)],origin="1970-01-01",tz="UTC"),units = "hours")
	dats$speed[i] <- (dats$p2p_dist[i]/1000)/as.numeric(d)
		
	}

# DATASET OUTPUT AS ASCII AND R DATA FRAME --------------------------------


write(t(dats), garmindatsout1, ncolumns=ncol(dats), sep=", ",append=F)
save(dats, file= garmindatsout2)

}
