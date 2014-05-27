# script to generate bird flu maps
# S. J. Lycett
# 12 Nov 2012

# VERY IMPORTANT - THE PATHS ARE HARD CODED FOR MY COMPUTER - if you use this script CHANGE THE PATHS #

source("C://Users//Samantha Lycett//Documents//Rcode//getEl.R")
source("C://Users//Samantha Lycett//Documents//Rcode//birdSpecies_and_continents.R")
library(maps)

####################################################################
# load sequence names from NCBI Influenza Virus resource

# 3504 complete genome sets on 12 Nov 2012
# 5210 HA full length on 12 Nov 2012
# this has fasta header
# >{serotype}_{host}_{accession}_{strain}_{country}_{year}/{month}/{day}_{segname}

# THIS IS HARD CODED FOR MY COMPUTER - if you use it CHANGE THE PATHS #
path  <- "C://data//h5n1//hxnx_nov_2012//"
name  <- "hxnx_avian_ha_nucls_names"
taxa  <- as.matrix(readLines(paste(path,name,".txt",sep="")))

# just process the HA segments ( e.g. if you had loaded hxnx_avian_nucls_names.txt instead )
seg	  <- apply(taxa, 1, getEl, ind=1, sep="_", fromEnd=TRUE)
ginds	  <- which(seg=="4 (HA)")
ngenomes<- length(ginds)

taxa	  <- as.matrix(taxa[ginds])

subtype <- apply(taxa, 1, getEl, ind=1, sep="_")
host    <- apply(taxa, 1, getEl, ind=2, sep="_")	# should all be Avian
accn    <- apply(taxa, 1, getEl, ind=3, sep="_")
isName  <- apply(taxa, 1, getEl, ind=4, sep="_")
country <- apply(taxa, 1, getEl, ind=3, sep="_", fromEnd=TRUE)
date    <- apply(taxa, 1, getEl, ind=2, sep="_", fromEnd=TRUE)
year	  <- as.integer(apply(as.matrix(date), 1, getEl, ind=1, sep="/"))
month	  <- as.integer(apply(as.matrix(date), 1, getEl, ind=2, sep="/"))
day	  <- as.integer(apply(as.matrix(date), 1, getEl, ind=3, sep="/"))

ucountries <- sort(unique(country))

#################################################################
# Map drawing

# get map values for all countries
mapvals <- map("world", xlim=c(-180,180),ylim=c(-70,80), fill=TRUE, value=TRUE, plot=FALSE)

# get country names for map regions
mapvals$countries <- apply(as.matrix(mapvals$names), 1, getEl, ind=1, sep=":")

#################################################################
# make correspondence between isolate country names and those in mapvals

mapCountries	<- array("-", length(ucountries))
for (i in 1:length(ucountries)) {
	jj			<- grep(ucountries[i], mapvals$names)
	if (length(jj) > 0) {
		mapCountries[i] 	<- unique(mapvals$countries[jj])[1]
	}
}

# manual corrections
	i 		 	<- which(ucountries=="Cote dIvoire")
	mapCountries[i] 	<- "Ivory Coast"
	
	i			<- grep("Czech", ucountries)
	mapCountries[i]	<- mapvals$countries[grep("Czech", mapvals$countries)]

	i			<- which(ucountries=="Slovenia")
	mapCountries[i]	<- mapvals$names[grep("slov",mapvals$names)]

	i			<- which(ucountries=="Croatia")
	mapCountries[i]	<- mapvals$names[grep("slov",mapvals$names)]

	i			<- which(ucountries=="Russia")
	mapCountries[i]	<- "USSR"

	i			<- which(ucountries=="Ukraine")
	mapCountries[i]	<- "USSR"

	i			<- which(ucountries=="United Kingdom")
	mapCountries[i]	<- "UK"

	i			<- which(ucountries=="Viet Nam")
	mapCountries[i]	<- "Vietnam"

	# HA only
	i			<- which(ucountries=="Kazakhstan")
	mapCountries[i]	<- "USSR"

	i			<- which(ucountries=="Turkmenistan")
	mapCountries[i]	<- "USSR"

	i			<- which(ucountries=="Middle East")
	mapCountries[i]	<- "United Arab Emirates"

	i			<- which(ucountries=="Bosnia and Herzegovina")
	mapCountries[i]	<- mapvals$names[grep("slov",mapvals$names)]

	# check
	ucountries[which(mapCountries=="-")]
	match(mapCountries, mapvals$countries)


## examples - not used now
## replot map
#map(mapvals, fill=TRUE, col=array("grey",length(mapvals$names)))

## plot map with China in red
#cinds		<- grep("China", mapvals$names)
#cols		<- array("grey", length(mapvals$names))
#cols[cinds] <- "red"
#map(mapvals, fill=TRUE, col=cols)

##########################################################################
# plot the countries where there is H5N1

########################
# MAPS for ALL BIRDS
########################

	st		<- c("H5N1","H9N2","H6N1")
	mcountries	<- mapCountries[ match(country, ucountries) ]

for (s in 1:length(st)) {
	sinds 	<- which(subtype==st[s])
	stable	<- table(mcountries[sinds])
	scountries 	<- rownames(stable)

	cols		<- array("grey",length(mapvals$names))
	shades	<- heat.colors(6)
	#shades	<- c("red","orange","yellow","green")	#heat.colors(4)		# red, orange, yellow, white
	bounds	<- c(200,100,50,10,1)		
	for (i in 1:length(scountries) ) {
		mapCo 	<- scountries[i]
		numSeqs 	<- stable[i]
		kk		<- which(bounds <= numSeqs)[1]
		jj		<- grep(mapCo, mapvals$countries)
		
		if ( all( cols[jj] == "grey" ) ) {
			cols[jj] 	<- shades[kk]
		}
	}

	h = 25*(80+50)
	w = 25*(190+30)
	png( file=paste(path,"avian_influenza_map_",st[s],".png",sep=""), height=h, width=w, res=600)
	map(mapvals, fill=TRUE, col=cols, xlim=c(-30,190), ylim=c(-50,80))
	legend("bottomleft",paste(bounds[1:5],"+",sep=""),pch=22,col=shades[1:5],pt.bg=shades[1:5],bty="n")
	title(paste("Avian Influenza",st[s]))
	dev.off()
}

########################
# MAPS for poultry only
########################
specificHost 	<- apply(as.matrix(isName), 1, getEl, ind=2, sep="/")
birdOrder		<- birdClass(specificHost)

for (s in 1:length(st)) {
	sinds 	<- which( (subtype==st[s]) & (birdOrder=="gal") )
	stable	<- table(mcountries[sinds])
	scountries 	<- rownames(stable)

	cols		<- array("grey",length(mapvals$names))
	shades	<- heat.colors(6)
	#shades	<- c("red","orange","yellow","green")	#heat.colors(4)		# red, orange, yellow, white
	bounds	<- c(200,100,50,10,1)		
	for (i in 1:length(scountries) ) {
		mapCo 	<- scountries[i]
		numSeqs 	<- stable[i]
		kk		<- which(bounds <= numSeqs)[1]
		jj		<- grep(mapCo, mapvals$countries)
		
		if ( all( cols[jj] == "grey" ) ) {
			cols[jj] 	<- shades[kk]
		}
	}

	h = 25*(80+50)
	w = 25*(190+30)
	png( file=paste(path,"avian_galliformes_influenza_map_",st[s],".png",sep=""), height=h, width=w, res=600)
	map(mapvals, fill=TRUE, col=cols, xlim=c(-30,190), ylim=c(-50,80))
	legend("bottomleft",paste(bounds[1:5],"+",sep=""),pch=22,col=shades[1:5],pt.bg=shades[1:5],bty="n")
	title(paste("Avian Influenza (Galliformes)",st[s]))
	dev.off()
}
