# function to calculate BEAST etc distribution stats
# S. J. Lycett
# 9 April 2011
# 13 May 2011
# 26 July 2011
# 3 August 2011
# 27 Oct 2011 - added HPDvals which just returns mean,median,upper,lower as an array
# 1 June 2012 - ignore NaNs etc for N, S robustCounting

HPD_stats	<- function( data, breaks=2000, hpd=95, plot=FALSE ) {
	mean_val 	<- mean(data)
	median_val	<- median(data)
	harm_val	<- 1/mean(1/data)
	std_val	<- sd(data)

	h		<- hist(data, breaks=breaks, plot=plot)
	xvals		<- h$mids
	pdf		<- h$density/sum(h$density)
	cum_pdf	<- pdf

	if (length(pdf) > 1) {

		for (i in 2:length(pdf)) {
			cum_pdf[i] <- cum_pdf[i-1]+pdf[i]
		}

		is		<- which( cum_pdf <= (1-(hpd/100)) )
		if (length(is) == 0) {
			is	<- 1
		}

		ie		<- array(0,length(is))
		for (i in 1:length(is)) {
			ie[i] <- which( ((cum_pdf-cum_pdf[is[i]]) >= (hpd/100)) )[1]
		}
		if ( (length(ie) == 1) & ( !is.finite(ie[1]) ) ) {
			ie	<- length(cum_pdf)
		}
	
		# is -> ie are the sets of values for which 95% lie
		# now choose the minimum width
		diff	<- ie-is
		dd	<- which.min(diff)
		lower_hpd	<- xvals[is[dd]]
		upper_hpd	<- xvals[ie[dd]]

		# old
		##ll <- (1-(hpd/100))/2
		##uu <- 1-ll
		#ll  <- 1-(hpd/100)
		#uu  <- (hpd/100)

		#is		<- which(cum_pdf >= ll)[1]
		#ie		<- which(cum_pdf >= uu)[1]

		#lower_hpd	<- xvals[is]
		#upper_hpd	<- xvals[ie]

	} else {
		lower_hpd	<- min(data)
		upper_hpd	<- max(data)
	}

	return( list(	mean=mean_val, median=median_val, harmonic=harm_val, sd=std_val,
				lower_hpd=lower_hpd, upper_hpd=upper_hpd, hpd=hpd, breaks=breaks) )
}

HPDvals	<- function( orig_data, breaks=2000, hpd=95 ) {

	# 1 June 2012 - for N, S robustCounting
	data <- orig_data[ which(is.finite(orig_data)) ]

	vals <- array(0, 4)
	rownames(vals) <- c("Mean","Median","Upper","Lower")

	if ( all(is.finite(data)) & !all(data==0) ) {
		vals[1] <- mean(data)
		vals[2] <- median(data)

	h		<- hist(data, breaks=breaks, plot=FALSE)
	xvals		<- h$mids
	pdf		<- h$density/sum(h$density)
	cum_pdf	<- pdf

	if (length(pdf) > 1) {

		for (i in 2:length(pdf)) {
			cum_pdf[i] <- cum_pdf[i-1]+pdf[i]
		}

		is		<- which( cum_pdf <= (1-(hpd/100)) )
		if (length(is) == 0) {
			is	<- 1
		}

		ie		<- array(0,length(is))
		for (i in 1:length(is)) {
			ie[i] <- which( ((cum_pdf-cum_pdf[is[i]]) >= (hpd/100)) )[1]
		}
		if ( (length(ie) == 1) & ( !is.finite(ie[1]) ) ) {
			ie	<- length(cum_pdf)
		}
	
		# is -> ie are the sets of values for which 95% lie
		# now choose the minimum width
		diff	<- ie-is
		dd	<- which.min(diff)
		lower_hpd	<- xvals[is[dd]]
		upper_hpd	<- xvals[ie[dd]]

	} else {
		lower_hpd	<- min(data)
		upper_hpd	<- max(data)
	}

		vals[3] <- upper_hpd
		vals[4] <- lower_hpd

	}
	
	return (vals)
}


