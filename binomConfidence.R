# find confidence intervals for binomial distribution estimate of p
# S. J. Lycett
# 7 Sept  09
# 14 Sept 2010 - added smallValueBinomConfidenceCalc for use with e.g. Illumina polymorphism counts

# see also
# http://statpages.org/confint.html#Binomial
# http://www.swogstat.org/stat/public/binomial_conf.htm

# p +- zscore(1-a/2)* sqrt( p_est*(1-p_est)/n )
binomConfidence	<- function( x, N, a=0.05 ) {
	qval 		<- 1-(a/2)
	zscore	<- qnorm(qval)
	p_est		<- x/N
	pp		<- zscore*sqrt(p_est*(1-p_est)/N)
	return( list(p_est=p_est, upper=(p_est+pp), lower=(p_est-pp), pp=pp) )
}

# this is not quite right, conf=0.1 => 95%
binomConfidenceCalc	<- function( x, N, conf=0.1) {
	p     <- (0:(N*100))/(N*100)
	dvals	<- dbinom(x, N, p)
	
	best  <- x/N
	dbest <- dbinom(x, N, best)
	inds  <- which( dvals >= dbest*conf )

	if (length(inds) > 1) {
		lower <- p[inds[1]]
		upper <- p[inds[ length(inds) ]]
		range <- upper-lower
		return ( list(best=best, upper=upper, lower=lower, range=range, dvals=dvals, p=p) )
	} else {
		return ( list(best=best, range=0, upper=best, lower=best, dvals=dvals, p=p) )
	}
	
}


smallValueBinomConfidenceCalc	<- function( x, N, conf=0.95, xfactor=100, Nfactor=10) {

	if (x <= 10) {
		xfactor = 100
		Nfactor = 10
	} else {
		if (x <= 50) {
			xfactor = 10
			Nfactor = 2
		} else {
			xfactor = 3
			Nfactor = 1
		}
	}

	best_p	<- x/N
	possible_p	<- (0:(x*xfactor))/(N*Nfactor)
	possible_p	<- possible_p[ which(possible_p <= 1) ]

	pdf		<- dbinom(x,N,possible_p)
	

}