# Bruce Test
# S. J. Lycett
# 18 Oct 2012

# Rcode_utils is free software: you can redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
# Rcode_utils is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
# See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with Rcode_utils.  
# If not, see <http://www.gnu.org/licenses/>.


# test to see whether one distribution is higher or lower than another
# take npairs random pairings
# count the number higher & lower
bruceTest <- function( data1, data2, npairs=length(data1) ) {

	#if (type=="random") {
	data1_r 	<- sample(data1, npairs, replace=( npairs > length(data1) ) )
	data2_r 	<- sample(data2, npairs, replace=( npairs > length(data2) ) )
	numHigher 	<- length(which(data1_r > data2_r))
	numLower	<- length(which(data2_r > data1_r))
	numDiffs	<- min( c(numHigher,numLower) )
	#}

	pval		<- numDiffs/npairs
	return( pval )
}


####################################################################################
# test the bruce test
doExample <- TRUE
if (doExample) {

	op <- par(mfrow=c(3,1))

	npts  	= 1000
	npairs 	= 1000
	dev1  	= 1
	dev2  	= 1
	mean1 	= 0
	mean2 	= seq(mean1, (mean1+(2*dev1)+(2*dev2)), 0.1)
	pvals		= array(0, length(mean2))
	for (i in 1:length(mean2)) {
		data1 = rnorm(npts, mean=mean1, sd=dev1)
		data2 = rnorm(npts, mean=mean2[i], sd=dev2)
		pvals[i] <- bruceTest(data1,data2,npairs=npairs)
	}

	plot(mean2-mean1, pvals, ylim=c(0,0.6),
		xlab="Difference in Mean", ylab="Pval", main="Bruce Test for Normals, Stdev1=1, Stdev2=1")


	mean2		= 0
	dev2		= seq(1, 3, 0.1)
	pvals2	= array(0, length(dev2))

	for (i in 1:length(dev2)) {
		data1 = rnorm(npts, mean=mean1, sd=dev1)
		data2 = rnorm(npts, mean=mean2, sd=dev2[i])
		pvals2[i] <- bruceTest(data1,data2,npairs=npairs)
	}

	plot(dev2, pvals2, ylim=c(0,0.6),
		xlab="Stdev 2", ylab="Pval", main="Bruce Test for Normals, Mean1=0, Stdev1=1, Mean2=0")

	mean2		= 2
	dev2		= seq(1, 3, 0.1)
	pvals3	= array(0, length(dev2))

	for (i in 1:length(dev2)) {
		data1 = rnorm(npts, mean=mean1, sd=dev1)
		data2 = rnorm(npts, mean=mean2, sd=dev2[i])
		pvals3[i] <- bruceTest(data1,data2,npairs=npairs)
	}

	plot(dev2, pvals3, ylim=c(0,0.6),
		xlab="Stdev 2", ylab="Pval", main="Bruce Test for Normals, Mean1=0, Stdev1=1, Mean2=2")



	par(op)

}

