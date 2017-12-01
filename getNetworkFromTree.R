# function to extract a network from a tree (e.g. for HIV clusters)
# S. J. Lycett
# 4 July 09
# 30 Sept 09 - add function to read trees from file (e.g. BEAST file) and make weighted network
# 28 Sept 10 - added append to top line of getNetworkFromTree

require(ape)

getNetworkFromTree	<- function( tree, clusterThres, fname="x", append=TRUE ) {

	# matrix of most recent common ancestors 
	ancs		<- mrca(tree)

	# tree distance between all nodes in tree (branchlengths)
	dn		<- dist.nodes(tree)

	N		<- length( tree$tip )

	# distance to ancestor (this is an internal node)
	dist_to_ancs<- matrix(0, N, N)
	for (i in 1 : N ) {
		for (j in 1 : N ) {
			if (i != j) {
				dist_to_ancs[i,j] = dn[i, ancs[i,j]]
			}
		}
	}

	# link between nodes if both distances to most recent common ancestor
	# is less than threshold
	D		<- matrix(0, N, N)
	rownames(D) <- tree$tip.label
	colnames(D) <- tree$tip.label
	for (i in 1:(N-1) ) {
		for (j in (i+1):N ) {
			if ((dist_to_ancs[i,j] <= clusterThres) & (dist_to_ancs[j,i] <= clusterThres)) {
				D[i,j]=1
				D[j,i]=1

				if (fname != "x") {
					line = paste(tree$tip[i],tree$tip[j],1,sep=",")
					write(line, file=fname, append=append)
				}

			}
		}
	}

	

	return ( D )

}

# 30 Sept 09
getWeightedNetwork	<- function( treeList, clusterThres=(4*365+366) ) {
	ntrees <- length(treeList)

	# do first tree first and assume all numbers of taxa are the same
	D	 <- getNetworkFromTree( treeList[[1]], clusterThres )
	D2	 <- D*D

	# define order of taxa from first tree
	taxaNames <- rownames(D)

	if (ntrees > 1) {
		for (t in 2:ntrees) {
			tempD		<- getNetworkFromTree( treeList[[t]], clusterThres )

			# reorder this matrix to match matrix from first tree
			newTaxa 	<- rownames(tempD)
			inds		<- match(taxaNames, newTaxa)
			tempD		<- tempD[inds,inds]

			D	<- D + tempD
			D2	<- D2 + (tempD*tempD)
		}
	}
	
	# mean
	D <- D/ntrees

	# dev
	D2<- D2/ntrees
	D2<- sqrt( (D2 - (D*D))*(ntrees/(ntrees-1)) )

	return( list(D=D, dev=D2, taxaNames=taxaNames, ntrees=ntrees) )
}


