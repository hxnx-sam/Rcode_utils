# sequence retrieval
# S. J. Lycett
# 16 May 2011

# Rcode_utils is free software: you can redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
# Rcode_utils is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
# See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with Rcode_utils.  
# If not, see <http://www.gnu.org/licenses/>.


# get primary ID number from accession number
query_getID	<- function( accession ) {
	qq 	<- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=nucleotide&term=",accession,sep="")
	lines	<- readLines( qq )
	ii	<- grep("<Id>",lines)
	line2	<- gsub("<Id>","", lines[ii] )
	line2 <- gsub("</Id>","", line2 )
	line2 <- gsub("\t","", line2)
	id	<- as.integer(line2)
	return ( id )
}

# get XML lines from primary ID
query_getSequenceXML <- function( id ) {
	qq	<- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id=",id,"&retmode=xml",sep="")
	lines	<- readLines( qq )
	return ( lines )
}

# get fasta lines from primary ID
query_getSequence <- function( id ) {
	qq	<- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id=",id,"&retmode=fasta",sep="")
	lines	<- readLines( qq )
	return ( lines )
}

# read a list of accession numbers from a file
# get the sequences (fasta format)
# write sequences to file
getSequences	<- function( fname, maxSeqs=100, waitTime=1.0 ) {
	# read in sequences list
	accns		<- readLines( fname )

	newName	<- gsub("\\.txt","_seqs.fas", fname)

	nseqs		<- length(accns)

	if (waitTime < (1/3)) {
		print("Make no more than 3 requests every 1 second.")
	}

	if (nseqs < maxSeqs) {
		for (i in 1:nseqs) {
			id 	<- query_getID( accns[i] )
			lines <- query_getSequence( id )
			write(lines, file=newName, append=(i>1))
			print( lines[1] )
			Sys.sleep(waitTime)
		}

	} else {
		print("Split sequence list to smaller batches (not run)")
		print("Run retrieval scripts on weekends or between 9 pm and 5 am Eastern Time weekdays for any series of more than 100 requests.")
		print("Change maxSeqs to run")
	}

	print("Done")
}