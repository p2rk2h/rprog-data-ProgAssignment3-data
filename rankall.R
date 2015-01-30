rankall <- function( outcome , num = 'best' ) {
	## Check first for valid outcome
	if ( ! outcome %in% c( "heart attack" , "heart failure" , "pneumonia" ) ) {
		stop( 'invalid outcome' )
	}
	index <- ifelse( outcome == "heart attack" , 11 , ifelse( outcome == "heart failure" , 17 , 23 ) )	# else pneumonia

	## Read outcome data
	data <- read.csv( 'outcome-of-care-measures.csv' , colClasses = 'character' )	# colClasses to output in character instead of <NA>
	data[ , index ] <- suppressWarnings( as.numeric( data[ , index ] ) )	# suppressWarnings 'NAs introduced by coercion'
	data <- na.omit( data )
	dataSrt <- data[ order( data[ , index ] , data[ , 2 ] , na.last = NA ) , ]

	## For each state, find the hospital of the given rank
	states <- sort( unique( dataSrt[ , 7 ] ) )

	## Return a data frame with the hospital names and the (abbreviated) state name
	num <- ifelse( num == "best" , 1 , ifelse( num == "worst" , - 1 , as.numeric( num ) ) ) ;
	df <- ldply( lapply( states , function( state ) {
		subSrt <- subset( dataSrt , State == state ) ;
		names( subSrt )[ index ] <- outcome ;
		stNum <- ifelse( num < 0 , length( subSrt[ , 'Hospital.Name' ] ) , num ) ;
		subSrt[ stNum , 7 ] <- state ;	# or subSrt[ stNum , ]$State
		subSrt[ stNum , c( 2 , 7 , index ) ] } ) , data.frame )
	rownames( df ) <- states	# replace index by states
	names( df )[ 1:2 ] <- c( 'hospital' , 'state' )	# partial colname change

	# return the ascendingly alpha-sorted state data
	df[ , 1:2 ]
}
