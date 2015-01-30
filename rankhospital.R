rankhospital <- function( state , outcome , num = 'best' ) {
	## Check first for valid outcome
	if ( ! outcome %in% c( "heart attack" , "heart failure" , "pneumonia" ) ) {
		stop( 'invalid outcome' )
	}

	## Read outcome data
	data <- read.csv( 'outcome-of-care-measures.csv' , colClasses = 'character' )	# colClasses to output in character instead of <NA>

	## Check that state is valid
	states <- table(data$State)
	if ( ! state %in% names( states ) ) {
		stop( 'invalid state' )
	}

	## Return hospital name in that state with the given rank with lowest 30-day death rate
	index <- ifelse( outcome == "heart attack" , 11 , ifelse( outcome == "heart failure" , 17 , 23 ) )	# else pneumonia
	data[ , index ] <- suppressWarnings( as.numeric( data[ , index ] ) )	# suppressWarnings 'NAs introduced by coercion'
	data <- na.omit( data )
	subdata <- subset( data , State == state )
	subSrt <- subdata[ order( subdata[ , index ] , subdata[ , 'Hospital.Name' ] , na.last = NA ) , ]
	num <- ifelse( num == "best", 1 , ifelse( num == "worst" , length( subSrt[ , 'Hospital.Name' ] ) , as.numeric( num ) ) )
	# return the ranked element from ascendingly alpha-sorted state data
	subSrt[ , 'Hospital.Name' ][ num ]
}
