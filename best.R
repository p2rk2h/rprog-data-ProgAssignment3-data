best <- function( state , outcome ) {
	## Read outcome data
	data <- read.csv( 'outcome-of-care-measures.csv' , colClasses = 'character' )	# colClasses to output in character instead of <NA>

	## Check that state and outcome are valid
	states <- table(data$State)
	if ( ! state %in% names( states ) ) {
		stop( 'invalid state' )
	}
	if ( ! outcome %in% c( "heart attack" , "heart failure" , "pneumonia" ) ) {
		stop( 'invalid outcome' )
	}

	## Return hospital name in that state with lowest 30-day death rate
	index <- ifelse( outcome == "heart attack" , 11 , ifelse( outcome == "heart failure" , 17 , 23 ) )	# else pneumonia
	data[ , index ] <- suppressWarnings( as.numeric( data[ , index ] ) )	# suppressWarnings 'NAs introduced by coercion'
	data <- na.omit( data )
	subdata <- subset( data , State == state )
	# return the first element of ascendingly sorted state data
	subdata[ order( subdata[ , index ] , na.last = TRUE )[ 1 ] , 'Hospital.Name' ]
}
