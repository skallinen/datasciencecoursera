best <- function(state, outcome) {
        ## Experimenting with style for readability
        ## Read outcome data
        setwd("~/datasciencecoursera/ProgrammingAssignment3")                                   ## set working directory

        data       <- read.csv("outcome-of-care-measures.csv", colClasses = "character")        ## load data

        ## Check that state and outcome are valid
        col        <- c( 11, 17, 23 )                                                           ## specify where death rates data is
        names(col) <- c( "heart attack", "heart failure", "pneumonia" )                         ## name values in vector
        data       <- data[ data[[ 7 ]] == state,  ]                                            ## filter data according to selected state

        if( length( col[ names( col ) == outcome] )     == 0 )  stop("invalid outcome")         ## check if entered outcome is valid
        if( length( data[[ 7 ]] )                       == 0 )  stop("invalid state")           ## check if entered state is valid
        
        ## Return hospital name in that state with lowest 30-day death rate
        data                        <- data[data[  , col[[ outcome ]] ] != "Not Available",  ]  ## filter out NAs
        data[  , col[[ outcome ]] ] <- as.numeric( data[  , col[[ outcome ]] ] )                ## format death rate data as numeric
        smallestDeathRate           <- min ( data[  , col[[ outcome ]] ] )                      ## calculate smallest deathrate
        data[ data[  , col[[ outcome ] ] ] == smallestDeathRate, 2 ]                            ## return hospital      
}