complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Function returns a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases 
        
        ## checking that entered index is legal
        id<-id[id < 333 & id > 0]
        ## creating a vector with correctly formatted filenames
        filenames <- sprintf("/%03d.csv", id)
        ## combining path to filenames
        filenames <- paste(directory, filenames, sep='')
        ## loading files
        loadedfiles <- lapply(filenames, read.csv, head=TRUE)
        df <- ldply(loadedfiles)
        df <- na.omit(df)
        ## counting number of observations per id
        df <- table(df$ID)
        df <- data.frame(df)
        sorteddf <- NULL
        ## checking that data exists for requested index...
        if(ncol(df)==2){
                ## ...if yes, then continue naming columns so easier to work with
                colnames(df) <-c("ID", "nobs")
                ## initializing data frame for sorting values according to requested id
                sorteddf <- data.frame(id = numeric(length(id)), nobs = numeric(length(id)))
                ## starting sorting woth a for loop
                for(i in seq(length(id))){
                        ## capturing number of observations from initial data frame
                        value <- df[df$ID == id[i], 2]
                        ## ignoring if nobs value is zero
                        if(length(value)!=0){
                                ## saving values from original data frame to sorted data frame
                                sorteddf[i, 1] <- id[i]
                                sorteddf[i, 2] <- value
                        }
                }
                # filtering out records with values that are zero 
                sorteddf<-sorteddf[sorteddf$nobs > 0, ]
                # saving in a new data frame to get row numbering clean
                finaldf<-data.frame(id=factor(sorteddf[ ,1]), nobs=sorteddf[ ,2])
                # outputting the result
                finaldf              
        }else{
                # if data does not exist for requested index then...
                # return empty data frame
                output <- data.frame(id = NA, nobs = NA)
                # outputting the result as an empty dataframe
                na.omit(output)
        }
}
