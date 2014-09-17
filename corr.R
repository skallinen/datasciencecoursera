corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        df <- complete(directory)
        df <- df[df$nobs > threshold, ]
        if (nrow(df) == 0) {
                numeric()
        } else {
                ## creating a vector with correctly formatted filenames
                filenames <- sprintf("/%03d.csv", 1:332)
                ## combining path to filenames
                filenames <- paste(directory, filenames, sep='')
                ## loading files
                loadedfiles <- lapply(filenames, read.csv, head=TRUE)
                ## format data frame
                all.records <- ldply(loadedfiles)
                all.records <- data.frame(all.records$sulfate, all.records$nitrate, all.records$ID)
                ## remove NA's
                all.records <- all.records[complete.cases(all.records), ]
                ## crosscheck and filter with id's from complete function
                all.records <- subset(all.records, all.records[,3] %in% df[ ,1])
                ## split data according to id's
                s <- split(all.records[ ,1:2], all.records[ ,3])
                ## do the correlation function
                values <-sapply(s, function(x) cor(x[ ,1], x[ ,2]))
                ## tidy and return the result
                unname(values)
        }
}

## correct test results:

## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589su

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000

## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589