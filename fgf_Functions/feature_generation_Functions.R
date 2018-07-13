require(drc)

#require(MESS)

fgf_meanSlopes <- function(data = data , pattern){
  
  #extract all concentrations for the specified pattern
  conc <- grep(paste0(pattern , ".*nM") , names(data) , value = T)
  
  #pattern is removed in a first step and afterwards nM and the remaining vector which contains only numerics is forced to 0
  conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = conc)))
  
  #the numeric vector is ordered increasing
  conc <- conc[order(conc)]
  
  #the pattern and unit are pasted again to the vector
  conc <- paste0(pattern , conc , "nM")
  
  #extract all concentrations for the specified pattern
  sort_conc <- grep(paste0(pattern , ".*nM") , names(data) , value = T)
  
  #pattern is removed in a first step and afterwards nM and the remaining vector which contains only numerics is forced to 0
  sort_conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = sort_conc)))
  
  #the numeric vector is ordered increasing
  sort_conc <- sort_conc[order(sort_conc)]
  
  #the pattern and unit are pasted again to the vector
  sort_conc <- paste0(pattern , sort_conc , "nM")
  
  #Sort the incoming pattern
  sortedPattern <- sort_conc
  
  #Subsetting the inital dataset to a smaller dataset which could be used to calculate the slope features
  tmpData <- data[,sortedPattern]
  
  #generating a vector which contains the conc from the already sorted tmpData
  conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = names(tmpData))))
  
  if(length(conc) < 2){
    
    stop("Error: meanSlopes applied to a pattern with less than 2 concentrations. The resulting value do not contain information. Stopping")
    
  }
  
  #calculating the slopes for the data the return is a matrix of length length(conc) - 1 , The function is performed on the log10(conc)
  slopes <- apply(tmpData , 1 , function(y , conc){
    
    # a tmporary dataframe is generated
    
    tmp <- data.frame(y = y , conc = conc)
    
    #only rows of the dataframe which contain finite values were used
    
    tmp <- tmp[is.finite(y) , ]
    
    # a matrix is initialized were the slopes are to be saved
    Slope <- matrix(nrow = (length(conc)-1) , ncol = (length(conc)-1)  )
    #Only if nrow is bigger than one which means that at least two differnent values are observed the slopes are calculated
    # The slopes are calculated using two for loops
    
    if(nrow(tmp) > 1){
      for(n in 1:(nrow(tmp)-1)){
        
        for(i in (n+1):(nrow(tmp))){
          
          Slope[n,i-1] <- (tmp [i , "y" ] - tmp[n , "y"]) / (tmp[i , "conc"] - tmp[n , "conc"])
          
        }
      }
    }

    return(apply(Slope , 1 , function(x) mean(x , na.rm = T)))
    
  } , conc = log10(conc))
  
  # get the initial length of data important to set the names
  initalLengthData <- length(data)
  
  #if else nessesary because otherwise a it is a problem to distinguish between a vector or matrix as a return value
  
  if(length(sortedPattern) > 2){
    
    data <- cbind(data , t(slopes))
    
  }else{
    data <- cbind(data , slopes)
    
  }
  
  #naming the meanSlopes from 1 to length(conc)-1
  names(data)[(initalLengthData+1):length(names(data))] <- paste0(gsub("[Ii]ntensity." , "" , pattern), "MeanSlopes" , 1:(length(conc)-1))
  
  #return the original dataframe including the new features
  return(data)
  
}


# This is a function which calculates the features rsquare of a fit and returns their parameter

fgf_polynomFit <- function(data , pattern , degree = 1){
  
  
  colnames(data) <- gsub("\\.0nM" , "DMSO" , colnames(data))
  
  #sorting the pattern due to the conc
  conc <- grep(paste0(pattern , ".*nM") , names(data) , value = T)
  
  # get the concentration
  conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = conc)))

  # extract rsquare and a number the parameters except of the intersect a
  rsquare <- apply(data[,grep(paste0(pattern , ".*nM") , names(data))] , 1 , function(x , conc , degree){
    
    # if only two data points are aviable the data the DMSO point is added and the calculation is done on not log data.
    if(length(conc) == 2 & degree == 1){
      
      tmp <- data.frame(conc = c(0 , conc) , response = c(1 , x) )
      
      # if a higher order polynom is fitted and the degree is smaller than the number of observation the model is fitted to the log conc
    }else if(length(conc) > degree + 1){
      
      tmp <- data.frame(conc = log10(conc) , response = x )
      
      # else a warning is droped and it is tried to do the same
    }else{
      
      warning("In fgf_polynomFit: the degree of polynom is bigger, equal to the number of data points a polynominal fit does not make sense")
      tmp <- data.frame(conc = log10(conc) , response = x )
      
    }
    

    
    if(sum(is.finite(tmp$response)) > 2){
      # fitting the model with the given degree; raw = T is important here to re
      mod <- lm(response ~ poly(conc , degree = degree , raw = T) , tmp) 
      
      # generating the summary of the model
      summary <- summary(mod)
      
      rsquare <- summary$r.squared
      #-1 removes the intercept and "Estimate" indicates the column for the coefficients
      coefficients <- summary$coefficients[-1,"Estimate"]
      
      return(c(rsquare , coefficients))
      
      # if not more than two datapoints are observed it returns NA times the number of degree +1
    }else{
      
      return(rep(NA , times = degree+1))
    }
    
  } , conc = conc , degree = degree)
  
  # gets the inital length important for naming afterwards
  initalLength <- length(names(data))
  
  data <- cbind(data , t(rsquare))
  names(data)[initalLength+1] <- paste0(gsub("[Ii]ntensity." , "" , pattern), "rsquare")
  names(data)[(initalLength+2):length(names(data))] <- paste0(gsub("[Ii]ntensity." , "" , pattern), "LM_Slope" )
  
  colnames(data) <- gsub( "DMSO" , "\\.0nM" ,colnames(data))
  
  
  return(data)

}


#function which counts the number of slopes which are bigger than 0

fgf_sumPositiveSlopes <- function(data , pattern){

  #extract all concentrations for the specified pattern
  sort_conc <- grep(paste0(pattern , ".*nM") , names(data) , value = T)
  
  #pattern is removed in a first step and afterwards nM and the remaining vector which contains only numerics is forced to 0
  sort_conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = sort_conc)))
  
  
  #the numeric vector is ordered increasing
  sort_conc <- sort_conc[order(sort_conc)]
  
  #the pattern and unit are pasted again to the vector
  sort_conc <- paste0(pattern , sort_conc , "nM")

  #Sort the incoming pattern
  sortedPattern <- sort_conc
  
  #subsetting the tmpdata and ordering the data
  tmpData <- data[,sortedPattern]
  
  #getting the concentrations
  conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = names(tmpData))))
  
  #calculating the sum of slopes, the 0 point is added to the data
  sumSlopes <- apply(tmpData , 1 , function(y , dose){
    
    # generating a new dataframe with the relevant data
    tmp <- data.frame(y = c(1 , y) , conc = c(0 ,dose))
    
    #using only those values which contain a finite y value
    tmp <- tmp[is.finite(y) , ]
    
    #initializing a vector for the result
    vec <- vector(mode = "numeric")
    
    #calculating the slopes
    for(i in 2:(nrow(tmp))){
      
      vec[i-1] <- (tmp [i , "y" ] - tmp[i-1 , "y"]) / (tmp[i , "conc"] - tmp[i-1 , "conc"])
      
    }
    
    # returning the sum of slopes which are bigger or equal to 0
    return(sum(vec >= 0) )
    
  } , dose = conc)
  
  #combining the calculated data and the old data
  data <- cbind(data , sumSlopes)
  
  #renaming the slopes
  names(data)[length(names(data))] <- paste0(gsub("[Ii]ntensity." , "" , pattern), "SumPositiveSlopes")
  
  return(data)
  
}


#function to calculate the Area under the curve with trapezoiden

fgf_auc <- function(data , pattern){
  
  #getting the concentrations
  conc <- grep(paste0(pattern , ".*nM") , names(data) , value = T)
  conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = conc)))
  
  #ordering the concentrations
  conc <- conc[order(conc)]
  
  #selecting the relevant columns
  columns <- paste0(pattern , conc , "nM")
  
  if(length(conc) > 2){
    
    #logaritmizing the concentrations
    conc <- log10(conc)
    
    #calculating the average distance in log world between the concentrations
    aveDistConc <- mean(diff(conc) , na.rm = T)
    
    #use as new 0 the minimum of the actuall concentration - the average distance 
    conc <- c( min(conc , na.rm = T) - aveDistConc , conc)
    
  }else{
    
    #if less than two concentrations are avialbe use the unlogaritmized concentartion and add 0
    conc <- c( 0 , conc)
    
  }
  
  #generating a new dataframe which contains the sorted columns for concentration as well as the value for 0 or the expected 0
  tmpdata <- cbind(rep(1 , times = nrow(data)) , data[,columns])
  
  auc <- apply(tmpdata , 1 , function(x , conc){
    #If any value is missing return NA
    if(any(!is.finite(x))){
      return(NA)
    }
    
    if(length(conc)>3){
      #filtering the data with a moving average of length 3
      tmpx<- stats::filter( x = x ,filter = rep(1 , 3)/3 )
      
      tmpx <- ifelse(is.na(tmpx) , x , tmpx)
    }else{
      
      tmpx <- x
    }
    
    #use the function auc form the MESS package to calculate the auc
    return(MESS::auc(conc , tmpx))
    
  },conc = conc)
  
  data <- cbind(data , auc)
  
  names(data)[length(names(data))] <- paste0(gsub("[Ii]ntensity." , "" , pattern), "AUC")
  
  return(data)
}

# a function that calculates the slopes of counts e.g. MS MS counts , unique peptieds and so on
fgf_slopeCounts <- function(data , pattern){
  
  #extract all concentrations for the specified pattern
  sort_conc <- grep(paste0(pattern , ".*nM") , names(data) , value = T)
  
  #pattern is removed in a first step and afterwards nM and the remaining vector which contains only numerics is forced to 0
  sort_conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = sort_conc)))
  
  
  #the numeric vector is ordered increasing
  sort_conc <- sort_conc[order(sort_conc)]
  
  #the pattern and unit are pasted again to the vector
  sort_conc <- paste0(pattern , sort_conc , "nM")

  #Sort the incoming pattern
  sortedPattern <- sort_conc
  
  #Check wheater the median of pattern is present or not
  med <- grep(paste0("Median." , pattern ,"DMSO") , names(data))
  
  # if the length of med is one indicating that the median is present it this column is selected else the column without median in front is selected
  if(length(med) == 1){
    tmpData <- data[,c(paste0("Median." , pattern ,"DMSO") , sortedPattern)]
    
  }else{
    tmpData <- data[,c(paste0( pattern ,"DMSO") , sortedPattern)]
    
  }
  
  # a simple diff is calculated without using the exact distance; idea if the data is roughly equal spaced it should be fine, the mean of the slopes is used
  
  slope <- apply(tmpData , 1 , function(x){
    
    return(mean(diff(x) , na.rm = T))
    
  })
  
  data <- cbind(data , slope)
  
  names(data)[length(data)] <- paste0("Slope." , pattern)
  
  return(data)
  
}

# function to calculate the difference from the variance and the minimal observed value, all values which are NA are assigned to 0 because a negative value
# is expected but there are some targets which do not have a error bar.

fgf_diffVar <- function(data , pattern){
  
  # grepping the Variance to the corresponding patterns for DMSO
  var <- grep(paste0("Var." , paste(strsplit(pattern , split = "." , fixed = T)[[1]][-1] , collapse = ".") , ".DMSO") , names(data) , value = T)
  
  # grepping the observed concentrations for the data
  conc <- grep(paste0(pattern , ".*nM") , names(data) , value = T)
  
  # assigning the subset of the data to the new data
  tmpdata <- data[,c(var , conc)]
  
  # Calculating the difference between var-1 + minimum concentrations
  diff <- apply(tmpdata ,1 , function(x , conc){
    
    #at least one concentration have to be pressent otherwise NA is removed
    if(length(na.omit(x[conc])) > 0){
      
      difference <- (x[var]-1) + min( x[conc] , na.rm = T)
      
    }else{
      
      difference <- NA
      
    }
    
    return(difference)
    
  } ,conc = conc)
  
  #assigning all infinite values and NA's to 0
  diff[is.na(diff) | is.infinite(diff)] <- 0
  
  #assigning the former data collumn to 0
  data[var] <- diff
  
  return(data)
  
}


# Normalizes the specified pattern onto the values of DMSO controll
fgf_normalize <- function(data , pattern){
  
  colnames(data) <- gsub("\\.DMSO", ".0nM", colnames(data))
  
  idxDMSO <- grep(paste0(pattern , "0nM") , names(data))
  
  idxPattern <- grep(pattern  , names(data))
  
  normalizedData <- apply(data , 1 , function(x , idxDMSO , idxPattern){
    
    x[idxPattern] <- as.numeric(x[idxPattern])/as.numeric(x[idxDMSO])
    
    return(x[idxPattern])
    
  } , idxDMSO = idxDMSO , idxPattern = idxPattern)
  
  data[ , idxPattern] <- as.numeric(t(normalizedData))
  
  return(data)
  
}

# function that performs a fit of LL.4 model from the drc package

fgf_nonlinearfit <- function(data , pattern){
  
  colnames(data) <- gsub("DMSO" , "0nM" , colnames(data))
  
  #extract all concentrations for the specified pattern
  conc <- grep(paste0(pattern , ".*nM") , names(data) , value = T)
  
  #pattern is removed in a first step and afterwards nM and the remaining vector which contains only numbers is made to numeric
  conc <- as.numeric(gsub("nM" , "" , gsub(pattern , replacement = "" , x = conc)))
  
  result <- apply(data[,grep(pattern , names(data))] , 1 , function(x , conc) {
    
    tmpData <- data.frame(concentration = conc , relativeResponse = x)
    
    fit <- try(drm(relativeResponse ~ concentration ,  data=tmpData, fct=LL.4()) , silent = T)
    
    if(class(fit) != "try-error"){
      coefficien <- fit$coefficients
      
      names(coefficien) <- c("Slope" , "Bottom" , "Top" , "Inflection")
      
      #Calculation of Rsquared
      
      meanobs <- mean(fit$origData$relativeResponse)
      sst <- sum((fit$origData$relativeResponse - meanobs)^2)
      predictobs <- sapply(fit$additionalInformation$origData$dose , function(dose) fit$curve[[1]](dose))
      ssr <- sum((fit$predres[,2])^2)
      
      rsquare <- (1 - ssr/sst)
      
      names(rsquare)<- "rsquare"
      
      res <- c(coefficien , rsquare)
      
    }else{
      
      
      res <- rep(NA , times = 5)
      
    }
    
    return(res)
    
  } , conc = conc)
  
  
  data <- cbind(data , t(result))
  
  names(data)[(ncol(data)-4):ncol(data)] <- c("Slope" , "Bottom" , "Top" , "Inflection" , "rsquare")
  
  return(data)
  
}
