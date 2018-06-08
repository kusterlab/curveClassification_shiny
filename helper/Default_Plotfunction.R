plot_default <- function(data , pattern = "M$"){
  
  par(mar = c(9.1,4.1,1,2.1))
  
  cols<- grep(pattern , colnames(data))
  
  isNumeric <- apply(data[,cols], 2 , function(x) return(class(x) == "numeric"))
  
  cols <- cols[isNumeric]
  

  barplot(unlist(data[,cols]) , las = 2 , cex.names = 0.75)
  
  par(mar =c(5.1,4.1,4.1,2.1))
  
}