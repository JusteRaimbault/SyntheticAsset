
setwd(paste0(Sys.getenv('CN_HOME'),'/Results/Synthetic/Network'))

# clean and filter data for each month

formatDates <- function(d){as.numeric(format(as.POSIXct(substr(d,1,15),format="%Y%m%d %H%M%S",tz="UTC"),format="%s"))}

gaussianFilter <- function(x,sigma){
  # kernel : cut at +- 2 sigma
  k = exp(- (((-2*sigma):(2*sigma))^2) / (sigma/2)^2)
  k = k/sum(k)
  return(convolve(x,k,type="filter"))
}


extractFromSupport<-function(reducedsupport,support,ts){
  res=c()
  # start is position of first of reduced support in support
  start = which(support==reducedsupport[1])
  end =  which(support==reducedsupport[length(reducedsupport)])
  
  k=start
  while(k <= end){
    
  }
  
  return(res)
}


cleanData <- function(month,assets){
  # load the data
  d = list()
  for(asset in assets){d[[asset]]=read.csv(paste0('data/raw/',asset,'_',month,'.csv'),header=FALSE)}
  # convert times
  for(asset in assets){d[[asset]][,1]=sapply(d[[asset]][,1],formatDates)}
  # compute mids
  for(asset in assets){d[[asset]]$mids=(d[[asset]][,2]+d[[asset]][,3])/2}
  # adjust TS
  support = d[[asset[1]]][,1]
  for(i in 2:length(asset)){support=intersect(support,d[[asset[i]]][,1])}
  
  
  
}






assets = c("EURUSD","EURGBP")
months = paste0("2015",c("06","07","08","09","10","11"))





