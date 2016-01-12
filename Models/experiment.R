

#########
## Experiment
#########

resdir = 'res/'
assets = c("EURUSD","EURGBP")
months = paste0("2015",c("06","07","08","09","10","11"))
#months=c("201506")
filterings = c(3,6,12)
#filterings=c(12)
correlations = seq(from=0,to=0.95,by=0.05)
omega0 = 72

source('functions.R')

library(doParallel)
cl <- makeCluster(20)
registerDoParallel(cl)



for(month in months){
  for(filtering in filterings){
    # load data
    s1=read.csv(paste0('data/filtered/EURGBP_',month,'_600.csv'),header=FALSE)[,2];x1=log(s1/s1[1])
    s2=read.csv(paste0('data/filtered/EURUSD_',month,'_600.csv'),header=FALSE)[,2];x2=log(s2/s2[1])
    # refilter and resample
    x1 = sample(gaussianFilter(x1,filtering),filtering);x2 = sample(gaussianFilter(x2,filtering),filtering)
    # generate synthetic data for each level of correlation
    synthlength = length(x1)-(4*omega0*3/filtering)
    synth = matrix(0,2*synthlength,length(correlations))
    for(i in 1:length(correlations)){
      s=synthAssets(x1,x2,correlations[i],omega0*3/filtering);
      synth[,i]=c(s[,1],s[,2])
    }
    for(j in 1:length(correlations)){x1s=synth[1:synthlength,j];x2s=synth[(synthlength+1):(2*synthlength),j];show(cor(diff(x1s),diff(x2s)))}
    
    # compute perfs in //
    res <- foreach(j=1:ncol(synth)) %dopar% {
      source('functions.R');source('models.R')
      x1s=synth[1:synthlength,j];x2s=synth[(synthlength+1):(2*synthlength),j];
      m=predictionMSE(x1s,x2s,144*3/filtering,2)
      error=(m$expected-m$pred)^2
      res=c(error[,1],error[,2])
      res
    }
    
    # get results into data frame
    vals_mat = matrix(0,length(res),length(res[[1]]))
    for(a in 1:length(res)){vals_mat[a,]=res[[a]]}
    v = data.frame(vals_mat);
    
    write.table(v,file=paste0(resdir,month,"_",filtering,".csv"),sep=";",row.names=FALSE,col.names=FALSE)
    
  }
}


stopCluster(cl)
