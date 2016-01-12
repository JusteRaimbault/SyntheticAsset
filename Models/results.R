
# Results of experiments

setwd(paste0(Sys.getenv('CS_HOME'),'/FinancialNetwork/SyntheticAsset/Models'))

res <- read.csv('res/201506_3.csv',sep=";",header=FALSE)
res=t(res)

# rough plot
plot(colMeans(res))
colMeans(res)
apply(res,2,sd)

# 95% CI on means

apply(res,2,function(c){2*1.96*sd(c)/sqrt(length(c))})
