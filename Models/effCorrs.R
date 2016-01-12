

######
# computation of effective correlations with confidence intervals, for different scales


setwd(paste0(Sys.getenv('CS_HOME'),'/FinancialNetwork/SyntheticAsset/Models'))
source('functions.R')

month="201506"
s1=read.csv(paste0('data/filtered/EURGBP_',month,'_600.csv'),header=FALSE)[,2];x1=log(s1/s1[1])
s2=read.csv(paste0('data/filtered/EURUSD_',month,'_600.csv'),header=FALSE)[,2];x2=log(s2/s2[1])

omega0 = 144
filtering=3

plot(diff(gaussianFilter(x1,omega0)),type='l');points(diff(gaussianFilter(x2,omega0)),type='l',col='red')
cor(diff(gaussianFilter(x1,omega0)),diff( gaussianFilter(x2,omega0)))
cor(diff(x1[(2*omega0+1):(length(x1)-2*omega0)]-gaussianFilter(x1,omega0)),diff(x2[(2*omega0+1):(length(x2)-2*omega0)]-gaussianFilter(x2,omega0)))

s=synthAssets(x1,x2,correlations[3],omega0*3/filtering)
cor(diff(s$xs1),diff(s$xs2))
plot(gaussianFilter(x1,omega0),type='l');
points(s$xs1,type='l',col='red')
points(x1[(2*omega0+1):(length(x1)-2*omega0)],type='l',col='purple')

###

plot(x1,type='l')
sd(diff(x1))
sd(diff(x2))
cor(diff(x1),diff(x2))
cov(diff(x1),diff(x2))
x10 = gaussianFilter(x1,omega0);x10=x10-mean(diff(x10));x1r = x1[(2*omega0+1):(length(x1)-2*omega0)]-mean(diff(x10));
x20 = gaussianFilter(x2,omega0);x20=x20-mean(diff(x20));x2r = x2[(2*omega0+1):(length(x1)-2*omega0)]-mean(diff(x20));
plot(x1r,type='l');points(x10,type='l',col='red')
plot(x1r - x10,type='l')

(sd(diff(x10))/sd(diff(x1)))^2

y1 = x1r - x10
y2 = x2r - x20
cor(diff(y1),diff(y2))

abs(mean(diff(x10))/max(diff(x10)))
abs(mean(diff(y1))/max(diff(y1)))

###

filterings = c(3,6,12)
correlations = seq(from=0,to=0.95,by=0.05)
bootstrap_num = 20


effcorrs = c();effcorrsmin=c();effcorrsmax=c()
filt=c();expcorr=c()

for(f in 1:length(filterings)){
  filtering=filterings[f]
  
  x1 = sample(gaussianFilter(x1,filtering),filtering);x2 = sample(gaussianFilter(x2,filtering),filtering)
  
  synthlength = length(x1)-(4*omega0*3/filtering)
  synth1 = matrix(0,bootstrap_num*synthlength,length(correlations))
  synth2 = matrix(0,bootstrap_num*synthlength,length(correlations))
  for(i in 1:length(correlations)){
    for(k in 0:(bootstrap_num-1)){
       s=synthAssets(x1,x2,correlations[i],omega0*3/filtering);
       synth1[k*synthlength+(1:synthlength),i]=s$xs1
       synth2[k*synthlength+(1:synthlength),i]=s$xs2
    }
  }
  for(j in 1:length(correlations)){
    ctest = cor.test(diff(synth1[,j]),diff(synth2[,j]));
    show(cor(diff(synth1[,j]),diff(synth2[,j])))
    effcorrs=append(effcorrs,ctest$estimate);effcorrsmin=append(effcorrsmin,ctest$conf.int[1]);effcorrsmax=append(effcorrsmax,ctest$conf.int[2])
    filt=append(filt,filtering);expcorr=append(expcorr,correlations[j])
  }
  
}

g=ggplot(data.frame(rho=expcorr,rho_eff=effcorrs,rhomin=effcorrsmin,rhomax=effcorrsmax,group=filt,col=filt),aes(x=rho,y=rho_eff,group=group))
g+geom_point(col=filt)+geom_line(col=filt)+geom_errorbar(aes(ymin=rhomin,ymax=rhomax))



