
# synthetic correlation between assets

setwd(paste0(Sys.getenv('CS_HOME'),'/FinancialNetwork/SyntheticAsset/Models'))


# tests asset viz
eurusd <- read.csv('data/EURUSD_201511.csv')
eurgbp <- read.csv('data/EURGBP_201511.csv')

# package smoother
library(smoother)

# midmarket signal , 4hours
s = (eurusd[1:14400,2]+eurusd[1:14400,3])/2
x = log(s/s[1])

plot(x[1801:12600],type='l',main='EUR/USD, 1st November 2015',ylab='log(s/s0)',xlab='time (s)')
#filtering test
#f1 = smth.gaussian(x,window=1800)
#f2=smth.gaussian(x,window=450)
points(f1[1801:12600],col='red',type='l',add=TRUE)
points(f2[1801:12600],col='purple',type='l',add=TRUE)



# corr synth data -> correlation between log-returns at a given scale.
dx = diff(x)

