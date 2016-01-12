

# functions





gaussianFilter <- function(x,sigma){
  k = exp(- (((-2*sigma):(2*sigma))^2) / (sigma/2)^2)
  k = k/sum(k)
  return(convolve(x,k,type="filter"))
}

sample <- function(x,sigma){
  s = seq(from=1,to=length(x),by=floor(sigma/3))
  return(x[s])
}

brownian <- function(sigma,steps){
  cumsum(rnorm(steps,0,sigma))
}

synthAssets<- function(x1,x2,rho,omega0){
  x10 = gaussianFilter(x1,omega0);x20 = gaussianFilter(x2,omega0)
  x10 = x10 - mean(diff(x10));x20 = x20 - mean(diff(x20))
  x1r = x1[(2*omega0+1):(length(x1)-2*omega0)];x2r = x2[(2*omega0+1):(length(x1)-2*omega0)]
  sigma1 = sd(diff(x1r-x10));sigma2 = sd(diff(x2r-x20))
  xs1 = brownian(sigma1,length(x1))
  xs1par = brownian(sigma1,length(x1))
  xs2 = rho*xs1 + sqrt(1 - ((sigma1/sigma2)*rho)^2)*xs1par
  #return(data.frame(xs1=xs1[(2*omega0+1):(length(xs1)-2*omega0)],xs2=xs2[(2*omega0+1):(length(xs1)-2*omega0)]))
  return(data.frame(xs1=x10+(xs1[(2*omega0+1):(length(xs1)-2*omega0)]-gaussianFilter(xs1,omega0)),xs2=x20+(xs2[(2*omega0+1):(length(xs2)-2*omega0)]-gaussianFilter(xs2,omega0))))
}









