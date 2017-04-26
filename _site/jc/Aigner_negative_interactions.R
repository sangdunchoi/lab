
###
#
# Aigner 2001 model with negative interactions
#
# Simon Joly, 5 mai 2016
#


###
# First, create some fitness distributions (bat and hum)

# trait values
x <- seq(0,12,by=0.1)

# Distributions of fitness contribution
bat <- dnorm(x,mean=4,sd=1.5)*0.7
hum <- dnorm(x,mean=6.9,sd=1.5)
hum <- dnorm(x,mean=7,sd=10)*6

###
# Now, specify the interactions parameters (alpha). In the example below,
# the hummingbird has more effect on the bat than the opposite. You can
# also try other values.

# The first is the effect of the hummingbirds on the fitness contribution
# of the bat. It can vary between 0 and 1/max(hum).
alpha.hum.on.bat = 0.8/max(hum)
# The second is the effect of the bats on the fitness contribution of
# the hummingbird.
alpha.bat.on.hum = 0.8/max(bat)

###
# Fitness components with interactions
fit.hum.inter = hum*(1-alpha.bat.on.hum*bat)
fit.gen.inter = bat*(1-alpha.hum.on.bat*hum)

###
# Now, calculate the combined effect. This is the Aigner formula
# with interactions.
fit.tot = bat + hum
fit.tot.inter = fit.gen.inter + fit.hum.inter

###
# Plot the results
require(ggplot2)
#dat <- data.frame(x=x,Fitness=c(bat,hum,tot),Pollinator=rep(c("bat","hummingbird","all"),each=length(x)))
dat <- data.frame(x=x,Fitness=c(bat,hum,fit.tot,fit.gen.inter,fit.hum.inter,fit.tot.inter),
                  Pollinator=rep(c("bat","hummingbird","total"),each=length(x)),
                  Interactions=rep(c("no","yes"),each=length(x)*3))
ggplot(dat,aes(x=x,y=Fitness)) + geom_line(aes(lty=Interactions,colour=Pollinator),lwd=0.75) + 
  xlab("Trait") + theme_minimal()



# Waser 1996 model -----

# N = abundance, V = visitation rate, g = pollination efficiency,
# sigma = effect of a mutation on efficiency (favouring pol 1).

# mutation for pollination efficiency

pol1.NV = 0.25
pol2.NV = 0.24
plo1.g = seq(0.1,1,by=0.1)
pol2.g = seq(0.1,1,by=0.1)

fit.waser <- function(pol1.NV,pol2.NV,pol1.g,pol2.g,sigma){
  return (pol1.NV*(pol1.g+sigma) + pol2.NV*(pol2.g-sigma))
}

res.mat <- matrix(NA,ncol=10,nrow=10)
res.mat.sigma <- matrix(NA,ncol=10,nrow=10)
sigma=0.1
for(i in 1:10) {
  res.mat[i,] <- fit.waser(pol1.NV,pol2.NV,plo1.g[i],pol2.g,0)
  res.mat.sigma[i,] <- fit.waser(pol1.NV,pol2.NV,plo1.g[i],pol2.g,sigma)
}
res.mat.sigma > res.mat
