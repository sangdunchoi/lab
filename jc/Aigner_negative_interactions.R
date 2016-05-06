
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

###
# Now, specify the interactions parameters (alpha). In the example below,
# the hummingbird has more effect on the bat than the opposite. You can
# also try other values.

# The first is the effect of the hummingbirds on the fitness contribution
# of the bat. It can vary between 0 and 1/max(hum).
alpha.hum.on.bat = 0.9/max(hum)
# The second is the effect of the bats on the fitness contribution of
# the hummingbird.
alpha.bat.on.hum = 0.4/max(bat)

###
# Now, calculate the combined effect. This is the Aigner formula
# with interactions.
tot = bat*(1-alpha.hum.on.bat*hum)+hum*(1-alpha.bat.on.hum*bat)

###
# Plot the results
require(ggplot2)
dat <- data.frame(x=x,Fitness=c(bat,hum,tot),Pollinator=rep(c("bat","hummingbird","all"),each=length(x)))
ggplot(dat,aes(x=x,y=Fitness)) + geom_line(aes(lty=Pollinator,colour=Pollinator),lwd=1) + 
  xlab("Trait") + theme_minimal()
