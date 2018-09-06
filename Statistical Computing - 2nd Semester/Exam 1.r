#MIDTERM 1: 534
#LOUIS BENSARD
library(plot3D)


source("functions/log_likelihood.r")
source("functions/gradient.r")
source("functions/max_seeker.r")
source("functions/fisher.r")
source("functions/pi_fct.r")
source("functions/pi_fct_MLE.r")
source("functions/verif_model.r")



data = read.table('http://mathfaculty.fullerton.edu/mori/math534/examdata/blowBF.txt', h=T)
x = data.frame(x_i1=1, x_i2=log(data[,1]), x_i3=data[,2], stringsAsFactors=FALSE)
x = data.matrix(x)

maxit = 10 #maximum number of iterations

beta_0 = rep(0,3)

#(d) & (g)

tolerr = 1e-4 #for part (d)
#tolerr = 1e-6 #for part (g)

# The booleans are: (1) Steepest or Fisher
#(2) Nice and full printed output as required (but longer to run)
#if FALSE, we get a rudimentary final output

max_seeker(data, x, beta_0, maxit, T,  T, tolerr)


#(h)
n = 100
S = seq(0,1, length=n)
D = 10
pi_S = pi_fct_MLE(S, log(D))

par(mfrow=c(2,2))
plot(S, pi_S, type='l', ylab = expression(paste(pi,'(S)')))
title('Prediction of my model for trees of diameter D=10')

#TEST TO SEE IF THE GRAPH MAKES SENSE
pi_S_data(data, D)

#(i)
n=100
D = seq(5,30, length=n)
S = 0.4
pi_D = pi_fct_MLE(S, log(D))

# dev.new()
plot(D, pi_D, type='l', ylab = expression(paste(pi,'(D)')))
title('Prediction of my model for winds of severity S=0.4')

#TEST TO SEE IF THE GRAPH MAKES SENSE
pi_D_data(data, S)


#(j)
n=50
x1 = seq(0,1, length=n) # = S
x2 = seq(min(x[,2]), max(x[,2]), length=n) #log (D)
x3 = outer(x1, x2, pi_fct_MLE) #pi(S, log(D))

dev.new()
persp(x1,x2,x3, theta=245, phi=25, r=2, shade=0.4, axes=TRUE, box=TRUE, 
	ticktype="detailed" , col="cyan", xlab="S", ylab="log(D)", 
	zlab="", main=expression(paste(pi,'(', beta, ')')), expand = 1)


dev.new()
filled.contour(x1, x2, x3, col = rainbow(30), nlevels=30, 
	plot.title = title(main= expression(paste('Constant value contours for ', pi,'(', beta, ')')), xlab="x1", ylab="x2"),
	key.title = title(main=expression(paste(pi,'(x1, x2)'))))