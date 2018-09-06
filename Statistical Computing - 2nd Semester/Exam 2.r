#exam 2 
#problem 3

data = read.table("http://mathfaculty.fullerton.edu/mori/Math534/Examdata/prob2data.txt", header=T)

y = data[,1]

#(a)

EM_a <- function(y, theta, maxit){

	n = length(y)

	for(it in 1:maxit){

		mu = theta[1]
		sigma_sq = theta[2]

		#step 1
		u_star = (5*sigma_sq)/(4*sigma_sq + (y-mu)^2) #vect n*1

		#step 2
		mu_hat = sum(u_star*y) / sum(u_star)

		sigma_sq_hat = (1/n)*sum(u_star*((y - mu_hat)^2))

		theta_hat = c(mu_hat, sigma_sq_hat)

		theta = theta_hat

		print(theta)

	}

}

theta_0 = c(0,1)

maxit=10

EM_a(y, theta_0, maxit)

#(b)


EM_b <- function(y, theta, maxit, tolerr){

	n = length(y)

	for(it in 1:maxit){

		mu = theta[1]
		sigma_sq = theta[2]

		#step 1
		u_star = (5*sigma_sq)/(4*sigma_sq + (y-mu)^2) #vect n*1

		#step 2
		mu_hat = sum(u_star*y) / sum(u_star)

		sigma_sq_hat = (1/n)*sum(u_star*((y - mu_hat)^2))

		theta_hat = c(mu_hat, sigma_sq_hat)

		mre_mu = abs(mu_hat-mu)/max(1,abs(mu))
		mre_sigma_sq = abs(sigma_sq_hat-sigma_sq)/max(1,abs(sigma_sq))

		if((mre_mu<tolerr) & (mre_sigma_sq<tolerr)) break

		theta = theta_hat

		cat("it=", it,"theta=", theta, "mre= ", mre_mu, mre_sigma_sq,"\n")

	}

}

theta_0 = c(0,1)

maxit=30; tolerr = 1e-3

EM_b(y, theta_0, maxit, tolerr)





