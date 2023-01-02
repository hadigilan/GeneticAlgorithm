

#
# 	Installing & loading package
#



install.packages("GA")						# installing the package
library(GA)								# loading the package
citation("GA")

############################################################	One dimensional problem 	##########################


#
#	Example 1: x^2 (maximize)
#

f1 <- function(x) x^2
left <- -1
right <- 5
curve(f1, from=left , to=right )

example1 <- ga(type = "real-valued", fitness = f1, lower = left , upper = right, maxiter = 100 )				# this find the maximum value of f1
summary(example1 )



#
#	Example 2: x^2 (minimize)
#

f2 <- function(x) -f1(x)
left <- -1
right <- 5

example2 <- ga(type = "real-valued", fitness = f2, lower = left , upper = right, maxiter = 100 )				# this find the minimum value of f2
summary(example2)
example2@solution																		# to extract final solution
plot(example2)



#
#	Example 3: (x^2 + x) * cos(x)	(global maximization)
#



f3 <- function(x) (x^2 + x) * cos(x)
left 	<- -10
right <- 10
curve(f3, from=left , to=right )


example3 <- ga(type = "real-valued", fitness = f3, lower = left , upper = right, maxiter = 100 )				# this find the maximum value of f3
example3@solution																		# to extract final solution

curve(f3, from=left , to=right )
text(x=example3@solution, y=example3@fitnessValue, labels="", pch="*")


############################################################	two dimensional problem 	##########################


#
#	example 4: OLS regression using GA
#

y<-  rnorm(100)																			# generates random number from normal standard
x<- runif(100)																			# generates random number from uniform distribution
data4<- data.frame(y, x)

f4<- function(beta, x, y){
  			y.hat <- beta[1]  + (beta[2]*x);
  			rss<-  -sum( (y-y.hat)^2) ;
  			return(rss);
  			}

left 	<- c(-10, -10)
right <- c(10, 10)


example4 <- ga(type = "real-valued", fitness = f4, x=data4$x, y=data4$y, lower = left , upper = right, maxiter = 100, names=c('beta1', 'beta2') )	

summary(example4)

lm(y ~ x, data=data4)																						# compare with the results of formal OLS 


############################################################	constrained optimization 	##########################


