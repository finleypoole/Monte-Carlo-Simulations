#Create the function h(x)
h=function(x){
  d=7*exp(-2*(x-5)^4)
  return(d)
}
#draw the plot
curve(h, from=-100, to=100)
curve(h, from=0, to=10)

#simple sampling####

#calculate the integral over the interval [0,10]
I=integrate(h,0,10)
I

#generate 10000 uniformly distributed values of the function h over the interval [0,10]
set.seed(1) #for reproducibility
h_values=h(runif(10^4,0,10))

#calculate the approximated value of the integral
expect=10*sum(h_values)/(10^4)
expect
cat("Simple Sampling Estimate:", expect, "\n")

#variance of estimator In
var_In=var(10*h_values)/length(h_values)
var_In

#importance sampling#####

#The first thing I will do, is test and find a suitable distribution for g

#loading necessary packages
library(ggplot2)
library(reshape2)

#Define a sequence of x values for plotting
x=seq(0,10,length.out = 1000)

#Create a data frame for the distributions and h(x)
df=data.frame(x=x,
              N51= dnorm(x,mean=5, sd=1),
              N505= dnorm(x,mean=5, sd=0.5),
              N5025= dnorm(x,mean=5, sd=0.25)
              )

#Reshape the dataframe df for plotting with ggplot2
df_long = reshape2::melt(df, id.vars = "x")

#Plot the distributions
ggplot(df_long, aes(x = x, y = value, color = variable)) +
  geom_line() +
  labs(x = "Value", y = "Density", title = "Distributions") +
  scale_color_manual(values = c("N51" = "red", "N505" = "blue", "N5025" = "green", "xdf"="black"),
                     labels = c("N51" = "N(5,1^2)", "N505" = "N(5,0.5^2)", "N5025" = "N(5,0.25^2)")) +
  theme_minimal()

#Pick N(5,0.5^2). Plot the distributions against h(x) to show this is the best fit:
df2=transform(df, h=7*exp(-2*(x-5)^4)) #add h(x) to a new dataset df2 with distributions
df_long2 = reshape2::melt(df2, id.vars = "x") #reshape for plotting
ggplot(df_long2, aes(x = x, y = value, color = variable)) + #plot with ggplot
  geom_line() +
  labs(title = "Distributions against h(x)") +
  scale_color_manual(values = c("N51" = "red", "N505" = "blue", "N5025" = "green","h"="black", "xdf"="black"),
                     labels = c("N51" = "N(5,1^2)", "N505" = "N(5,0.5^2)", "N5025" = "N(5,0.25^2)", "h"="h(x)")) +
  theme_minimal()


#Importance sampling with N(5,0.5^2)

#Box-Muller algorithm for generating Normally distributed numbers

set.seed(3) #for reproducibility
n = 10000 #number of samples

#Generate two uniformly distributed random vectors of size n
U1=runif(n/2)
U2=runif(n/2)

#trasform U1 and U2 into standard normal random variables 
X1=sqrt(-2*log(U1))*cos(2*pi*U2)
X2=sqrt(-2*log(U1))*sin(2*pi*U2)

#make one vector of standard normals from X1 and X2
X=c(X1,X2)

#Transform samples from standard normal to N(5,0.5)
samples_N505 = 5+ 0.5*X


#weights
weights_N505 = (h(samples_N505) / dnorm(samples_N505, mean = 5, sd = 0.5))

#estimate
importance_sampling_estimate_N505 = mean(weights_N505)

#Print the results using the cat function
cat("Importance Sampling Estimate with N(5,0.5^2):", importance_sampling_estimate_N505, "\n")

#Plot of histogram of the random numbers generated using g(x) against theoretical curve of g(x)
x_curve = seq(0,10,length=length(samples_N505))
y_curve = dnorm(x_curve, mean = 5, sd=0.5) # Define the (numerical) function N(5,0.5^2) on x
hist(samples_N505,breaks=30,probability=TRUE, main = "Histogram of N(5,0.5^2) samples against theoretical curve of N(5,0.5^2)")
lines(x_curve,y_curve)

#variance of estiamtor Jn
var_Jn = var(weights_N505) / length(weights_N505)

#print both variances
cat("Var(I_n):", var_In, "\n")
cat("Var(J_n):", var_Jn, "\n")

