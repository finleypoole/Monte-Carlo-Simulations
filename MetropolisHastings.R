#####random walk metropolis-hastings algorithm
set.seed(146)
target_distribution = function(x) { #define target distribution
  return(1/sqrt(2*pi)*exp(-x^2)+1/sqrt(2*pi)*exp(-2*(x-2)^2)+1/sqrt(2*pi)*exp(-4*(x-4)^2))
}

#Visualise the distribution
x=seq(-5,8,by=0.01)
plot(x,target_distribution(x),type = 'l',ylab='f(x)')
title(main='Target distribution')

generate_candidate = function(x, delta) {
  #Generate candidate from normal distribution centered at x
  return(x+rnorm(1,0, delta))
}

#Function to accept or reject a candidate
accept_candidate = function(x_current, x_proposed) {
  #Compute acceptance probability
  acceptance_prob = min(1, target_distribution(x_proposed) / target_distribution(x_current))
  #Accept or reject the candidate based on acceptance probability
  if (runif(1) < acceptance_prob) {
    return(x_proposed)
  } else {
    return(x_current)
  }
}

#Random walk Metropolis-Hastings algorithm
metropolis_hastings = function(iterations, initial_value, delta) {
  #Initialize vector to store samples
  samples = numeric(iterations)
  #Initialize current state
  current_state = initial_value
  #Run the algorithm
  for (i in 1:iterations) {
    #Generate candidate state
    candidate_state = generate_candidate(current_state, delta)
    #Accept or reject the candidate
    current_state = accept_candidate(current_state, candidate_state)
    #Store the sample
    samples[i] = current_state
  }
  return(samples)
}
#Set parameters
iterations = 10000
initial_value = 1 #Initial value for the chain
delta = 1 #Standard deviation of the proposal distribution
#Run the Metropolis-Hastings algorithm
samples = metropolis_hastings(iterations, initial_value, delta)
#We can plot the distribution of the samples vs the target distribution
#Plot the samples histogram
hist(samples, breaks = 100, main = "Samples from the target distribution",
     xlab = "Sample value", freq = FALSE, ylim=c(0,0.5))

#and the target distribution

curve(target_distribution, add = TRUE, col = "red", lwd = 2)


