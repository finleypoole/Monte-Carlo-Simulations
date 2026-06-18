library(ggplot2)
#Generate a random transition probability matrix for a homogeneous Markov chain whose state space has eight states (n_states = 8)
set.seed(146)
n_states = 8
transition_matrix = matrix(runif(n_states * n_states), nrow = n_states)
transition_matrix = t(apply(transition_matrix, 1, function(x) x/sum(x))) #normalise each row to sum to 1
transition_matrix

#calculate invariant distributions

#define a function
invariant_distributions = function(P) {
  
  #calculate eigenvalues and eigenvectors of P
  e = eigen(t(P))
  eigenvalues = e$values
  eigenvectors = e$vectors
  
  #Choose the eigenvector(s) corresponding to the eigenvalue(s) close to 1
  close_to_one = abs(eigenvalues - 1) < 1e-10
  stationary = list()
  
  #Normalise the eigenvector(s) so that the sum of its components is one
  for(i in which(close_to_one)) {
    stationary[[length(stationary)+1]] = eigenvectors[, i] / sum(eigenvectors[, i])

  }
  return(stationary)#Return the components of the eigenvectors
}

#Print eigenvalues and invariant distribution
cat("Eigenvalues: ", eigen(transition_matrix)$values)

invariant_dists = invariant_distributions(transition_matrix)
for (pi in invariant_dists){
  cat("Invariant distribution:", pi)
}

#Simulate MC and compute expected value

#Function to calculate next step of the MC
sample_next = function(P, current) {
  return(sample(1:nrow(P), 1, prob = P[current, ]))
}

#Monte Carlo estimator
markov_sample_mean = function(P, start, func, n){
  total = 0
  state = start
  for (i in 1:n){
    state = sample_next(P, state)
    total = total + func[state]
  }
  return(total / n)
}

#Define a function f(xi)=i
func = 1:n_states

#compute true expectation
#invariant distributions formatted as numeric vector
stationary_dist = as.numeric(invariant_dists[[1]]) 
exact_expectation = sum(stationary_dist * func) #exact expectation
cat("E[f(X)] =", exact_expectation, "\n")

#for loop to generate sample means for different values of n
set.seed(123)
n_values = numeric(7)
sample_means = numeric(7)

for (i in 5:12){
  n = 2^i
  sample_mean = markov_sample_mean(transition_matrix, 1, func, n)
  
  n_values[i]=n
  sample_means[i]= sample_mean
  cat("sample mean for n =", n, ":", sample_mean, "\n")
}


#plot convergence
# put into data frame
df_sample_means = data.frame(n=n_values, sample_mean = sample_means)

#plot
ggplot(df_sample_means, aes(x = n, y = sample_mean)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey"))+
  geom_line(col = "red", size = 1) +
  geom_hline(yintercept = exact_expectation, size =1) +
  coord_cartesian(ylim=c(3.5,5))+
  labs(title = expression(paste("Approximation of expectation")),
       x = "number of steps",
       y = "estimated value") +
  theme_minimal()

#sample size of 1000 starting at X_0=1
#create function to store steps individually
markov_sample_1000 = function(P, start, func, n){
 
   state = start
   individual_values = numeric(n) #store each step as numeric
   
  for (i in 1:n){ #markov chain simulation
    state = sample_next(P, state)
    individual_values[i] = func[state] #store values corresponding to state
  }
  return(individual_values)
}
set.seed(5)
individual_values = markov_sample_1000(transition_matrix, 1, func, 1000)
var_chain = var(individual_values) / 1000 #find variance
cat("The Variance for n=1000 is:", var_chain)

#calculate empirical distribution of chain for n=1000
#calculate the proportion of time the chain spends in each state
empirical_dist = table(individual_values) / length(individual_values)
empirical_dist
sum(empirical_dist)#check adds to 1
