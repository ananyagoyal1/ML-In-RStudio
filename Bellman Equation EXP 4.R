# Define a simple MDP environment
# States, actions, rewards, transitions

# List of states
states <- c("A", "B", "C", "D")

# Actions available in each state
actions <- list(
  A = c("go_B", "go_C"),
  B = c("go_A", "go_D"),
  C = c("go_A", "go_D"),
  D = c("go_B", "go_C")
)

# Transition probabilities for each state-action pair
transitions <- list(
  A = list(go_B = "B", go_C = "C"),
  B = list(go_A = "A", go_D = "D"),
  C = list(go_A = "A", go_D = "D"),
  D = list(go_B = "B", go_C = "C")
)

# Reward function for each state-action pair
rewards <- list(
  A = list(go_B = 1, go_C = 5),
  B = list(go_A = -1, go_D = 3),
  C = list(go_A = 0, go_D = 4),
  D = list(go_B = 2, go_C = 6)
)

# Bellman equation with value iteration
value_iteration <- function(states, actions, transitions, rewards, gamma = 0.9, iterations = 100) {
  # Initialize value function to zero for each state
  values <- setNames(rep(0, length(states)), states)
  
  # Perform value iteration
  for (i in seq_len(iterations)) {
    new_values <- values  # Copy current values
    
    for (s in states) {
      action_values <- sapply(actions[[s]], function(a) {
        next_state <- transitions[[s]][[a]]
        reward <- rewards[[s]][[a]]
        reward + gamma * values[[next_state]]  # Bellman update
      })
      
      # Assign the maximum value to the current state
      new_values[[s]] <- max(action_values)
    }
    
    values <- new_values  # Update value function
  }
  
  return(values)
}

# Hyperparameter tuning for the discount factor (gamma)
# Test different gamma values
gamma_values <- seq(0.7, 0.99, by = 0.05) 
# Store optimal values for each gamma
optimal_values <- list()  

for (gamma in gamma_values) {
  optimal_values[[paste0("gamma_", gamma)]] <- value_iteration(
    states,
    actions,
    transitions,
    rewards,
    gamma = gamma,
    iterations = 100
  )
}

# Display optimal values for each gamma
print("Optimal values for different gamma:")
print(optimal_values)

# Example: Optimal values for gamma = 0.9
optimal_values$gamma_0.9  # Output optimal values for gamma = 0.9

