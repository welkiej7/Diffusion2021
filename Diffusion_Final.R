##Core Model

library(tidyverse)
library(gridExtra)
library(grid)
library(Rmisc)


matchmaking_two_value <- function(eta_lt,rho_lt,rho_rt,theta,time,
                                  target = c("eta_lt","eta_rt","rho_lt","rho_rt")){
  
  storage_v2 <- matrix(NA_real_, nrow = time, ncol = 4)
  graphs <- list()
  storage_v2 <- as.data.frame(storage_v2)
  colnames(storage_v2) <- c("eta_lt","eta_rt","rho_lt","rho_rt")
  eta_rt <- 1 - eta_lt - rho_lt - rho_rt
  for (i in 1:time) {
    storage_v2[i,1] <- eta_lt
    storage_v2[i,2] <- eta_rt
    storage_v2[i,3] <- rho_lt
    storage_v2[i,4] <- rho_rt
    
    
    eta_lt_next <- (2*eta_rt*rho_lt) - (2*eta_lt*rho_rt) + 
      (2*theta*eta_lt*eta_rt) - (2*(1-theta)*eta_rt*eta_lt) + 
      eta_lt^2
    eta_rt_next <- 1 - eta_lt_next - rho_lt - rho_rt
    
    if(eta_lt_next == eta_lt){
     paste("Reached Steady State at Cycle No: ", i) -> Steady_State_Cycle
      break
    } else{
      Steady_State_Cycle <- paste("Unstable")
    if(eta_lt_next <= 0) {
      eta_lt <- 0
      eta_rt <- 1 - (eta_lt + rho_lt + rho_rt)
    } else {
      eta_lt <- eta_lt_next
      eta_rt <- eta_rt_next
    }
  }
}
  plot <- storage_v2%>%ggplot(mapping = aes(x = 1:time)) + 
    geom_line(aes(y = eta_lt, color = "Eta_L")) + 
    geom_line(aes(y = eta_rt, color = "Eta_R")) + 
    geom_line(aes(y = rho_lt, color = "Rho_L")) + 
    geom_line(aes(y = rho_rt, color = "Rho_R")) + 
    
    ggtitle("Population", subtitle = paste("Theta:", theta,", ", Steady_State_Cycle)) + 
    ylab("Proportions") + xlab("Time") + 
    scale_color_manual(name = "Agent Types", 
                       breaks = c("Eta_L","Eta_R","Rho_L","Rho_R"), 
                       values = c("Eta_L" = "khaki4","Eta_R" = "salmon3", 
                                  "Rho_L" = "darkslateblue", "Rho_R" = "tomato4")) + 
    theme_minimal()
  
  list(data = storage_v2, plot = plot)
  
}

matchmaking_two_value(eta_lt = 0.6, rho_lt = 0.40,rho_rt = 0.10, theta = 1, time = 40 )

## Function for checking if there is a solution for eta_l given rho_l, rho_r






solution <-  function(eta_l,rho_l,rho_r,theta){
  delta <- (-1-(2*rho_r) + 4*theta*(1- eta_l -rho_l -rho_r) - 
              (2*(1 - eta_l -rho_l -rho_r)^2))- 
    4 * (2 * (1 - eta_l - rho_l - rho_r)*rho_l)
  
  if(delta >= 0){
    a <- paste("There exists a solution or solutions: ")
  } else {
    a <- paste("There is no solution")
  }
  print(a)
}

for(i in seq(0,1,0.1)){
  solution(eta_l = i, rho_r = 0.1, rho_l = 0.1, theta = 1)
  
}

