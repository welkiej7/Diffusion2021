library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)

final_matchmaking <- function(eta_l,eta_r,rho_l,rho_r,theta,time,alpha = 1,beta = 1){
  
  ##Premable ##
  
  storage <- as.data.frame(matrix(NA_real_, nrow = time, ncol = 4))
  colnames(storage) <- c("Eta_i","Eta_j","Rho_i","Rho_j")
  graphs <- list()
  
  
  ##Matchmaking##
  for (i in 1:time) {
    storage[i,1] <-  eta_l
    storage[i,2] <-  eta_r
    storage[i,3] <-  rho_l
    storage[i,4] <-  rho_r
    
  
    
    eta_l_next <-  2*(eta_r*rho_l*eta_r)*alpha - 2*(eta_r*rho_l*eta_r)*(1-alpha) - 
      2*(eta_l*rho_r*eta_l)*beta + 2*(eta_l*rho_r*eta_l)*(1-beta)+
      (eta_l*eta_r*eta_r)*theta - (1-theta)*(eta_l*eta_r*eta_l) + eta_l
    
    eta_r_next <-  1 - (eta_l_next + rho_l + rho_r)
    
    if(eta_l_next <= 0){
      eta_l_next <- 0
      eta_r_next <- 1 - (eta_l_next - rho_l - rho_r )
    } else if(eta_r_next <= 0){
      eta_r_next  <- 0
      eta_l_next <-  1 -(eta_r_next + rho_l + rho_r)
    } else {
      eta_r_next <- eta_r_next
      eta_l_next <- eta_l_next
    }

    
    eta_l <- eta_l_next
    eta_r <- 1 - (eta_l + rho_l + rho_r)

  }
  ##Output##
  
  print(storage)
  
  plot <- storage%>%ggplot(mapping = aes(x = 1:time)) + 
    geom_line(aes(y = Eta_i, color = "Eta_i")) + 
    geom_line(aes(y = Eta_j, color = "Eta_j")) + 
    geom_line(aes(y = Rho_i, color = "Rho_i")) + 
    geom_line(aes(y = Rho_j, color = "Rho_j")) + 
    
    ggtitle("Population", subtitle = paste("Theta: ", theta)) + 
    ylab("Proportions") + xlab("Time") + 
    scale_color_manual(name = "Agent Types", 
                       breaks = c("Eta_i","Eta_j","Rho_i","Rho_j"), 
                       values = c("Eta_i" = "khaki4","Eta_j" = "salmon3", 
                                  "Rho_i" = "darkslateblue", "Rho_j" = "tomato4")) + 
    theme_minimal() +scale_y_continuous(limits = c(0,1)) + 
    theme(axis.text.x = element_text(size = 15), 
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 15),
          axis.title =  element_text(size = 15))
  list(data = storage, plot = plot)

}
final_matchmaking(eta_l = 0.40, eta_r = 0.10, rho_l = 0.5, rho_r = 0, theta = 0.5, time = 20)



results <- lapply(seq(0, 0.5, 0.1), function(i){
  final_matchmaking(eta_l = i, eta_r = 0.5-i,
                        rho_l = 0.25,rho_r = 0.25,
                        theta= 0.2, time = 100)})


allplots <- lapply(results, function(x)x$plot)
gridExtra::grid.arrange(grobs = allplots, ncol = 2)





