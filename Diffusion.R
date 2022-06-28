library(tidyverse)
library(cowplot)
library(gridExtra)
library(grid)
library(Rmisc)


matchmaking_two_value <- function(eta_lt,eta_rt,rho_lt,rho_rt,theta,time,
                                  target = c("eta_lt","eta_rt","rho_lt","rho_rt")){
  
  storage_v2 <- matrix(NA_real_, nrow = time, ncol = 4)
  graphs <- list()
  storage_v2 <- as.data.frame(storage_v2)
  colnames(storage_v2) <- c("eta_lt","eta_rt","rho_lt","rho_rt")
  
  for (i in 1:time) {
    storage_v2[i,1] <- eta_lt
    storage_v2[i,2] <- eta_rt
    storage_v2[i,3] <- rho_lt
    storage_v2[i,4] <- rho_rt
    
    
    eta_lt_next <- (2*eta_rt*rho_lt) - (2*eta_lt*rho_rt) + 
      (2*theta*eta_lt*eta_rt) - (2*(1-theta)*eta_rt*eta_lt) + 
      eta_lt^2
    eta_rt_next <- 1 - eta_lt_next - rho_lt - rho_rt
    
    if(eta_lt_next <= 0) {
      eta_lt <- 0
      eta_rt <- 1 - (eta_lt + rho_lt + rho_rt)
    } else{
      eta_lt <- eta_lt_next
      eta_rt <- eta_rt_next
    }
 
 
    
 
  }
plot <- storage_v2%>%ggplot(mapping = aes(x = 1:time)) + 
         geom_line(aes(y = eta_lt, color = "Eta_L")) + 
         geom_line(aes(y = eta_rt, color = "Eta_R")) + 
         geom_line(aes(y = rho_lt, color = "Rho_L")) + 
         geom_line(aes(y = rho_rt, color = "Rho_R")) + 
         
         ggtitle("Population", subtitle = paste("Theta: ", theta)) + 
         ylab("Proportions") + xlab("Time") + 
         scale_color_manual(name = "Agent Types", 
                            breaks = c("Eta_L","Eta_R","Rho_L","Rho_R"), 
                            values = c("Eta_L" = "khaki4","Eta_R" = "salmon3", 
                                       "Rho_L" = "darkslateblue", "Rho_R" = "tomato4")) + 
         theme_minimal()
  
  list(data = storage_v2, plot = plot)

}






for (a in seq(0,1,0.05)) {
  

result <- lapply(seq(0,0.9,0.1), function(i){
matchmaking_two_value(eta_lt = i, eta_rt = 0.9-i,
                          rho_lt = 0.05,rho_rt = 0.05,
                          theta= a, time = 50)
  })

allplots <- lapply(result, function(x)x$plot)
gridExtra::grid.arrange(grobs=allplots, ncol = 5)}



