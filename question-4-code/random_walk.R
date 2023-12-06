#install.packages("ggplot2")
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

brownian_motion <- function (n_steps) {
  
  df <- data.frame(x = rep(NA, n_steps), y = rep(NA, n_steps), time = 1:n_steps)
  
  df[1,] <- c(0,0,1)
  
  for (i in 2:n_steps) {
    
    h_x <- rnorm(1, mean = 0, sd = sqrt(0.25)) 
    h_y <- rnorm(1, mean = 0, sd = sqrt(0.25))
    # step size derived from normal distribution with mean = 0, standard deviation = 0.5.
    
    df[i,1] <- df[i-1,1] + h_x
    
    df[i,2] <- df[i-1,2] + h_y
    
    df[i,3] <- i
    
  }
  
  return(df)
  
}

set.seed(75)

data1 <- brownian_motion(500)

plot1 <- ggplot(aes(x = x, y = y), data = data1) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

data2 <- brownian_motion(500)

plot2 <- ggplot(aes(x = x, y = y), data = data2) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

grid.arrange(plot1, plot2, ncol=2)
