library(tidyverse)
library(MASS)

create_mult_df_strong <- function(my_df){
  set.seed(1234)
  
  mean1 <- my_df$mean_x
  mean2 <- my_df$mean_y
  sd1 <- mean1 * .05
  sd2 <- mean2 * .05
  
  mu <- c(mean1, mean2) 
  
  myr <- .8 * sqrt(sd1) * sqrt(sd2)
  
  mysigma <- matrix(c(sd1, myr, myr, sd2), 2, 2) 
  
  my_data <- NULL
  sample_size <- 50
  
  my_data <- data.frame(mvrnorm(sample_size, mu, mysigma, empirical = TRUE))
  
  colnames(my_data) <- c(my_df$'x-label', my_df$'y-label')
  
  return(my_data)
}

create_mult_df_weak <- function(my_df){
  set.seed(1234)
  
  mean1 <- my_df$mean_x
  mean2 <- my_df$mean_y
  sd1 <- mean1 * .05
  sd2 <- mean2 * .05
  
  mu <- c(mean1, mean2) 
  
  myr <- .2 * sqrt(sd1) * sqrt(sd2)
  
  mysigma <- matrix(c(sd1, myr, myr, sd2), 2, 2) 
  
  my_data <- NULL
  sample_size <- 50
  
  my_data <- data.frame(mvrnorm(sample_size, mu, mysigma, empirical = TRUE))
  
  colnames(my_data) <- c(my_df$'x-label', my_df$'y-label')
  
  return(my_data)
}

# creating a wrapper function to wrap the title text
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

my_scatter_graph <- function(df, labx, laby, title) {
  set.seed(1234)
  colnames(df) <- c("x", "y")
  df %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    labs(x = labx, y = laby) +
    theme(text = element_text(size = 18),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA)) +
  ggtitle(wrapper(title, width = 43))
    
  
}

# the following function takes a graph and saves it in the graphs folder, 
# generates summary statistics of the data and saves them as a .csv file in the 
# summary_stats folder. In both cases a unique index is created to allow the
# graph and the summary stats to be paired up later durind coding of the 
# respones of participants. 
save_graph_strong_yes <- function(current_graph){
  ggsave(paste0("graphs/graph_strong_yes", index, ".jpg"), current_graph)
 
}  

save_graph_strong_no <- function(current_graph){
  ggsave(paste0("graphs/graph_strong_no", index, ".jpg"), current_graph)

}  

save_graph_weak_yes <- function(current_graph){
  ggsave(paste0("graphs/graph_weak_yes", index, ".jpg"), current_graph)
  
}  

save_graph_weak_no <- function(current_graph){
  ggsave(paste0("graphs/graph_weak_no", index, ".jpg"), current_graph)

}  

# MAIN CODE ####
# this reads in the .csv file that contains the parameters of the graphs to be
# generated
my_graphs <- read_csv("graphs_book.csv")

# this loops through the my_graphs which contains the paramters of the 
# graphs to be generated.  It runs once per each unique graph_id 
for(index in my_graphs$graph_id) {
  
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_mult_df_strong() 
  
  my_scatter_graph(build_this_one, 
           my_graphs[my_graphs$graph_id == index,]$'x-label', 
           my_graphs[my_graphs$graph_id == index,]$'y-label', 
           my_graphs[my_graphs$graph_id == index,]$title_yes) %>%
    save_graph_strong_yes()
  
  my_scatter_graph(build_this_one, 
                          my_graphs[my_graphs$graph_id == index,]$'x-label', 
                          my_graphs[my_graphs$graph_id == index,]$'y-label', 
                          my_graphs[my_graphs$graph_id == index,]$title_no) %>%
    save_graph_strong_no()
  
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_mult_df_weak() 
  
  my_scatter_graph(build_this_one, 
                          my_graphs[my_graphs$graph_id == index,]$'x-label', 
                          my_graphs[my_graphs$graph_id == index,]$'y-label', 
                          my_graphs[my_graphs$graph_id == index,]$title_yes) %>%
    save_graph_weak_yes()
  
  my_scatter_graph(build_this_one, 
                          my_graphs[my_graphs$graph_id == index,]$'x-label', 
                          my_graphs[my_graphs$graph_id == index,]$'y-label', 
                          my_graphs[my_graphs$graph_id == index,]$title_no) %>%
    save_graph_weak_no()
}

