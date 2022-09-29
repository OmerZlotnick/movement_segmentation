library(ggplot2)
library(dplyr)

tw <- 9
ks <- c(2,3,4,5,6)

dir.create("../plots/real_trajectories", showWarnings = F)
dir.create(paste0("../plots/real_trajectories/tw_",tw), showWarnings = F)
dir.create(paste0("../plots/real_trajectories/tw_",tw,"/without_segmentation"), showWarnings = F)
dir.create(paste0("../plots/real_trajectories/tw_",tw,"/with_segmentation"), showWarnings = F)

examples_list <- list.files(paste0("../outputs/9 examples/tw_",tw,"/k_2"))

for(e in examples_list){
  
  input_path <- paste0("../outputs/9 examples/tw_",tw,"/k_2/",e)
  input_df <- read.csv(input_path)
  
  output_path <- paste0("../plots/real_trajectories/tw_",tw,"/without_segmentation/",substring(e,1,nchar(e)-4),".tiff")
  
  ggplot(input_df, aes(x = X, y = Y)) +
    theme_minimal() + 
    geom_path(size = 0.05) +
    geom_point(size = 0.1) + 
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank())
  
  ggsave(output_path)
  
  for(k in ks){
    
    input_path <- paste0("../outputs/9 examples/tw_",tw,"/k_",k,"/",e)
    input_df <- read.csv(input_path)
    
    input_df$group <- as.factor(input_df$group)
    
    dir.create(paste0("../plots/real_trajectories/tw_",tw,"/with_segmentation/k_",k), showWarnings = F)
    
    output_path <- paste0("../plots/real_trajectories/tw_",tw,"/with_segmentation/k_",k,"/",substring(e,1,nchar(e)-4),".tiff")
    
    
    ggplot(input_df, aes(x = X, y = Y, color = group)) +
      theme_minimal() + 
      geom_path(color = "black", size = 0.05) +
      geom_point(size = 0.45) + 
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
    
    ggsave(output_path)
    
    
  }
  
}
