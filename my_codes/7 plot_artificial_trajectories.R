library(ggplot2)
library(dplyr)

tw <- 9 
ks <- c(2,3,4,5,6)
number_of_traj <- 10    # per tag & k

tags_list <- c()
for(f in list.files("../outputs/1 locations_data")){
  tags_list <- cbind(tags_list, substring(f, 1,3))
}

dir.create("../plots/artificial_trajectories", showWarnings = F)
dir.create(paste0("../plots/artificial_trajectories/tw_",tw), showWarnings = F)

for(k in ks){
  dir.create(paste0("../plots/artificial_trajectories/tw_",tw,"/k_",k), showWarnings = F)
  dir.create(paste0("../plots/artificial_trajectories/tw_",tw,"/k_",k,"/with_segmentation"), showWarnings = F)
  dir.create(paste0("../plots/artificial_trajectories/tw_",tw,"/k_",k,"/without_segmentation"), showWarnings = F)
  
  for(i in 1:number_of_traj){
    
    input_path <- paste0("../outputs/11 artificial_trajectories/tw_",tw,"/k_",k,"/",i,".csv")
    comp_df <- read.csv(input_path)
    comp_df$state <- as.factor(comp_df$state)
    
    ### with segmentation ###
    
    output_path <- paste0("../plots/artificial_trajectories/tw_",tw,"/k_",k,"/with_segmentation/",i,".tiff")
    
    ggplot(comp_df, aes(x = x, y = y, color = state)) +
      theme_minimal() + 
      geom_path(color = "black", size = 0.05) +
      geom_point(size = 0.45) + 
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
    
    ggsave(output_path)
    
    ### without segmentation ###
    
    output_path <- paste0("../plots/artificial_trajectories/tw_",tw,"/k_",k,"/without_segmentation/",i,".tiff")
    
    ggplot(comp_df, aes(x = x, y = y)) +
      theme_minimal() + 
      geom_path(color = "black", size = 0.05) +
      geom_point(size = 0.2) + 
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
    
    ggsave(output_path)
    
  }
}


