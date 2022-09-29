library(dplyr)
library(ggplot2)
library(usdm)
library(reshape2)

features_statistics <- function(tw){
  path <- paste0("../outputs/4 features/tw_",tw,"/features_all_tags_normalized.csv")
  df <- read.csv(path)
  
  rand_df <- sample_n(df, 1000000)
  
  dir.create("../plots", showWarnings = FALSE)
  dir.create("../plots/features_statistics", showWarnings = FALSE)
  
  ### histograms ###
  
  dir.create("../plots/features_statistics/histograms", showWarnings = FALSE)
  plots_path <- "../plots/features_statistics/histograms"
  
  # mean time
  
  min(df$mean_time)
  max(df$mean_time)
  
  ggplot(df, aes(x = mean_time)) +
    theme_bw() +
    geom_histogram(binwidth = 2, aes(y=..count../sum(..count..))) +
    xlim(0,120) +
    ylab("proportion") + 
    #scale_y_continuous(trans = "log10", breaks = c(0,10,1000,100000,10000000),
    #labels = c(0,10,1000,100000,10000000)) +
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/mean_time.tiff", sep =""))
  
  # speed
  
  min(df$mean_speed)
  max(df$mean_speed)
  
  ggplot(df, aes(x = mean_speed)) +
    theme_bw() +
    geom_histogram(binwidth = 0.01, aes(y=..count../sum(..count..))) +
    xlim(-1,5) +
    ylab("proportion") + 
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/mean_speed.tiff", sep =""))
  
  # sd_speed
  
  min(df$sd_speed)
  max(df$sd_speed)
  
  ggplot(df, aes(x = sd_speed)) +
    theme_bw() +
    geom_histogram(binwidth = 0.01, aes(y=..count../sum(..count..))) +
    xlim(-1,5) +
    ylab("proportion") + 
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/sd_speed.tiff", sep =""))
  
  # total_distance
  
  min(df$total_distance)
  max(df$total_distance)
  
  ggplot(df, aes(x = total_distance)) +
    theme_bw() +
    geom_histogram(binwidth = 0.01, aes(y=..count../sum(..count..))) +
    xlim(-1,5) +
    ylab("proportion") + 
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/total_distance.tiff", sep =""))
  
  
  # mean_angle
  
  min(df$mean_angle)
  max(df$mean_angle)
  
  ggplot(df, aes(x = mean_angle)) +
    theme_bw() +
    geom_histogram(binwidth = 0.01, aes(y=..count../sum(..count..))) +
    #xlim(-5,5) +
    ylab("proportion") + 
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/mean_angle.tiff", sep =""))
  
  # sd_angle
  
  min(df$sd_angle)
  max(df$sd_angle)
  
  ggplot(df, aes(x = sd_angle)) +
    theme_bw() +
    geom_histogram(binwidth = 0.01, aes(y=..count../sum(..count..))) +
    #xlim(-5,5) +
    ylab("proportion") + 
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/sd_angle.tiff", sep =""))
  
  
  # absolute_max_displacement
  
  min(df$absolute_max_displacement)
  max(df$absolute_max_displacement)
  
  ggplot(df, aes(x = absolute_max_displacement)) +
    theme_bw() +
    geom_histogram(binwidth = 0.01, aes(y=..count../sum(..count..))) +
    xlim(-1,10) +
    ylab("proportion") + 
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/absolute_max_displacement.tiff", sep =""))
  
  # tortuosity
  
  min(df$tortuosity)
  max(df$tortuosity)
  
  ggplot(df, aes(x = tortuosity)) +
    theme_bw() +
    geom_histogram(binwidth = 0.01, aes(y=..count../sum(..count..))) +
    xlim(-2,100) +
    ylab("proportion") + 
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/tortuosity.tiff", sep =""))
  
  
  ### correlations ###
  
  small_rand_df <- sample_n(df, 10000)
  pairs(small_rand_df[,5:8])
  vif(rand_df[,5:8])
  
  dir.create("../plots/features_statistics/correlations", showWarnings = FALSE)
  plots_path <- "../plots/features_statistics/correlations"
  
  # mean_time VS mean_speed
  
  ggplot(rand_df, aes(x = mean_time, y = mean_speed)) +
    theme_bw() +
    geom_hex(bins = 100) +
    xlim(0,50) +
    ylim(-1,5) +
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/mean_time_mean_speed.tiff", sep =""))
  
  # mean_time VS sd_speed
  
  ggplot(rand_df, aes(x = mean_time, y = sd_speed)) +
    theme_bw() +
    geom_hex(bins = 100) +
    xlim(0,50) +
    ylim(-1,500) +
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/mean_time_sd_speed.tiff", sep =""))
  
  # mean_time VS tortuosity
  
  ggplot(rand_df, aes(x = mean_time, y = tortuosity)) +
    theme_bw() +
    geom_hex(bins = 100) +
    xlim(0,50) +
    ylim(-1,100) +
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/mean_time_tortuosity.tiff", sep =""))
  
  
  # total distance VS  absolute max displacement
  
  ggplot(rand_df, aes(x = total_distance, y = absolute_max_displacement)) +
    theme_bw() +
    geom_hex(bins = 5000) +
    xlim(-1,5) +
    ylim(-1,10) +
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/total_distance_absolute_max_displacement.tiff", sep =""))
  
  
  # tortuosity  VS  absolute max displacement
  
  ggplot(rand_df, aes(x = tortuosity, y = absolute_max_displacement)) +
    theme_bw() +
    geom_hex(bins = 5000) +
    xlim(-1,25) +
    ylim(-1,25) +
    theme(panel.grid = element_blank())
  ggsave(paste(plots_path, "/tortuosity_absolute_max_displacement.tiff", sep =""))
  
}

exploring_groups_by_features <- function(tw, ks){
  for (k in ks){
    
    path <- paste0("../outputs/5 kmeans/tw_9/k_",k,"/all_tags.csv")
    df <- read.csv(path)
    
    dir.create("../plots", showWarnings = FALSE)
    dir.create("../plots/exploring_groups_by_features", showWarnings = FALSE)
    dir.create(paste0("../plots/exploring_groups_by_features/", k), showWarnings = FALSE)
    
    ### histograms ###
    
    dir.create(paste0("../plots/exploring_groups_by_features/", k, "/histograms"), showWarnings = FALSE)
    plots_path <- paste0("../plots/exploring_groups_by_features/", k, "/histograms")
    
    # mean time
    
    min(df$mean_time)
    max(df$mean_time)
    
    ggplot(df, aes(x = mean_time)) +
      theme_bw() +
      geom_histogram(binwidth = 4, aes(y=..count../sum(..count..))) +
      xlim(0,50) +
      ylab("proportion") +
      #scale_y_continuous(trans = "log10", breaks = c(0,10,1000,100000,10000000),
      #labels = c(0,10,1000,100000,10000000)) +
      facet_grid(group ~ .) +
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/mean_time.tiff", sep =""))
    
    # speed
    
    min(df$mean_speed)
    max(df$mean_speed)
    
    ggplot(df, aes(x = mean_speed)) +
      theme_bw() +
      geom_histogram(binwidth = 0.05, aes(y=..count../sum(..count..))) +
      #xlim(-1,1.5) +
      #ylim(0,25) +
      ylab("proportion") +
      theme(panel.grid = element_blank()) +
      facet_grid(group ~ .)
    ggsave(paste(plots_path, "/mean_speed.tiff", sep =""))
    
    # sd_speed
    
    min(df$sd_speed)
    max(df$sd_speed)
    
    ggplot(df, aes(x = sd_speed)) +
      theme_bw() +
      geom_histogram(binwidth = 0.05, aes(y=..count../sum(..count..))) +
      xlim(-1,1.5) +
      ylab("proportion") +
      theme(panel.grid = element_blank()) +
      facet_grid(group ~ .)
    ggsave(paste(plots_path, "/sd_speed.tiff", sep =""))
    
    # # mean_angle
    # 
    # min(df$mean_angle)
    # max(df$mean_angle)
    # 
    # ggplot(df, aes(x = mean_angle)) +
    #   theme_bw() +
    #   geom_histogram(binwidth = 0.05, aes(y=..count../sum(..count..))) +
    #   xlim(-1,1.5) +
    #   ylab("proportion") +
    #   theme(panel.grid = element_blank()) +
    #   facet_grid(group ~ .)
    # ggsave(paste(plots_path, "/mean_angle.tiff", sep =""))
    
    # sd_angle
    
    min(df$sd_angle)
    max(df$sd_angle)
    
    ggplot(df, aes(x = sd_angle)) +
      theme_bw() +
      geom_histogram(binwidth = 0.05, aes(y=..count../sum(..count..))) +
      xlim(-1,1.5) +
      ylab("proportion") +
      theme(panel.grid = element_blank()) +
      facet_grid(group ~ .)
    ggsave(paste(plots_path, "/sd_angle.tiff", sep =""))
    
    # absolute_max_displacement
    
    min(df$absolute_max_displacement)
    max(df$absolute_max_displacement)
    
    ggplot(df, aes(x = absolute_max_displacement)) +
      theme_bw() +
      geom_histogram(binwidth = 0.05, aes(y=..count../sum(..count..))) +
      xlim(-1,1.5) +
      ylab("proportion") +
      theme(panel.grid = element_blank()) +
      facet_grid(group ~ .)
    ggsave(paste(plots_path, "/absolute_max_displacement.tiff", sep =""))
    
    # tortuosity
    
    min(df$tortuosity)
    max(df$tortuosity)
    
    ggplot(df, aes(x = tortuosity)) +
      theme_bw() +
      geom_histogram(binwidth = 0.05, aes(y=..count../sum(..count..))) +
      xlim(-1,1.5) +
      ylab("proportion") +
      theme(panel.grid = element_blank()) +
      facet_grid(group ~ .)
    ggsave(paste(plots_path, "/tortuosity.tiff", sep =""))
    
    
    ### grouping ###
    
    rel_groups <- c(0,5,7,9)
    
    grouped_df <- df %>%
      #filter(group %in% rel_groups) %>%
      group_by(group) %>%
      dplyr::summarise(n = n(),
                       mean_time = mean(mean_time),
                       sd_time = sd(mean_time, na.rm = T),
                       mean_speed = mean(mean_speed),
                       sd_speed = mean(mean_speed),
                       mean_tortuosity = mean(tortuosity),
                       sd_tortuosity = sd(tortuosity),
                       absolute_max_displacement = mean(absolute_max_displacement))
    
    rand_df <- sample_n(df, 1000000)
    
    long_df <- rand_df %>%
      dplyr::select(-c(1,2)) %>%
      #filter(group %in% rel_groups) %>%
      melt(id.vars = c("group"))
    
    ### boxplots ###
    
    dir.create(paste0("../plots/exploring_groups_by_features/", k, "/boxplots"), showWarnings = FALSE)
    plots_path <- paste0("../plots/exploring_groups_by_features/", k, "/boxplots")
    
    
    
    # mean_time
    
    small_df <- long_df %>%
      filter(variable == "mean_time")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(0,100) +
      ylab("mean_time (s)") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/mean_time.tiff", sep =""))
    
    
    # mean_speed
    
    small_df <- long_df %>%
      filter(variable == "mean_speed")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(-1,1.5) +
      ylab("mean_speed (m/s)") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/mean_speed.tiff", sep =""))
    
    # sd_speed
    
    small_df <- long_df %>%
      filter(variable == "sd_speed")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(-1,1.5) +
      ylab("sd_speed (m/s)") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/sd_speed.tiff", sep =""))
    
    
    # tortuosity
    
    small_df <- long_df %>%
      filter(variable == "tortuosity")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(-1,2) +
      ylab("tortuosity") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/tortuosity.tiff", sep =""))
    
    
    # # mean_angle
    # 
    # small_df <- long_df %>%
    #   filter(variable == "mean_angle")
    # 
    # small_df$group <- as.factor(small_df$group)
    # 
    # ggplot(small_df, aes(x = group, y = value, color = group)) +
    #   theme_bw() +
    #   geom_boxplot(size = 0.5) +
    #   ylim(-1,1.5) +
    #   ylab("mean_angle") +
    #   theme(panel.grid = element_blank())
    # ggsave(paste(plots_path, "/mean_angle.tiff", sep =""))
    
    
    # sd_angle
    
    small_df <- long_df %>%
      filter(variable == "sd_angle")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(-1,1.5) +
      ylab("sd_angle") +
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/sd_angle.tiff", sep =""))
    
    
    # absolute_max_displacement
    
    small_df <- long_df %>%
      filter(variable == "absolute_max_displacement")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(-2,4) +
      ylab("absolute_max_displacement") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/absolute_max_displacement.tiff", sep =""))
    
  }
}

exploring_groups_by_attributes <- function(tw, ks){
  
  for (k in ks){
    
    path <- paste0("../outputs/7 smoothed_groups/tw_9/k_",k,"/all_tags.csv")
    df <- read.csv(path)
    
    df$group <- as.factor(df$group)
    
    dir.create("../plots", showWarnings = FALSE)
    dir.create("../plots/exploring_groups_by_attributes", showWarnings = FALSE)
    dir.create(paste0("../plots/exploring_groups_by_attributes/",k), showWarnings = FALSE)
    
    
    ### histograms ###
    
    dir.create(paste0("../plots/exploring_groups_by_attributes/",k,"/histograms"), showWarnings = FALSE)
    plots_path <- paste0("../plots/exploring_groups_by_attributes/",k,"/histograms")
    
    # mean time
    
    ggplot(df, aes(x = dT, fill = group)) +
      theme_bw() +
      geom_histogram(binwidth = 4, aes(y=..count../tapply(..count.., ..fill.. ,sum)[..fill..])) +
      xlim(0,20) +
      ylab("proportion") +
      #scale_y_continuous(trans = "log10", breaks = c(0,10,1000,100000,10000000),
      #labels = c(0,10,1000,100000,10000000)) +
      facet_grid(group ~ .) +
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/mean_time.tiff", sep =""))
    
    
    # step length
    
    ggplot(df, aes(x = distance, fill = group)) +
      theme_bw() +
      geom_histogram(binwidth = 1, aes(y=..count../tapply(..count.., ..fill.. ,sum)[..fill..])) +
      xlim(0,100) +
      ylab("proportion") +
      #scale_y_continuous(trans = "log10", breaks = c(0,10,1000,100000,10000000),
      #labels = c(0,10,1000,100000,10000000)) +
      facet_grid(group ~ .) +
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/step_length.tiff", sep =""))
    
    
    # speed
    
    ggplot(df, aes(x = spd, fill = group)) +
      theme_bw() +
      geom_histogram(binwidth = 0.05, aes(y=..count../tapply(..count.., ..fill.. ,sum)[..fill..])) +
      xlim(0,15) +
      ylab("proportion") +
      #scale_y_continuous(trans = "log10", breaks = c(0,10,1000,100000,10000000),
      #labels = c(0,10,1000,100000,10000000)) +
      facet_grid(group ~ .) +
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/speed.tiff", sep =""))
    
    
    
    # angle
    
    ggplot(df, aes(x = angle, fill = group)) +
      theme_bw() +
      geom_histogram(binwidth = 2, aes(y=..count../tapply(..count.., ..fill.. ,sum)[..fill..])) +
      xlim(0,360) +
      ylab("proportion") +
      #scale_y_continuous(trans = "log10", breaks = c(0,10,1000,100000,10000000),
      #labels = c(0,10,1000,100000,10000000)) +
      facet_grid(group ~ .) +
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/angles.tiff", sep =""))
    
    
    
    rand_df <- sample_n(df, 1000000)
    
    long_df <- rand_df %>%
      dplyr::select(-c(1:7)) %>%
      #filter(group %in% rel_groups) %>%
      melt(id.vars = c("group"))
    
    
    ### boxplots ###
    
    dir.create(paste0("../plots/exploring_groups_by_attributes/", k, "/boxplots"), showWarnings = FALSE)
    plots_path <- paste0("../plots/exploring_groups_by_attributes/", k, "/boxplots")
    
    
    
    # mean_time
    
    small_df <- long_df %>%
      filter(variable == "dT")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(0,50) +
      ylab("mean_time (s)") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/mean_time.tiff", sep =""))
    
    
    # step length
    
    small_df <- long_df %>%
      filter(variable == "distance")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(0,50) +
      ylab("step length (m)") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/step_length.tiff", sep =""))
    
    
    # speed
    
    small_df <- long_df %>%
      filter(variable == "spd")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      ylim(0,50) +
      ylab("speed (m/s)") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/speed.tiff", sep =""))
    
    
    # angle
    
    small_df <- long_df %>%
      filter(variable == "angle")
    
    small_df$group <- as.factor(small_df$group)
    
    ggplot(small_df, aes(x = group, y = value, color = group)) +
      theme_bw() +
      geom_boxplot(size = 0.5) +
      #ylim(0,1000) +
      ylab("angle") + 
      theme(panel.grid = element_blank())
    ggsave(paste(plots_path, "/angle.tiff", sep =""))
    
  }
}

tw <- 9
ks <- c(2,3,4,5,6)

features_statistics(tw)
exploring_groups_by_features(tw, ks)
exploring_groups_by_attributes(tw,ks)






