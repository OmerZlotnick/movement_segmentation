import pandas as pd

import omer_functions as om
import os

os.makedirs("../outputs/", exist_ok = True)

### calculate angles ###
# calculates to each step the change in angle from the previous step (0-360) and adds it as a column

input_path = "../outputs/2 filtered_data/"
output_path = "../outputs/3 data_with_angles/"

#om.calculate_angles(input_path, output_path)


### calculate features ###
# calculates the wanted features($features_list$) for groups of $tw$ steps
# if $normalized$ is True then before saving the code performs normalization of the features,
# otherwise, the raw features are saved in the files

tw = 9
input_path = "../outputs/3 data_with_angles/"
output_path = "../outputs/4 features/"
normalized = True
features_list = ["total_distance", "mean_speed", "sd_speed", "mean_angle",
                 "sd_angle", "net_displacement", "max_displacement",
                 "absolute_max_displacement", "tortuosity"]

# full features list (all options)
#features_list = ["total_distance", "mean_speed", "sd_speed", "mean_angle",
#                 "sd_angle", "net_displacement", "max_displacement",
#                 "absolute_max_displacement", "tortuosity"]

#om.calculate_features(input_path, output_path, tw, normalized, features_list, sep_files = False)


### calculate kmeans ###
# divide all the $tw$ series to k different groups (for different k's defined in $k_range$)
# using the features named in $rel_features_list$
# $filt_lim$ - used to filtrate the $tw$ series with one or more features outside the range of
# -sd * filt_lim - +sd * filt_lim

input_path = "../outputs/4 features/tw_" + str(tw) + "/features_all_tags_normalized.csv"
output_path = "../outputs/5 kmeans/"
k_range = [2,3,4,5,6]
filt_lim = 10
rel_features_list = ["mean_speed", "sd_speed", "sd_angle",
                     "absolute_max_displacement", "tortuosity"]

#om.calculate_kmeans(input_path, output_path, tw, normalized, k_range, rel_features_list, filt_lim = filt_lim, sep_files = False, plot = False)


### groups_to_locations ###
# uses the kmeans results to assign a group for each step, based on:
# the group that was assigned to most $tw$ series the steps was part of
# the groups that were assigned to the series in which the step was close to the middle of the series ($dist_weight$)
# the groups that were assigned to the series with the best mean dT ($time_weight$)

input_path = "../outputs/5 kmeans/"
locations_path = "../outputs/3 data_with_angles/"
output_path = "../outputs/6 groups_to_locations/"

#om.groups_to_locations(input_path, locations_path, output_path, tw, k_range, dist_weight = 1, time_weight = 1, sep_files = False)


### smooth ###
# smoothes the groups assigned to all steps, so no group will be assigned to less than or exactly
# $max_window$ steps

input_path = "../outputs/6 groups_to_locations/"
output_path = "../outputs/7 smoothed_groups/"
max_window = 3

#om.smooth(input_path, output_path, tw, k_range, max_window, sep_files = False)


### transitions ###
# calculates the number of transitions from each group x to each group y

input_path = "../outputs/7 smoothed_groups/"
output_path = "../outputs/8 transitions/"

om.transitions(input_path, output_path, tw, k_range)


### get_examples ###
# creates one day file as example for each tag (with the assigned groups)

input_path = "../outputs/7 smoothed_groups/"
output_path = "../outputs/9 examples/"

om.get_examples(input_path, output_path, tw, k_range)


### get_distributions ###
# creates distributions of step lengths and angles for each k-means group

input_path = "../outputs/7 smoothed_groups/"
output_path = "../outputs/10 distributions/"

#om.get_distributions(input_path, output_path, tw, k_range)






