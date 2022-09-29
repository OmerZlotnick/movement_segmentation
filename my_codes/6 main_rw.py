import pandas as pd

import omer_functions as om
import os

os.makedirs("../outputs/11 artificial_trajectories", exist_ok = True)

tw = 9
k_range = [2,3,4,5,6]

num_of_trajectories = 10

os.makedirs("../outputs/11 artificial_trajectories/" + "tw_" + str(tw), exist_ok = True)

for k in k_range:

    print("k: " + str(k))

    os.makedirs("../outputs/11 artificial_trajectories/" + "tw_" + str(tw) + "/k_" + str(k), exist_ok = True)

    transition_path = "../outputs/8 transitions/" + "tw_" + str(tw) + "/k_" + str(k) + "/transition_matrix.csv"
    distributions_path = "../outputs/10 distributions/" + "tw_" + str(tw) + "/k_" + str(k) + "/all_tags.csv"
    number_of_steps = 10800     # one day

    for i in range(num_of_trajectories):

        output_path = "../outputs/11 artificial_trajectories/" + "tw_" + str(tw) + "/k_" + str(k) + "/" + str(i+1) + ".csv"

        om.multistate_random_walk(transition_path, output_path, distributions_path, number_of_steps)







