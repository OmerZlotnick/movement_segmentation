import numpy as np
import math
import os
import pandas as pd
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from os import listdir
from sklearn.cluster import KMeans
import random
from os import walk
#import seaborn as sns

def list_of_lists_to_file(lst, output_path, column_names = None):

    out_file = open(output_path, "w")

    if column_names != None:
        for col in column_names[:-1]:
            out_file.write(col + ",")
        out_file.write(column_names[-1] + "\n")

    for row in lst:
        for col in row[:-1]:
            out_file.write(str(col) + ",")
        out_file.write(str(row[-1]) + "\n")

    out_file.close()

def create_whole_path(whole_path):
    whole_path_sep = whole_path.split("/")

    for i in range(len(whole_path_sep)):
        new_path = ""

        for j in range(i):
            new_path = new_path + whole_path_sep[j] + "/"

        new_path = new_path.strip("/")

        os.makedirs(new_path, exist_ok=True)

def pca_plot(lst_of_lst, column_names, group_col, path):

    data_df = pd.DataFrame(lst_of_lst, columns = column_names)

    pca = PCA(n_components=2)
    principalComponents = pca.fit_transform(data_df)
    principalDf = pd.DataFrame(data = principalComponents, columns = ['principal component 1', 'principal component 2'])

    fig = plt.figure(figsize = (8,8))
    ax = fig.add_subplot(1,1,1)
    ax.set_xlabel('Principal Component 1', fontsize = 15)
    ax.set_ylabel('Principal Component 2', fontsize = 15)
    ax.set_title('PCA', fontsize = 20)

    groups = list(np.unique(np.array(group_col)))
    colors = cm.rainbow(np.linspace(0, 1, len(groups)))

    for i_group in range(len(groups)):

        rel_rows = group_col == groups[i_group]

        ax.scatter(principalDf.loc[rel_rows, 'principal component 1'], principalDf.loc[rel_rows, 'principal component 2']
                   , c = colors[i_group], s = 50)
    ax.grid()
    plt.savefig(path)

def calculate_angles(input_path, output_path):

    print("***** calculate_angles *****")

    # create list of input files
    file_list = [f for f in listdir(input_path) if f.endswith(".csv")]

    # create output path
    os.makedirs(output_path, exist_ok=True)

    for f in file_list:

        tag = f[0:3]
        print("tag: " + tag)

        locations_df = pd.read_csv(input_path + f, index_col = False)
        angles_lst = [0.0,0.0]

        for i in range(2, len(locations_df)):

            x_i = locations_df.loc[i, "X"]
            y_i = locations_df.loc[i, "Y"]
            x_minus_1 = locations_df.loc[i-1, "X"]
            y_minus_1 = locations_df.loc[i-1, "Y"]
            x_minus_2 = locations_df.loc[i-2, "X"]
            y_minus_2 = locations_df.loc[i-2, "Y"]

            d_x_1 = x_i - x_minus_1
            d_y_1 = y_i - y_minus_1
            d_x_2 = x_minus_1 - x_minus_2
            d_y_2 = y_minus_1 - y_minus_2

            angle_1 = (math.degrees(math.atan2(d_y_1,d_x_1)) + 360) % 360
            angle_2 = (math.degrees(math.atan2(d_y_2,d_x_2)) + 360) % 360

            d_angle = (angle_2 - angle_1 + 360) % 360

            angles_lst.append(d_angle)

        locations_df["angle"] = angles_lst
        locations_df.to_csv(output_path + f, index = False)

def calculate_features(input_path, output_path, tw, normalized, features_list, sep_files = False):

    print("***** calculate_features *****")

    output_path = output_path + "tw_" + str(tw) + "/"

    # create list of input files
    file_list = [f for f in listdir(input_path) if f.endswith(".csv")]

    # create output path
    os.makedirs(output_path, exist_ok=True)

    columns_names = ["tag", "first_point", "last_point", "mean_time"] + features_list

    all_tags_lst = []
    all_tags_normalized_lst = []

    for file in file_list:

        tag = file[0:3]
        print("tag: " + tag)

        # read file for chosen tag
        df = pd.read_csv(input_path + tag + ".csv")

        original_columns_names = list(df.columns)
        df.dT = df["dT"].astype("float")

        big_lst = df.values.tolist()[:-1]

        tag_tw_lst = []

        for i in range(2,len(big_lst)+1):
            #print(i)

            if (i > tw):
                c_lst = big_lst[(i - tw):i]
            else:
                c_lst = big_lst[1:i]

            n = len(c_lst)

            c_sum_dT = 0
            c_total_distance = 0
            spds = []
            angles = []
            for row in c_lst:
                c_sum_dT += row[original_columns_names.index("dT")]  # in seconds
                c_total_distance += row[original_columns_names.index("distance")]
                spds.append(row[original_columns_names.index("spd")])
                angles.append(row[original_columns_names.index("angle")])


            c_mean_dT = c_sum_dT / n
            tw_row = [int(tag), i - n, i - 1, int(round(c_mean_dT,0))]

            if "total_distance" in features_list:
                tw_row.append(round(c_total_distance,3))

            if "mean_speed" in features_list:
                c_mean_spd = c_total_distance / c_sum_dT
                tw_row.append(round(c_mean_spd,3))

            if "sd_speed" in features_list:
                c_sd_spd = np.std(spds)

                if (i == 2):
                    c_sd_spd = 0

                tw_row.append(round(c_sd_spd,3))

            if "mean_angle" in features_list:
                c_mean_angle = np.mean(angles)
                tw_row.append(np.round(c_mean_angle,3))

            if "sd_angle" in features_list:
                c_sd_angle = np.std(angles)
                tw_row.append(np.round(c_sd_angle,3))


            first_long = c_lst[0][original_columns_names.index("X")]
            first_lat = c_lst[0][original_columns_names.index("Y")]

            last_long = c_lst[n-1][original_columns_names.index("X")]
            last_lat = c_lst[n-1][original_columns_names.index("Y")]

            # calculate net displacement
            net_displacement = np.sqrt((last_long - first_long) ** 2 + (last_lat - first_lat) ** 2)

            if "net_displacement" in features_list:
                tw_row.append(round(net_displacement,3))

            if "max_displacement" in features_list:
                # first location to furthest
                lons = np.array([item[original_columns_names.index("X")] for item in c_lst])
                lats = np.array([item[original_columns_names.index("Y")] for item in c_lst])

                dists = np.sqrt((lons - first_long) ** 2 + (lats - first_lat) ** 2)
                max_displacement = max(dists)
                tw_row.append(round(max_displacement,3))

            if "absolute_max_displacement" in features_list:
                # two furthest locations
                abs_max_displacement = 0

                for j in range(n):
                    j_lon = c_lst[j][original_columns_names.index("X")]
                    j_lat = c_lst[j][original_columns_names.index("Y")]

                    for k in range(n):
                        k_lon = c_lst[k][original_columns_names.index("X")]
                        k_lat = c_lst[k][original_columns_names.index("Y")]

                        dist_j_k = np.sqrt((j_lon - k_lon) ** 2 + (j_lat - k_lat) ** 2)

                        if (dist_j_k > abs_max_displacement):
                            abs_max_displacement = dist_j_k

                tw_row.append(round(abs_max_displacement,3))

            if "tortuosity" in features_list:
                if (i == 2):
                    tortuosity = 0
                elif net_displacement == 0:
                    tortuosity = 0
                else:
                    tortuosity = c_total_distance / net_displacement

                tw_row.append(round(tortuosity,3))

            tag_tw_lst.append(tw_row)

        if not normalized:
            if sep_files:
                os.makedirs(output_path + tag, exist_ok=True)

                file_output_path = output_path + tag + "/features_df.csv"
                list_of_lists_to_file(tag_tw_lst, file_output_path, columns_names)

            all_tags_lst = all_tags_lst + tag_tw_lst

        else:
            normalized_mat = np.array(tag_tw_lst)

            for col_i in range(4,len(columns_names)):
                col = normalized_mat[:,col_i]
                normalized_mat[:,col_i] = (col - np.median(col)) / (np.quantile(col, q = 0.9) - np.quantile(col, q = 0.1))

            normalized_lst = list(np.round(normalized_mat,3))

            if sep_files:
                os.makedirs(output_path + tag, exist_ok=True)
                file_output_path = output_path + tag + "/features_df_normalized.csv"

                list_of_lists_to_file(normalized_lst, file_output_path, columns_names)

            all_tags_normalized_lst = all_tags_normalized_lst + normalized_lst


    if not normalized:
        all_tags_path = output_path + "/features_all_tags.csv"
        list_of_lists_to_file(all_tags_lst, all_tags_path, columns_names)

    else:
        all_tags_normalized_path = output_path + "/features_all_tags_normalized.csv"
        list_of_lists_to_file(all_tags_normalized_lst, all_tags_normalized_path, columns_names)


def calculate_kmeans(input_path, output_path, tw, normalized, k_range, rel_features_list, filt_lim = None, sep_files = False, plot = False):

    print("***** calculate_kmeans *****")

    input_path = input_path
    output_path = output_path + "tw_" + str(tw) + "/"

    columns_list = ["tag", "first_point", "last_point", "mean_time"] + rel_features_list

    all_tags_df = pd.read_csv(input_path)
    all_tags_df = all_tags_df.loc[:,columns_list]
    columns_names = list(all_tags_df.columns)

    if normalized and filt_lim != None:
        good_indexes = np.array((range(len(all_tags_df.index))))

        for col in list(all_tags_df.columns)[4:]:
            col_good_indexes = all_tags_df[(all_tags_df[col] < filt_lim) &
                                            (all_tags_df[col] > (-1 * filt_lim))].index

            good_indexes = np.intersect1d(good_indexes, col_good_indexes)

        fixed_all_tags_df = all_tags_df.iloc[good_indexes,:]

    else:
        fixed_all_tags_df = all_tags_df

    big_data_a = np.array(fixed_all_tags_df.values.tolist())

    rel_big_data_a = big_data_a[:,4:]

    for k in k_range:
        print("k: " + str(k))
        os.makedirs(output_path + "k_" + str(k), exist_ok=True)

        model = KMeans(k)
        model.fit(rel_big_data_a)
        full_kmeans = model.predict(rel_big_data_a)

        big_lst = list(rel_big_data_a)

        if plot:
            path_to_fig = output_path + "k_" + str(k) + "/" + "pca_plot.jpg"
            pca_plot(big_lst, columns_names[4:], full_kmeans, path_to_fig)

        mask = np.array(list(fixed_all_tags_df.index)).astype("int")
        groups = np.array([-1] * len(all_tags_df))
        groups[mask] = full_kmeans

        all_tags_df["group"] = groups
        all_tags_path = output_path + "k_" + str(k) + "/" + "all_tags.csv"
        all_tags_df.to_csv(all_tags_path, index = False)

        if sep_files:
            for tag in all_tags_df["tag"].unique():
                tag_df = all_tags_df[all_tags_df["tag"] == tag]
                kmeans_path = output_path + "k_" + str(k) + "/" + tag + ".csv"
                tag_df.to_csv(kmeans_path, index=False)

def groups_to_locations(input_path, locations_path, output_path, tw, k_range, dist_weight = 1, time_weight = 1, sep_files = False):

    print("***** groups_to_locations *****")

    output_path = output_path + "tw_" + str(tw) + "/"
    os.makedirs(output_path, exist_ok=True)

    input_path = input_path + "tw_" + str(tw) + "/"

    for k in k_range:

        print("k: " + str(k))
        k_lst = []
        os.makedirs(output_path + "/k_" + str(k), exist_ok=True)
        k_input_path = input_path + "k_" + str(k) + "/all_tags.csv"

        all_tags_df = pd.read_csv(k_input_path)

        for tag in all_tags_df["tag"].unique():
            print("tag: " + str(int(tag)))
            kmeans_df = all_tags_df[all_tags_df["tag"] == tag]

            tag_location_path = locations_path + str(int(tag)) + ".csv"
            filtered_df = pd.read_csv(tag_location_path)

            rel_cols = ["TAG", "X", "Y", "LON", "LAT", "dateTime", "date",  "distance", "dT", "spd", "angle"]
            filtered_df = filtered_df.loc[:,rel_cols]
            filtered_df["dT"] = round(filtered_df["dT"], 0)
            filtered_df["distance"] = round(filtered_df["distance"], 3)
            filtered_df["spd"] = round(filtered_df["spd"], 3)
            filtered_df["angle"] = round(filtered_df["angle"], 3)

            kmeans_time_col = np.array(kmeans_df["mean_time"])
            kmeans_time_weight_col = (1 / kmeans_time_col) ** time_weight
            kmeans_group_col = np.array(kmeans_df["group"])

            final_kmeans = np.array([-1] * len(filtered_df))

            for i in range(tw,len(filtered_df) - tw):

                rel_kmeans_group_col = kmeans_group_col[i-1:i+tw-1]
                rel_kmeans_time_weight_col = kmeans_time_weight_col[i-1:i+tw-1]

                k_count_dict = {}
                k_weighted_dict = {}

                for k_i in range(-1,k):
                    k_count_dict[k_i] = 0
                    k_weighted_dict[k_i] = 0

                mid = tw // 2 + 1

                for j in range(1,tw+1):
                    dist = abs(j - mid) + 1
                    dist_weight = (1 / dist) ** dist_weight

                    time_weight = rel_kmeans_time_weight_col[j-1]
                    row_k = rel_kmeans_group_col[j-1]

                    k_count_dict[row_k] += 1
                    k_weighted_dict[row_k] += time_weight * dist_weight

                # first - try to find the max
                max_val = max(k_count_dict)
                max_keys = [m for m, v in k_count_dict.items() if v == max_val]

                if len(max_keys) == 1:
                    chosen_group = max_keys[0]
                else:
                    # if there isn't a majority, check highest rank
                    max_rank = max(k_weighted_dict)
                    max_rank_keys = [g for g, v in k_weighted_dict.items() if v == max_val]

                    if len(max_rank_keys) == 1:
                        chosen_group = max_rank_keys[0]

                    else:
                        chosen_group = rel_kmeans_group_col[mid]

                final_kmeans[i] = chosen_group

            filtered_df["group"] = final_kmeans
            filtered_lst = filtered_df.values.tolist()
            k_lst = k_lst + filtered_lst

            if sep_files:
                tag_output_path = output_path + "/k_" + str(k) + "/" + tag + ".csv"
                filtered_df.to_csv(tag_output_path, index=False)

        k_df = pd.DataFrame(k_lst, columns = filtered_df.columns)
        k_output_path = output_path + "/k_" + str(k) + "/all_tags.csv"
        k_df.to_csv(k_output_path, index=False)

def smooth(input_path, output_path, tw, k_range, max_window, sep_files = False):

    print("***** smooth *****")

    output_path = output_path + "tw_" + str(tw) + "/"
    os.makedirs(output_path, exist_ok=True)

    input_path = input_path + "tw_" + str(tw) + "/"

    for k in k_range:

        print("k: " + str(k))

        k_lst = []

        os.makedirs(output_path + "k_" + str(k), exist_ok=True)

        path_to_smoothed = input_path + "k_" + str(k) + "/all_tags.csv"
        input_df = pd.read_csv(path_to_smoothed)

        for tag in input_df["TAG"].unique():
            print("tag: " + str(tag))

            smoothed_df = input_df.loc[input_df["TAG"] == tag,:]
            smoothed_a = np.array(smoothed_df["group"])
            smoothed_df = smoothed_df.drop(columns = ["group"])

            for i in range(1,len(smoothed_a)-max_window):
                prev_g = smoothed_a[i-1]
                curr_g = smoothed_a[i]
                if curr_g != prev_g:
                    following_g = [g for g in smoothed_a[i+1:i+max_window+1]]
                    for g in following_g:
                        if prev_g == g:
                            smoothed_a[i] = g
                            break

            smoothed_df["group"] = list(smoothed_a)
            smoothed_lst = smoothed_df.values.tolist()
            k_lst = k_lst + smoothed_lst

            if sep_files:
                tag_output_path = output_path + "k_" + str(k) + "/" + str(tag) + ".csv"
                smoothed_df.to_csv(tag_output_path, index=False)

        k_df = pd.DataFrame(k_lst, columns=smoothed_df.columns)
        k_output_path = output_path + "k_" + str(k) + "/all_tags.csv"
        k_df.to_csv(k_output_path, index=False)

def transitions(input_path, output_path, tw, k_range):

    print("***** transitions *****")

    output_path = output_path +"tw_" + str(tw) + "/"
    os.makedirs(output_path, exist_ok=True)

    input_path = input_path + "tw_" + str(tw) + "/"

    for k in k_range:

        print("k: " + str(k))
        os.makedirs(output_path + "k_" + str(k), exist_ok=True)

        path_to_input = input_path + "k_" + str(k) + "/all_tags.csv"
        input_df = pd.read_csv(path_to_input)

        transitions_dict = {}
        for i in range(-1,k):
            for j in range(-1,k):
                transitions_dict[(i,j)] = 0

        kmeans = np.array(input_df["group"])

        for i in range(1,len(kmeans)):
            former_g = kmeans[i-1]
            current_g = kmeans[i]

            transitions_dict[(former_g,current_g)] += 1

        transitions_df = pd.DataFrame(0, index = range(-1,k), columns = range(-1,k))

        for i in range(-1,k):
            for j in range(-1,k):
                transitions_df.iloc[i+1,j+1] = transitions_dict[(i,j)]

        k_output_path = output_path + "k_" + str(k) + "/transition_matrix.csv"
        transitions_df.to_csv(k_output_path)

def get_examples(input_path, output_path, tw, k_range):

    print("***** get_examples *****")

    output_path = output_path + "tw_" + str(tw) + "/"
    input_path = input_path + "tw_" + str(tw) + "/"

    examples_dict = {}

    examples_dict["567"] = []
    examples_dict["568"] = []
    #examples_dict["572"] = []
    examples_dict["680"] = []
    examples_dict["682"] = []

    examples_dict["567"].append("2021-06-30")
    examples_dict["567"].append("2021-07-30")
    examples_dict["567"].append("2021-08-15")
    examples_dict["568"].append("2021-06-30")
    examples_dict["568"].append("2021-07-30")
    examples_dict["568"].append("2021-08-15")
    #examples_dict["572"].append("2021-07-02")
    #examples_dict["572"].append("2021-07-03")
    #examples_dict["572"].append("2021-07-04")
    examples_dict["680"].append("2022-03-30")
    examples_dict["680"].append("2022-04-30")
    examples_dict["680"].append("2022-05-30")
    examples_dict["682"].append("2022-03-10")
    examples_dict["682"].append("2022-04-30")
    examples_dict["682"].append("2022-05-30")

    os.makedirs(output_path, exist_ok = True)

    for k in k_range:
        print("k: " + str(k))
        k_input_path = input_path + "k_" + str(k) + "/all_tags.csv"

        full_df = pd.read_csv(k_input_path)
        os.makedirs(output_path + "k_" + str(k) + "/", exist_ok = True)

        for tag, date_lst in examples_dict.items():
            for date in date_lst:
                print("tag: " + str(tag))
                date_dots = date.replace("/", ".")

                ex_df = full_df[(full_df["TAG"] == int(tag)) & (full_df["date"] == date)]
                ex_path = output_path + "k_" + str(k) + "/" + str(tag) + "_" + str(date_dots) + ".csv"

                ex_df.to_csv(ex_path, index = False)

def get_distributions(input_path, output_path, tw, k_range):

    print("***** get_distributions *****")

    input_path = input_path + "tw_" + str(tw) + "/"
    output_path = output_path + "tw_" + str(tw) + "/"

    os.makedirs(output_path, exist_ok = True)

    for k in k_range:
        print("k: " + str(k))
        k_input_path = input_path + "k_" + str(k) + "/all_tags.csv"
        k_df = pd.read_csv(k_input_path)

        os.makedirs(output_path + "k_" + str(k) + "/", exist_ok = True)

        sample_k_lst = []
        for k_i in range(k):
            group_length = len(k_df[k_df["group"] == k_i])
            sample_k = (k_df[k_df["group"] == k_i]).sample(min(10000,group_length)).values.tolist()
            sample_k_lst = sample_k_lst + sample_k

        sample_k_df = pd.DataFrame(sample_k_lst, columns = k_df.columns)
        sample_k_path = output_path + "k_" + str(k) + "/all_tags.csv"
        sample_k_df.to_csv(sample_k_path, index = False)



def multistate_random_walk(transition_path, output_path, distributions_path, number_of_steps):

    transition_df = pd.read_csv(transition_path, index_col = 0)
    distributions_df = pd.read_csv(distributions_path)

    ks_list = list(distributions_df["group"].unique())

    # create initial probabilities array
    counts_a = np.array(transition_df.iloc[1:,1:]).sum(axis = 1)
    transition_a = (np.round((counts_a / sum(counts_a)),4)) * 10000

    initial_state_array = []
    for k in ks_list:
        initial_state_array = initial_state_array + ([k] * int(transition_a[k]))
    initial_state_array = np.array((initial_state_array))

    # create distribution array for each state
    step_length_dist_dict = {}
    angle_dist_dict = {}

    for k in ks_list:
        step_length_dist_dict[k] = np.array(distributions_df[distributions_df["group"] == k]["distance"])
        angle_dist_dict[k] = np.array(distributions_df[distributions_df["group"] == k]["angle"])

    rel_transition_df = transition_df.iloc[1:,1:]
    prob_transition_lst = []

    for k in list(rel_transition_df.columns):
        counts = np.array(rel_transition_df.iloc[int(k),:])
        probabilities = []

        for i in range(len(counts)):
            new_val = counts[i] / sum(counts)
            probabilities.append(round(new_val,4))

        prob_transition_lst.append(probabilities)

    prob_transition_mat = np.array(prob_transition_lst)
    prob_dict = {}
    for k_1 in ks_list:

        k_prob_lst = []

        for k_2 in ks_list:
            prob = prob_transition_mat[k_1,k_2]
            k_prob_lst = k_prob_lst + ([k_2] * round((10000 * prob)))

        prob_dict[k_1] = np.array(k_prob_lst)

    curr_state = np.random.choice(initial_state_array)
    curr_x = 0
    curr_y = 0
    new_step_length = 0
    new_turning_angle = 0
    new_angle = 0

    follow_lst = []
    follow_column_lst = ["state", "x", "y", "step_length", "angle"]

    for i in range(number_of_steps):

        new_follow_row = [curr_state, round(curr_x,3), round(curr_y,3), round(new_step_length,3), round(new_angle,3)]
        follow_lst.append(new_follow_row)

        # select state
        transitions_array = prob_dict[curr_state]
        curr_state = np.random.choice(transitions_array)

        new_step_length = np.random.choice(step_length_dist_dict[curr_state])
        new_turning_angle = np.random.choice(angle_dist_dict[curr_state])
        new_angle = (new_angle + new_turning_angle) % 360

        curr_x = curr_x + math.cos(math.radians(new_angle)) * new_step_length
        curr_y = curr_y + math.sin(math.radians(new_angle)) * new_step_length

    follow_df = pd.DataFrame(follow_lst, columns = follow_column_lst)
    follow_df.to_csv(output_path)

