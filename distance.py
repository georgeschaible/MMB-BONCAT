from EMAN2 import *
import math
import matplotlib.pyplot as plt
from PIL import Image
import numpy as np
import csv


def append_to_csv(data, distance):
    with open(distance, 'a', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(data)

consortia=EMData()
consortia.read_image('single_particles_filtered.hdf')
consortia_array=consortia.numpy()
print(consortia_array)
x_max = consortia_array.shape[0]
x_origin=int(x_max/2)

y_max = consortia_array.shape[1]
y_origin = int(y_max/2)

print(str(x_origin))
print(str(y_origin))

current_x = x_origin

def generate_output_data():
    output_data = []
    global current_x
    while current_x <= x_max:
        std_dev = np.std(consortia_array[(current_x):(int(current_x)+3)])
        print(std_dev)
        current_x = current_x + 1
        if float(std_dev) < .01:
            print("The radius is ", str(current_x - x_origin), "pixels")
            current_x = (x_max + 1)
            value = current_x
            output_data.append(value)
    return output_data

output_data = generate_output_data()

append_to_csv(output_data, 'distance.csv')

