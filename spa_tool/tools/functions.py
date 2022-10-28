################## Function Definitions ##########################################
import math
import pandas as pd
from spa_tool.tools.trips import *
from spa_tool.tools.tours import *
from spa_tool.tools.joint_tours import *
from spa_tool.tools.joint_ultrips import *
from spa_tool.tools.configs import *

def add_place_distance(route_file, place_file, out_file):
    # read in ROUTE records into a data frame object
    df_route = pd.read_csv(const.get('IN_DIR') + route_file, quotechar='"', encoding='ISO-8859-1')
    # read in PLACE records into a data frame object, df_place       
    df_place = pd.read_csv(const.get('IN_DIR') + place_file, quotechar='"', encoding='ISO-8859-1')
          
    df_route = df_route.rename(columns={'DPLANO': 'PLANO'})      
    route_grouped = df_route.groupby(['SAMPN','PERNO','OPLANO','PLANO']).sum().reset_index()
    df_place = pd.merge(df_place, route_grouped[['SAMPN','PERNO','PLANO','Distance']], how='left', on=['SAMPN','PERNO','PLANO'])
    
    #write out to a new place file
    df_place.to_csv(const.get('IN_DIR') + out_file)


def distance_on_unit_sphere(lat1, long1, lat2, long2):
    """source: http://www.johndcook.com/python_longitude_latitude.html """
    # returns distance in mile
    
    # Convert latitude and longitude to 
    # spherical coordinates in radians.
    degrees_to_radians = math.pi/180.0
        
    # phi = 90 - latitude
    phi1 = (90.0 - lat1)*degrees_to_radians
    phi2 = (90.0 - lat2)*degrees_to_radians
        
    # theta = longitude
    theta1 = long1*degrees_to_radians
    theta2 = long2*degrees_to_radians
        
    # Compute spherical distance from spherical coordinates.
        
    # For two locations in spherical coordinates 
    # (1, theta, phi) and (1, theta, phi)
    # cosine( arc length ) = 
    #    sin phi sin phi' cos(theta-theta') + cos phi cos phi'
    # distance = rho * arc length
    
    cos = (math.sin(phi1)*math.sin(phi2)*math.cos(theta1 - theta2) + 
           math.cos(phi1)*math.cos(phi2))
    arc = math.acos( cos )

    # Multiply arc by the radius of the earth in feet
    return arc*3960

def calculate_duration(start_hr, start_min, end_hr, end_min):   
    #TODO: check how time is coded when clock goes over 12am into the next time
    start_time = start_hr*60+start_min  #convert to minutes
    end_time = end_hr*60+end_min        #convert to minutes
    total_minutes = end_time - start_time
    dur_hr = total_minutes//60
    dur_min = total_minutes % 60
    return (dur_hr, dur_min)
    
def convert2minutes(hours,minutes):
    """ given a duration of (hours, minutes), return the equivalent in minutes """
    return hours*60+minutes

def convert2bin(hour,minute):
    """ given a time specified as hour:minute, return the equivalent in time window bins"""
    min_from_start_of_day = convert2minutes(hour, minute) - const.get('START_OF_DAY_MIN')
    if min_from_start_of_day<0:
        min_from_start_of_day = min_from_start_of_day + 60*24   #add another day
        
    bin_number = 1+ math.floor(min_from_start_of_day / const.get('TIME_WINDOW_BIN_MIN'))    #minutes from 'start of the say' divided by width of a bin gives the bin number
    return bin_number
    
def add_quote_char(string):
    return '"'+string+'"'

def print_in_separate_files(hh_list, out_dir):     
    #specify output files
    hh_file = open(out_dir+'clean_hh/households.csv', 'w')
    per_file = open(out_dir+'clean_hh/persons.csv', 'w')
    trip_file = open(out_dir+'clean_hh/trips.csv', 'w')
    tour_file = open(out_dir+'clean_hh/tours.csv', 'w')
    joint_trip_file = open(out_dir+'clean_hh/joint_ultrips.csv', 'w')
    unique_jtrip_file = open(out_dir+'clean_hh/unique_joint_ultrips.csv', 'w') 
    unique_jtour_file = open(out_dir+'clean_hh/unique_joint_tours.csv', 'w')
     
    err_log_file = open(out_dir+'error_log.txt', 'w')
    problem_hh_file = open(out_dir+'err_hh/households_with_err.csv', 'w')
    problem_per_file = open(out_dir+'err_hh/persons_from_err_hh.csv', 'w')
    problem_trip_file = open(out_dir+'err_hh/trips_from_err_hh.csv', 'w')
    problem_tour_file = open(out_dir+'err_hh/tours_from_err_hh.csv', 'w')
    problem_joint_trip_file = open(out_dir+'err_hh/joint_ultrips_from_err_hh.csv', 'w')
    problem_unique_jtrip_file = open(out_dir+'err_hh/unique_joint_ultrips_from_err_hh.csv', 'w') 
    problem_unique_jtour_file = open(out_dir+'err_hh/unique_joint_tours_from_err_hh.csv', 'w')
    recode_log_file = open(out_dir+'recode_log.txt', 'w')
    
    #print column headers in tables
    Household.print_header(hh_file)
    Person.print_header(per_file)
    Trip.print_header(trip_file)   
    Tour.print_header(tour_file)   
    Joint_ultrip.print_header(joint_trip_file)
    Joint_ultrip.print_header_unique(unique_jtrip_file)
    Joint_tour.print_header(unique_jtour_file)

    Household.print_header(problem_hh_file)
    Person.print_header(problem_per_file)
    Trip.print_header(problem_trip_file)
    Tour.print_header(problem_tour_file)    
    Joint_ultrip.print_header(problem_joint_trip_file)
    Joint_ultrip.print_header_unique(problem_unique_jtrip_file)
    Joint_tour.print_header(problem_unique_jtour_file)
    
    num_err_hh = 0
    for hh in hh_list:
        #print log of all recodes
        hh.print_recode_tags(recode_log_file)
        
        #households with error tag go to one set of files
        if hh.error_flag==True:
            #household contains error: print error messages to one file; print trips/j-trips to a separate trip file
            num_err_hh = num_err_hh+1
            hh.print_tags(err_log_file)
            hh.print_vals(problem_hh_file)
            hh.print_joint_trips(problem_joint_trip_file)
            hh.print_unique_joint_trips(problem_unique_jtrip_file)
            hh.print_unique_joint_tours(problem_unique_jtour_file)
            for psn in hh.persons:
                psn.print_vals(problem_per_file)
                for tour in psn.tours:
                    for trip in tour.trips:                
                        trip.print_vals(problem_trip_file)
                    tour.print_vals(problem_tour_file)
        else:
            #'clean' households go to another set of files
            hh.print_vals(hh_file)
            hh.print_joint_trips(joint_trip_file)
            hh.print_unique_joint_trips(unique_jtrip_file)
            hh.print_unique_joint_tours(unique_jtour_file)
            for psn in hh.persons:
                psn.print_vals(per_file)
                for tour in psn.tours:
                    for trip in tour.trips:                
                        trip.print_vals(trip_file)
                    tour.print_vals(tour_file)
                    
    hh_file.close()
    per_file.close()
    trip_file.close()
    tour_file.close()
    joint_trip_file.close()
    unique_jtrip_file.close()
    unique_jtour_file.close()
     
    err_log_file.close()
    problem_hh_file.close()
    problem_per_file.close()
    problem_trip_file.close()
    problem_tour_file.close()
    problem_joint_trip_file.close()
    problem_unique_jtrip_file.close()
    recode_log_file.close()
     
    print("{} households processed.".format(len(hh_list)))
    print("{} households contain error.".format(num_err_hh))
    
def print_in_same_files(hh_list, out_dir):
    """ print problematic records (relating to joint travel) in the same files as the 'clean' records """     
    #specify output files
    hh_file = open(out_dir+'households.csv', 'w')
    per_file = open(out_dir+'persons.csv', 'w')
    trip_file = open(out_dir+'trips.csv', 'w')
    joint_trip_file = open(out_dir+'joint_ultrips.csv', 'w')
    unique_jtrip_file = open(out_dir+'unique_joint_ultrips.csv', 'w') 
    tour_file = open(out_dir+'tours.csv', 'w')
    unique_jtour_file = open(out_dir+'unique_joint_tours.csv', 'w')
     
    err_log_file = open(out_dir+'error_log.txt', 'w')
    recode_log_file = open(out_dir+'recode_log.txt', 'w')
    
    #print column headers in tables
    Trip.print_header(trip_file)   
    Joint_ultrip.print_header(joint_trip_file)
    Joint_ultrip.print_header_unique(unique_jtrip_file)
    Tour.print_header(tour_file)   
    Joint_tour.print_header(unique_jtour_file)
    Person.print_header(per_file)
    Household.print_header(hh_file)
    
    num_err_perons = 0
    num_err_hh = 0
    count_persons = 0
    count_tours = 0
    count_trips = 0
    
    
    for hh in hh_list:
        #print log of all recodes
        hh.print_recode_tags(recode_log_file)
        
        if hh.error_flag==True:
            num_err_hh = num_err_hh+1
            hh.print_tags(err_log_file)
        
        hh.print_vals(hh_file)
        hh.print_joint_trips(joint_trip_file)
        hh.print_unique_joint_trips(unique_jtrip_file)
        hh.print_unique_joint_tours(unique_jtour_file)
        for psn in hh.persons:
            count_persons = count_persons+1
            psn.print_vals(per_file)
            if psn.error_flag==True:
                num_err_perons = num_err_perons+1
            for tour in psn.tours:
                count_tours = count_tours+1
                tour.print_vals(tour_file)
                for trip in tour.trips:  
                    count_trips = count_trips+1              
                    trip.print_vals(trip_file)
                    
    hh_file.close()
    per_file.close()
    trip_file.close()
    joint_trip_file.close()
    tour_file.close()
    unique_jtrip_file.close()
    unique_jtour_file.close()
    err_log_file.close()
    recode_log_file.close()
     
    print("Processed {} households, {} individuals, {} person-trips, {} person-tours.".format(len(hh_list), count_persons, count_trips, count_tours))
    print("{} households contain error.".format(num_err_hh))
    print("{} persons contain error.".format(num_err_perons))