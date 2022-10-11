from collections import defaultdict
################## Constants ##########################################
COMPUTE_TRIP_DIST = True     #true if trip distance is to be computed from route file
NEGLIGIBLE_DELTA_TIME = 10   #time stamps within this margin of difference can be considered as identical 
MAX_VOLUNTEER_MINUTES = 120  #work duration le this value is considered volunteer work 
MAX_XFERS = 3                #number of place holders for transit transfers in the output trip file
START_OF_DAY_MIN = 180      #3:00am
TIME_WINDOW_BIN_MIN = 30    #bin width in minutes
WORK_LOCATION_BUFFER = 0.25 # distance buffer to check if activity location is primary work location

IN_DIR =  'E:/Projects/Clients/sandag/TO21_Recalibration/SANDAG_Data_Proc/processed/new/'
OUT_DIR_PATH = 'E:/Projects/Clients/sandag/TO21_Recalibration/SANDAG_Data_Proc/processed/output/'

SurveyChangeModeCode = 7
SurveyHomeCode = [1,2]

SURVEY_DO_PURP_CODE = 8
SURVEY_PU_PURP_CODE = 9
SurveySchoolPurp = [5,6]
SurveyWorkPurp = [3,4]
SurveyWorkRelatedPurp = 11

################## Dictionaries ##########################################

# BMP - updated to represent trip purposes in SANDAG survey
SurveyTpurp2Purp = {  #map TPURP from OHAS to the corresponding PURP code
    1: 0,
    2: 0,
    3: 1,
    4: 1,
    5: 2,
    6: 3,
    7: 12, #should not be "Change Mode" unless this is a partial-tour
    8: 4,
    9: 4,
    10: 13,
    11: 10,
    12: 6,
    13: 5,
    14: 5,
    15: 6,
    16: 6,
    17: 7,
    18: 6,
    19: 9,
    20: 9,
    21: 9,
    22: 8,
    96: 11,
    97: 13,
    99: 13, 
    -9998: 13
    }

# BMP [09/06/17] - updated to represent agg modes in SANDAG survey
SurveyMode = {  #map Survey mode name to mode code
    'SOV-FREE': 1,
    'SOV-PAY':  2,
    'HOV2-FREE':  3,
    'HOV2-PAY': 4,
    'HOV3-FREE': 5,
    'HOV3-PAY': 6,
    'WALK': 7,
    'BIKE': 8,
    'WALK-LB': 9,
    'WALK-EB': 10,
    'WALK-LR': 11,
    'WALK-CR': 12,
    'PNR-LB': 13,
    'PNR-EB': 14,
    'PNR-LR': 15,
    'PNR-CR': 16,
    'KNR-LB': 17,
    'KNR-EB': 18,
    'KNR-LR': 19,
    'KNR-CR': 20,
    'SCHOOLBUS': 21,
    'TAXI': 22,
    'OTHER': 23
    }    

# BMP [09/06/17] - survey definition for driver/passanger
SurveyDriver = {
    'DRIVER': 1,
    'PASSENGER': 2,
    'NA': 0
    }

# BMP [09/06/17] - survey definition for toll payment
SurveyToll = {
    'TOLL': 0,
    'NOTOLL': 1
    }

SurveyTbus2TransitType = defaultdict(lambda: 'BUS', {1: 'BUS', 2: 'LRT', 3: 'BRT'})   



