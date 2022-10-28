from collections import defaultdict
import yaml


# FIXME Need to make it so an argument can get passed here
with open('../../configs/settings.yaml', 'r') as file:
    const = yaml.safe_load(file)

""" Constants
This section is somewhat of a relic of original coding.
it is too tedious to refactor the project, so just creating a pass through for now.
"""

COMPUTE_TRIP_DIST = const.get('COMPUTE_TRIP_DIST')     #true if trip distance is to be computed from route file
NEGLIGIBLE_DELTA_TIME = const.get('NEGLIGIBLE_DELTA_TIME')   #time stamps within this margin of difference can be considered as identical
MAX_VOLUNTEER_MINUTES = const.get('MAX_VOLUNTEER_MINUTES')  #work duration le this value is considered volunteer work
MAX_XFERS = const.get('MAX_XFERS')                #number of place holders for transit transfers in the output trip file
START_OF_DAY_MIN = const.get('START_OF_DAY_MIN')      #3:00am
TIME_WINDOW_BIN_MIN = const.get('TIME_WINDOW_BIN_MIN')    #bin width in minutes
WORK_LOCATION_BUFFER = const.get('WORK_LOCATION_BUFFER') # distance buffer to check if activity location is primary work location
INDEX_JTRIP_BY_DEPTIME = const.get('WORK_LOCATION_BUFFER')  #1 if index by trip departure time at origin, 0 if by trip arrival time at destination

IN_DIR = const.get('IN_DIR')
OUT_DIR_PATH = const.get('OUT_DIR_PATH')

SurveyChangeModeCode = const.get('SurveyChangeModeCode')
SurveyHomeCode = const.get('SurveyHomeCode')

SURVEY_DO_PURP_CODE = const.get('SURVEY_DO_PURP_CODE')
SURVEY_PU_PURP_CODE = const.get('SURVEY_PU_PURP_CODE')
SurveySchoolPurp = const.get('SurveySchoolPurp')
SurveyWorkPurp = const.get('SurveyWorkPurp')
SurveyWorkRelatedPurp = const.get('SurveyWorkRelatedPurp')



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


################## Dictionaries Defining Coding of Variables ##########################################
# BMP [09/06/17] - updated to represent agg modes in SANDAG survey
NewTripMode = {
    'SOV-FREE': 1,
    'SOV-PAY': 2,
    'HOV2-FREE': 3,
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

NewTransitAccess = {
    'PNR':  1,
    'KNR':  2,
    'WALK': 3
    }

NewTourMode = {   #map new mode name to mode code
    'SOV':      1,
    'HOV2':     2,
    'HOV3':     3,    
    'WALK':     4,
    'BIKE':     5,
    'WT':       6,
    'PNR':      7,
    'KNR':      8,
    'SCHOOLBUS': 9,
    'TAXI':     10,
    'OTHER':    11
    }

NewPurp = {     #map new purpose name to purpose code
    'HOME':         0,
    'WORK':         1,
    'UNIVERSITY':   2,
    'SCHOOL':       3,
    'ESCORTING':    4,
    'SHOPPING':     5,
    'MAINTENANCE':  6,
    'EAT OUT':      7,
    'SOCIAL/VISIT': 8,
    'DISCRETIONARY':9,
    'WORK-RELATED': 10,
    'LOOP':         11,
    'CHANGE MODE':  12,
    'OTHER':        13
    }

NewPartialTour = {  #map partial tour types to numeric code
    'NOT_PARTIAL':      0,  #not a partial tour
    'PARTIAL_START':    1,  #first tour of the day not starting at home
    'PARTIAL_END':      2   #last tour of the day not ending at home
    }                  

NewEscort = {
    'NEITHER':  0,  #neither pick up or drop off
    'PICK_UP':  1,  #pick up passenger
    'DROP_OFF': 2,   #drop off passenger
    'PICK_UP_NON_HH':     3,   #pick up non household member
    'DROP_OFF_NON_HH':    4,   #pick up non household member    
    'BOTH_PUDO':          5
    }

NewEscortType = {
        'RIDE_SHARING': 1,
        'PURE_ESCORT': 2,
        'NO_ESCORT': 3
        }

NewPerType = {
    'FW' : 1,
    'PW' : 2,
    'US' : 3,
    'NW' : 4,
    'RE' : 5,
    'DS' : 6,
    'ND' : 7,
    'PS' : 8
    }

NewStuCategory = {
    'SCHOOL':       1,
    'UNIVERSITY':   2,
    'NON-STUDENT':  3
    }

NewEmpCategory = {
    'FULLTIME':     1,
    'PARTTIME':     2,
    'UNEMPLOYED':   3,
    'NON-WORKER':   4
    }

NewCompType = {  #map travel party composition label to code
    'ALL_ADULTS':       1,
    'ALL_CHILDREN':     2,
    'MIXED':            3
    }

NewJointCategory = {
    'NOT-JOINT':    0,
    'JOINT':        1,
    'JOINT-GROUPED':  2
    }

NewJointTourStatus = {
'INDEPENDENT':          1,
'PART_JOINT':           2,
'FULL_JOINT':           3,
'PROB_JOINT':           4
}

################## Dictionaries Defining Fields in Output files ##########################################


TripCol2Name = {    #map trip table column number to column header
    1: 'HH_ID',
    2: 'PER_ID',
    3: 'TOUR_ID',
    4: 'TRIP_ID',
    5: 'ORIG_PLACENO',
    6: 'ORIG_X',
    7: 'ORIG_Y',
    8: 'ORIG_TAZ',
    9: 'ORIG_MAZ',
    10: 'DEST_PLACENO',
    11: 'DEST_X',
    12: 'DEST_Y',
    13: 'DEST_TAZ',
    14: 'DEST_MAZ',
    15: 'ORIG_PURP',
    16: 'DEST_PURP',
    17: 'ORIG_ARR_HR',
    18: 'ORIG_ARR_MIN',
    19: 'ORIG_ARR_BIN',
    20: 'ORIG_DEP_HR',
    21: 'ORIG_DEP_MIN',
    22: 'ORIG_DEP_BIN',
    23: 'DEST_ARR_HR',
    24: 'DEST_ARR_MIN',
    25: 'DEST_ARR_BIN',
    26: 'DEST_DEP_HR',
    27: 'DEST_DEP_MIN',
    28: 'DEST_DEP_BIN',
    29: 'TRIP_DUR_HR',
    30: 'TRIP_DUR_MIN',
    31: 'TRIP_DUR_BIN',
    32: 'TRIPMODE',
    33: 'ISDRIVER',
    34: 'CHAUFFUER_ID',
    35: 'AUTO_OCC',
    36: 'TOURMODE',
    37: 'TOURPURP',
    38: 'BOARDING_PLACENO',
    39: 'BOARDING_PNAME',
    40: 'BOARDING_X',
    41: 'BOARDING_Y',
    42: 'BOARDING_TAP',
    43: 'ALIGHTING_PLACENO',
    44: 'ALIGHTING_PNAME',
    45: 'ALIGHTING_X',
    46: 'ALIGHTING_Y',
    47: 'ALIGHTING_TAP',
    48: 'TRANSIT_NUM_XFERS',
    49: 'TRANSIT_ROUTE_1',
    50: 'TRANSIT_MODE_1',
    51: 'XFER_1_PLACENO',
    52: 'XFER_1_PNAME',
    53: 'XFER_1_X',
    54: 'XFER_1_Y',
    55: 'XFER_1_TAP',
    56: 'TRANSIT_ROUTE_2',
    57: 'TRANSIT_MODE_2',
    58: 'XFER_2_PLACENO',
    59: 'XFER_2_PNAME',
    60: 'XFER_2_X',
    61: 'XFER_2_Y',
    62: 'XFER_2_TAP',
    63: 'TRANSIT_ROUTE_3',
    64: 'TRANSIT_MODE_3',
    65: 'XFER_3_PLACENO',
    66: 'XFER_3_PNAME',
    67: 'XFER_3_X',
    68: 'XFER_3_Y',
    69: 'XFER_3_TAP',
    70: 'PARKING_PLACENO',
    71: 'PARKING_PNAME',
    72: 'PARKING_X',
    73: 'PARKING_Y',
    74: 'SUBTOUR',
    75: 'IS_INBOUND',
    76: 'TRIPS_ON_JOURNEY',
    77: 'TRIPS_ON_TOUR',
    78: 'ORIG_IS_TOUR_ORIG',
    79: 'ORIG_IS_TOUR_DEST',
    80: 'DEST_IS_TOUR_DEST',
    81: 'DEST_IS_TOUR_ORIG',
    82: 'PEREXPFACT',
    83: 'HHEXPFACT',
    84: 'PERSONTYPE',
    85: 'FULLY_JOINT',
    86: 'PARTIAL_TOUR',
    87: 'JTRIP_ID',
    88: 'ESCORTED',
    89: 'ESCORTING',
    90: 'NUM_PERSONS_ESCORTED',
    91: 'ESCORT_PERS_1',
    92: 'ESCORT_PERS_2',
    93: 'ESCORT_PERS_3',
    94: 'ESCORT_PERS_4',
    95: 'ESCORT_PERS_5',
    96: 'DEST_ESCORTING',
    97: 'JOINT',
    98: 'NUM_UL_JTRIPS',
    99: 'DIST',
    100: 'ERROR'
    }

#only fields listed below get written out to the tour file
TourCol2Name = {    #map tour table column number to column header
    1: 'HH_ID',
    2: 'PER_ID',
    3: 'TOUR_ID',
    4: 'ORIG_PLACENO',
    5: 'DEST_PLACENO',
    6: 'ORIG_X',
    7: 'ORIG_Y',
    8: 'ORIG_TAZ',
    9: 'ORIG_MAZ',
    10: 'DEST_X',
    11: 'DEST_Y',
    12: 'DEST_TAZ',
    13: 'DEST_MAZ',
    14: 'DEST_MODE',
    15: 'ORIG_MODE',
    16: 'TOURPURP',
    17: 'TOURMODE',
    18: 'DRIVER',
    19: 'ANCHOR_DEPART_HOUR',
    20: 'ANCHOR_DEPART_MIN',
    21: 'ANCHOR_DEPART_BIN',
    22: 'PRIMDEST_ARRIVE_HOUR',
    23: 'PRIMDEST_ARRIVE_MIN',
    24: 'PRIMDEST_ARRIVE_BIN',
    25: 'PRIMDEST_DEPART_HOUR',
    26: 'PRIMDEST_DEPART_MIN',
    27: 'PRIMDEST_DEPART_BIN',
    28: 'ANCHOR_ARRIVE_HOUR',
    29: 'ANCHOR_ARRIVE_MIN',
    30: 'ANCHOR_ARRIVE_BIN',
    31: 'TOUR_DUR_HR',
    32: 'TOUR_DUR_MIN',
    33: 'TOUR_DUR_BIN',
    34: 'MAJOR_UNIV_DEST',
    35: 'SPEC_EVENT_DEST',
    36: 'IS_SUBTOUR',
    37: 'PARENT_TOUR_ID',
    38: 'PARENT_TOUR_MODE',
    39: 'NUM_SUBTOURS',
    40: 'CHILD_TOUR_ID_1',
    41: 'CHILD_TOUR_ID_2',
    42: 'CHILD_TOUR_ID_3',
    43: 'ESCORTED_TOUR',
    44: 'CHAUFFUER_ID',
    45: 'ESCORTING_TOUR',
    46: 'NUM_PERSONS_ESCORTED',
    47: 'ESCORT_PERS_1',
    48: 'ESCORT_PERS_2',
    49: 'ESCORT_PERS_3',
    50: 'ESCORT_PERS_4',
    51: 'ESCORT_PERS_5',
    52: 'OUTBOUND_STOPS',
    53: 'INBOUND_STOPS',
    54: 'OSTOP_1_PLACENO',
    55: 'OSTOP_1_X',
    56: 'OSTOP_1_Y',
    57: 'OSTOP_1_TAZ',
    58: 'OSTOP_1_MAZ',
    59: 'OSTOP_1_ARR_HR',
    60: 'OSTOP_1_ARR_MIN',
    61: 'OSTOP_1_ARR_BIN',
    62: 'OSTOP_1_DEP_HR',
    63: 'OSTOP_1_DEP_MIN',
    64: 'OSTOP_1_DEP_BIN',
    65: 'OSTOP_1_DUR_HR',
    66: 'OSTOP_1_DUR_MIN',
    67: 'OSTOP_1_DUR_BIN',
    68: 'OSTOP_1_PURP',
    69: 'OSTOP_1_MODE',
    70: 'OSTOP_1_ESCORT_ID',
    71: 'OSTOP_1_PUDO',
    72: 'OSTOP_1_MAJUNIV',
    73: 'OSTOP_1_SPECEVENT',
    74: 'OSTOP_2_PLACENO',
    75: 'OSTOP_2_X',
    76: 'OSTOP_2_Y',
    77: 'OSTOP_2_TAZ',
    78: 'OSTOP_2_MAZ',
    79: 'OSTOP_2_ARR_HR',
    80: 'OSTOP_2_ARR_MIN',
    81: 'OSTOP_2_ARR_BIN',
    82: 'OSTOP_2_DEP_HR',
    83: 'OSTOP_2_DEP_MIN',
    84: 'OSTOP_2_DEP_BIN',
    85: 'OSTOP_2_DUR_HR',
    86: 'OSTOP_2_DUR_MIN',
    87: 'OSTOP_2_DUR_BIN',
    88: 'OSTOP_2_PURP',
    89: 'OSTOP_2_MODE',
    90: 'OSTOP_2_ESCORT_ID',
    91: 'OSTOP_2_PUDO',
    92: 'OSTOP_2_MAJUNIV',
    93: 'OSTOP_2_SPECEVENT',
    94: 'OSTOP_3_PLACENO',
    95: 'OSTOP_3_X',
    96: 'OSTOP_3_Y',
    97: 'OSTOP_3_TAZ',
    98: 'OSTOP_3_MAZ',
    99: 'OSTOP_3_ARR_HR',
    100: 'OSTOP_3_ARR_MIN',
    101: 'OSTOP_3_ARR_BIN',
    102: 'OSTOP_3_DEP_HR',
    103: 'OSTOP_3_DEP_MIN',
    104: 'OSTOP_3_DEP_BIN',
    105: 'OSTOP_3_DUR_HR',
    106: 'OSTOP_3_DUR_MIN',
    107: 'OSTOP_3_DUR_BIN',
    108: 'OSTOP_3_PURP',
    109: 'OSTOP_3_MODE',
    110: 'OSTOP_3_ESCORT_ID',
    111: 'OSTOP_3_PUDO',
    112: 'OSTOP_3_MAJUNIV',
    113: 'OSTOP_3_SPECEVENT',
    114: 'OSTOP_4_PLACENO',
    115: 'OSTOP_4_X',
    116: 'OSTOP_4_Y',
    117: 'OSTOP_4_TAZ',
    118: 'OSTOP_4_MAZ',
    119: 'OSTOP_4_ARR_HR',
    120: 'OSTOP_4_ARR_MIN',
    121: 'OSTOP_4_ARR_BIN',
    122: 'OSTOP_4_DEP_HR',
    123: 'OSTOP_4_DEP_MIN',
    124: 'OSTOP_4_DEP_BIN',
    125: 'OSTOP_4_DUR_HR',
    126: 'OSTOP_4_DUR_MIN',
    127: 'OSTOP_4_DUR_BIN',
    128: 'OSTOP_4_PURP',
    129: 'OSTOP_4_MODE',
    130: 'OSTOP_4_ESCORT_ID',
    131: 'OSTOP_4_PUDO',
    132: 'OSTOP_4_MAJUNIV',
    133: 'OSTOP_4_SPECEVENT',
    134: 'ISTOP_1_PLACENO',
    135: 'ISTOP_1_X',
    136: 'ISTOP_1_Y',
    137: 'ISTOP_1_TAZ',
    138: 'ISTOP_1_MAZ',
    139: 'ISTOP_1_ARR_HR',
    140: 'ISTOP_1_ARR_MIN',
    141: 'ISTOP_1_ARR_BIN',
    142: 'ISTOP_1_DEP_HR',
    143: 'ISTOP_1_DEP_MIN',
    144: 'ISTOP_1_DEP_BIN',
    145: 'ISTOP_1_DUR_HR',
    146: 'ISTOP_1_DUR_MIN',
    147: 'ISTOP_1_DUR_BIN',
    148: 'ISTOP_1_PURP',
    149: 'ISTOP_1_MODE',
    150: 'ISTOP_1_ESCORT_ID',
    151: 'ISTOP_1_PUDO',
    152: 'ISTOP_1_MAJUNIV',
    153: 'ISTOP_1_SPECEVENT',
    154: 'ISTOP_2_PLACENO',
    155: 'ISTOP_2_X',
    156: 'ISTOP_2_Y',
    157: 'ISTOP_2_TAZ',
    158: 'ISTOP_2_MAZ',
    159: 'ISTOP_2_ARR_HR',
    160: 'ISTOP_2_ARR_MIN',
    161: 'ISTOP_2_ARR_BIN',
    162: 'ISTOP_2_DEP_HR',
    163: 'ISTOP_2_DEP_MIN',
    164: 'ISTOP_2_DEP_BIN',
    165: 'ISTOP_2_DUR_HR',
    166: 'ISTOP_2_DUR_MIN',
    167: 'ISTOP_2_DUR_BIN',
    168: 'ISTOP_2_PURP',
    169: 'ISTOP_2_MODE',
    170: 'ISTOP_2_ESCORT_ID',
    171: 'ISTOP_2_PUDO',
    172: 'ISTOP_2_MAJUNIV',
    173: 'ISTOP_2_SPECEVENT',
    174: 'ISTOP_3_PLACENO',
    175: 'ISTOP_3_X',
    176: 'ISTOP_3_Y',
    177: 'ISTOP_3_TAZ',
    178: 'ISTOP_3_MAZ',
    179: 'ISTOP_3_ARR_HR',
    180: 'ISTOP_3_ARR_MIN',
    181: 'ISTOP_3_ARR_BIN',
    182: 'ISTOP_3_DEP_HR',
    183: 'ISTOP_3_DEP_MIN',
    184: 'ISTOP_3_DEP_BIN',
    185: 'ISTOP_3_DUR_HR',
    186: 'ISTOP_3_DUR_MIN',
    187: 'ISTOP_3_DUR_BIN',
    188: 'ISTOP_3_PURP',
    189: 'ISTOP_3_MODE',
    190: 'ISTOP_3_ESCORT_ID',
    191: 'ISTOP_3_PUDO',
    192: 'ISTOP_3_MAJUNIV',
    193: 'ISTOP_3_SPECEVENT',
    194: 'ISTOP_4_PLACENO',
    195: 'ISTOP_4_X',
    196: 'ISTOP_4_Y',
    197: 'ISTOP_4_TAZ',
    198: 'ISTOP_4_MAZ',
    199: 'ISTOP_4_ARR_HR',
    200: 'ISTOP_4_ARR_MIN',
    201: 'ISTOP_4_ARR_BIN',
    202: 'ISTOP_4_DEP_HR',
    203: 'ISTOP_4_DEP_MIN',
    204: 'ISTOP_4_DEP_BIN',
    205: 'ISTOP_4_DUR_HR',
    206: 'ISTOP_4_DUR_MIN',
    207: 'ISTOP_4_DUR_BIN',
    208: 'ISTOP_4_PURP',
    209: 'ISTOP_4_MODE',
    210: 'ISTOP_4_ESCORT_ID',
    211: 'ISTOP_4_PUDO',
    212: 'ISTOP_4_MAJUNIV',
    213: 'ISTOP_4_SPECEVENT',
    214: 'PEREXPFACT',
    215: 'HHEXPFACT',
    216: 'PERSONTYPE',
    217: 'FULLY_JOINT',
    218: 'PARTIAL_TOUR',
    219: 'JTOUR_ID',
    220: 'ERROR',
    221: 'JOINT_STATUS',
    222: 'JOINT_TOUR_PURP',
    223: 'DIST',
    224: 'OUT_ESCORT_TYPE',
    225: 'OUT_CHAUFFUER_ID',
    226: 'OUT_CHAUFFUER_PURP',
    227: 'OUT_CHAUFFUER_PTYPE',
    228: 'INB_ESCORT_TYPE',
    229: 'INB_CHAUFFUER_ID',
    230: 'INB_CHAUFFUER_PURP',
    231: 'INB_CHAUFFUER_PTYPE',
    232: 'OUT_ESCORTING_TYPE',
    233: 'INB_ESCORTING_TYPE',
    234: 'OUT_ESCORTEE_TOUR_PURP',
    235: 'INB_ESCORTEE_TOUR_PURP',
    236: 'OUT_ESCORTING_EPISODES',
    237: 'INB_ESCORTING_EPISODES'
}

