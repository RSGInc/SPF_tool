class Trip:
    """ Trip class """
    def __init__(self, hh_obj, per_obj, tour_obj, trip_id):
        """ instantiation of a Trip object """
        self.hh_obj = hh_obj
        self.per_obj = per_obj
        self.tour_obj = tour_obj
        self.trip_id = trip_id
        self.joint_descriptors = []
        self.escort_pers = []           #store the id(s) of hh members being picked up at the trip's origin or dropped off at the trip's destination
        self.fields = {'HH_ID':self.get_hh_id(), 'PER_ID':self.get_per_id(), 'TOUR_ID':self.get_tour_id(), 'TRIP_ID':self.trip_id}
        self.error_flag = False
        self.arr_times = []
        self.dep_times = []

    def get_joint_descriptions(self):
        return self.joint_descriptors
    
    def get_hh_id(self):
        return self.hh_obj.get_id()

    def get_per_id(self):
        return self.per_obj.get_id()
    
    def get_tour_id(self):
        return self.tour_obj.get_id()

    def get_id(self):
        return self.trip_id
       
    def is_orig_home(self):
        return self.fields['ORIG_PURP'] == NewPurp['HOME']
    
    def is_dest_home(self):
        return self.fields['DEST_PURP'] == NewPurp['HOME']
    
    def get_is_escorting(self):
        if 'DEST_PURP' in self.fields:   #make sure the key exists
            return self.fields['DEST_PURP']==NewPurp['ESCORTING']
        else:
            #TODO: better error handling?
            return False

    def get_is_joint(self):
        is_joint = False
        if 'JOINT' in self.fields:   #make sure the key exists
            if self.fields['JOINT']!=NewJointCategory['NOT-JOINT']:
                is_joint = True            
        return is_joint

    def get_joint_status(self):
        return self.fields['JOINT'] 
    
    def get_jtripID(self):
        if 'JTRIP_ID' in self.fields: #make sure the key exists
            return self.fields['JTRIP_ID']
    
    def get_dest_escort_pudo(self):
        if 'DEST_ESCORTING' in self.fields:
            return self.fields['DEST_ESCORTING']
        else:
            return NewEscort['NEITHER']

    def get_chauffuer(self):
        if 'CHAUFFUER_ID' in self.fields:
            return self.fields['CHAUFFUER_ID']
        else:
            return None
    
    def get_escorted(self):
        if 'ESCORTED' in self.fields:
            return self.fields['ESCORTED'] 
        else:
            return NewEscort['NEITHER']

    def get_depart_time(self):
        """ return the departure time at trip origin in minutes """ 
        _hr = self.fields["ORIG_DEP_HR"]
        _min = self.fields["ORIG_DEP_MIN"]
        return int(_hr*60+_min)

    def get_arrival_time(self):
        """ return the arrival time at trip destination in minutes """ 
        _hr = self.fields["DEST_ARR_HR"]
        _min = self.fields["DEST_ARR_MIN"]
        return int(_hr*60+_min)

    def get_orig_purpose(self):
        return self.fields['ORIG_PURP']

    def get_dest_purpose(self):
        return self.fields['DEST_PURP']
        
    def get_place_number(self):
        return self.fields['DEST_PLACENO']
    
    def get_dist(self):
        if 'DIST' in self.fields:
            if self.fields['DIST'] >= 0:
                return self.fields['DIST']
            else:
                return np.NAN
        else:
            return np.NAN
    
    """
    def get_hhmem(self):
        return self.fields['HHMEM']
    """
    def set_id(self, new_id):
        self.trip_id = new_id
        self.fields['TRIP_ID'] = int(new_id)    #make sure that output field is updated too

    def set_jt_grouping_status(self):
        #check if all joint legs of this trip are grouped (i.e. has a JTIP_ID assigned)
        #if so, then this is a JOINT-GROUPED trip; otherwise, it is left as a JOINT trip  
        _all_grouped = True
        _jt_ids = []
        for jt in self.joint_descriptors:
            _jt_ids.append(jt.get_joint_ultrip_id()) 
            if not (jt.get_joint_ultrip_id()>0):
                #found one leg that is not grouped
                _all_grouped = False
        if len(self.joint_descriptors)==0:
            self.fields['JOINT'] = NewJointCategory['NOT-JOINT']
        elif _all_grouped:
            self.fields['JOINT'] = NewJointCategory['JOINT-GROUPED'] 
            self.fields['JTRIP_ID'] = add_quote_char(','.join(['%s' %id for id in _jt_ids]))
        else:
            self.fields['JOINT'] = NewJointCategory['JOINT'] 
            
    def set_per_type(self, per_type):
        self.fields['PERSONTYPE'] = per_type

    def set_chauffuer(self, driver_id):
        self.fields['CHAUFFUER_ID'] = driver_id
    
    def set_escorting(self, pudo, escort_set):
        
        _num_escorted = len(escort_set)       
        self.escort_pers = sorted(list(escort_set))       
        #set fields
        self.fields['ESCORTING'] = pudo
        self.fields['NUM_PERSONS_ESCORTED'] = _num_escorted
        for _i in range(0, _num_escorted):        
            #TODO: as many fields as needed are set, but only 5(?) persons will be written out 
            self.fields['ESCORT_PERS_'+str(_i+1)] = self.escort_pers[_i]       

    def set_escorted(self, escorted_code):
        self.fields['ESCORTED'] = escorted_code
  
    def set_stop_number_on_half_tour(self, stop_num):
        self.fields['STOP_NUM_HTOUR'] = stop_num
        
    def set_trip_direction(self):
        outbound_stops = self.tour_obj.get_outbound_stops()
        is_inbound = 0                              #1 if trip is in inbound direction, else 0
        if self.trip_id > (outbound_stops+1):
            is_inbound=1             
        self.fields['IS_INBOUND']=is_inbound    
    
    def set_tour_attributes(self):      
        is_subtour = self.tour_obj.get_is_subtour()
        outbound_stops = self.tour_obj.get_outbound_stops()
        inbound_stops = self.tour_obj.get_inbound_stops()
        num_trips = self.tour_obj.get_num_trips()

        self.fields['SUBTOUR'] = is_subtour         #1 if trip is on an At-Work Subtour, else 0   

        #is_inbound = 0                              #1 if trip is in inbound direction, else 0
        #if self.trip_id > (outbound_stops+1):
        #    is_inbound=1             
        #self.fields['IS_INBOUND']=is_inbound            
        
        is_inbound = self.fields['IS_INBOUND']
        #Number of trips on journey to or from primary destination
        if is_inbound==1:
            self.fields['TRIPS_ON_JOURNEY'] = inbound_stops+1 
        else:
            self.fields['TRIPS_ON_JOURNEY'] = outbound_stops+1
            
        #number of trips on the tour
        self.fields['TRIPS_ON_TOUR'] = num_trips  

        if self.trip_id==1:
            self.fields['ORIG_IS_TOUR_ORIG'] = 1    #1 if trip origin is tour anchor/origin, else 0
        else:
            self.fields['ORIG_IS_TOUR_ORIG'] = 0
            
        if self.trip_id==(outbound_stops+2):
            self.fields['ORIG_IS_TOUR_DEST'] = 1    #1 if trip origin is tour primary destination, else 0
        else:
            self.fields['ORIG_IS_TOUR_DEST'] = 0
            
        if self.trip_id==(outbound_stops+1):
            self.fields['DEST_IS_TOUR_DEST'] = 1    #1 if trip destination is tour primary destination, else 0
        else:
            self.fields['DEST_IS_TOUR_DEST'] = 0

        if self.trip_id==(num_trips):
            self.fields['DEST_IS_TOUR_ORIG'] = 1    #1 if trip destination is tour anchor/origin
        else:
            self.fields['DEST_IS_TOUR_ORIG'] = 0
     
        self.fields['FULLY_JOINT'] = self.tour_obj.get_is_fully_joint()
        self.fields['PARTIAL_TOUR'] = self.tour_obj.get_partial_status()
        
        self.fields['TOURMODE'] = self.tour_obj.get_mode()
        self.fields['TOURPURP'] = self.tour_obj.get_purp()


    def _set_transit_attributes(self, df):
        """Process the given DataFrame object to determine the various transit-related trip attributes"""
        #initialize
        _transit_leg_count = 0
        _access_mode = []
        _egress_mode = []
        _last_row = len(df)-1       
                        
        #First, loop through all access (non-transit) legs
        _cur_row = 1    #not 0 because the leg starts at row[0] and ends at row[1]    
        _mode = df['MODE'].iloc[_cur_row]
        while (_mode!=SurveyMode['TRANSIT']):   
            _access_mode.append(_mode)
            _cur_row = _cur_row+1
            _mode = df['MODE'].iloc[_cur_row]
            ### end while
            
        #if no access leg was found
        if len(_access_mode)==0:
            #assume access was by walk
            _access_mode.append(SurveyMode['WALK']) #note: OHAS code for walk mode is 1
                
        #Second, loop through transit legs
        #Need to make sure row pointer does not go past the end of the rows (in the event of missing egress legs) 
        while (_mode==SurveyMode['TRANSIT']):
            _transit_leg_count = _transit_leg_count+1
            _prev_row = _cur_row-1
            if (_transit_leg_count==1):
                #if this is the first transit legs, the previous place is the boarding stop
                self.fields['BOARDING_PLACENO'] = df['PLANO'].iloc[_prev_row]
                self.fields['BOARDING_PNAME']   = add_quote_char(df['PNAME'].iloc[_prev_row])
                self.fields['BOARDING_X']       = df['XCORD'].iloc[_prev_row]
                self.fields['BOARDING_Y']       = df['YCORD'].iloc[_prev_row]
            else:
                #this is the 2+ transit leg, the previous place is a transfer stop
                #transfer stop count = transit leg count - 1
                self.fields['XFER_'+str(_transit_leg_count-1)+'_PLACENO'] = df['PLANO'].iloc[_prev_row]
                self.fields['XFER_'+str(_transit_leg_count-1)+'_PNAME']   = add_quote_char(df['PNAME'].iloc[_prev_row])
                self.fields['XFER_'+str(_transit_leg_count-1)+'_X']       = df['XCORD'].iloc[_prev_row]
                self.fields['XFER_'+str(_transit_leg_count-1)+'_Y']       = df['YCORD'].iloc[_prev_row]             
            #current transit leg    
            self.fields['TRANSIT_ROUTE_'+str(_transit_leg_count)] = df['ROUTE'].iloc[_cur_row] #TODO: type check. add_quote_char() if string
            self.fields['TRANSIT_MODE_'+str(_transit_leg_count)]  = df['TBUS'].iloc[_cur_row]
            #point to the next row 
            _cur_row = _cur_row+1
            if (_cur_row>_last_row):
                break;
            else:   #look up next mode
                _mode = df['MODE'].iloc[_cur_row]
            ### end while
        
        #number of transfers is number of transit legs - 1
        _num_transfers = _transit_leg_count-1
        self.fields['TRANSIT_NUM_XFERS'] = _num_transfers
        if _num_transfers>MAX_XFERS:
            self.log_warning("found {} transfers, but only {} were written out".format(_num_transfers, MAX_XFERS))
            
        #PLACE of the previous row is the ALIGHTING location
        self.fields['ALIGHTING_PLACENO'] = df['PLANO'].iloc[_cur_row-1]
        self.fields['ALIGHTING_PNAME']   = add_quote_char(df['PNAME'].iloc[_cur_row-1])
        self.fields['ALIGHTING_X']       = df['XCORD'].iloc[_cur_row-1]
        self.fields['ALIGHTING_Y']       = df['YCORD'].iloc[_cur_row-1]
            
        #Lastly, loop through remaining, egress legs 
        while (_cur_row<=_last_row):   
            _egress_mode.append(df['MODE'].iloc[_cur_row])
            #point to the next place entry
            _cur_row = _cur_row+1
            ### end while

        #if no egress leg was found
        if len(_egress_mode)==0:
            #assume access was by walk
            _egress_mode.append(SurveyMode['WALK']) #note: OHAS code for walk mode is 1
        
        return (_access_mode, _egress_mode)
    ###### end of _set_transit_attributes() ######
    
    def _find_best_transit_mode(self, df):
        _best_transit = 'BUS'   #initialize to BUS

        #start from row#1, select the rows corresponding to transit mode use       
        df_transit = df.iloc[1:]
        df_transit = df_transit[df_transit['MODE']==SurveyMode['TRANSIT']]
        #transform TBUS values to transit type labels; put into a Pandas Series 
        _transit_types = df_transit['TBUS'].apply(lambda x: SurveyTbus2TransitType[x])
        
        #promote to LRT or BRT if such modes were used        
        if 'LRT' in _transit_types:
            _best_transit = 'LRT'
        elif 'BRT' in _transit_types:
            _best_transit = 'BRT'
        
        return _best_transit
    
    def _set_parking_attributes(self, df):
        """ determine parking location and related attributes for auto trips ending with non-auto, access mode (most likely walk) """
        _modes_col = df['MODE'].iloc[1:]    #note: skip row#0
        #locate the auto segments of the trip 
        _auto_idx = np.where((_modes_col==SurveyMode['DRIVER'])|(_modes_col==SurveyMode['PASSENGER'])|(_modes_col==SurveyMode['CAR/VANPOOL']))        
        _auto_idx_last  = np.max(_auto_idx)         #the last row where auto is used
        if _auto_idx_last < (len(_modes_col)-1):    #is this the last row for the trip?
            #found non-auto modes used before arriving at trip destination
            #location where the last auto mode used is assumed to be the parking location
            self.fields['PARKING_PLACENO']  = df['PLANO'].iloc[1+_auto_idx_last]    #add 1 because row#0 was skipped earlier
            self.fields['PARKING_PNAME']    = add_quote_char(df['PNAME'].iloc[1+_auto_idx_last])
            self.fields['PARKING_X']        = df['XCORD'].iloc[1+_auto_idx_last]
            self.fields['PARKING_Y']        = df['YCORD'].iloc[1+_auto_idx_last]

    
    def _set_auto_occupancy(self, df):
        """ determine the number of people traveling together for an auto trip """
        _auto_occ = int(0)
        #note that mode check starts from row #1 of the DataFrame 
        #thus, need to add 1 back to narray index to get back to the row number 
        _modes_array = df['MODE'].iloc[1:]
        #locate the DRIVER segments of the trip, if any
        _auto_idx = np.where(_modes_array==SurveyMode['DRIVER'])[0]  
        if np.size(_auto_idx)==0: 
            #person was not a driver
            #locate the PASSENGER segments of the trip, if any
            _auto_idx = np.where(_modes_array==SurveyMode['PASSENGER'])[0]
            if np.size(_auto_idx)==0: 
                #person was not a passenger
                #locate the CAR/VANPOOL segments of the trip, if any
                _auto_idx = np.where(_modes_array==SurveyMode['CAR/VANPOOL'])[0]
        
        if np.size(_auto_idx)==0: 
            #trip did not involve any of the 3 above auto modes 
            self.log_error("no auto-related segment found when attempting to calculate auto occupancy")
        else:
            _auto_occ = df['TOTTR'].iloc[1+_auto_idx[0]]        #add 1 to convert back to row number      

        return _auto_occ

    def _calc_purpose(self, old_purp_code, old_place_no, old_place_name, dur_hr, dur_min):
        """ derive the new purpose code from input data purpose code; resolve inconsistencies between activity purpose and person status """
        _new_purp = -1
        if old_purp_code in SurveySchoolPurp:
            #school related activity
            _pertype = self.per_obj.get_per_type()            
            if _pertype==NewPerType['US']:
                #person is an university student
                _new_purp = NewPurp['UNIVERSITY']                
            elif _pertype in [NewPerType['DS'],NewPerType['ND']]:
                #person is a driving or non-driving age student        
                _new_purp = NewPurp['SCHOOL']
            elif _pertype==NewPerType['PS']:  
                #person is a pre-schooler, who may or may not be a student        
                _new_purp = NewPurp['SCHOOL']
                if self.per_obj.get_student_category()==NewStuCategory['NON-STUDENT']:       
                    #inconsistent with non-student status
                    self.per_obj.recode_student_category(NewStuCategory['SCHOOL'],
                        "found school activity (PLANO={}, PNAME={}) for non-student preschooler; reset STU_CAT to SCHOOL".format(old_place_no, old_place_name))            

            else: #person types: FW, PW, NW, RE
                _new_purp = NewPurp['UNIVERSITY']  
                self.per_obj.recode_student_category(NewStuCategory['UNIVERSITY'],
                        "found school activity (PLANO={}, PNAME={}) for Pertype={}; reset STU_CAT to UNIVERSITY".format(old_place_no, old_place_name, _pertype))
                if _pertype in [ NewPerType['PW'], NewPerType['NW'], NewPerType['RE'] ] : #person types PW,NW,RE by definition are not supposed to have school activities
                    self.per_obj.recode_per_type(NewPerType['US'],
                                "found school activity (PLANO={}, PNAME={}) for Pertype={}; reset PERSONTYPE to US".format(old_place_no, old_place_name, _pertype))
    
        else: 
            _new_purp = SurveyTpurp2Purp[old_purp_code]
            if _new_purp in [ NewPurp['WORK'], NewPurp['WORK-RELATED'] ]:
                #found work or work-related activity, check if worker
                _pertype = self.per_obj.get_per_type()
                if _pertype in [ NewPerType['NW'], NewPerType['RE'] ]:
                    if convert2minutes(dur_hr,dur_min)>MAX_VOLUNTEER_MINUTES:
                        #if duration of work activity is over xxx hours, recode person as a part time worker and emp category as part time
                        self.per_obj.recode_emp_category(NewEmpCategory['PARTTIME'],
                            "found work or work-related activity (PLANO={}, PNAME={}) for Pertype={}; reset EMP_CAT to PARTTIME".format(old_place_no, old_place_name, _pertype))
                        self.per_obj.recode_per_type(NewPerType['PW'], 
                            "found work or work-related activity (PLANO={}, PNAME={}) for Pertype={}; reset PERSONTYPE to PW".format(old_place_no, old_place_name, _pertype))
                    else:
                        #consider this as volunteer work, recode to discretionary purpose
                        _new_purp = NewPurp['DISCRETIONARY']
                if _pertype in [ NewPerType['ND'],NewPerType['PS'] ] :
                    #found work activity for non-driving age child; tag as an error
                    #self.per_obj.log_error("found work or work-related activity (PLANO={}, PNAME={}) for non-driving age child".format(old_place_no, old_place_name, _pertype))
                    #consider this as volunteer work, recode to discretionary purpose
                    _new_purp = NewPurp['DISCRETIONARY']
        return _new_purp        

    def populate_attributes(self, df):
        """ determine trip attributes based on PLACE records in a DataFrame object"""
        
        #trip origin: 1st place on trip
        self.fields['ORIG_PLACENO'] = df['PLANO'].iloc[0]              
        self.fields['ORIG_X']       = df['XCORD'].iloc[0]                    
        self.fields['ORIG_Y']       = df['YCORD'].iloc[0]           
        self.fields['ORIG_ARR_HR']  = df['ARR_HR'].iloc[0]
        self.fields['ORIG_ARR_MIN'] = df['ARR_MIN'].iloc[0]         
        self.fields['ORIG_DEP_HR']  = df['DEP_HR'].iloc[0]
        self.fields['ORIG_DEP_MIN'] = df['DEP_MIN'].iloc[0]         
           
        #trip destination: last place on trip
        _last_row = len(df)-1       
        self.fields['DEST_PLACENO'] = df['PLANO'].iloc[_last_row]    
        self.fields['DEST_X']       = df['XCORD'].iloc[_last_row]    
        self.fields['DEST_Y']       = df['YCORD'].iloc[_last_row]    
        self.fields['DEST_ARR_HR']  = df['ARR_HR'].iloc[_last_row]
        self.fields['DEST_ARR_MIN'] = df['ARR_MIN'].iloc[_last_row]         
        self.fields['DEST_DEP_HR']  = df['DEP_HR'].iloc[_last_row]
        self.fields['DEST_DEP_MIN'] = df['DEP_MIN'].iloc[_last_row]         

        #arrival and departure time along all segments of the trip
        #calculate trip duration based on departure at origin and arrival at destination
        (self.fields['TRIP_DUR_HR'], self.fields['TRIP_DUR_MIN']) = calculate_duration(self.fields['ORIG_DEP_HR'], self.fields['ORIG_DEP_MIN'], 
                                                                                       self.fields['DEST_ARR_HR'], self.fields['DEST_ARR_MIN']) 
        self.fields['ORIG_PURP']    = self._calc_purpose(df['TPURP'].iloc[0], 
                                                         df['PLANO'].iloc[0], 
                                                         df['PNAME'].iloc[0], 
                                                         self.fields['TRIP_DUR_HR'], 
                                                         self.fields['TRIP_DUR_MIN'])   

        self.fields['DEST_PURP']    = self._calc_purpose(df['TPURP'].iloc[_last_row], 
                                                         df['PLANO'].iloc[_last_row], 
                                                         df['PNAME'].iloc[_last_row], 
                                                         self.fields['TRIP_DUR_HR'], 
                                                         self.fields['TRIP_DUR_MIN'])   
        
        #calculate time window bin number
        self.fields['ORIG_ARR_BIN'] = convert2bin(self.fields['ORIG_ARR_HR'],self.fields['ORIG_ARR_MIN'])
        self.fields['ORIG_DEP_BIN'] = convert2bin(self.fields['ORIG_DEP_HR'],self.fields['ORIG_DEP_MIN'])
        self.fields['DEST_ARR_BIN'] = convert2bin(self.fields['DEST_ARR_HR'],self.fields['DEST_ARR_MIN'])
        self.fields['DEST_DEP_BIN'] = convert2bin(self.fields['DEST_DEP_HR'],self.fields['DEST_DEP_MIN'])
        self.fields['TRIP_DUR_BIN'] = 1 + self.fields['DEST_ARR_BIN'] - self.fields['ORIG_DEP_BIN']

        #check for loop trips, i.e. purposes at both ends are HOME
        #recode destination purpose as LOOP
        if (self.fields['ORIG_PURP']==NewPurp['HOME']) & (self.fields['DEST_PURP']==NewPurp['HOME']):
            self.fields['DEST_PURP']=NewPurp['LOOP']
            self.log_warning("RECODE: Destination Purpose of HOME-HOME trip recoded as LOOP")
        
             
        #determine trip mode & related attributes
        #SANDAG survey has aggregated trip modes, this block has been updated to work with aggregate mode codes [BMP - 09/06/17]
        #note that mode check is done from row #1 (not row #0) to the last row 
        
        #if a driver for any segment of the trip 
        _toll_paid          = np.any(df['TOLL_NO'].iloc[1:]==SurveyToll['TOLL'])
        _min_tottr     = np.min(df['TOTTR'].iloc[1:])
        _max_tottr     = np.max(df['TOTTR'].iloc[1:])
        _is_driver     = np.any(df['DRIVER'].iloc[1:]==SurveyDriver['DRIVER'])          
        _is_passenger  = np.any(df['DRIVER'].iloc[1:]==SurveyDriver['PASSENGER'])      #if a passenger for any segment of the trip 
        #_is_carpool    = np.any(df['MODE'].iloc[1:]==SurveyMode['CAR/VANPOOL'])    #if car/van pool for any segment of the trip 
        _is_sov        = np.any((df['MODE'].iloc[1:]==SurveyMode['SOV-FREE'])|(df['MODE'].iloc[1:]==SurveyMode['SOV-PAY']))  
        _is_hov2       = np.any((df['MODE'].iloc[1:]==SurveyMode['HOV2-FREE'])|(df['MODE'].iloc[1:]==SurveyMode['HOV2-PAY']))  
        _is_hov3       = np.any((df['MODE'].iloc[1:]==SurveyMode['HOV3-FREE'])|(df['MODE'].iloc[1:]==SurveyMode['HOV3-PAY']))  
        _is_auto       = 1 if ((_is_sov==1)|(_is_hov2==1)|(_is_hov3==1)) else 0
        _is_lb         = np.any((df['MODE'].iloc[1:]==SurveyMode['WALK-LB'])|(df['MODE'].iloc[1:]==SurveyMode['PNR-LB'])|(df['MODE'].iloc[1:]==SurveyMode['KNR-LB'])) 
        _is_eb         = np.any((df['MODE'].iloc[1:]==SurveyMode['WALK-EB'])|(df['MODE'].iloc[1:]==SurveyMode['PNR-EB'])|(df['MODE'].iloc[1:]==SurveyMode['KNR-EB'])) 
        _is_lr         = np.any((df['MODE'].iloc[1:]==SurveyMode['WALK-LR'])|(df['MODE'].iloc[1:]==SurveyMode['PNR-LR'])|(df['MODE'].iloc[1:]==SurveyMode['KNR-LR']))
        _is_cr         = np.any((df['MODE'].iloc[1:]==SurveyMode['WALK-CR'])|(df['MODE'].iloc[1:]==SurveyMode['PNR-CR'])|(df['MODE'].iloc[1:]==SurveyMode['KNR-CR']))
        _is_transit    = 1 if ((_is_lb==1)|(_is_eb==1)|(_is_lr==1)|(_is_cr==1)) else 0        #if any segment of the trip used transit
        _is_tr_pnr     = np.any((df['MODE'].iloc[1:]==SurveyMode['PNR-LB'])|(df['MODE'].iloc[1:]==SurveyMode['PNR-EB'])|(df['MODE'].iloc[1:]==SurveyMode['PNR-LR'])|(df['MODE'].iloc[1:]==SurveyMode['PNR-CR']))
        _is_tr_knr     = np.any((df['MODE'].iloc[1:]==SurveyMode['KNR-LB'])|(df['MODE'].iloc[1:]==SurveyMode['KNR-EB'])|(df['MODE'].iloc[1:]==SurveyMode['KNR-LR'])|(df['MODE'].iloc[1:]==SurveyMode['KNR-CR']))
        _is_auto_dr    = 1 if ((_is_auto==1) & (_is_driver==1)) else 0    #if auto driver for any segment
        _is_auto_ps    = 1 if ((_is_auto==1) & (_is_passenger==1)) else 0   #if auto passanger for any segment
        _is_sch_bus    = np.any(df['MODE'].iloc[1:]==SurveyMode['SCHOOLBUS'])      #if any segment of the trip used school bus
        _is_bike        = np.any(df['MODE'].iloc[1:]==SurveyMode['BIKE'])          #if any segment of the trip used bike
        _is_walk        = np.any(df['MODE'].iloc[1:]==SurveyMode['WALK'])          #if any segment of the trip used walk
        _is_taxi        = np.any(df['MODE'].iloc[1:]==SurveyMode['TAXI'])          #if any segment of the trip used txi
        _auto_occ      = 0 #initialize to 0        
        _new_mode      = 0
        if _is_transit:
            #this is a transit trip 
            #process transit related attributes (transfers, access, egress)
            # BMP [09/06/17] - it may not be possible to add transit attributes for trips whose segments
            # were coded as PNR or KNR when aggregating multiple mode reportings in trip file
            # TO-DO: update _set_transit_attributes() to work with aggregate modes
            #(_access_mode_list, _egress_mode_list) = self._set_transit_attributes(df)
            #find the 'best' transit mode used in the transit trip
            #_transit_type = self._find_best_transit_mode(df)

            # get list of access modes
            _access_mode = 0
            if (_is_tr_pnr)|(_is_auto_dr)|((_is_auto)&(_min_tottr==1)):
                _access_mode = NewTransitAccess['PNR']
            elif (_is_tr_knr)|(_is_auto_ps)|((_is_auto)&(_min_tottr>1)):
                _access_mode = NewTransitAccess['KNR']
            else:
                _access_mode = NewTransitAccess['WALK']
            
            if _access_mode==NewTransitAccess['PNR']:
                if _is_lb:
                    _new_mode = NewTripMode['PNR-LB']
                elif _is_eb:
                    _new_mode = NewTripMode['PNR-EB']
                elif _is_lr:
                    _new_mode = NewTripMode['PNR-LR']
                else:
                    _new_mode = NewTripMode['PNR-CR']  
            elif _access_mode==NewTransitAccess['KNR']:
                if _is_lb:
                    _new_mode = NewTripMode['KNR-LB']
                elif _is_eb:
                    _new_mode = NewTripMode['KNR-EB']
                elif _is_lr:
                    _new_mode = NewTripMode['KNR-LR']
                else:
                    _new_mode = NewTripMode['KNR-CR']
            else:
                # all other access modes are assigned as WALK access
                if _is_lb:
                    _new_mode = NewTripMode['WALK-LB']
                elif _is_eb:
                    _new_mode = NewTripMode['WALK-EB']
                elif _is_lr:
                    _new_mode = NewTripMode['WALK-LR']
                else:
                    _new_mode = NewTripMode['WALK-CR']
                
        elif (_is_auto):
            #This is an auto trip
            #if _is_driver:
                #if person is driver, identify parking location, if any 
                #TO-DO: update _set_parking_attributes() to work with aggregated modes
                #self._set_parking_attributes(df)
            #code auto mode based on auto occupancy
            #_auto_occ = self._set_auto_occupancy(df)                       
            if _is_hov3:
                if _toll_paid: 
                    _new_mode = NewTripMode['HOV3-PAY']
                else:
                    _new_mode = NewTripMode['HOV3-FREE']                    
            elif _is_hov2:
                if _toll_paid: 
                    _new_mode = NewTripMode['HOV2-PAY']
                else:
                    _new_mode = NewTripMode['HOV2-FREE'] 
            else:
                 if _toll_paid: 
                    _new_mode = NewTripMode['SOV-PAY']
                 else:
                    _new_mode = NewTripMode['SOV-FREE'] 

        elif _is_sch_bus:
            _new_mode = NewTripMode['SCHOOLBUS']
        elif _is_bike:
            _new_mode = NewTripMode['BIKE']
        elif _is_walk:
            _new_mode = NewTripMode['WALK']
        elif _is_taxi:
            _new_mode = NewTripMode['TAXI']

        else:
            _new_mode = NewTripMode['OTHER']
        
        #set the following fields for every trip, regardless of mode used    
        self.fields['TRIPMODE'] = _new_mode        
        #auto occupancy = travel party size if auto; 0 otherwise
        self.fields['AUTO_OCC'] = _max_tottr
        #driver status
        self.fields['ISDRIVER'] = int(_is_driver)  

        #process joint travel
        _num_joint_episodes = 0
        for row_index in range(1,_last_row+1):
            #indicator for joint trip (with other household members)
            _hh_mem_on_trip = 1 + df['HHMEM'].iloc[row_index]   #add 1 because HHMEM does NOT include the current person

            if (_hh_mem_on_trip>1):
                #this is a joint episode: 
                _num_joint_episodes = _num_joint_episodes+1
                #create and populate a joint travel descriptor object           
                _new_joint_leg = Joint_ultrip(self, 
                                              df['TOTTR'].iloc[row_index],
                                              _hh_mem_on_trip, 
                                              convert2minutes(df['DEP_HR'].iloc[row_index-1], df['DEP_MIN'].iloc[row_index-1]),
                                              convert2minutes(df['ARR_HR'].iloc[row_index], df['ARR_MIN'].iloc[row_index]),
                                              [self.get_per_id(),df['PER1'].iloc[row_index],df['PER2'].iloc[row_index],df['PER3'].iloc[row_index],
                                              df['PER4'].iloc[row_index], df['PER5'].iloc[row_index], df['PER6'].iloc[row_index]])
                self.joint_descriptors.append(_new_joint_leg)
                if (df['DRIVER'].iloc[row_index]==SurveyDriver['DRIVER']):         #if a driver on this leg of the trip 
                    _new_joint_leg.add_driver(self.get_per_id(), self.get_tour_id(), self.trip_id)           
                                    
        if (_num_joint_episodes>0):
            #this is a joint trip: 
            self.fields['JOINT'] = NewJointCategory['JOINT']
        else:
            self.fields['JOINT'] = NewJointCategory['NOT-JOINT']
        self.fields['NUM_UL_JTRIPS'] = _num_joint_episodes

        #if escorting, determine if it was picking-up or dropping-off
        #TODO: in MTC, DO & PU are coded the same
        _PU = 0
        _DO = 0
        for row_index in range(1,_last_row+1):
            if df['TPURP'].iloc[row_index]==SURVEY_PU_PURP_CODE:
                _PU = _PU+1
            elif df['TPURP'].iloc[row_index]==SURVEY_DO_PURP_CODE:
                _DO = _DO+1           
        if (_PU>0) & (_DO>0) :
            self.fields['DEST_ESCORTING'] = NewEscort['BOTH_PUDO']  #TODO: this would be an error?
        elif _PU>0:    
            self.fields['DEST_ESCORTING'] = NewEscort['PICK_UP']
        elif _DO>0:
                self.fields['DEST_ESCORTING'] = NewEscort['DROP_OFF']
        else:
            self.fields['DEST_ESCORTING'] = NewEscort['NEITHER']
        
        if COMPUTE_TRIP_DIST:
            if np.all(df['Distance'].iloc[1:]>=0):          #if all distance measures are valid
                self.fields['DIST'] = np.sum(df['Distance'].iloc[1:])
            else:
                self.log_error("cannot compute trip distance")
                
        return self.fields['JOINT']
    ###### end of populate_attributes() ######
            
    def log_error(self, err_msg=None):
        self.error_flag = True
        self.fields['ERROR'] = "E: "
        if err_msg:     #if there is an error message
            self.hh_obj.log_error("\t Person#{} Tour#{} Trip#{}: ".format(self.per_obj.get_id(), self.tour_obj.get_id(), self.trip_id)+err_msg)
            self.fields['ERROR'] = self.fields['ERROR']+err_msg        

    def log_warning(self, msg):
        self.hh_obj.log_warning("\t <Warning> Person#{} Tour#{} Trip#{}: ".format(self.per_obj.get_id(), self.tour_obj.get_id(), self.trip_id)+msg)

    def log_recode(self, msg):
        self.per_obj.log_recode("TRIP_ID={} \t".format(self.trip_id)+msg)
        
    def print_header(fp):
        _header=[]
        for _col_num, _col_name in sorted(TripCol2Name.items()):    #TODO: save a sorted copy of the dict to avoid repeated sorting 
            _header.append(_col_name)            
        fp.write(','.join(['%s' %name for name in _header])+'\n')
        
    def print_vals(self, fp):
        if 'ERROR' in self.fields:
            self.fields['ERROR'] = add_quote_char(self.fields['ERROR'])

        _vals = []
        for _col_num, _col_name in sorted(TripCol2Name.items()):
            if _col_name in self.fields:
                _vals.append(self.fields[_col_name])
            else:
                _vals.append(np.NAN)
        fp.write(','.join(['%s' %value for value in _vals])+'\n')

