import numpy as np
from core.functions import *
from spa_tool.tools.legacy_tools.tours import Tour


class Person:
    """ Person class """
    def __init__(self, hh, per_id, df, constants):
        self.constants = constants
        self.hh_obj = hh
        self.hh_obj.add_person(self)     # can start logging errors/warning
        self.per_id = per_id
        self.fields = {'HH_ID': hh.get_id(), 'PER_ID': per_id}
        self.set_per_type(self._calc_per_type(df)) #errors are logged to the hh obj
        self.tours = []
        self.error_flag = False

    def print_header(fp):
        _header = ["HH_ID", "PER_ID", "PERSONTYPE", "AGE_CAT", "EMPLY", "HOURS_CAT", "EMP_CAT", "STUDE", "SCHOL",
                   "STU_CAT", "PERSONTYPE0", "EMP_CAT0", "STU_CAT0", "ERROR"]
        fp.write(','.join(['%s' % field for field in _header])+'\n')
        
    def print_vals(self, fp):
        _fields = defaultdict(lambda: '', self.fields)
        _vals = [_fields['HH_ID'], _fields['PER_ID'], _fields['PERSONTYPE'],
                 _fields['AGE_CAT'], _fields['EMPLY'], _fields['HOURS_CAT'],
                 _fields['EMP_CAT'], _fields['STUDE'], _fields['SCHOL'], _fields['STU_CAT'],
                 _fields["PERSONTYPE0"], _fields["EMP_CAT0"], _fields["STU_CAT0"], add_quote_char(_fields["ERROR"])]
        fp.write(','.join(['%s' % v for v in _vals])+'\n')

    def _calc_emp_cat(self, age, emply, hours):
        EMPLOYMENT = self.constants.get('EMPLOYMENT')
        age = int(age)
        emply = int(emply)

        _emp = np.NAN     # BMP [08/29/17] - updated to work with SANDAG HTS coding of age and employment status
        if (age >=3) & (emply==1):
            _emp = EMPLOYMENT['FULLTIME']
        elif (age>=3) & (emply==2):
            _emp = EMPLOYMENT['PARTTIME']
        elif (age>=3) & (emply>2):
            _emp = EMPLOYMENT['UNEMPLOYED']
        elif age<3:
            _emp = EMPLOYMENT['NON-WORKER']

        self.fields['AGE_CAT'] = age
        self.fields['EMPLY'] = emply
        self.fields['HOURS_CAT'] = hours
        self.fields['EMP_CAT'] = _emp

        return _emp

    def _calc_emp_cat_no_recode(self, age, emply, hours):
        EMPLOYMENT = self.constants.get('EMPLOYMENT')
        age = int(age)
        emply = int(emply)

        _emp = np.NAN     #BMP [08/29/17] - updated to work with SANDAG HTS coding of age and employment status
        if (age>=3) & (emply==1):
            _emp = EMPLOYMENT['FULLTIME']
        elif (age>=3) & (emply==2):
            _emp = EMPLOYMENT['PARTTIME']
        elif (age>=3) & (emply>2):
            _emp = EMPLOYMENT['UNEMPLOYED']
        elif age<3:
            _emp = EMPLOYMENT['NON-WORKER']
        else: #emp_cat cannot be determined based on the above rules
            pass

        self.fields['AGE_CAT'] = age
        self.fields['EMPLY'] = emply
        self.fields['HOURS_CAT'] = hours
        self.fields['EMP_CAT'] = _emp

        return _emp

    def _calc_stu_cat(self, age, stude, schol, emp_cat):
        STUDENT = self.constants.get('STUDENT')
        age = int(age)
        stude = int(stude)
        schol = int(schol)
        emp_cat = int(emp_cat)

        _stu = np.NAN     #BMP [08/29/17] - updated to work with SANDAG HTS coding of age and schooling status
        if (age<=3) & (stude in [2,3]) & (schol<=9) & (emp_cat in [2,3,4]):
            _stu = STUDENT['SCHOOL']
        elif (age<=2) & (stude in [2,3]) & (schol>9) & (emp_cat in [2,3,4]):
            _stu = STUDENT['SCHOOL']
        elif (age>=4) & (stude in [2,3]) & (schol<=9) & (emp_cat in [2,3,4]):
            _stu = STUDENT['UNIVERSITY']
        elif (age>=3) & (stude in [2,3]) & (schol>9) & (emp_cat in [2,3,4]):
            _stu = STUDENT['UNIVERSITY']
        elif emp_cat==1:
            _stu = STUDENT['NON-STUDENT']
        elif stude==1:
            _stu = STUDENT['NON-STUDENT']
        else:
            #stu_cat cannot be determined -> assign based on age
            if (age<=3):        #TODO: define constant
                stude = 3
                schol = 9
                _stu = STUDENT['SCHOOL']
                self.recode_student_category(_stu,
                        "missing student status/grade info; assign STUDE=1, SCHOL=4, STU_CAT=1 (grade 9-12 student) based on age<=17")
            else:
                stude=1         #TODO: define constant
                schol=np.nan
                _stu = STUDENT['NON-STUDENT']
                self.recode_student_category(_stu,
                        "missing student status/grade info; assign STUDE=1, SCHOL=NA, STU_CAT=3 (non-student) based on age>=16")

        self.fields['STUDE'] = stude
        self.fields['SCHOL'] = schol
        self.fields['STU_CAT'] = _stu

        return _stu

    def _calc_stu_cat_no_recode(self, age, stude, schol, emp_cat):
        STUDENT = self.constants.get('STUDENT')

        _stu = np.NAN        #BMP [08/29/17] - updated to work with SANDAG HTS coding of age and schooling status
        if (age<=3) & (stude in [2,3]) & (schol<=9) & (emp_cat in [2,3,4]):
            _stu = STUDENT['SCHOOL']
        elif (age<=2) & (stude in [2,3]) & (schol>9) & (emp_cat in [2,3,4]):
            _stu = STUDENT['SCHOOL']
        elif (age>=4) & (stude in [2,3]) & (schol<=9) & (emp_cat in [2,3,4]):
            _stu = STUDENT['UNIVERSITY']
        elif (age>=3) & (stude in [2,3]) & (schol>9) & (emp_cat in [2,3,4]):
            _stu = STUDENT['UNIVERSITY']
        elif emp_cat==1:
            _stu = STUDENT['NON-STUDENT']
        elif stude==1:
            _stu = STUDENT['NON-STUDENT']
        else:
            pass

        self.fields['STUDE'] = stude
        self.fields['SCHOL'] = schol
        self.fields['STU_CAT'] = _stu

        return _stu

    def _calc_per_type(self, df_per):
        STUDENT = self.constants.get('STUDENT')
        EMPLOYMENT = self.constants.get('EMPLOYMENT')
        PERTYPE = self.constants.get('PERTYPE')

        # BMP[08/30/17] - updated to work SNADAG HTS coding of variables
        _type = np.NAN

        if len(df_per) != 1:
            self.log_error("person record not found")

        _age = df_per['AGE_CAT'].iloc[0]
        _emply = df_per['EMPLY'].iloc[0]
        _hours = df_per['HOURS_CAT'].iloc[0]
        #_volun = df_per['VOLUN'].iloc[0]
        _stude = df_per['STUDE'].iloc[0]
        _schol = df_per['SCHOL'].iloc[0]
        _emp_cat = self._calc_emp_cat(_age, _emply, _hours)
        _stu_cat = self._calc_stu_cat(_age, _stude, _schol, _emp_cat)

        #if (_age>=16) & (_emp_cat==EMPLOYMENT['FULLTIME']) & (_stu_cat==STUDENT['NON-STUDENT']):
        if (_age>=3) & (_emp_cat==EMPLOYMENT['FULLTIME']):
            #Full-time worker
            _type = PERTYPE['FW']

        elif (_age>=3) & (_emp_cat==EMPLOYMENT['PARTTIME']) & (_stu_cat==STUDENT['NON-STUDENT']):
            #Part-time worker
            _type = PERTYPE['PW']

        #if (_age>=16) & (_emp_cat in [2,3]) & (_stu_cat==2):
        #elif (_age>=17) & (_stu_cat==STUDENT['UNIVERSITY']):
        elif (_age>=4) & (_emp_cat in [EMPLOYMENT['PARTTIME'],EMPLOYMENT['UNEMPLOYED']]) & (_stu_cat==STUDENT['UNIVERSITY']):
            #University Student
            _type = PERTYPE['US']

        elif (_age>=3) & (_age<=10) & (_emp_cat==EMPLOYMENT['UNEMPLOYED']) & (_stu_cat==STUDENT['NON-STUDENT']):
            #Non-worker
            _type = PERTYPE['NW']

        elif (_age>=11) & (_emp_cat==EMPLOYMENT['UNEMPLOYED']) & (_stu_cat==STUDENT['NON-STUDENT']):
            #Retired (non-working) adult
            _type = PERTYPE['RE']

        #elif (_age>=16) & (_age<=19) & (_emp_cat in [2,3]) & (_stu_cat==1):
        elif (_age==3) & (_stu_cat==STUDENT['SCHOOL']):
            #Driving Age Student
            _type = PERTYPE['DS']

        elif (_age==3) & (_stu_cat==STUDENT['UNIVERSITY']):    #recode 16-17 year olds going to university as going to school
            #Driving Age Student
            _type = PERTYPE['DS']
            self.recode_student_category(STUDENT['SCHOOL'], "16-year old found to attend university; reset STU_CAT to SCHOOL and assign PERTYPE to DS")

        elif (_age==2) & (_emp_cat==EMPLOYMENT['NON-WORKER']) & (_stu_cat==STUDENT['SCHOOL']):
            #Non-driving student
            _type = PERTYPE['ND']

        elif (_age==2) & (_emp_cat==EMPLOYMENT['NON-WORKER']) & (_stu_cat==STUDENT['NON-STUDENT']):    #recode none students as students; see email exchange with JF on 6/5/2014
            #Non-driving student
            _type = PERTYPE['ND']
            self.recode_student_category(STUDENT['SCHOOL'], "6~15 year old not attending school; reset STU_CAT to SCHOOL and assign PERTYPE to ND")

        elif (_age==1):
            #Preschool
            _type = PERTYPE['PS']

        else:
            self.log_error("per_type not found")

        return _type

    def _calc_per_type_no_recode(self, df_per):
        STUDENT = self.constants.get('STUDENT')
        EMPLOYMENT = self.constants.get('EMPLOYMENT')
        PERTYPE = self.constants.get('PERTYPE')

        # BMP[08/30/17] - updated to work SNADAG HTS coding of variables
        _type = np.NAN

        if len(df_per) != 1:
            self.log_error("person record not found")

        _age = df_per['AGE_CAT'].iloc[0]
        _emply = df_per['EMPLY'].iloc[0]
        _hours = df_per['HOURS_CAT'].iloc[0]
        #_volun = df_per['VOLUN'].iloc[0]
        _stude = df_per['STUDE'].iloc[0]
        _schol = df_per['SCHOL'].iloc[0]
        _emp_cat = self._calc_emp_cat_no_recode(_age, _emply, _hours)
        _stu_cat = self._calc_stu_cat_no_recode(_age, _stude, _schol, _emp_cat)

        #if (_age>=16) & (_emp_cat==EMPLOYMENT['FULLTIME']) & (_stu_cat==STUDENT['NON-STUDENT']):
        if (_age>=16) & (_emp_cat==EMPLOYMENT['FULLTIME']):
            #Full-time worker
            _type = PERTYPE['FW']

        elif (_age>=16) & (_emp_cat==EMPLOYMENT['PARTTIME']) & (_stu_cat==STUDENT['NON-STUDENT']):
            #Part-time worker
            _type = PERTYPE['PW']

        #if (_age>=16) & (_emp_cat in [2,3]) & (_stu_cat==2):
        #elif (_age>=17) & (_stu_cat==STUDENT['UNIVERSITY']):
        elif (_age>=17) & (_emp_cat in [EMPLOYMENT['PARTTIME'],EMPLOYMENT['UNEMPLOYED']]) & (_stu_cat==STUDENT['UNIVERSITY']):
            #University Student
            _type = PERTYPE['US']

        elif (_age>=16) & (_age<=64) & (_emp_cat==EMPLOYMENT['UNEMPLOYED']) & (_stu_cat==STUDENT['NON-STUDENT']):
            #Non-worker
            _type = PERTYPE['NW']

        elif (_age>=65) & (_emp_cat==EMPLOYMENT['UNEMPLOYED']) & (_stu_cat==STUDENT['NON-STUDENT']):
            #Retired (non-working) adult
            _type = PERTYPE['RE']

        #elif (_age>=16) & (_age<=19) & (_emp_cat in [2,3]) & (_stu_cat==1):
        elif (_age>=16) & (_age<=19) & (_stu_cat==STUDENT['SCHOOL']):
            #Driving Age Student
            _type = PERTYPE['DS']

        elif (_age==16) & (_stu_cat==STUDENT['UNIVERSITY']):    #recode 16-year olds going to university as going to school
            #Driving Age Student
            _type = PERTYPE['DS']
            self.recode_student_category(STUDENT['SCHOOL'], "16-year old found to attend university; reset STU_CAT to SCHOOL and assign PERTYPE to DS")

        elif (_age>=6) & (_age<=15) & (_emp_cat==EMPLOYMENT['NON-WORKER']) & (_stu_cat==STUDENT['SCHOOL']):
            #Non-driving student
            _type = PERTYPE['ND']

        elif (_age>=6) & (_age<=15) & (_emp_cat==EMPLOYMENT['NON-WORKER']) & (_stu_cat==STUDENT['NON-STUDENT']):    #recode none students as students; see email exchange with JF on 6/5/2014
            #Non-driving student
            _type = PERTYPE['ND']
            self.recode_student_category(STUDENT['SCHOOL'], "6~15 year old not attending school; reset STU_CAT to SCHOOL and assign PERTYPE to ND")

        elif (_age>=0) & (_age<=5): #see email exchange with Joel 6/5/2014
            #Preschool
            _type = PERTYPE['PS']
            
        else:
            self.log_error("per_type not found")

        return _type

    def add_tour(self, tour):
        self.tours.append(tour)
        
    def get_id(self):
        return self.per_id
    
    def get_is_adult(self):
        PERTYPE = self.constants.get('PERTYPE')

        ptype = self.get_per_type()
        return ptype in [
            PERTYPE['FW'],
            PERTYPE['PW'],
            PERTYPE['US'],
            PERTYPE['NW'],
            PERTYPE['RE']
        ]
    
    def set_per_type(self, ptype):
        self.fields['PERSONTYPE'] = ptype

    def get_per_type(self):
        if 'PERSONTYPE' in self.fields:
            return self.fields['PERSONTYPE']
        else:
            return np.NAN

    def recode_per_type(self, ptype, msg):
        prev_value = self.get_per_type()
        #update field to new value
        self.fields['PERSONTYPE'] = ptype
        #save old value in a different field
        self.fields['PERSONTYPE0'] = prev_value         
        #save edits to the log
        self.log_recode(msg)
                    
    def set_student_category(self, stu_cat):
        self.fields['STU_CAT'] = stu_cat

    def get_student_category(self):
        if 'STU_CAT' in self.fields:
            return self.fields['STU_CAT']
        else:
            return np.NAN
    
    def recode_student_category(self, stu_cat, msg):
        prev_value = self.get_student_category()
        #update field to new value
        self.fields['STU_CAT'] = stu_cat
        #save old value in a different field
        self.fields['STU_CAT0'] = prev_value         
        #save edits to the log
        self.log_recode(msg)
                    
    def set_emp_category(self, emp_cat):
        self.fields['EMP_CAT'] = emp_cat

    def get_emp_category(self):
        if 'EMP_CAT' in self.fields:
            return self.fields['EMP_CAT']
        else:
            return np.NAN

    def recode_emp_category(self, emp_cat, msg):
        prev_value = self.get_emp_category()
        #update field to new value
        self.fields['EMP_CAT'] = emp_cat
        #save old value in a different field
        self.fields['EMP_CAT0'] = prev_value         
        #save edits to the log
        self.log_recode(msg)


    def add_subtour(self, trips):
        """ add an at-work subtour to its list of tours; return the subtour obj """
        #id of the new tour is 1 up the id of the last tour currently in the list
        _new_tour_id = 1 + self.tours[-1].get_id()
        #create tour object
        #_new_tour = Tour(self.hh_obj.get_id(), self.per_id, _new_tour_id, 1)
        _new_tour = Tour(self.hh_obj, self, _new_tour_id, 1, trips, self.constants)
        #add new tour to its list
        self.tours.append(_new_tour)
        
        return _new_tour
    
    def lastTripEndAtHome(self):
        NewPurp = self.constants.get('NewPurp')

        _returned_home = True
        _last_trip = self.tours[-1].trips[-1]
        if _last_trip.fields['DEST_PURP'] != NewPurp['HOME']:
            _returned_home = False
        return _returned_home
    
    def log_recode(self, msg):
        self.hh_obj.log_recode("PER_ID={} \t".format(self.per_id)+msg)

    def log_warning(self, msg):
        self.hh_obj.log_warning("\t <Warning> Person#{}: ".format(self.per_id)+msg)

    def log_error(self, err_msg=None):
        self.error_flag = True
        self.fields['ERROR'] = "E:"
        if err_msg:
            self.hh_obj.log_error("\t Person#{}: ".format(self.per_id)+err_msg)
            self.fields['ERROR'] = self.fields['ERROR']+err_msg
