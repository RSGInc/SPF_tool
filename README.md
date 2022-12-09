# Survey Processing Framework

The Survey Processing Framework (SPF) is intended provide a basic framework structure and workflow "pipeline" for processing and analyzing survey data. The objective is to move away from bespoke one-off data processing scripts and towards a generalizable and programmatic processing tool. The goal is to reduce the amount of time rewriting code, debugging sloppy methods, and the changing countless hardcoded values. 


While the SPF is a work in progress, and still heavily relies on the original SPA Tool code, the codebase is gradually being cleaned up and organized under the guiding principles of the Zen of Python:

- Beautiful is better than ugly.
- Explicit is better than implicit.
- Simple is better than complex.
- Complex is better than complicated.
- Flat is better than nested.
- Sparse is better than dense.
- Readability counts.
- Special cases aren't special enough to break the rules.
- Although practicality beats purity.
- Errors should never pass silently.
- Unless explicitly silenced.
- In the face of ambiguity, refuse the temptation to guess.
- There should be one– and preferably only one –obvious way to do it.
- Although that way may not be obvious at first unless you're Dutch.
- Now is better than never.
- Although never is often better than right now.
- If the implementation is hard to explain, it's a bad idea.
- If the implementation is easy to explain, it may be a good idea.
- Namespaces are one honking great idea – let's do more of those!

Plus the Django principle to keep it DRY (Don't repeat yourself)!


### Installation
The easiest installation is to clone this repo and create your spa_tool conda environment with this command: 
`conda env create -f environment.yml`

### Usage

The SPF is now a command line program with flagged arguments specifying necessary directory locations, following a usage structure similar to ActivitySim and PopulationSim. The command line provides a single point of entry to the SPA Tool "pipeline":

`python spa_tool -c configs -d data`

It has two mandatory arguments:
- --configs, -c "path/to/config"
- --data, -d "path/to/data"

And one optional output flag. The reason is that you may want to treat your data folder as a "working data" directory in which all data are written to/from. Otherwise, if you specify an output folder, data will be saved to that folder and may not be found by downstream models.

- --output, -o "path/to/output"

These arguments point to the directories of all required files. Typically, these might be housed in an SPA Tool 2.0 project folder. For example:

```
SPATool_Project
│
├───configs (Your configuration files go here)
│   │   settings.yaml
│   │
│   ├─── custom_module_configs (if you want)
│   ├─── preprocess_specs   (Configs for preprocessing step)
│   └─── spa_specs          (Configs for main SPA Tool step)
│
├───data    (Your input data and intermediate processed data goes here)
│   
├───output  (Final output goes here)
│   
└───spa_tool (SPA Tool 2.0 codebase)
```


### Architecture
In the past, the SPA Tool was a standalone tool in which data must be pre- and post-processed as it enters and leaves the SPA Tool, typically using separate one-off processing scripts written in _R_. Another major obstacle is that the SPA Tool requires a "place" table, but travel surveys often only include a trips table. This introduced yet another pre-processing script to the workflow. 

The SPF is structured around a basic stepped "module" workflow structure. In the settings file, users specify all required processing steps in a sequence. For example:  

```
PROCESSING_STEPS:
# Declare the custom processing steps to run here
# START_FROM: SPAToolModule
PROCESSING_STEPS:
    ExpressionPreProcess:
        #        skip: True
        module: expressions
        from_pipeline: False # Read directly from pipeline, or pull from directory
        output_dir: preprocessed
        data:
            household:
                file: raw/household.csv
                index: household_id
            person:
                file: raw/person.csv
                index: person_id
            trip:
                file: raw/trip.csv
                index: trip_id
            day:
                file: raw/day.csv
                index: day_id
            vehicle:
                file: raw/vehicle.csv
                index: vehicle_id
            car_salikgates:
                file: raw/car_salikgates.csv
                index: car_salikgates_id
            trip_hhcompanions_pnum:
                file: raw/trip_hhcompanions_pnum.csv
                index: trip_hhcompanions_pnum_id
            zone:
                file: raw/zone.csv
                index: zone_id
        configs:
            mapping_file: preprocess_specs/preprocess_mapping.csv
            long_expressions: preprocess_specs/long_expressions.py # Can define more complex functions to run in a python file.
    TripsToPlace:
        module: trips_to_place
        from_pipeline: False # Read directly from pipeline, or pull from directory
        output_dir: preprocessed
        data:
            household:
                file: preprocessed/household.csv
                index: household_id
            person:
                file: preprocessed/person.csv
                index: person_id
            trip:
                file: preprocessed/trip.csv
                index: trip_id

    SPAToolModule:
        module: legacy_spa
        from_pipeline: False
        output_dir: final_data
        data:
            household:
                file: preprocessed/household.csv
                index: household_id
            person:
                file: preprocessed/person.csv
                index: person_id
            trip:
                file: preprocessed/trip.csv
                index: trip_id
            place:
                file: preprocessed/place.csv
                index: place_id
        configs:
            # These are variable key mappings for the variables aggregated in the SPA tool.
            # Future work to clean this up...
            spa_mapping:  spa_specs/variable_mapping.yaml
          
```

This runs the ExpressionPreProcess step, then the TripsToPlace step, then finally runs the SPAToolModule. Each one of these steps, called "modules" are a python class which are based on the base module structure in `spa_tool/core/base.BaseModule` class. 

Each module step has the following parameters:
```
module:          - The python module file name.  
from_pipeline:   - Boolean flag whether to read data from csv (specified below) or from whether it gets passed directly from previous step.
output_dir:      - The folder where any output is created. This is a sub folder within the "-o output" directory. 
data:           - List of input files 
    index:      - Column name of index in the file
    read_csv:   - Skip the file in case it's not a csv
configs:         - Dictionary list of file names and path in the data folder.
```


### Creating new modules

The fundamental purpose of this processing pipeline is enable flexibility to add or remove functionality, but under an organized framework and structure. If you need to add completely new functionality (e.g., you want a module that generates a bunch of plots and tables), custom modules can be written.

Pipeline modules have a few requirements:
1. They must be under a class object
2. They must have a `def run(self):` function as a single point of entry for the pipeline to call. 
3. They must inherit the base module.
4. They must pass the `namespace` and `**kwargs` (your parameters) to the base module with `super().__init__(namespace, **kwargs)`

The class module structure ensures that objects are cleanly handled in the pipeline and a function in one module does not affect a function in another module. It also enables inheritance, which is a useful feature of Python. By inheriting the base module, you do not need to rewrite all the basic functions such as reading arguments, config files, and input data. This is all provided by the base module, so you can focus on your module instead, remember keep it DRY (Don't repeat yourself)!

Strictly speaking, you technically don't need to inherit the base or pass the arguments, but then your module would be isolated and not interact with the processing pipeline. Which kind of defeats the point of everything. 

To create a new pipeline module, start by creating a new module folder with requisite `__init__.py` python file and create your module in this folder (e.g., new_module.py) in the `spa_tool/modules` folder and create your module's python class object and inherit the "BaseModule" into it. An example custom module could be something like this:

```
"""
This module is located in framework/modules/new_module/new_module.py
"""

from spa_tool.core.base import BaseModule

class CustomModuleTemplate(base.BaseModule):
    # Required: This passes arguments to the base model
    def __init__(self, namespace, **kwargs):        
        super().__init__(namespace, **kwargs)
    
    # Required: The SPA framework calls .run()
    def run(self):
        self.do_something()
        return self.raw_data_formatted
    
    # Have fun
    def do_something(self):
        ....
```

### GetFromPOPS

... TODO ... module that reads directly from RSG's POPS database rather than sending CSVs around.


### ExpressionPreProcess

...TODO add some documentation here...


The ExpressionPreProcess module is used to preprocess the raw data into the required SPA Tool input format. Similar to preprocessing files in ActivitySim, a comma separated value (CSV) file is used to specify the python expression used to create each SPA input field. To aid in writing these expressions, a temporary "expression testing environment" can be run by setting the boolean argument to true:

- --expression_testing, -e True

This will initialize the SPA Tool 2.0 and load in the input tables, then pauses the runtime at the expression processing module step and allow users to test python expressions.


### Visualizer

Not yet working...
