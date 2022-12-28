##################################################################
# Script for generating RSG's visualizer
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################
import os
import json
import yaml
import datetime
import subprocess
import numpy as np
import pandas as pd

from core import base, utils
from modules.visualizer.summaries import VisualizerSummaries
from modules.visualizer.summary_helpers import VisualizerInputData, VisualizerIntermediateData, VisualizerFunctions

class Visualizer(base.BaseModule,
                 VisualizerSummaries,
                 VisualizerInputData,
                 VisualizerIntermediateData,
                 VisualizerFunctions):
    def __init__(self, namespace, **kwargs):
        super().__init__(namespace, **kwargs)

        self.save_parameters()
        self.intermediate_data = {}
        self.summaries = {}

    def run(self):
        self.generate_summaries()

        # Store data to pass into R
        self.save_summaries(save_csv=True)

        # Run the R script
        self.generate_html()
        return self.summaries

    def generate_html(self):

        data_path = self.kwargs.get('output_dir')
        Rscript_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'rvisualizer_run.R')

        subprocess.call(['Rscript', Rscript_path, data_path], shell=True)

    def generate_summaries(self):

        # run for base and build
        assert self.input_tables.get('build') and self.input_tables.get('base'), \
            'Missing base and build specifications'

        for scenario, data in self.input_tables.items():

            # This data will get passed to the summaries
            self.scenario_data = data

            # FIXME One off fix for dubai
            # drop_labourers = True if scenario == 'build' else False

            # Generate intermediate data
            self.modify_input_data()
            self.setup_intermediate_data()

            # Get summary list and generate the summaries
            summary_list = self.constants.get('SUMMARIES')
            summary_list = [summary_list] if not isinstance(summary_list, list) else summary_list

            for method in summary_list:
                assert method in dir(self), f'Missing summary method {method}'

            # Generate the summaries
            self.summaries[scenario] = {summary: getattr(self, summary)() for summary in summary_list}

    def save_summaries(self, save_csv=False):
        for scenario, summary_data in self.summaries.items():
            summary_file = os.path.join(self.kwargs.get('output_dir'), f'summaries_{scenario}.json')
            with open(summary_file, 'w') as fp:
                json.dump(summary_data, fp, cls=utils.JSONEncoder, indent=2)

            # Save CSVs
            if save_csv:
                summary_dir = os.path.join(self.kwargs.get('output_dir'), f'summaries_{scenario}')
                if not os.path.exists(summary_dir):
                    os.mkdir(summary_dir)
                for k, df in summary_data.items():
                    assert isinstance(df, pd.DataFrame), f'{k} result is not a dataframe'
                    df.to_csv(os.path.join(summary_dir, f'{k}.csv'), index=False)

    def setup_time_bins(self):
        def time_lab(time, time_next, twelve_hour=True):
            time = (datetime.datetime(1, 1, 1) + datetime.timedelta(hours=time))
            time_next = (datetime.datetime(1, 1, 1) + datetime.timedelta(hours=time_next))
            if twelve_hour:
                time = time.strftime("%I:%M %p")
                time_next = time_next.strftime("%I:%M %p")
            else:
                time = time.strftime("%H:%M")
                time_next = time_next.strftime("%H:%M")

            return f"{time} to {time_next}"

        # Setup time periods
        twelve_hour = self.constants.get('TWELVE_HOUR_CLOCK', True)
        n_periods = self.constants.get('N_TIME_PERIODS', 48)
        day_start = self.constants.get('START_OF_DAY_MIN', 0) / 60

        if n_periods == 40:
            tod_seq = np.concatenate([[3, 5], np.arange(5.5, 24, 0.5), [0, 3]])
            tod_bins = [time_lab(t, tod_seq[i], twelve_hour) for i, t in enumerate(tod_seq[:-1], start=1)]
            dur_seq = np.arange(0.5, 20.5, 0.5)
            dur_bins = [f"({x} hours)" for x in dur_seq]

        else:
            tod_seq = np.concatenate([np.arange(day_start, 24 + 0.5, 0.5), np.arange(0.5, day_start + 0.5, 0.5)])
            tod_bins = [time_lab(t, tod_seq[i], twelve_hour) for i, t in enumerate(tod_seq[:-1], start=1)]
            dur_seq = np.arange(1, 24 + 0.5, 0.5)
            dur_bins = [f"({x} hours)" for x in dur_seq]

        # Create the dataframes
        tod_df = pd.DataFrame(
            {
                'code': range(1, 49),
                'name': tod_bins
             }
        )

        dur_df = pd.DataFrame(
            {
                'code': range(1, 48),
                'name': dur_bins
            }
        )

        return tod_df, dur_df

    def save_parameters(self):
        # Re-read the parameters file
        params_path = self.kwargs['configs'].get('parameters')
        if params_path:
            with open(params_path, "r") as f:
                user_parameters = yaml.safe_load(f)

        # Pop out the summaries list to use as the base/build data names parameter
        data_names = user_parameters.pop('SUMMARIES')

        # Set the path the template file
        SYSTEM_APP_PATH = os.path.dirname(os.path.realpath(__file__))
        TEMPLATE_PATH = os.path.join(SYSTEM_APP_PATH, 'template.Rmd')

        # Fetch TOD bins
        tod_df, dur_df = self.setup_time_bins()

        # FIXME this will need to be corrected in future versions
        if self.constants.get('IS_BASE_SURVEY'):
            #   Survey Base
            BASE_SCENARIO_ALT = "IBI"
            DISTRICT_FLOW_CENSUS = "IBI"
            AO_CENSUS_SHORT = "IBI"
            AO_CENSUS_LONG = "IBI"
        else:
            #   Non-Survey Base
            BASE_SCENARIO_ALT = self.constants.get('BASE_SCENARIO_NAME')
            DISTRICT_FLOW_CENSUS = self.constants.get('BASE_SCENARIO_NAME')
            AO_CENSUS_SHORT = self.constants.get('BASE_SCENARIO_NAME')
            AO_CENSUS_LONG = self.constants.get('BASE_SCENARIO_NAME')

        # Store the output for R
        # TODO Make visualizer consistent to avoid mapping manually like this
        mapped_parameters = {
            'tod_df': tod_df,
            'dur_df': dur_df,
            'todBins': tod_df.name.to_list(),
            'durBins': dur_df.name.to_list(),
            'distBin': [f'{a}-{b}' for a, b in zip(range(0, 50), range(1, 51))],
            'timePeriods': self.constants.get('TIME_PERIODS').keys(),
            'timePeriodBreaks': [0] + list(self.constants.get('TIME_PERIODS').values()),
            'dap_types': self.constants.get('DAP_TYPES'),
            'jtf_alternatives': self.constants.get('JTF_ALTS').keys(),
            'tourMode': self.constants.get('TOURMODE').keys(),
            'tripMode': self.constants.get('TRIPMODE').keys(),
            # 'stopPurposes': self.constants.get('TOURMODE').keys(),
            # 'purpose_type_df': self.map_to_df(self.constants.get('TOURPURP')),
            # 'person_type_df': self.map_to_df(self.constants.get('PERSONTYPE')),
            'person_type_char': ['Total'] + list(self.constants.get('PERSONTYPE').keys()),
            'mtf_df': self.map_to_df(self.constants.get('MTF')),
            'mtf_names': self.constants.get('MTF').keys(),
            'SYSTEM_APP_PATH': SYSTEM_APP_PATH,
            'TEMPLATE_PATH': TEMPLATE_PATH,
            'base_data_names': data_names,
            'build_data_names': data_names,
            'BASE_SCENARIO_ALT': BASE_SCENARIO_ALT,
            'DISTRICT_FLOW_CENSUS': DISTRICT_FLOW_CENSUS,
            'AO_CENSUS_SHORT': AO_CENSUS_SHORT,
            'AO_CENSUS_LONG': AO_CENSUS_LONG,
        }

        # Concatenate the parameters for JSON
        parameters = {**user_parameters, **mapped_parameters}

        # Store serialized JSON file
        params_file = os.path.join(self.kwargs.get('output_dir'), 'summary_parameters.json')
        with open(params_file, 'w') as fp:
            json.dump(parameters, fp, cls=utils.JSONEncoder, indent=2)

        self.constants['parameters'] = parameters


if __name__ == "__main__":
    import argparse
    from core.run import add_run_args

    # Test scripts
    parser = argparse.ArgumentParser()
    add_run_args(parser)
    args = parser.parse_args()

    # manually inject args
    args.configs = "C:\\gitclones\\Dubai_survey_processing\\configs"
    args.data = "C:\\gitclones\\Dubai_survey_processing\\data"

    # Fetch data from the database
    VIZ = Visualizer(args)
    VIZ.run()
