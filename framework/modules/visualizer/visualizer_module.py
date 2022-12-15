##################################################################
# Script for generating RSG's visualizer
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

import os
import json
from core import base, utils
from modules.visualizer import summaries

class Visualizer(base.BaseModule, summaries.VisualizerFunctions, summaries.VisualizerSummaries):
    def __init__(self, namespace, **kwargs):
        super().__init__(namespace, **kwargs)

        self.intermediate_data = {}
        self.summaries = {}

    def run(self):
        self.generate_summaries()
        self.save_summaries()
        return self.summaries

    def generate_summaries(self):
        self.setup_intermediate_data()
        summary_list = self.constants.get('SUMMARIES')
        summary_list = [summary_list] if not isinstance(summary_list, list) else summary_list

        self.summaries = {summary: getattr(self, summary)() for summary in summary_list}

    def save_summaries(self):
        summary_file = os.path.join(self.kwargs.get('output_dir'), 'summaries.json')
        with open(summary_file, 'w') as fp:
            json.dump(self.summaries, fp, cls=utils.JSONEncoder)


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
