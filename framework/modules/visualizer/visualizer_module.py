##################################################################
# Script for generating RSG's visualizer
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

from core import base
from modules.visualizer import summaries

class Visualizer(base.BaseModule, summaries.VisualizerFunctions):
    def __init__(self, namespace, **kwargs):
        super().__init__(namespace, **kwargs)

        self.summary_files = None

    def run(self):
        self.generate_summaries()
        self.save_tables(
            output_dict=self.summary_files,
            output_dir=self.kwargs.get('output_dir'),
            index=True
        )
        return self.summary_files

    def generate_summaries(self):
        pass



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
