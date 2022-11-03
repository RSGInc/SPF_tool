import os
import sys
import argparse

from core import configs

def add_run_args(parser):
    """Run command args"""
    parser.add_argument(
        "-w",
        "--working_dir",
        type=str,
        metavar="PATH",
        help="path to example/project directory (default: %s)" % os.getcwd(),
    )
    parser.add_argument(
        "-c",
        "--config",
        type=str,
        action="append",
        metavar="PATH",
        help="path to config dir",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=str,
        metavar="PATH",
        help="path to output dir"
    )
    parser.add_argument(
        "-d",
        "--data",
        type=str,
        action="append",
        metavar="PATH",
        help="path to data dir",
    )
    parser.add_argument(
        "-s", "--settings_file", type=str, metavar="FILE", help="settings file name"
    )


class SPATool:
    def __init__(self, args):
        settings_file = os.path.join(args.config, 'settings.yaml')
        self.settings = configs.read_config(settings_file)

        self.run()

    def run(self):
        results = {}

        for class_name, params in self.settings.get('PROCESSING_STEPS').items():
            module_name = '.'.join(['tools', params.get('module')]).replace('.py', '')
            class_args = {
                **params,
                # **self.settings,
                **{'pipeline': results}
            }

            module_obj = __import__(module_name, fromlist=['tools'])
            class_obj = getattr(module_obj, class_name)

            results[params.get('output_name', class_name)] = class_obj(args, **class_args).run()



if __name__ == "__main__":
    # For testing
    parser = argparse.ArgumentParser()
    add_run_args(parser)
    args = parser.parse_args()

    # manually inject args
    args.config = 'C:\gitclones\Dubai_survey_processing\configs'
    args.data = [
        'C:\gitclones\Dubai_survey_processing\data',
        'C:\\Users\\nick.fournier\\Resource Systems Group, Inc\\Model Development - Dubai RTA ABM Development Project\\data\\fromIBI\\2014 Survey Data\\2014-12-20_SP_RP_Data_Aur'
    ]

    # Run
    sys.exit(SPATool(args))
