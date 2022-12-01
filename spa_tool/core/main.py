import os
import sys
import argparse

from core import functions


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
        "--configs",
        type=str,
        action="append",
        metavar="PATH",
        help="path to configs dir",
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
        "-s",
        "--settings_file",
        type=str,
        metavar="FILE",
        help="settings file name"
    )
    parser.add_argument(
        "-e",
        "--expression_testing",
        action=argparse.BooleanOptionalAction,
        default=False,
        metavar="ENV",
        help="expression testing environment"
    )


class SPAToolFramework:
    def __init__(self, namespace_args):
        self.nargs = self.check_namespace(namespace_args)

        settings_file = functions.find_source_root('settings.yaml', namespace_args.configs)
        self.settings = functions.read_config(settings_file)

        self.run()

    def check_namespace(self, namespace):
        assert namespace.configs, 'Missing required argument "-c configs"'
        assert namespace.data, 'Missing required argument "-d data"'
        assert namespace.output, 'Missing required argument "-o output"'

        return namespace

    def run(self):
        if self.nargs.expression_testing:
            params = self.settings.get('PROCESSING_STEPS').get('ExpressionPreProcess')
            module_name = '.'.join(['tools', params.get('module')]).replace('.py', '')
            assert params['from_pipeline'] == False, 'Expression tester must run from flat files, not pipeline data.'
            module_obj = __import__(module_name, fromlist=['tools'])
            class_obj = getattr(module_obj, 'ExpressionPreProcess')

            ExpressionClass = class_obj(args, **params)

            # Setup tables in local env
            expressions = ExpressionClass.expressions
            for k, df in ExpressionClass.input_tables.items():
                locals()[k] = df

            exp = ''
            while exp != 'quit()':
                print('Loaded tables: ' + ', '.join(ExpressionClass.input_tables.keys()))
                exp = input("Expression Environment, quit() to quit")
                print(eval(exp))

        else:
            results = {}

            # Find which step to start from
            steps = self.settings.get('PROCESSING_STEPS')
            skip = True

            assert self.settings.get('START_FROM') in self.settings.get('PROCESSING_STEPS').keys(), \
                'Missing START_FROM step'

            for class_name, params in steps.items():
                print(f'Running {class_name} module...')
                if class_name == self.settings.get('START_FROM'):
                    skip = False
                if params.get('skip') or skip:
                    print('Skip')
                    continue
                module_name = '.'.join(['tools', params.get('module')]).replace('.py', '')
                class_args = {
                    **params,
                    **{'pipeline': results}
                }

                module_obj = __import__(module_name, fromlist=['tools'])
                class_obj = getattr(module_obj, class_name)

                results[params.get('output_dir', class_name)] = class_obj(self.nargs, **class_args).run()


if __name__ == "__main__":
    # For testing
    parser = argparse.ArgumentParser()
    add_run_args(parser)
    args = parser.parse_args()

    # manually inject args
    args.configs = 'C:\gitclones\Dubai_survey_processing\configs'
    args.data = [
        'C:\gitclones\Dubai_survey_processing\data',
        'C:\\Users\\nick.fournier\\Resource Systems Group, Inc\\Model Development - Dubai RTA ABM Development Project\\data\\fromIBI\\2014 Survey Data\\2014-12-20_SP_RP_Data_Aur'
    ]
    args.output = 'C:\\gitclones\\Dubai_survey_processing\\output'

    # args.expression_testing = True
    # Run
    sys.exit(SPAToolFramework(args))
