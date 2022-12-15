import os
import sys
import argparse
from core import utils

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
        "-o", "--output", type=str, metavar="PATH", help="path to output dir"
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
    parser.add_argument(
        "-e",
        "--expression_testing",
        action=argparse.BooleanOptionalAction,
        default=False,
        metavar="ENV",
        help="expression testing environment",
    )


class SPTFramework:
    def __init__(self, namespace_args):
        self.namespace = self.check_namespace(namespace_args)

        settings_file = os.path.join(self.namespace.configs, 'settings.yaml')
        self.settings = utils.read_config(settings_file)

        self.run()

    def check_namespace(self, namespace):
        assert namespace.configs, 'Missing required argument "-c configs"'
        assert namespace.data, 'Missing required argument "-d data"'

        if namespace.output is None:
            print('No output directory specified, using "data" directory as shared data IO directory')
            namespace.output = namespace.data

        assert namespace.output, 'Missing required argument "-o output"'
        assert isinstance(namespace.output, str), 'Only one output directory can be specified, check your "-d data" flag'

        for k, v in namespace._get_kwargs():
            if v:
                v = [v] if not isinstance(v, list) else v
                for argpath in v:
                    assert os.path.exists(argpath), f'Could not find the "-c {k}" directory'

        return namespace

    def run(self):
        results = {}

        # Find which step to start from
        steps = self.settings.get("PROCESSING_STEPS")
        skip = True

        assert (
            self.settings.get("START_FROM")
            in self.settings.get("PROCESSING_STEPS").keys()
        ), f"Missing START_FROM step, did you mean one of these?: {self.settings.get('PROCESSING_STEPS').keys()}"

        for class_name, params in steps.items():
            print(f"Running {class_name} module...")
            if class_name == self.settings.get("START_FROM"):
                skip = False
            if params.get("skip") or skip:
                print("Skip")
                continue
            module_name = ".".join(["modules", params.get("module")]).replace(
                ".py", ""
            )
            class_args = {**params, **{"pipeline": results}}


            module_obj = __import__(module_name, fromlist=["modules"])
            class_obj = getattr(module_obj, class_name)

            results[params.get("output_dir", class_name)] = class_obj(
                self.namespace, **class_args
            ).run()


if __name__ == "__main__":
    # For testing
    parser = argparse.ArgumentParser()
    add_run_args(parser)
    args = parser.parse_args()

    # manually inject args
    args.configs = "../../configs"
    args.data = [
        "../../data",
        "C:\\Users\\nick.fournier\\Resource Systems Group, Inc\\Model Development - Dubai RTA ABM Development Project\\data\\fromIBI\\2014 Survey Data\\2014-12-20_SP_RP_Data_Aur",
    ]
    args.output = "../../data"

    # args.expression_testing = True
    # Run
    sys.exit(SPTFramework(args))
