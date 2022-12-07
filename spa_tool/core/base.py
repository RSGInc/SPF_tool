##################################################################
# This establishes the base model to inherit base functions.
# Creates basic building block for users. Less is more! Simpler is better!
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

import os
from core import functions


class BaseModule:
    def __init__(self, namespace, **kwargs):
        self.namespace = self.check_namespace(namespace)
        self.kwargs = self.update_kwargs(**kwargs)
        self.input_tables = functions.load_pipeline_tables(self.kwargs)

    def check_namespace(self, namespace):
        assert namespace.configs, 'Missing required argument "-c configs"'
        assert namespace.data, 'Missing required argument "-d data"'

        if namespace.output is None:
            print('No output directory specified, using "data" directory as shared data IO directory')
            namespace.output = namespace.data

        assert namespace.output, 'Missing required argument "-o output"'

        return namespace
    def set_global_tables(self):
        # This function can be used to create local variables as table names for debugging and script testing
        for k, df in self.input_tables.items():
            globals()[k] = df

    def update_kwargs(self, **kwargs):
        # This adds the settings file data to the kwargs passed
        if not kwargs.get("module"):
            settings_file = os.path.join(self.namespace.configs, "settings.yaml")
            kwargs = {
                **kwargs,
                **functions.read_config(settings_file)
                .get("PROCESSING_STEPS")
                .get(self.__class__.__name__),
            }

        # Append file paths
        kwargs['output_dir'] = os.path.join(self.namespace.output,
                                            kwargs.get('output_dir', kwargs.get('module'))
                                            )

        namespace_map = {"configs": self.namespace.configs, "data": self.namespace.data}
        for spec in ["configs", "data"]:
            if not kwargs.get(spec):
                continue
            for file_name, file_params in kwargs.get(spec).items():
                if isinstance(file_params, dict):
                    file_params["file"] = functions.find_source_root(
                        file_params["file"], namespace_map[spec]
                    )
                else:
                    file_params = functions.find_source_root(
                        file_params, namespace_map[spec]
                    )
                kwargs[spec][file_name] = file_params

        return kwargs

    def save_tables(self, output_dict, output_dir, index=False):
        if output_dict is None:
            print(
                f"Data not loaded yet, need to run {self.__class__.__name__} module"
            )
            return
        else:
            for k, df in output_dict.items():
                df.to_csv(os.path.join(output_dir, k + ".csv"), index=index)
        return
