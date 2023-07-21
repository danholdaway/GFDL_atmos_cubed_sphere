#!/usr/bin/env python

import glob
import os
import yaml


# Path to where this file is
file_path = os.path.dirname(__file__)
base_path = os.path.abspath(os.path.join(file_path, '../../'))

# Load the options from YAML
options_yaml = os.path.join(file_path, 'tap_options.yaml')
with open(options_yaml, 'r') as f:
    tap_options = yaml.load(f, Loader=yaml.FullLoader)

# Get modes
modes = tap_options['modes']

# Build list of files to process
file_list = glob.glob(os.path.join(base_path, 'pert', 'model_nlm', '*0'))
file_list += glob.glob(os.path.join(base_path, 'pert', 'tools_nlm', '*0'))
file_list += glob.glob(os.path.join(base_path, 'stubs', '*0'))
file_list += glob.glob(os.path.join(base_path, 'stubs/fms', '*0'))
file_list += glob.glob(os.path.join(base_path, 'stubs/netcdf', '*0'))
file_list += glob.glob(os.path.join(base_path, 'stubs/mpi', '*0'))

# Move element containing 'fv_dynamics_nlm.F90' to the front of the list
file_list.insert(0, file_list.pop(file_list.index(os.path.join(base_path, 'pert', 'model_nlm',
                                                               'fv_dynamics_nlm.F90'))))

file_list_str = ' '.join(file_list)

# Variables string
variables = '(' + ' '.join(tap_options['variables']) + ')'
variables = variables + '/' + variables

# Parse config
real_precision = tap_options['real_precision']
tlm_file_append = tap_options['tlm_file_append']
tlm_function_append = tap_options['tlm_function_append']
adm_file_append = tap_options['adm_file_append']
adm_function_append = tap_options['adm_function_append']

# Compile options
opts = f'-r{real_precision} -tgtvarname _{tlm_file_append} -tgtfuncname _{tlm_function_append} ' + \
       f'-adjvarname _{adm_file_append} -adjfuncname _{adm_function_append}'

for mode in modes:

    # Output directory
    output_directory = os.path.join(base_path, 'pert', f'model_{mode}')

    # Remove output directory if it exists and remake it
    if os.path.isdir(output_directory):
        os.system(f'rm -rf {output_directory}')
    os.makedirs(output_directory)

    print('output_directory: ', output_directory)

    # Output
    command = f'tapenade {opts} -msglevel 25 -linelength \"500\" -d -O {output_directory} ' + \
              f'-head \"fv_dynamics_nlm_mod.fv_dynamics_nlm {variables}\" {file_list_str}'
    os.system(command)


