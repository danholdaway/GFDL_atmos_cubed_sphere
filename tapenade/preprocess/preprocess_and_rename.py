#!/usr/bin/env python

import glob
import os
from pathlib import Path
import re

# Base path
base_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../'))

# Step 1: Run the GNU preprocessor on the files in the model and tools directories
# --------------------------------------------------------------------------------

# Directories to preprocess
directory_names = ['model', 'tools']

# Prepend each directory with base path
directories_to_process = [os.path.join(base_path, directory) for directory in directory_names]

# Gnu include path
include_dirs = f'-I{base_path}' + ' -I' + ' -I'.join(directories_to_process)

# Definitions to use for compilation
defs = '-DSPMD -DMOIST_CAPPA -DUSE_COND -DGFS_PHYS=0 -DGFS_TYPES -DUSE_GFSL63 -Duse_WRTCOMP'

# Loop over all directories and process contained files
for directory, directory_name in zip(directories_to_process, directory_names):

    # List of all Fortran files in the directory
    directory_files = glob.glob(os.path.join(directory, '*0'))

    # Make directory if it doesn't exist
    output_dir = os.path.join(base_path, 'pert', directory_name+'_nlm')
    os.makedirs(output_dir, exist_ok=True)

    # Loop over all the files in the directory
    for file in directory_files:

        # Print some information
        print('Processing file: ', os.path.join(directory_name, Path(file).stem + Path(file).suffix))

        # Name of file without extension
        output_file = os.path.join(output_dir, Path(file).stem + '_nlm' + Path(file).suffix)

        # Preprocess the file
        command = f'gfortran -E -P -cpp {defs} {include_dirs} -o {output_file} {file}'
        os.system(command)


# Step 2: Rename the modules in the files
# ---------------------------------------

preprocessed_files = glob.glob(os.path.join(base_path, 'pert', '*', '*0'))

for preprocessed_file in preprocessed_files:

    with open(preprocessed_file, 'r') as f:
        file_lines = f.read().split('\n')

        for ind, file_line in enumerate(file_lines):

            file_line_strip = file_line.strip().lower()

            file_line_strip_split = file_line_strip.split(' ')

            # Remove any element with comments
            file_line_strip_split = [element for element in file_line_strip_split if '!' not in element]

            if len(file_line_strip_split) > 0:

                if file_line_strip_split[0] == 'module' and len(file_line_strip_split) == 2:

                    # Found module beginning
                    module_name = file_line_strip_split[1]

                    # Check if module name ends in _mod
                    if module_name[-4:] == '_mod':
                        # Place _nlm before _mod
                        new_module_name = module_name[:-4] + '_nlm' + module_name[-4:]
                    else:
                        new_module_name = module_name + '_nlm'

                    # Replace module name with new module name in file_lines
                    if '_nlm' not in module_name:
                        file_lines[ind] = file_line.replace(module_name, new_module_name)

                elif file_line_strip_split[0] == 'end' and file_line_strip_split[1] == 'module' or \
                     file_line_strip_split[0] == 'endmodule':

                    # Replace module name with new module name in file_lines
                    if '_nlm' not in module_name:
                        file_lines[ind] = file_line.replace(module_name, new_module_name)

        # Write the new file with updated file_lines
        with open(preprocessed_file, 'w') as f:
            f.write('\n'.join(file_lines))
