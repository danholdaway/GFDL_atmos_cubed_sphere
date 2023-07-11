#!/usr/bin/env python

import glob
import os
from pathlib import Path
import re

# Base path
base_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../'))

# Step 1: Run the GNU preprocessor on the files in the model and tools directories
# --------------------------------------------------------------------------------

# Include directories beyond those being preprocessed
additional_include_dirs = ['stubs/fms']
additional_include_dirs = [os.path.join(base_path, directory) for directory in additional_include_dirs]

# Directories to preprocess
directory_names = ['model', 'tools']

# Prepend each directory with base path
directories_to_process = [os.path.join(base_path, directory) for directory in directory_names]

# Gnu include path
include_dirs = f'-I{base_path}' + ' -I' + ' -I'.join(directories_to_process) + ' -I' + ' -I'.join(additional_include_dirs)
print(include_dirs)
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

# RegEx for finding module names
module_name_pattern = re.compile(r"^\s*(?:module)\s+(?!procedure\s)([a-z]\w*)", re.IGNORECASE | re.MULTILINE)
# RegEx for partitioning modules names if they end with "_mod"
name_partition_pattern = re.compile(r"\b([a-z]\w*?)(_mod)?\b", re.IGNORECASE)

# Map from old_module_name -> new_module_name
name_map = {}

# Identify modules for renaming
for preprocessed_file in preprocessed_files:

    with open(preprocessed_file, 'r') as f:
        file_content = f.read()

        matches = module_name_pattern.findall(file_content)
        for module_name in matches:
            new_module_name = name_partition_pattern.sub(r"\g<1>_nlm\g<2>", module_name)
            name_map[module_name] = new_module_name.lower()

print("Modules to rename with \"_nlm\": ", list(name_map.keys()))

# RegEx for matching names to replace
names_to_replace_pattern = re.compile("\\b(" + "|".join(name_map.keys()) + ")\\b", re.IGNORECASE | re.MULTILINE)

# Find and replace the identified modules in each file
for preprocessed_file in preprocessed_files:

    with open(preprocessed_file, 'r') as f:
        file_content = f.read()           

        # Replace all identified module names
        new_file_content = names_to_replace_pattern.sub(lambda x: name_map[x.group(0).lower()], file_content)

    # Write the new file with updated file_lines
    with open(preprocessed_file, 'w') as f:
        f.write(new_file_content)
