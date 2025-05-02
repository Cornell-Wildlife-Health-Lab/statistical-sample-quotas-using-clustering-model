'''
Script Name: Statistical Sample Size Quotas Using Clustering Model Input Processing
Author: Nicholas Hollingshead, Cornell University
Description: Prepares data from the CWD Data Warehouse for the Statistical 
             Sample Size Quotas Using Clustering Model R script.
Inputs: 
  params.json
  subadmin_areas.json
  demography.json (optional)
Outputs: 
  model_input.csv
  attachments.json
  info.html
  execution_log.log 
'''

#############
# Params file content
#   sensitivity - a single number
#   cluster_size OR cluster_sizes - a single number or list
#   correlation OR correlations - a single number or list

#   demography_type - string value either "deer density" or "total population"
#   _demography - id of the demography file passed
#   demographic_values - if no _demography, then user must provide this
  

##############
# Environment
import sys
import os
import json
import ndjson
import pathlib
import csv
import logging
import datetime

##################
# SCRIPT VARIABLES

if os.name == 'nt':  # Windows
  base_path = pathlib.Path("data")
else: # Assuming Linux/Docker
  base_path = pathlib.Path("/data")

parameters_file_path = base_path / "params.json"
subadmin_file_path = base_path / "sub_administrative_area.ndJson"
demography_file_path = base_path / "demography.json"

model_metadata_log_file = base_path / "attachments" / "info.html"
logging_path = base_path / "attachments" / "execution_log.log"
attachments_json_path = base_path / "attachments.json"

###################
# FUNCTIONS

def model_log_html(line='', html_element="p", filename=model_metadata_log_file):
    """
    Writes a single line to the model_metadata_log text file with specified HTML element.

    Args:
        line: The line to be written.
        filename: The name of the file.
        html_element: The HTML element tag to use (e.g., "h1", "h2", "p", "div").
    """
    with open(filename, 'a') as f:
        f.write(f"<{html_element}>{line}</{html_element}>" + '\n')

def add_item_to_json_file_list(file_path, new_item):
  """
  Adds a new item to the list within a JSON file.

  Args:
    file_path: Path to the JSON file.
    new_item: The item to be added to the list.

  Raises:
    FileNotFoundError: If the specified file does not exist.
    json.JSONDecodeError: If the file content is not valid JSON.
  """

  try:
    with open(file_path, 'r') as f:
      data = json.load(f)

    if isinstance(data, list):
      data.append(new_item)
    else:
      raise ValueError("The JSON file does not contain a list.")

    with open(file_path, 'w') as f:
      json.dump(data, f, indent=2) 

  except FileNotFoundError:
    print(f"Error: File '{file_path}' not found.")
    raise
  except json.JSONDecodeError:
    print(f"Error: Invalid JSON in '{file_path}'.")
    raise
  except ValueError as e:
    print(f"Error: {e}")
    raise

######################
# SETUP FILE STRUCTURE

# Create the attachments directory structure recursively if it doesn't already exist.
os.makedirs(os.path.dirname(model_metadata_log_file), exist_ok=True)

# Create attachments.json file which will contain a list of all attachments generated
# Initially, the attachments is simply an empty list
with open(attachments_json_path, 'w', newline='') as f:
  writer = json.dump(list(), f)

# Append execution log to attachments.json for developer feedback
attachment = {
  "filename": "execution_log.log", 
  "content_type": "text/plain", 
  "role": "downloadable"
  }
add_item_to_json_file_list(attachments_json_path, attachment)

# append info log to the attachments.json for user feedback
attachment = {
  "filename": "info.html", 
  "content_type": "text/html", 
  "role": "feedback"}
add_item_to_json_file_list(attachments_json_path, attachment)

###############
# SETUP LOGGING

# Create log file including any parent folders (if they don't already exist)
os.makedirs(os.path.dirname(logging_path), exist_ok=True)

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = logging_path, 
                    filemode = 'w', # a is append, w is overbite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

# Uncaught exception handler
def handle_uncaught_exception(exc_type, exc_value, exc_traceback):
  """
  Handles uncaught exceptions by logging the traceback and other details.

  Args:
    exc_type: The type of the exception.
    exc_value: The exception instance.
    exc_traceback: The traceback object.
  """
  logging.error("Uncaught exception:", exc_info=(exc_type, exc_value, exc_traceback))
sys.excepthook = handle_uncaught_exception 

# Initiate model metadata log
# Clear model log file contents if necessary.
open(model_metadata_log_file, 'w').close()
model_log_html("Model Execution Summary", "h3")
model_log_html("Model: Sample Size Quotas Model")
model_log_html('Date: ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' GMT')
logging.info("Model: Sample Size Quotas Model")
logging.info('Date: ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' GMT')
logging.info("This log records data for debugging purposes in the case of a model execution error.")

####################
# Process Parameters

try:
  with open(parameters_file_path, 'r') as f:
    params = json.load(f)
    logging.info("Parameter file loaded successfully")
except FileNotFoundError:
  # The parameters file cannot be found. Exit with an error immediately.
  logging.error("params.json File does not exist.")
  model_log_html("ERROR", "h4")
  model_log_html("Parameters (params.json) file not found.")
  sys.exit(1)

# Get provider admin area for logging
provider_admin_area = params['_provider']['_administrative_area']['administrative_area']
model_log_html(f'Provider area: {provider_admin_area}')
model_log_html('User provided parameters', "h4")

########################
# Process Subadmin Areas

try:
  with open(subadmin_file_path, 'r') as f:
    subadmin_areas = ndjson.load(f)
    logging.info("Subadmin areas file loaded successfully")
except FileNotFoundError:
  # The subadmin areas file cannot be found. Exit with an error immediately.
  logging.error("subadmin_areas.json File does not exist.")
  model_log_html("ERROR", "h4")
  model_log_html("Subadmin areas (subadmin_areas.json) file was expected but not found.")
  sys.exit(1)

# Get just the properties needed for further processing:
# _id, name, aland, and full_name (all required fields in the data collection)
subadmin_areas_data = []
for sa in subadmin_areas:
  subadmin_area_dict = {}
  # All of the following properties are required, so do not need to check if they exist  
  subadmin_area_dict['_id'] = sa['_id']
  subadmin_area_dict['name'] = sa['name']
  subadmin_area_dict['full_name'] = sa['full_name']
  subadmin_area_dict['aland'] = sa['aland']
  subadmin_areas_data.append(subadmin_area_dict)

####################
# Process Demography
# Demography is either a provided file (json) or a dict of values in the params file.
# The model requires one or the other.

if 'demography' in params and params['demography']: # a dict of demography values is provided
  demographic_data = params['demography']
  logging.info("Demography data loaded successfully from params file")
  model_log_html(f"Demographic data: list of {params['demography_type']} values provided")
elif "_demography" in params and params['_demography']: # a demography file is provided
  try:
    with open(demography_file_path, 'r') as f:
      demography = json.load(f)
  except FileNotFoundError:
    # The demography file cannot be found. Exit with an error immediately.
    logging.error("demography.json File does not exist.")
    model_log_html("ERROR", "h4")
    model_log_html("Demography (demography.json) file was expected but not found.")
    sys.exit(1)
  # Get the demography data
  demographic_data = demography['data']
  # Set the params demography type to reflect what was provided in the file
  params['demography_type'] = {demography['metric']}
  logging.info("Demography data loaded successfully from file")
  model_log_html(f"Demographic data: dataset from Warehouse ({demography['species']} {demography['metric']} for season-year {demography['season_year']})")
else:
  # The demography data cannot be found. Exit with an error immediately.
  logging.error("Demography data not provided.")
  model_log_html("ERROR", "h4")
  model_log_html("Demography data was expected in the parameters file or as a dataset but not found.")
  sys.exit(1)

if not isinstance(demographic_data, dict): 
  # The demography data is not a dict
  logging.error("Demography data is not a dictionary.")
  model_log_html("ERROR", "h4")
  model_log_html("Demography data were expected to be a list of value pairs.")
  sys.exit(1)

# Append the total population to the subadmin areas data 
for sa_id, sa_metric in demographic_data.items():
  for sa in subadmin_areas_data:
    if sa['_id'] == sa_id:
      if params['demography_type'] == 'deer density':
        # Deer density values were provided but total population is needed.
        # Convert the density to total population by multiplying it by the land area.
        # Deer density is in deer per square kilometer and aland is in square meters.
        # Convert aland to square kilometers by dividing by 1,000,000.
        sa['total_population'] = int(round(sa_metric * (sa['aland']/1000000)))
      else: # demography type is total population so no need to convert
        sa['total_population'] = sa_metric

#############
# Correlation
# A single value or a dict of values (required)

if 'correlation' in params and params['correlation']: # a single correlation value is provided
  for sa in subadmin_areas_data:
    sa['correlation'] = params['correlation']
elif 'correlations' in params and params['correlations'] and isinstance(params['correlations'], dict): # a dictionary of correlation values is provided
  for sa_id, sa_correlation in params['correlations'].items():
    for sa in subadmin_areas_data:
      if sa['_id'] == sa_id:
        sa['correlation'] = sa_correlation
else:
  # The correlation data cannot be found. Exit with an error immediately.
  logging.error("Correlation data not provided.")
  model_log_html("ERROR", "h4")
  model_log_html("Correlation data is required but not found.")
  sys.exit(1)

##############
# Cluster size
# A single value or a list of values (required)

if 'cluster_size' in params and params['cluster_size']: # a single cluster size value is provided
  for sa in subadmin_areas_data:
    sa['cluster_size'] = params['cluster_size']
elif 'cluster_sizes' in params and params['cluster_sizes']: # a list of cluster size values is provided
  for sa_id, sa_cluster_size in params['cluster_sizes'].items():
    for sa in subadmin_areas_data:
      if sa['_id'] == sa_id:
        sa['cluster_size'] = sa_cluster_size
else:
  # The correlation data cannot be found. Exit with an error immediately.
  logging.error("Cluster size data not provided.")
  model_log_html("ERROR", "h4")
  model_log_html("Cluster size data is required but not found.")
  sys.exit(1)
  
#############
# Sensitivity
# A single value (required)

if 'sensitivity' in params and params['sensitivity']:
  for sa in subadmin_areas_data:
    sa['sensitivity'] = params['sensitivity']
else:
  # The sensitivity data cannot be found. Exit with an error immediately.
  logging.error("Sensitivity data not found in params file.")
  model_log_html("ERROR", "h4")
  model_log_html("Sensitivity data was expected in the parameters file but not found.")
  sys.exit(1)    

###############
# Write the subadmin areas data to a CSV file
# The subadmin_areas_data list has the following properties:
# _id, name, full_name, aland, total_population, correlation, cluster_size, sensitivity

# Write the subadmin_areas_data to a CSV file
with open(pathlib.Path(base_path / "model_input.csv"), 'w', newline='') as f:
  field_names = subadmin_areas_data[0].keys()
  writer = csv.DictWriter(
    f, 
    quoting=csv.QUOTE_NONNUMERIC,
    fieldnames=field_names, 
    extrasaction='ignore')
  writer.writeheader()
  writer.writerows(subadmin_areas_data)

logging.info("Model input processing successfully completed.")
sys.exit(0)