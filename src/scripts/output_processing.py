'''
Script Name: Statistical Sample Size Quotas Using Clustering Model Output Processing
Author: Nicholas Hollingshead, Cornell University
Description: Converts output files form the Sample Size Quotas script
             to file formats appropriate for the CWD Data Warehouse.
Inputs: 
  SampleSizeQuotasOutput.csv
Outputs: 
  output.json
  attachments.json
  info.html
  execution_log.log 
'''

##############
# Environment
import pathlib
import json
import csv
import os
import sys
import logging

##################
# SCRIPT VARIABLES

if os.name == 'nt':  # Windows
  base_path = pathlib.Path("data")
else: # Assuming Linux/Docker
  base_path = pathlib.Path("/data")

model_output_file =  base_path / "attachments" / "SampleSizeQuotasOutput.csv"
model_metadata_log_file = base_path / "attachments" / "info.html"
logging_path = base_path / "attachments" / "execution_log.log"
attachments_json_path = base_path / "attachments.json"

###################
# Functions

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

################
# LOGGING CONFIG

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = logging_path, 
                    filemode = 'a', # a is append, w is overwrite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

# Uncaught exception handler
def handle_uncaught_exception(type, value, traceback):
  logging.error(f"{type} error has occurred with value: {value}. Traceback: {traceback}")
sys.excepthook = handle_uncaught_exception

#######
# Handle the model output file

model_log_html("Model Exports", "h3")

# Convert the model output CSV to a JSON
model_output_dictlist = list()
with open(pathlib.Path(model_output_file), 'r') as f:
  csv_reader = csv.DictReader(f)
  model_output_dictlist = list(csv_reader)

# csv.DictReader reads all fields as text.
# Convert fields that are numbers from strings to integers.

float_fields = ("1%","1.5%","2%","3%","4%","5%")

try:
  for each_row in model_output_dictlist:
    for each_field in float_fields:
      # Convert NA values to None first
      if each_row[each_field] == "NA":
        each_row[each_field] = None
      else:
        try:
          each_row[each_field] = int(each_row[each_field])
        except ValueError:
          # Handle text or invalid values gracefully
          each_row[each_field] = None      
except Exception as e:
  logging.exception("Failed to convert model output field value to floating point.")
  model_log_html("ERROR: Failed to create output file.")
  sys.exit(1)

# Write to JSON file
model_output_json_path = pathlib.Path(base_path / "attachments" / "output.json")
with open(pathlib.Path(model_output_json_path), 'w', newline='') as f:
    writer = json.dump(model_output_dictlist, f)

# Add output.json to the attachments list
attachment = {"filename": "output.json", "content_type": "application/json", "role": "primary"}
add_item_to_json_file_list(attachments_json_path, attachment)

model_log_html("Model exports successfully created.")