#!/bin/bash

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path"

Rscript "R/helper_scripts/preprocess_data_eawag.R"
Rscript "R/helper_scripts/preprocess_data_bs.R"
Rscript "R/helper_scripts/preprocess_data_cases_che.R"