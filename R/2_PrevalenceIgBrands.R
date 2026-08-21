# Copyright 2026 European Medicines Agency
#
# This software is developed by the DARWIN EU Coordination Centre
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This study code contains unmodified open source dependencies.
# Obj2. Prevalence of individuals with a prespecified immunoglobulin brands ----
# prescription among prevalent immunoglobulin users The quarterly prevalence of
# individuals with a prespecified immunoglobulin brands prescription will be
# estimated (expressed as the proportion of individuals prescribed a specific
# immunoglobulin brand among prevalent immunoglobulin users).

# Parameters for IncidencePrevalence---
interval <- c("overall","quarters")
target_cohort <- "ig_brand_users"
denominator_name <- "denom_ig_users"
strata <- list() 

# labels for file exporting---
obj_name <- "obj2"
analysis <- "main"
type <- "overall"

msgLog(glue::glue("Evaluating prevalence - Strata: {type} \n Target Cohort {target_cohort} \n Denom.: {denominator_name}"))
# See helper 'estimatePrevalence' function in 1_Prevalence.R, results are saved and assigned to the environment as "prev_{obj_name}_{analysis}_{type}"
estimatePrevalence(obj_name = obj_name,
                   denominator_table_name = denominator_name,
                   strata = strata, 
                   analysis = analysis, 
                   type =  type,
                   target_id = NULL,
                   interval = interval,
                   outcome_table = target_cohort
)
