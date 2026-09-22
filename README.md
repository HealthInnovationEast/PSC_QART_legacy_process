# QART Automation

## W**hat is QART?**

QART is a quarterly assurance process used by the Patient Safety Improvement team at NHS England to monitor the progress of different Patient Safety Improvement Programmes (SIPs) in England. Data on the adoption of different programme components is recorded by the 15 Patient Safety Collaboratives (PSCs), which are hosted within the Health Innovation Networks (HINs).

The Patient Safety Analysis team has developed and maintained a reproducible analytical pipeline to automate components of data collection and processing to support QART. The code underpinning that pipeline is stored in this repo.

## Process overview

At the beginning of a new quarter (e.g., 2026/27 Q1), the PSCs will report on the adoption of different programme components for different SIPs. Data is lagged by a quarter, meaning submissions received in Q1 would be for adoption progress observed in Q4. The steps below summarise the workflow supported by this pipeline, as at Q1 of financial year 26/27 (April, May, June 2026):

- Excel templates are generated for each PSC (these templates are used to record data at organisation level).

- PSCs record data within 3 weeks and make a submission.

- PSC submissions are collated and checked for data completeness.

- Data is processed for analysis (reporting doesn't go beyond summary statistics).

- Analytical outputs are delivered. These have changed throughout the lifespan of the pipeline, reflecting changes in analytical needs. The current output is an interactive HTML report for Martha's Rule, a patient safety initiative under the Managing Deterioration SIP.

## SharePoint for file management

The pipeline has been designed to connect to a designated SharePoint area to facilitate file storage and business support. Throughout the entire process, the pipeline downloads and uploads files frequently in SharePoint. Connection is enabled by the `Microsoft365R` package.

Currently the SharePoint area consists of:

- Individual PSC folders, where excel templates for data collection and returned data files are saved every quarter.

- An output folder, where analytical outputs are saved.

- A master files folder, where files needed (and produced by) the pipeline are saved. Key files include:

  - *qart_template_2627.xlsx* - Excel blueprint file. Used as a template to fill out details for organisations under the geography of a PSC. Tabs are colour-coded to reflect relevance to specific SIPs. This file is downloaded from SharePoint by the `create-empty-templates.R` script, which creates the 15 templates to send out.

    - **IMPORTANT:** You will have to make sure **every quarter** that you go into the template file and update the intro tab to update the reporting quarter. Do this *before* creating templates for a new reporting quarter.

  - *MR_Phase1_2\_&\_3_Master.xlsx* - Look up file containing all of Martha's Rule hospital site codes and other organisation details. File is provided by business support.

  - *hin_names.csv* - List of up to date HIN names. It would require updating if at least one of the HIN's changes their name. Names are current as of April 2025.

  - Cumulative data files and lookups. Described below as they are generated sequentially by the pipeline.

## Pipeline overview

These are the scripts that reflect the most recent version of the pipeline, as at Q1 2627:

- *param-flow.R* - This is the master script and sources the scripts below in a order of execution based on a few parameters. the script handles the two-stage process in which QART is executed: preparing templates and processing submissions.

- *config-sharepoint-location.R* - Creates 3 important variables (`site_url`, `chosenlib`, and `base_url`) which will be used in other scripts to navigate around SharePoint. Once a SharePoint location has been selected, these variables should not be changed.

- *process/active-organisation-check.R* - Used in template preparation. Developed in response to data quality issues observed in the MatNeo SIP data collection. This script updates NHS-trust-to-ICB pairs, before filling out templates, accounting for changes such as mergers and name updates. The code looks at the organisation list from the previous quarter (e.g., 2025/26 Q4), and prepares a list to use for reporting in the new quarter (e.g., 2026/27 Q1). Makes use of the [NHS ODS API](https://digital.nhs.uk/developer/api-catalogue/organisation-data-service-ord) for validation. This script outputs a file named `psc_icb_trust_lookup.csv`, which will be saved to the SharePoint master files folder. It also checks if all the ICBs in `ICBs_by_HIN.csv` are known and not renamed.

- *process/trust-site-codes.R* - Similar to above, but produces up to date hospital-site-to-NHS-trust pairs. Developed to accommodate Martha's Rule data collection needs. Extracts hospital site codes from `MR_All_Phases_Master.xlsx` and validates them through the ODS API. Output file is named `psc_trust_site_lookup.csv`, also saved in master files folder.

- *process/create-empty-templates.R* - Used to generate data collection templates for every PSC. Fills out templates with updated organisation details across all SIPs for every PSC, by using `qart_template_2627.xlsx`, and `psc_icb_trust_lookup.csv`, `psc_trust_site_lookup.csv` and `ICBs_by_HIN.csv` lookups. Templates are uploaded to the corresponding PSC folder in SharePoint.

- *process/read-qart-submissions.R* - Used to process submissions. Goes through every PSC folder, identifies the submission file (i.e., file name ending in 'returned'), and extracts SIP data. All data reported will be collated in a csv file, which is saved locally and will be used for modifying a cumulative data file in `append-data.R` below.

  - Note: The output of this script is dependent on which SIPs require analytical outputs through this pipeline. Previously generated a file for MatNeo SIP submissions. Currently, 3 csv files are produced, each reflecting a different clinical setting where Martha's Rule is being adopted.

- *process/append-data.R* - Takes submission files produced in `read-qart-submissions.R` and appends it to previous submissions, generating an up to date cumulative data file for a SIP. The appended data files are uploaded to the master files folder in SharePoint. Cumulative data files follow this naming convention: `<programme_name_setting>_upto_<financial_year>_<quarter_number>.csv`

  - **Note:** this script currently generates a new cumulative data files every quarter.

- *run-quarto.R* - Used to generate analytical outputs after submissions have been processed. Currently uses parameters set in `param_flow.R` to render `marthas-qart-report.qmd`. Saves output(s) in SharePoint output folder.

- *marthas-qart-report.qmd* - Quarto file containing code responsible for report layout and data visualisation. Produces HTML report for Martha's Rule. Executed via `run-quarto.R`*,* this script uses data files produced in `append-data.R` and aggregates data by PSC.

## Attic

This folder is a repository for scripts that previously supported this pipeline but are no longer needed. During the development of this automation project, the QART process was audited and data quality issues were identified and resolved. That work served to scale up the pipeline to its current version. There is a separate README file with more details about the issues resolved.
