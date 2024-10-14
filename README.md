# QART_Automation

There are 3 main scripts for this automation process and 2 config scripts.

## Config scripts

- *config_sharepoint_location.R* - This creates the chosenlib variable and base_url 
which will be used in all of the other scripts.
*site_url*, *base_url* and *chosenlib* variables will 
probably need to be altered depending on required location.
Once the location has been selected, these variables should not be changed.
This file also reads in lookup tables.

- *config_quarter_info.R* - This creates the quarter information.
*current_quarter_year* will need to be changed each time this is run.


## Main scripts

-   *setup_site_folders.R* - this is only run once to setup the folders for 
each site in the chosen location. 

- *create_empty_templates* - this will be run once per quarter to create the templates.
To run, change the *current_quarter_year* variable in *config_quarter_location.R* and source the *create_empty_templates.R* script.It will add a
folder to each location folder for this new quarter and add an empty template file, 
populated with the locations for that site.

- *combine_templates* - this will be run once the templates have been populated by the sites.
To run, confirm that the  *current_quarter_year* variable is still correct (it should have been changed when *create_empty_tables.R* was run), 
and source the *combine_templates.R* script.

## Template files

- *template_files/readme.txt* - this file is the readme file that will be pasted in each site folder when setup_site_folders.R is run

- *template_files/preferred_template.xlsx*- this file is the excel file that network information will be added to in order to create each networks template.