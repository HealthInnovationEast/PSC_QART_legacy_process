# QART_Automation

There are 4 main scripts for this automation process and 1 config script.

## Config script

-   *config_sharepoint_location.R* - This creates the chosenlib variable and base_url which will be used in all of the other scripts.
    *site_url*, *base_url* and *chosenlib* variables will need to be altered depending on required location.
    Once the location has been selected, these variables should not be changed.
    This file also reads in lookup tables.

## Main scripts

-   *setup_site_folders.R* - this is only run once to setup the folders for each patient safety collaborative (PSC) in the chosen location.

-   *create_empty_templates* - this will be run once per quarter to create the excel templates.
    To run, change the *current_quarter_year* variable in *config_quarter_location.R* and source the *create_empty_templates.R* script.It will add a folder to each location folder for this new quarter and add an empty template file, populated with the locations for that site.

-   *qart_slides.qmd* - produces PowerPoint slide deck displaying data submitted by the PSC's for a financial year quarter.

-   *run_quarto.R* - parameterised script to render *qart_slides.qmd*.
    Specify reporting quarter of interest via the `report_quarter` parameter.