# STA304 - PS5

# Sampling Bias in COVID-19 Contact Tracing Leads to Overestimation of Case Numbers from Institutional and Congregate Settings
In this paper, I simulate a two-stage contact tracing process at various accuracies to examine the impact of sampling bias on COVID-19 case data in Toronto, Canada.

# File Structure
The "data" folder contains the raw data files used for the body of the paper. These data files are publicly available through the City of Toronto's Open Data Portal and through the Ontario Laboratories Information System (OLIS) via the Government of Ontario. The COVID-19 case data for Toronto is updated weekly, and the file used for this paper contains data from January 23 to December 6, 2020. The OLIS data is also updated periodically, and the data used for these analyses includes data reported between May 1 and December 3, 2020.

The "outputs" folder contains the PDF images used in the final RMarkdown output as well as the final paper PDF. These images are created using the cleaning, simulation, and visualization code in "scripts", and are divided into summary plots ("figures" folder) and simulation output ("simulations" folder). The final PDF of the report can be found in the "paper" folder.

The "scripts" folder contains the code to produce the simulation results and plots, to clean and produce the summary plots for the data in the "data" file, and the RMarkdown file for the final paper PDF.

# Reproducability
- Ensure all file names and file paths in the scripts match the locations and names of the files you wish to use.
- Ensure that any data taken directly from the source (ie. City of Toronto Open Data Portal or OLIS) corresponds to the dates considered in the report. Alternatively, use the data provided in the "data" folder.
- Run the summary plots and contact tracing simulations code in "scripts" first before running the .Rmd file to generate the report.
