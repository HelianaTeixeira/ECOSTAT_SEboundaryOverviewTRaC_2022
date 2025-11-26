###**WFD CIS Working Group on Ecological Status (ECOSTAT)**
##**Nutrients and other physico-chemical supporting elements**
*Description of files included in this repository*

The information here contained supports the work presented in the report by Teixeira, H., Salas Herrero, M.F., Kelly, M., Philips, G., Solheim, A.L. and Poikane, S., Physico-chemical supporting elements: transitional and coastal waters: a review of national standards to support good ecological status, EUR 30958 EN, Publications Office of the European Union, Luxembourg, 2022, ISBN 978-92-76-46489-1, doi:10.2760/07826, JRC128071. 

This work provides an overview and comparison of WFD Supporting Element boundaries reported by Member States (as they stood by 2020) for transitional and coastal waters (TRaC). It is available for download at: https://publications.jrc.ec.europa.eu/repository/handle/JRC128071 

The repository contains TRAC Waters PhCh SE data (revised vs3 from 2021) and scripts needed to reproduce outputs included in the report. Below there is a description and instructions on how to run them.

README file (this document)
PhCh_TRACrevised.Rproj R project file
Data folder – containing input files needed for statistical analyses
.csv files:
            •           dat2_CTW.csv (input file) – original round 1 data 
            •           CTW_ver7e_revised_April2021.csv (input file) – final revised data
            •           UnitsV3marine.csv (input file) – units conversion file used
            •           ICType.csv (input file) – correspondence between type code and description
            •           dat.TRACver3temporary.csv (output / input file)
            •           dat.TRACver3final.csv (output all data / input file)
            •           dat.TWver3.csv (output TW data)
            •           dat.CWver3.csv (output CW data)
            •           QE_GMbound_new.csv (input file for Chapter 4 analyses as of 2020 revisions)
            •           QE_GMbound_old.csv (input file for Chapter 4 analyses as of 2020 revisions)
.RData files:
            •           all .RData output files for each PhCh SE in each water category (transitional; coastal)
Tables folder – summary tables for Chapter 1 and Chapter 2 in html format
            •           Ch1Tabl1.1.html (output file) chapter 1 Table 1.1
            •           Ch2Tabl2.2.html (output file) chapter 2 Table 2.2
R folder – containing .R files
            •           Merge-old-new-GM.R (run this script for Chapter 4 results)
	Background reading files only – NO NEED TO RUN SCRIPTS:
            •           MyFunctV2.R
            •           Prep_CWv5.R
            •           Prep_CWv5b.R
            •           Prep_CWv5_nut.R
            •           Prep_CWv5.R
            •           Prep_TWv5_nut.R
Rmd folder – contains the following files (run by numbering order) for Chapters 1, 2 and 4
            •           1_TRACjoin-revised.Rmd (joins original data to MS updates/corrections)
            •           2_TRACrevised_Summary.Rmd (produces Chapter 1 and Chapter 2 tables as well as other overview tables summarising tables)
	The two above corresponding  .nb.html files in this folder can be opened directly in a web browser for visualization of summary outputs produced by the .Rmd files
            •           TRAC-GMbound-compare.Rmd Chapter 4
            •           TRAC-GMbound-outliers.Rmd Chapter 4
            •           LongDatOutliers.Rmd (not needed – supporting analyses)
            •           chp4 subfolder – where outputs (single word files .docx) of each SE are stored to be merged into a single document chp4_ecostat2014-WFDreporting.docx
	Background reading files only:
            •           draft-styles.docx (background reading file)
Rmd_CW folder – contains Chapter 3 files (run .Rmd as ordered)
            •           some SE have a separate script (…AppV?.Rmd) which needs to be run prior to the correspondent SE Rmd script
            •           .Rmd files with script for Chapter 3 results for each SE in CW
            •           respective outputs to word documents per SE (outputs)
            •           separate folders per SE with figures results (outputs)
	Background reading files only:
            •           draft-styles.docx (background reading file)
Rmd_TW folder – contains Chapter 3 files (run as ordered)
            •           some SE have a separate script (…AppV?.Rmd) which needs to be run prior to the correspondent SE Rmd script
            •           .Rmd files with script for Chapter 3 results for each SE in CW
            •           respective outputs to word documents per SE (outputs)
            •           separate folders per SE with figures results (outputs)
	Background reading files only:
            •           draft-styles.docx (background reading file)

