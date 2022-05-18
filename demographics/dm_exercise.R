
#### PharmaSUG Atorus Hands on Training -----
#### Making Clinical Tables with Tplyr 
#### Authors: Mike Stackhouse and Jessica Higgins PhD


## QUESTION: ----

#Now that you have seen how Tplyr works, you can update the code to create your own demographics table. Using the skills you have learned, add the following metrics to the demographics table. `ETHNIC`, and `WEIGHTBL`. Some of the code has been provided for you to start, use the example as a template.






## Start of Exercise ----

# Demographics Table

## Load packages ---
# This loads all of the packages needed to create the tables
library(Tplyr)
library(pharmaRTF)
library(dplyr)
library(tidyr)


## Reading in the data and data manipulations ----



## Read in necessary source data ----
# This part of the code reads the data into R 
adsl <- haven::read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adsl.xpt"))



## Some pre-processing ----
# This section of code takes care of the pre-processing, involving renaming variables and adjusting the case to fit with what we want displayed. We will not be discussing this pre-processing in this training.
adsl <- adsl %>%
  # In the data, SEX is M and F but we want Male and Female in the display
  mutate(SEX = recode(SEX, 'M' = "Male", 'F' = "Female")) %>% 
  # We also want to change the casing of the ETHNIC and RACE variables, so we'll do that here
  mutate(RACE = recode(RACE, "WHITE" = "White", "BLACK OR AFRICAN AMERICAN"="Black or African American", 
                       "AMERICAN INDIAN OR ALASKA NATIVE"="American Indian or Alaska Native")) %>% 
  mutate(ETHNIC = recode(ETHNIC, "HISPANIC OR LATINO" = "Hispanic or Latino", "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino"))


# In R, factors are a ranked category data type. Factors play nice with the Tplyr package! We can order and insert 0 rows for counts within our displays as needed. 
adsl$RACE <- factor(adsl$RACE, levels=c("American Indian or Alaska Native", "Asian", "Black or African American", 
                                        "Native Hawaiian or Other Pacific Islander",
                                        "White", "Multiple"))
adsl$AGEGR1 <- factor(adsl$AGEGR1, levels = c("<65", "65-80", ">80"))



## Demographics Table Work Area ----

# Now that the data is processed and ready for our table we can begin constructing the table itself.

# 1. Create the Tplyr table object - this is like a specification for the table and includes the grouping variable (in this example TRT01P)
# 2. Add layers as needed, remember to choose the correct layer type for the type of data you are displaying
t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(SEX, by = "Sex n (%)")
  ) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age Categories n (%)")
  ) %>% 
  add_layer(
    group_count(RACE, by = "Race n (%)")
  ) %>% 
  ### HINT: Add the ETHNIC and WEIGHTBL layers here. Remember to choose the right layer function for each variable from the Tplyr package.

  
  
  
# 3. Now build the table - this is where the number crunching happens and is a very important step
  demog1 <- t %>% 
  build()

# 4. View the data
demog1

# 5. Now let's make it pretty and adjust how it is displayed.
demog2 <- demog1 %>% 
  apply_row_masks(row_breaks = TRUE) %>% # Blank out repeating row labels and insert row breaks
  select(row_label1, row_label2, var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`) %>% # Take only the columns we need
  add_column_headers(
    paste0("Demographic Parameter | | Placebo\\line(N=**Placebo**)| Xanomeline Low Dose\\line(N=**Xanomeline Low Dose**)",
           "|Xanomeline High Dose\\line(N=**Xanomeline High Dose**)"), 
    header_n = header_n(t)
  ) # Huxtable-ready column headers and header Ns


## This section contains the code needed for adjusting the table aesthetics using the huxtable package, we will not be discussing it in this training. ----

# This is a table styling library named huxtable that has good RTF capabilities
ht <- huxtable::as_hux(demog2, add_colnames=FALSE) %>% # `add_colnames` is FALSE because we already added our own
  huxtable::set_bold(1, 1:ncol(demog2), TRUE) %>% # Bold the header row
  huxtable::set_align(1, 3:ncol(demog2), 'center') %>% # Center align the results headers
  huxtable::set_align(2:nrow(demog2), 3:ncol(demog2), 'center') %>% # Center align the results
  huxtable::set_valign(1, 1:ncol(demog2), 'bottom') %>% # Bottom align the header row
  huxtable::set_bottom_border(1, 1:ncol(demog2), 1) %>% # Put a border under the header row
  huxtable::set_top_border(1, 1:ncol(demog2), 1) %>% # Put a border above the header row
  huxtable::set_width(1.5) %>% # Set the table width
  huxtable::set_escape_contents(FALSE) %>% # Don't escape RTF syntax
  huxtable::set_col_width(c(.22, .3, .16, .16, .16)) # Set the column widths 



## This section of code is where we will use pharmaRTF to create the RTF output. Here we are adding the titles and footnotes. We will not be discussing this in the training today. ----

# plugging gaps not supported by Huxtable for clinical tables
doc <- rtf_doc(ht, header_rows=1) %>%
  # This gets our rows nice and close together - find more info in the docs
  set_ignore_cell_padding(TRUE) %>% 
  set_font_size(9) %>% 
  # Add in a title
  add_titles(
    hf_line("Demographic Summary", bold=TRUE, align = 'left', font_size = 11),
    hf_line("Safety Population", bold=TRUE, align = 'left', font_size = 11)
  ) %>% 
  # Add some footnotes
  add_footnotes(
    hf_line(paste0("Abbreviations:  N = number of subjects in the population; Q1=25th Percentile;", 
                   " Q3= 75th Percentile; SD=Standard Deviation."), bold = TRUE, align='left'),
    hf_line("Number of subjects with non-missing data, used as the denominator.", bold = TRUE, align='left')
  )

# Now we can write out the RTF file! 
write_rtf(doc, here::here("demographics", "demog.rtf"))
