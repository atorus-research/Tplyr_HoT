# Demographics Table
# 
# Date: 2022-05-16


## Load packages ---
library(Tplyr)
library(pharmaRTF)
library(dplyr)
library(tidyr)

## Read in necessary source data ----
adsl <- haven::read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adsl.xpt"))

## Some pre-processing ----
adsl <- adsl %>%
  # In the data, SEX is M and F but we want Male and Female in the display
  mutate(SEX = recode(SEX, 'M' = "Male", 'F' = "Female")) %>% 
  # We also want to change the casing of the ETHNIC and RACE variables, so we'll do that here
  mutate(RACE=recode(RACE, "WHITE" = "White", "BLACK OR AFRICAN AMERICAN"="Black or African American", 
                     "AMERICAN INDIAN OR ALASKA NATIVE"="American Indian or Alaska Native")) %>% 
  mutate(ETHNIC = recode(ETHNIC, "HISPANIC OR LATINO" = "Hispanic or Latino", "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino"))


# Factors play nice with Tplyr! We can order and insert 0 rows for counts within our displays
adsl$RACE <- factor(adsl$RACE, levels=c("American Indian or Alaska Native", "Asian", "Black or African American", 
                                        "Native Hawaiian or Other Pacific Islander",
                                        "White", "Multiple"))
adsl$AGEGR1 <- factor(adsl$AGEGR1, levels = c("<65", "65-80", ">80"))

## Demographics Work Area ----

# Create the Tplyr table object - this is like a specification for the table
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
  ## User supplied section ---- 
  add_layer(
    group_count(ETHNIC, by = "Ethnic n (%)")
  ) %>% 
  add_layer(
    group_desc(WEIGHTBL, by = "Weight (kg)")
  )

# Now build the table - this is where the number crunching happens
demog1 <- t %>% 
  build()

# View the data 
demog1

# Now let's make it pretty
demog2 <- demog1 %>% 
  apply_row_masks(row_breaks = TRUE) %>% # Blank out repeating row labels and insert row breaks
  select(row_label1, row_label2, var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`) %>% # Take only the columns we need
  add_column_headers(
    paste0("Demographic Parameter | | Placebo\\line(N=**Placebo**)| Xanomeline Low Dose\\line(N=**Xanomeline Low Dose**)",
           "|Xanomeline High Dose\\line(N=**Xanomeline High Dose**)"), 
    header_n = header_n(t)
  ) # Huxtable-ready column headers and header Ns
  
# This is a table styling library named Huxtable that has good RTF capabilities
ht <- huxtable::as_hux(demog2, add_colnames=FALSE) %>% # `add_colnames` is FALSE because we already added our own
  huxtable::set_bold(1, 1:ncol(demog2), TRUE) %>% # Bold the header row
  huxtable::set_align(1, 3:ncol(demog2), 'center') %>% # Center align the results headers
  huxtable::set_align(2:nrow(demog2), 3:ncol(demog2), 'center') %>% # Center align the results
  huxtable::set_valign(1, 1:ncol(demog2), 'bottom') %>% # Bottom align the header row
  huxtable::set_bottom_border(1, 1:ncol(demog2), 1) %>% # Put a border under the header row
  huxtable::set_top_border(1, 1:ncol(demog2), 1) %>% # Put a border above the header row
  huxtable::set_width(1.5) %>% # Set the table width
  huxtable::set_escape_contents(FALSE) %>% # Don't escape RTF syntax
  huxtable::set_col_width(c(.22, .27, .15, .18, .18)) # Set the column widths 

# Now we'll use pharmaRTF to create the RTF output - plugging gaps not supported by Huxtable for clinical tables
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
