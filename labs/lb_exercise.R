
#### PharmaSUG Atorus Hands on Training -----
#### Making Clinical Tables with Tplyr 
#### Authors: Mike Stackhouse and Jessica Higgins PhD

## QUESTION: ----

# Now that you have seen how Tplyr works, you can update the code to set the auto decimal precision as follows:
# Mean, auto decimal precision +1 
# SD, auto decimal precision +2
# Median, auto decimal precision +1
# Q1 and Q3, auto decimal precision +1
# Min, Max, auto decimal precision +0
# Some of the code has been provided for you to start, use the example as a template.
# Having trouble? Check this link for some help:
# https://atorus-research.github.io/Tplyr/articles/desc.html#auto-precision
# HINT: Insert code at lines 58-62






## Load packages ---
# This loads all of the packages needed to create the tables
library(Tplyr)
library(pharmaRTF)
library(dplyr)
library(tidyr)

## Read in necessary source data ----
# This part of the code reads the data into R 
adsl <- haven::read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adsl.xpt"))
adlb <- haven::read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adlbc.xpt"))

## Labs Work Area ----


# Now that the data is read into R, we can begin constructing the table itself.

# 1. Create the Tplyr table object - this is like a specification for the table and includes the grouping variable (in this example TRTA, SAFFL == Y, and PARAMCD == CA, and where the AVISITN is not NA and AVISITN > 0)
# 2. Set the population data and treatment variable, in this example they are not found in the adlb so we need to use the adsl for population data.
# 3. Add layers as needed, remember to choose the correct layer type for the type of data you are displaying
# 4. Adjust the decimal precision as needed and set the precision to a specific variable which in this example is AVAL.


# Filter down to Calcium tests in the Safety group post-baseline
t <- tplyr_table(adlb, TRTA, where = SAFFL == "Y" & PARAMCD == "CA" & !is.na(AVISITN) & AVISITN > 0) %>% 
  set_pop_data(adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  # Override the table filter for population data because target data filters
  # have variables not in the population data
  set_pop_where(SAFFL == "Y") %>% 
  add_layer(
    group_desc(vars(BASE, AVAL), by = AVISIT) %>% 
      set_format_strings(
        "N"        = f_str("xx", n), ##### HINT: This is where you should set the auto decimal precision as described in the question 
        "Mean"     = f_str(, mean), 
        "SD"       = f_str(, sd), 
        "Median"   = f_str(, median),
        "Q1, Q3"   = f_str(, q1, q3),
        "Min, Max" = f_str(, min, max), 
        cap = c(dec=2)  # If limit auto-collected precision to 2 decimal spaces
      ) %>%
      set_precision_on(AVAL)
  )

# 5. Now build the table - this is where the number crunching happens and is a very important step
labs1 <- t %>% 
  build()

# 6. View the data 
labs1

# 7. Now let's make it pretty
labs2 <- labs1 %>% 
  apply_row_masks(row_breaks = TRUE, ord_layer_1) %>% # Blank out repeating row labels and insert row breaks
  select(row_label1, row_label2, var1_Placebo, var2_Placebo, 
         `var1_Xanomeline Low Dose`, `var2_Xanomeline Low Dose`,
         `var1_Xanomeline High Dose`, `var2_Xanomeline High Dose`) %>% # Take only the columns we need
  add_column_headers(
    paste0("Timepoint| | Placebo\\line(N=**Placebo**) {At Timepoint | Change from Baseline}",
           "| Xanomeline Low Dose\\line(N=**Xanomeline Low Dose**) {At Timepoint | Change from Baseline} |",
           "Xanomeline High Dose\\line(N=**Xanomeline High Dose**) {At Timepoint | Change from Baseline}"),
    header_n = header_n(t)
  ) # Huxtable-ready column headers and header Ns




## This section contains the code needed for adjusting the table aesthetics using the huxtable package, we will not be discussing it in this training. ----
ht <- huxtable::as_hux(labs2, add_colnames=FALSE) %>% # `add_colnames` is FALSE because we already added our own
  huxtable::merge_cells(1, 3:4) %>% # Merges cells for placebo
  huxtable::merge_cells(1, 5:6) %>% # Merges cells for Low Xanomeline Dose
  huxtable::merge_cells(1, 7:8) %>% # Merges cells for High Xanomeline Dose
  huxtable::set_bold(1:2, 1:ncol(labs2), TRUE) %>% # Bold the header rows
  huxtable::set_align(1:2, 3:ncol(labs2), 'center') %>% # Center align the results headers
  huxtable::set_align(3:nrow(labs2), 3:ncol(labs2), 'center') %>% # Center align the results
  huxtable::set_valign(1:2, 1:ncol(labs2), 'bottom') %>% # Bottom align the header rows
  huxtable::set_top_border(1, 1:ncol(labs2), 1) %>% # Put a border above the header row
  huxtable::set_bottom_border(2, 1:ncol(labs2), 1) %>% # Put a border under the header row
  huxtable::set_width(1.5) %>% # Set the table width
  huxtable::set_escape_contents(FALSE) %>% # Don't escape RTF syntax
  huxtable::set_col_width(c(.11, .11, .13, .13, .13, .13, .13, .13)) # Set the column widths

# Now we'll use pharmaRTF to create the RTF output 
# Two header rows because we have two rows of column headers
doc <- rtf_doc(ht, header_rows=2) %>%
  # This gets our rows nice and close together - find more info in the docs
  set_ignore_cell_padding(TRUE) %>% 
  set_font_size(9) %>% 
  # Add in a title
  add_titles(
    hf_line("Summary of Change from Baseline Results: Calcium (mmol/L)", bold=TRUE, align = 'left', font_size = 11),
    hf_line("Safety Population", bold=TRUE, align = 'left', font_size = 11)
  ) %>%
  add_footnotes(
    hf_line(paste0("Abbreviations: N = number of subjects in the population; Q1=25th Percentile;",
                   " Q3= 75th Percentile; SD=Standard Deviation."), bold = TRUE, align='left', font_size = 9)
  )

# Now we can write out the RTF file! 
write_rtf(doc, here::here("labs", "labs.rtf"))