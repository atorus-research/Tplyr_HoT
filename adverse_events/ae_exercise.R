
#### PharmaSUG Atorus Hands on Training -----
#### Making Clinical Tables with Tplyr 
#### Authors: Mike Stackhouse and Jessica Higgins PhD


## QUESTION: ----

#Now that you have seen how Tplyr works, you can update the code to adjust the formatting string for how the numbers are displayed as follows:

# '00 (00.0%) [0]' for the distinct n, the distinct percentage, and the overall n 

# Some of the code has been provided for you to start, use the example as a template.#Some of the code has been provided for you to start, use the example as a template.
# Need some help? Check this link:
# https://atorus-research.github.io/Tplyr/articles/count.html#distinct-versus-event-counts
# HINT: Add your code at lines 53-54



## Load packages ---
# This loads all of the packages needed to create the tables
library(Tplyr)
library(pharmaRTF)
library(dplyr)
library(tidyr)


## Read in necessary source data ----
# This part of the code reads the data into R, in this example we need boy the adsl and the adae
adsl <- haven::read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adsl.xpt"))
adae <- haven::read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adae.xpt"))

## Adverse Event Work Area ----


# Now that the data is read into R, we can begin constructing the table itself.

# 1. Create the Tplyr table object - this is like a specification for the table and includes the grouping variable (in this example TRTA)
# 2. Set the population data and treatment variable, in this example they are not found in the adae so we need to use the adsl for population data.
# 3. Add layers as needed, remember to choose the correct layer type for the type of data you are displaying

t <- tplyr_table(adae, TRTA, where = SAFFL == "Y") %>% 
  set_pop_data(adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  add_total_group() %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% 
      # Distinct counts by subject
      set_distinct_by(USUBJID) %>% 
      # Nest the row labels together
      set_nest_count(TRUE) %>% 
      # Specify the results format
      set_format_strings(f_str()) %>% #### HINT: Add your code to set the format string here
      
      # These three functions set you up to be able to sort by descending
      # occurrence within the Xanomeline High Dose group
      set_order_count_method("bycount", break_ties = 'desc') %>%
      set_ordering_cols("Xanomeline High Dose") %>%
      set_result_order_var(distinct_n)
  )

# 4. Now build the table - this is where the number crunching happens and is a very important step 
ae1 <- t %>% 
  build()

# 5. View the data 
ae1

# 6. Now let's make it pretty and adjust how it is displayed.
ae2 <- ae1 %>% 
  arrange(desc(ord_layer_1), desc(ord_layer_2)) %>% 
  apply_row_masks(row_breaks = TRUE, ord_layer_index, ord_layer_1) %>% # Blank out repeating row labels and insert row breaks
  arrange(desc(ord_layer_1), ord_break, desc(ord_layer_2)) %>%  # NOTE: Issue filed in Tplyr to update and avoid this second sort
  select(row_label1, var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`, var1_Total) %>% # Take only the columns we need
  add_column_headers(
    paste0("System Organ Class\\line   Preferred Term | Placebo\\line(N=**Placebo**)\\line n (% [E]| Xanomeline Low Dose\\line(N=**Xanomeline Low Dose**)\\line n (% [E]",
           "|Xanomeline High Dose\\line(N=**Xanomeline High Dose**)\\line n (% [E]|Total\\line(N=**Total**)\\line n (% [E]"), 
    header_n = header_n(t)
  ) # Huxtable-ready column headers and header Ns

## This section contains the code needed for adjusting the table aesthetics using the huxtable package, we will not be discussing it in this training. ----

# This is a table styling library named huxtable that has good RTF capabilities
ht <- huxtable::as_hux(ae2, add_colnames=FALSE) %>% # `add_colnames` is FALSE because we already added our own
  huxtable::set_bold(1, 1:ncol(ae2), TRUE) %>% # Bold the header row
  huxtable::set_align(1, 2:ncol(ae2), 'center') %>% # Center align the results headers
  huxtable::set_align(2:nrow(ae2), 2:ncol(ae2), 'left') %>% # Center align the results
  huxtable::set_valign(1, 1:ncol(ae2), 'bottom') %>% # Bottom align the header row
  huxtable::set_bottom_border(1, 1:ncol(ae2), 1) %>% # Put a border under the header row
  huxtable::set_top_border(1, 1:ncol(ae2), 1) %>% # Put a border above the header row
  huxtable::set_width(1.5) %>% # Set the table width
  huxtable::set_escape_contents(FALSE) %>% # Don't escape RTF syntax
  huxtable::set_col_width(c(.4, .15, .15, .15, .15)) # Set the column widths 

# Now we'll use pharmaRTF to create the RTF output - plugging gaps not supported by Huxtable for clinical tables
doc <- rtf_doc(ht, header_rows=1) %>%
  # This gets our rows nice and close together - find more info in the docs
  set_ignore_cell_padding(TRUE) %>% 
  set_font_size(9) %>% 
  # Add in a title
  add_titles(
    hf_line(paste0("Summary of Adverse Events by Preferred Term in Descending Frequency", 
                   " of Xanomeline High Dose within System Organ Class"), 
            bold=TRUE, align = 'left', font_size = 11),
    hf_line("Safety Population", bold=TRUE, align = 'left', font_size = 11)
  ) %>%
  # Add some footnotes
  add_footnotes(
    hf_line(paste0("Abbreviations: N = number of subjects in the population; n=number of patients with at least one row event; ",
                   "E=number of events. Percentages are calculated relative to the treatment group N."), bold = TRUE, align='left', font_size = 9),
    hf_line(paste0("Subjects may be counted in more than one row."), bold = TRUE, align='left', font_size = 9)
  ) 

# Now we can write out the RTF file! 
write_rtf(doc, here::here("adverse_events", "ae.rtf"))

