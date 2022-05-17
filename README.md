# Tplyr Hands on Training
 
Getting to know the world of R can be difficult. With the huge variety of open source packages available on CRAN, it can be challenging to know the right tools to get the job done. This workshop will help makes things clearer by giving you a hands on walk through of using the R package Tplyr to create clinical safety summaries. By the end of the 90 minutes, you will have R code in hand to create a demographics, adverse events, and lab summaries, and a basic understanding of how Tplyr can support these tables for you.

# Section 1: Basics
- What is a Tplyr table? 
- Where does Tplyr begin? Where does it end? And why? 
  - Built to work loosely off ADaM structures, assumes that data are "presentation ready"
  - Output is a data frame that's ready for a presentation library (using some additional post-processing tools that Tplyr has to help)
  - Why? Because there are a lot of styling libraries out there, so we want you to do what you want
- Tplyr works in "layers" - and Tplyr has 3 types
  - `group_count()` for categorical summaries
  - `group_desc()` for descriptive statistics
  - `group_shift()` for creating shift tables (largely the same concepts as `group_count()`)
- <Overview the basic demographics Tplyr code>
  
# Exercise 1: 10 minutes
- Open <file> in the demographics folder and fill in the specified sections

# Section 2: 
- Population Data
  - Header N counts may come from separate data, like in an Adverse Events table
- Format strings 
  - Tplyr allows for highly flexible structure of how counts are presented in the table
  - The formatting helps ensure that decimals align, but keeps the structure rather free
- Nesting
  - Particularly in Adverse Event tables, you might need to summarize two variables. These are nested count layers, and you can produce them by providing two target variables. 
- Order variables
  - How about sorting? Tplyr gives you several options to create order variables, which you can use to sort the resulting table
  - This can happen several ways
    - If your by variable has an N version (i.e. AVISITN for AVISIT), Tplyr will find it
    - If your variable is a factor, that order will be respected (this was in the Demographics example!)
    - If it's a character variable, alphabetical orders will be established
    - Or, you can specify a result value to pick the order variable - for example, if you want to sort by descending occurrence of AEs within the Xanomeline High Dose group. 
    
# Exercise 2:
- Open <file> in the demographics folder and fill in the specified sections

# Section 3:
- Descriptive statistics layers can summarize multiple variables
- Numeric formatting can also support auto precision
  - using 'a' instead of 'x' will trigger auto-precision to be used in a format
  - This can be to support the integer width or the decimal width
  - The precision can be incremented by specifying 'a+n', where 'n' is the additional number of spaces you'd like to add to the precision
  - In the latest version of Tplyr sitting in our devel branch (and installed in the workshop environment), precision can be provided by an external source (like a company standard). 
  - When auto-calculating precision and summarizing multiple variables, you need to specify which variable to use for precision. Additionally, you can specify the by grouping.
