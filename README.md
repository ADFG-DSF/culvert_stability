# Design and environmental factors influencing the stability of fish passage culverts in the Anchorage area

## Overview

The intent of this analysis was to investigate the effects of a suite of variables 
(design elements and on-site measurements or characteristics) on culvert stability.  This
required two things:

- A reasonable metric for quantifying instability with respect to design
- An analysis framework for testing the effects of design or measured variables on (in)stability.

### Quantifying instability

To the first point, instability was scored based on a comparison between specific measurements (below)
and their respective design values.  If measurements were very similar to design values, this was
taken as evidence of stability; dissimilarity was therefore taken as evidence of instability.
Instability was scored for four (originally six) measurements.  Note: Instability scores are 
generally referred to as **V Scores** following. 

- Interior Channel Width
- Interior Gradient
- Height
- Bank Length
- (Bank Height)
- (Bank Width)

Four scoring metrics were explored (ratio, ratio difference, log ratio, absolute log ratio).
The two variants of log ratio were deemed most appropriate theoretically, as the log transformation
linearizes multiplicative effects (for example, dissimilarity by a factor of 1/2 and a factor of 2 
will have the same magnitude).  

Both the log ratio and absolute log ratio are retained in describing effects, since the stories
they express are subtly different.  Log ratio preserves the *direction* of instability (that is,
whether things got smaller or larger than the design value), whereas absolute log ratio 
sets the result on a numerical scale between stable (values near zero) and unstable 
(large positive values).

### The effects of variables on (in)stability

First, the relationships between each variable (individually) and instability scores were assessed
graphically and tested as appropriate.  This effort should be considered as an exploratory data
analysis (EDA), hopefully illustrating any stories that may exist in the data.  
Plots were generated for all combinations of:

* Design or measured variable: All variables indicated as important by Gillian, as well as 
alternate expressions (for example, constriction ratio as numeric vs. categorical)
* Variable used for instability (V) scoring (Interior Channel Width, Interior Gradient, etc.)
These were considered separately, since instability could potentially manifest in different ways
and as the result of different causes.
* V score type (directional vs. absolute, numeric vs. categorical).  It was certainly
within conjecture that certain variables might have a directional effect (things got smaller 
vs. larger vs. stayed the same, rather than simply trending toward stable vs. unstable).
Similarly, it was within conjecture that an instability response might behave less like 
a linear, numeric effect, and more like a threshold-driven or nonlinear categorical effect.
Therefore it seemed prudent to explore all possibilities.

Next, the isolated effects of individual variables were tested using multiple linear regression (MLR).
The preponderance of work on this front was devoted to model selection, as many variables
could potentially be included, but not all added information to the model.  
An alternate analysis using nonlinear regression trees was also considered.

### Is there a punchline?

This analysis is unfinished at present, and is in the unfortunate stage of expansion and 
experimentation (explosion?) that comes before things coalesce into something that is 
shaped like a final product.  

I do believe that the log ratio gives a reasonable basis for numeric comparison, given
the data that we have.  At the very least, it's a useful metric for comparing
measurements of what things are versus what we expect them to be.

The MLR analysis is definitely more in line with the intent of the operational plan, and 
probably represents a more robust analysis product.  The punchline I see from this effort
is that there are certainly sets of design and measured variables that have surprisingly
strong relationships with instability (V) scores, particularly that for the change in
interior channel width.  MLR had the additional benefit of isolating the independent
effects of variables, which might not be apparent when considering variables one at a time.
The best model for all four instability scores considered is reported below: this 
may be interpreted as the variables with the strongest predictive power after accounting
for the effects of all other variables.  

* V Interior Channel Width ~ bank design type + features (y/n) + reach 3 design CR
* V Interior Gradient ~ (no variables improved LOOCV RMSE)
* V Height ~ reach 3 gradient + reach 3 design CR
* V Bank Length ~ banks (y/n) + culvert shape + reach 3 gradient

The intent of the EDA was purely exploratory, however.  Many variables have no effect
on instability scores (which is fine!), but some do.  I mostly wanted to automate the
generation of all possible plots, so they could be quickly scanned for stories, rather 
than having to build one plot at a time in Excel ($\times$ a few hundred plots).

Both analyses come with an important caveat: many of the variables are inter-related with
one another.  In the EDA, this means that an observed effect might be due to the variable
being considered, or due some other inter-related variable.  In the MLR, this means that once
a variable is in the model, similar variables might no longer be important.  
Therefore, the collection of variables in the top model for a given
instability score might not be those with the strongest "raw" relationship
with instability, but they will likely represent the collection of variables with the 
strongest mutually independent effects, as well as the greatest predictive power.

It's likely that the two different analyses might be inferentially useful in different
ways, depending on what is desired.

## Folder contents

### Work products and the **/Output** subfolder

* **Instability_EDAplots.docx**:
The intent of this document was an Exploratory Data Analysis, in which all V scores were plotted
against all possible design or measured variables.  Test p-values are included with each plot 
(linear regression, ANOVA, or $\chi^2$, depending on data types), but should not necessarily be 
interpreted biometrically; rather, to aid qualitative assessment of patterns.

  Generation of all plots was automated.  **Plots are organized as follows:**
  - Cluster (with subheading): possible design or measured variable
  - Row: V score (Interior Channel Width, Interior Gradient, Height, Bank Length)
  - Column: V score type (numeric, directional vs absolute; categorical, directional vs absolute)
  
  This documentation is a presentation of the body of work found in R/3_Instability_EDA.R
  
* **Output/pval_table.xlsx**:
This is a single-table summary of the p-values associated with all plots in the previous document, 
with a bit of
conditional formatting to highlight significant relationships.  Again, these should be interpreted
qualitatively and descriptively.  The file **Output/raw_pvaltable.csv** was directly exported from R,
and is the same information, minus all formatting.

* **Instability_MLRmodels.docx**:
The intent of this document was to explore the isolated and simultaneous effects of all design or measured 
variables on instability, as quantified by all V scores considered.  Multiple linear regression (MLR) was the 
primary tool used for this, but regression trees were also constructed as a supplemental alternative.

  With many (35!) possible variables to consider, model selection was a challenging task.  Much
  of the document is devoted to an agglomerative model-construction algorithm using
  leave-one-out cross validation (LOOCV) as a loss function, which I still think was super cool.
  
  Note: this analysis only considered **directional and numeric** V scores as a means of quantifying
  instability, as these appeared to tell the cleanest story.  We could certainly consider doing this
  for other V score types if desired.
  
  This documentation is a presentation of the body of work found in R/4_Instability_MLR_all.R

* **Instability_MLRmodels_subset.docx**:
This was a supplemental analysis building on that for the MLR models.  Since it was identified that a 
fundamental difference could exist between high- and low-gradient stream systems, the above
analysis was repeated, but with subsets of systems with a gradient less than / greater than 1.5.

* **new_Vscore.docx**:
This document is essentially deprecated, and was an informal presentation of some of my early ideas
for scoring instability.  It presents the absolute log ratio and some initial (mostly abandoned) ideas
for using Principal Components Analysis (PCA) to aggregate scores.

### **/R** subfolder

* **1_culvert_data.R**: This script reads all data and performs some data cleanup and formatting.
It is `source()`'d in subsequent scripts.

* **2_InstabilityScore_PCA.R**:
  The purpose of this script was to define instability scores associated
with each variable of interest, and to explore the possibility of aggregating
multiple instability scores without losing information.

  To that end, four scoring methods were defined, and graphically compared.
Matrices were then calculated using the selected methods, for use going forward, and
correlation between V scores for all six variables was calculated and visualized.
Finally, Principal Components Analysis (PCA) was employed to investigate possible aggregation of data.

  This script creates instability score matrices that are used as data object in subsequent scripts,
as well as defining some plotting functions that are used going forward.  It is therefore `source()`'d
in subsequent scripts.

* **3_Instability_EDA.R**:
  This script really starts to get into the meat of the analysis, or at least
that pertaining to the idea of "effects of design and measured variables on instability".

  Functionally, there are two main portions:
  - Creating a dataset that represents the subset of variables we care about, with 
  some additional variables created as re-expressions of others.
  Note: these objects are used in all subsequent analyses.
  - Performing an exploratory data analysis of the effects of all variables
  (by themselves) on all possible instability scores.

  The ideas and output of this script are perhaps better visualized in the file
  Instability_EDAplots.docx (created by Instability_EDAplots.Rmd)

  Some output is also summarized in the file
  Output/pval_table.xlsx

* **4_Instability_MLR_all.R**:
  This is more of the meat of the analysis, and goes beyond the effect of one
variable at a time, looking for which COMBINATIONS of variables have the greatest
simultaneous effect on instability, therefore isolating the effects of individual variables.

  The ideas explored in this script are perhaps better expressed and more
succinctly presented in
  - Instability_MLRmodels.docx (.Rmd)
  - Instability_MLRmodels_subset.docx (.Rmd)  - separating analyses by gradient.
  

* **5_Instability_MLR_topmodels.R**:
  The main purpose of this script was to visualize the effects within the
multiple regression models, since so far the variables had only been plotted
one at a time.

  For the top models for each V score (directional), plots were produced for each
variable in turn, but overlayed with MLR effects plots.  In some cases, the
data plot indicated no evidence of relationship (and p-values are taken from this)
but the MLR effects are present and evident by the effects plots.

  Effects plots were made by expressing the MLR as an equivalent Bayesian model.

  This was only done for directional V scores, never with absolute or categorical.
This script is incomplete, and I think was abandoned at one point.  Still cool though.

* **/deprecated**: A few miscellaneous R scripts representing first-tries, abandoned exploration, or data validation.
No longer relevant, but retained as a record.

### **/Data** subfolder

* **Design variables with importance Matt 8_7_24.xlsx**: This was the version used for all analysis
and data exploration.  In terms of the data, this dataset should be functionally identical to the other dataset
retained (dated June 10), but with the inclusion of Importance Scores.  Importance Scores were as annotated
by Gillian, and represent the subset of variables (design and measured) that we care about.

* **Assessment Data June 10 2024.xlsx**: This was the original dataset provided, and is retained in case
it is useful in some way.

* **Methods and Data Summary June 10 2024.docx**: A project and data description provided by Gillian 
to inform my (Matt) analysis efforts.  This is likely a very information-rich and informative project overview.
