# Design and environmental factors influencing the stability of fish passage culverts in the Anchorage area

## Overview

basic intent: quantifying instability, effects of design and measured variables on instability

what I did: 
- tried four ways of calculating instability score for four variables
- EDA for the scores vs all possible variables
- MLR & model selection for simultaneous effects (plus regression tree)

punchline(s)??

## Work products and the **/Output** subfolder

* **Instability_EDAplots.docx**:
The intent of this document was an Exploratory Data Analysis, in which all Vscores were plotted
against all possible design or measured variables.  Test p-values are included with each plot 
(linear regression, ANOVA, or $\chi^2$, depending on data types), but should not necessarily be 
interpreted biometrically; rather, to aid qualitative assessment of patterns.

  Generation of all plots was automated.  **Plots are organized as follows:**
  - Cluster (with subheading): possible design or measured variable
  - Row: Vscore (Interior Channel Width, Interior Gradient, Height, Bank Length)
  - Column: Vscore type (numeric, directional vs absolute; categorical, directional vs absolute)
  
  This documentation is a presentation of the body of work found in R/3_Instability_EDA.R

* **Instability_MLRmodels.docx**:
The intent of this document was to explore the simultaneous effects of all design or measured 
variables on instability, as quantified by all Vscores considered.  Multiple regression was the 
primary tool used for this, but regression trees were also constructed as a supplemental alternative.

  With many (35!) possible variables to consider, model selection was a challenging task.  Much
  of the document is devoted to an agglomerative model-construction algorithm using
  leave-one-out cross validation (LOOCV) as a loss function, which I still think was super cool.
  
  Note: this analysis only considered **directional and numeric** Vscores as a means of quantifying
  instability, as these appeared to tell the cleanest story in the EDA.
  
  This documentation is a presentation of the body of work found in R/4_Instability_MLR_all.R

* **Instability_MLRmodels_subset.docx**:
This was a supplemental analysis building on the MLR models.  Since it was identified that a 
fundamental difference could exist between high- and low-gradient stream systems, the above
analysis was repeated, but with subsets of systems with a gradient less than / greater than 1.5.

## **/R** subfolder

* **1_culvert_data.R**: This script reads all data and performs some data cleanup and formatting.
It is `source()`'d in subsequent scripts.

* **2_InstabilityScore_PCA.R**:
  The central purpose of this script was to define instability scores associated
with each variable of interest, and to explore the possibility of aggregating
multiple instability scores without losing information.

  To that end, four scoring methods were defined, and graphically compared.
Matrices were then calculated using the selected methods, for use going forward, and
correlation between Vscores for all six variables was calculated and visualized.
Finally, Principal Components Analysis (PCA) was employed to investigate possible aggregation of data.

  This script creates instability score matrices that are used as data object in subsequent scripts,
as well as defining some plotting functions that are used going forward.  It is therefore `source()`'d
in subsequent scripts.

* **3_Instability_EDA.R**:
  This script really starts to get into the meat of the analysis, or at least
that pertaining to the idea of "effects of design and measured variables on instability".

  Functionally, there are two main portions:
  - Creating a dataset that represents the subset of variables we care about
  Creating some additional variables as re-expressions of others
  note: these objects are used in all subsequent analyses
  - Performing an exploratory data analysis of the effects of all variables
  (by themselves) on all possible instability scores

  The ideas and output of this script are perhaps better visualized in the file
  Instability_EDAplots.docx (created by Instability_EDAplots.Rmd)

  Some output is also summarized in the file
  Output/pval_table.xlsx

* **4_Instability_MLR_all.R**:
  This is more of the meat of the analysis, and goes beyond the effect of one
variable at a time, looking for which COMBINATIONS of variables have the greatest
simultaneous effect on instability.

  The ideas explored in this script are perhaps better expressed and more
succinctly presented in
  - Instability_MLRmodels.docx (.Rmd)
  - Instability_MLRmodels_subset.docx (.Rmd)  - separating analyses by gradient.
  

* **5_Instability_MLR_topmodels.R**:
  The main purpose of this script was to visualize the effects within the
multiple regression models, since so far the variables had only been plotted
one at a time.

  For the top models for each Vscore (directional), plots were produced for each
variable in turn, but overlayed with MLR effects plots.  In many cases, the
data plot indicated no evidence of relationship (and p-values are taken from this)
but the MLR effects are present and evident by the effects plots.

  Effects plots were made by expressing the MLR as an equivalent Bayesian model.

  This was only done for directional Vscores, never with absolute or categorical.
This script is incomplete, and I think was abandoned at one point.  Still cool though.

* **/deprecated**: A few miscellaneous R scripts representing first-tries, abandoned exploration, or data validation.
No longer relevant, but retained as a record.

## **/Data** subfolder

* **Design variables with importance Matt 8_7_24.xlsx**: This was the version used for all analysis
and data exploration.  In terms of the data, this dataset should be functionally identical to the other dataset
retained (dated June 10), but with the inclusion of Importance Scores.  Importance Scores were as annotated
by Gillian, and represent the subset of variables (design and measured) that we care about.

* **Assessment Data June 10 2024.xlsx**: This was the original dataset provided, and is retained in case
it is useful in some way.

* **Methods and Data Summary June 10 2024.docx**: A project and data description provided by Gillian 
to inform my (Matt) analysis efforts.  This is likely a very information-rich and informative project overview.
