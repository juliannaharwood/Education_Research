# Physics Education Research
## How do students learn first year physics material?

This is a repo for a sample of programs from my research for Dr. Candice Etson's work in physics education. We analyzed how students changed their answers to a standardized assessment given at the beginning and end of first year physics classes.

The first function takes the output of a Scantron (two excel sheets, one for the test at the beginning of the course and one for the test at the end of the course) and scores it against the test key (the third argument of the function). It then computes the five developed metrics (defined in lines 101-106) from the "transitions" (defined in lines 62-74) for each student and each question and adds the calculations to the margins. In addition to this data set, the function will return a summary of the five metrics for the entire data set and a pie chart of the same metrics for visualization. This is valuable as a general summary for how successful a course was and also gives the user a data table that is easily analyzed to answer any specific questions the user might have.

The "flags" function takes two arguments - a metric and the data set created by "scoreTests" - and loops through each question of the test and returns a list of the all the questions that fall below (or above in some cases) some critical mark that indicates an insignificant level for the metric. This function essentially shows the user for which questions in the class of data did the transitions of answers from the pre to posttest not behave in the expected way.

The first function was co-written with Dr. Rob Kabacoff, PhD.
