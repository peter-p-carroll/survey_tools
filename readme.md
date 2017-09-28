Peter Carroll 

28 September 2017

This R function takes a dataset of survey durations with variables for the survey duration ("duration"), enumerator ID ("enum_id"), and, optionally, survey type ("survey_type"), and produces a graph that displays boxplots of survey duration, by enumerator, and overlaid, jittered points for each survey duration, also by enumerator.

Note on survey_type: Providing a string or numeric variable for the "survey_type" argument colors the dots by survey type. Sometimes some respondents receive distinct versions of the same survey. These survey versions may have varying expected durations. Displaying differing point colors for each survey type may be helpful in getting a general sense of the duration of these distinct surveys. However, it may be more useful to subset the data and produce separate graphs for these different survey versions.