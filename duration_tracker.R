# Peter Carroll
# Tool for tracking survey durations, by enumerator
require(ggplot2)

# Generate test data
set.seed(20170928)
data <- data.frame(enum_id = rep(seq(1:15), 20),
                   duration = round(rnorm(300, 60, 15)),
                   survey_type = round(runif(300, 1, 3)),
                   stringsAsFactors = F)
head(data)

# Graph function takes arguments for the data, enumerator ID ("enum_id"), duration, and survey_type (sometimes some respondents receive different versions of the survey with varying lengths. Providing a string or numeric variable for this argument colors the dots by survey type. Note that it may be more useful to subset the data and produce separate graphs for these different survey versions).
survey_tracker_graph <- function(data, enum_id, duration, survey_type) {
  if (missing(survey_type)) {
    # No survey type specified
    p <- ggplot(data, aes(x = factor(enum_id), y = duration)) + 
      # Produce boxplots (asterisks for outliers)
      geom_boxplot(na.rm = T, outlier.shape = 8) +
      # Flip coordinates so that boxplots are horizontal
      coord_flip() + 
      # Plot points from individual surveys; jitter these dots for easier viewing
      geom_point(aes(), position = position_jitter(width = 0.1),
                 alpha = 0.6) + 
      # Blank theme
      theme_bw() +
      # Label axes
      ylab("Duration") + 
      xlab("Enumerator") +
      # Graph title
      ggtitle("Survey duration by enumerator") +
      # add mean and median
      geom_hline(aes(yintercept = mean(data$duration), linetype = "mean"), 
                 color = "red", 
                 lwd = 1.25,
                 alpha = 0.75) +
      geom_hline(aes(yintercept = median(data$duration), linetype = "median"), 
                 color = "blue", 
                 lwd = 1.25,
                 alpha = 0.75) +
      scale_linetype_manual(name = "Statistics", values = c(2, 2), 
                            guide = guide_legend(override.aes = list(color = c("red", "blue"))))
  } else {
    # Survey type included
    p <- ggplot(data, aes(x = factor(enum_id), y = duration)) + 
      # Produce boxplots (asterisks for outliers)
      geom_boxplot(na.rm = T, outlier.shape = 8) +
      # Flip coordinates so that boxplots are horizontal
      coord_flip() + 
      # Plot points from individual surveys
      # Use survey type to color values
      geom_point(aes(color = factor(survey_type)),
                 alpha = 0.6, 
                 # Jitter these dots for easier viewing
                 position = position_jitter(width = 0.1)) +
      labs(color = "Survey type") +
      # Blank theme
      theme_bw() +
      # Label axes
      ylab("Duration") + 
      xlab("Enumerator") +
      # Graph title
      ggtitle("Survey duration by enumerator") +
      # add mean and median
      geom_hline(aes(yintercept = mean(data$duration), linetype = "mean"), 
                 color = "red", 
                 lwd = 1.25,
                 alpha = 0.75) +
      geom_hline(aes(yintercept = median(data$duration), linetype = "median"), 
                 color = "blue", 
                 lwd = 1.25,
                 alpha = 0.75) +
      scale_linetype_manual(name = "Statistics", values = c(2, 2), 
                            guide = guide_legend(override.aes = list(color = c("red", "blue"))))
  }
  return(p)
}

# Graph without survey type
make_graph(data = data, 
           enum_id = enum_id,
           duration = duration)

# Graph with survey type
make_graph(data = data, 
           enum_id = enum_id,
           duration = duration, 
           survey_type = survey_type)
