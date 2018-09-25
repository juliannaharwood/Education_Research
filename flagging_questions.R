## flag questions


flags <- function (metric, data) {
  question <- 1
  flagged_questions <- NULL
  row <- which(data$row_name == metric)
  values <- as.numeric (data[row,])
  values <- values[3:length(values)]
  for (value in values) {
    if (!is.na(value)) {
      if (metric == "P") {
        if (value < .13) {flagged_questions <- c(flagged_questions, question)}
        quiestion <- question + 1
      }
      if (metric == "C") {
        if (value > .13) {flagged_questions <- c(flagged_questions, question)}
        question <- question + 1
      }
      if (metric == "K") {
        if (value > .14) {flagged_questions <- c(flagged_questions, question)}
        question <- question + 1
      }
      if (metric == "M") {
        if (value > .13) {flagged_questions <- c(flagged_questions, question)}
        question <- question + 1
      }
    }
  }
  return (flagged_questions)
}

  