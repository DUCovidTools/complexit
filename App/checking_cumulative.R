## Checking all jumps are positive

setwd("C:/Users/Rachel Oughton/Dropbox/postdocs/COVID19/ComplexIt/App_regression")
source("global.R")


for (i in 1:nrow(inData)){
  row_i = inData[i, which(!is.na(inData[i,]))]
  row_i_nums = row_i[-(1:4)]
  names_i = names(row_i_nums)
  row_i_nums = as.numeric(row_i_nums)
  n_row_i = length(row_i_nums)
  row_i_diffs = row_i_nums[-1] - row_i_nums[-n_row_i]
  if(any(row_i_diffs<0)){
    neg_dates = names_i[row_i_diffs < 0]
    cat(as.character(row_i$areaName), paste(neg_dates, collapse = ", "), "\n\n")
  }
}
