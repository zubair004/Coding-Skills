rm(list = ls())

#required libraries
require(reshape)
library(data.table)
library(dplyr)

# Need to use "Predict.csv" and "Exceptions.csv" files and write the code to get the output as "FinalOutput.csv". 

#loading the data
predict = read.csv('Predict.csv')
excep = read.csv('Exceptions.csv')
output  = read.csv('FinalOutput.csv')


df = melt(excep, measure.vars = names(excep))
names(df) = c('COMMENT','Product')

df = merge(predict, df, by = 'Product', all = T) # merging by product

df = df[!is.na(df$Account.Number),] # removing empty Account numbers

df$COMMENT[is.na(df$COMMENT)] = names(which.max(table(df$COMMENT))) # missing value imputation with mode


exception_products = function(acc){
  x = subset(df,Account.Number == acc)
  tmp = data.frame()
  for(i in unique(x$Product[x$COMMENT == 'SubProduct'])){
    flag = (x$Product[x$COMMENT == 'MainProduct']) %in% (excep$MainProduct[excep$SubProduct == i])
    tmp = rbind(tmp, data.frame(Account.Number = acc, Product = i, output = flag))
  }
  return(tmp)
}


tmp1 = data.frame()
for(i in as.character(unique(df$Account.Number))){
  df_tmp = exception_products(i)
  tmp1 = rbind(tmp1, df_tmp)
  
}

output_df = merge(df, tmp1, by = c('Account.Number','Product'), all.x = T)
output_df$output[is.na(output_df$output)] = 'NO'
output_df$output = ifelse(output_df$output == TRUE,'NO','YES')

#final data set as mentioned
View(output_df)

