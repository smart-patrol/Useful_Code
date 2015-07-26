
#copy the data that you wnat from excel first and then run this to 
# enter in as an R dataset 
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

dat=read.excel()


