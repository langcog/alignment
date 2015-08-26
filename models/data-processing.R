library(data.table)
library(bit64)
library(Matrix)

d2 <- fread('pros.csv')

d2$sid <- as.factor(d2$sid)
d2$rid <- as.factor(d2$rid)

uab <- sparseMatrix(i=as.numeric(d2$sid),j=as.numeric(d2$rid),x=d2$ba+d2$nba)
unab <- sparseMatrix(i=as.numeric(d2$sid),j=as.numeric(d2$rid),x=d2$ba+d2$nbna)
cab <- sparseMatrix(i=as.numeric(d2$sid),j=as.numeric(d2$rid),x=d2$ba)
cnab <- sparseMatrix(i=as.numeric(d2$sid),j=as.numeric(d2$rid),x=d2$bna)

