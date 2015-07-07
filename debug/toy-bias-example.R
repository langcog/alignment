---
title: "Untitled"
author: "Gabe Doyle"
date: "Wednesday, July 01, 2015"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
users <- c(1,2,3,4)
power <- c(T,F,T,F)
markers <- c('the','of')
for (user1 in users) {
  for (user2 in users) {
    if (user1==user2) {
      next
    }
    print(c(user1,user2))
    basea = .4
    baseb = .6
    bias = 0
    numiters = 1000
    if (power[user1]) {
      bias = bias + .2
    }
    if (power[user2]) {
      bias = bias - .2
    }
    m1a = runif(numiters)>basea
    m1b = runif(numiters)>baseb
    m2a = runif(numiters)>basea+(m1a*bias)
    m2b = runif(numiters)>baseb+(m1b*bias)
    
    msg1 = paste('hiya',ifelse(m1a,markers[1],''),ifelse(m1b,markers[2],''),sep=' ')
    msg2 = paste('hiya',ifelse(m2a,markers[1],''),ifelse(m2b,markers[2],''),sep=' ')
    
    df <- data.table(msgid=round(runif(numiters)*100000),msguser=rep(user1,numiters),msgtext=msg1,replyid=round(runif(numiters)*100000),replyuser=rep(user2,numiters),replytext=msg2)
    write.table(df,file="toy.users",append=T,row.names=F,sep='\t',quote=F,col.names=F)
  }
}
```