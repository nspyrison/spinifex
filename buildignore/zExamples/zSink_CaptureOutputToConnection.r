log <- vector('character')
con <- textConnection('log', 'wr', local = TRUE)
  
## Divert to connection
sink(con)
1:10
a <- 4
a
100
# end divert
sink()

# close con
close(con)
# print 
log
str(log)
cat(log, sep ="\n")
