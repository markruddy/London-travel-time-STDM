#######################################################
# 
# Add random 'white noise' to historic time series link.
# Accepts 
# data=data.frame of ujts, 
# names = list of pairs of an historic link name with its purported matching link to be modelled from c("historic","mapping")
# sds=data.frame of sds by link and period (eg AMP, IP, PMP)
# 
# 
# Mark Ruddy
# April 2, 2015
# 
#######################################################

# 1. Take one historic link week
# 2. Repeat to length time series
# 2. Add rnorm with sd from each period for each link to time series
# 3. Replace historic link data with randomised historic link data

hist_rand <- function(data, names) {
  
  D <- data
  
  for (i in 1:length(names)) {
    
    print(names[[i]][1])
    d.hist <- D[,names[[i]][1]] # historic link
    
    d.map <- D[,names[[i]][2]] # link from which sd will be taken for historic link
    
    print(head(d.hist))
    print(head(d.map))
    
    d.rand <- d.hist+rnorm(n=length(d.hist), mean = 0, sd = sd(unlist(d.map)))
    print(head(d.rand))
    
    D[,names[[i]][1]] <- d.rand
    
  }
  return(D)
}
