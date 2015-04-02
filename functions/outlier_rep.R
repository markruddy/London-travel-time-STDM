###########################################################################
# 
# Function removes outlier UJT values for a set of links on given dates 
# then replaces these date-link sets with mean UJTs from rest of dataset BEFORE other UJT means have
# been re-added
# 
# Accepts: data=tidy data.frame of |dtime|time|date|link|ujt|, od=outlier dates, ol=outlier links
# 
# April 1, 2015
# Mark Ruddy
###########################################################################

o.rep <- function(data, od, ol) { 
  require(dplyr)
  d.mod <- data
  
  # remove outliers
  for (i in 1:length(od)) {
    
    for (j in 1:length(ol[[i]])){ 
      d.mod <- d.mod %>% filter(!date == od[i] | !link == ol[[i]][j])
    }
    print(paste(od[i],'-',ol[[i]]))
  }
  D <- d.mod 
  
  # replace outlier date-links with mean UJTs from non-outlier dataset
  for (i in 1:length(od)) {
    if (length(ol[[i]])>1) {
      for (j in 1:length(ol[[i]])) {
        d.out <- d.mod %>% filter(link == ol[[i]][j])

        d.means <- d.out %>% 
          group_by(dtime) %>%
          mutate(mean=mean(ujt)) %>%
          slice(1:n_distinct(dtime)) %>%
          mutate(ndate=rep(od[i], n_distinct(dtime))) %>%
          select(dtime, time, -date, date=ndate, link, ujt=mean)
      
        D <- rbind(D, d.means)
      }
         } else {
         d.out <- d.mod %>% filter(link == ol[[i]])
         d.means <- d.out %>% 
          group_by(dtime) %>%
          mutate(mean=mean(ujt)) %>%
          slice(1:n_distinct(dtime)) %>%
          mutate(ndate=rep(od[i], n_distinct(dtime))) %>%
          select(dtime, time, -date, date=ndate, link, ujt=mean)
    
        D <- rbind(D, d.means)
      }
  }
  rm(d.mod, d.means)
  return(replaced=D)
}