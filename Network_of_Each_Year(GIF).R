setwd("C:/Users/michael/Desktop/Scotus_Network_2/scotus/with_node_attributes")

library(igraph)

#Load the network 
citation_net <-read.graph( file='scotus_net_EL_date.txt', format="gml")

#########################
#Basic network summaries#
#########################
dates <- as.Date(V(citation_net)$date)

years <-dates %>% 
  as.character %>% 
  strsplit('-') %>%  
  sapply(function(x) x[1]) %>% 
  as.integer

V(citation_net)$year <- years

# loading citation_net_years
citation_net_years <-read.graph( file='scotus_net_EL_year.txt', format="gml")
unique_years <- sort(unique(V(citation_net_years)$year))
unique_years <- as.integer(unique_years)
unique_years
which(V(citation_net_years)$year == '1954')
V(citation_net_years)[234]
E(citation_net_years)[from(234)]
# loading citation_net_years

#degrees_in <- unname(degree(citation_net, v = V(citation_net), mode = c("in")))
#summary(degrees_in)


years
unique_years
citation_net_2000 <- delete.vertices(citation_net, V(citation_net)[V(citation_net)$year != 2000])
citation_net_2000
plot(citation_net_2000)

plot_network_year <- function(network, yearA) {
  network_year <- delete.vertices(network, V(network)[V(network)$year != yearA])
  main = paste("Histogram of In-Degrees in ", yearA, " of SCOTUS Network")
  plot(network_year, main=paste("SCOTUS Network of ", yearA))
}

plot_network_year(citation_net, 2000)

tb <- table(years)
prob_years <- as.integer(names(tb[tb> 1000]))
prob_years
plot_network_year(citation_net, 1993)
plot_network_year(citation_net, 1994)
#plot_network_year(citation_net, 2002)
#plot_network_year(citation_net, 2003)
#plot_network_year(citation_net, 2004)
#plot_network_year(citation_net, 2005)


unique_years

for (i in 1:10) {
  plot_network_year(citation_net, unique_years[i])
}


library(animation)
plot_seq_network <- function(year_sequence) {
  for (i in 1:length(year_sequence)) {
    plot_network_year(citation_net, year_sequence[i])
  }
}

year_sequenceA <- 1950:1980
year_sequenceB <- unique_years
#saveGIF(plot_seq_network(year_sequenceA), interval = 0.2, movie.name="network_1950-1980.gif")
#saveGIF(plot_seq_network(year_sequenceA), interval = 0.5, movie.name="network_1950-1980 NEW.gif")
#higher interval => slower GIF
saveGIF(plot_seq_network(year_sequenceB), interval = 0.35, movie.name="network_unique_years.gif")
