#Q1
Drugs <- read.csv("Drugs.txt", header = T, ) #load data from file "Drugs.txt" in dataframe "Drugs"
Drugs$FLAG_CODES <- NULL #remove the variable "FLAG_CODES"

#Q2
length(unique(Drugs$LOCATION)) #number of countries
t <- table(Drugs$LOCATION) #create a table with countries' acronyms and datapoints (years) per country
t <- t[order(t)] #sort the table in ascending order

#Q3
#the countries in top 25%, belongs to the 75% of the distribution based upon the number of datapoints that can be found in the table "t"
q <- quantile(t, p=0.75)
r <- row.names(as.table(which(t>= q))) #get the names of the countries stored in table "t" and are above the quantile  
s <- subset(Drugs, Drugs$LOCATION %in% r) #show the available information of the selected countries

#Q4
x1 <- s$TIME #create a vector with the column "TIME" in "s" table
loc <- unique(s$LOCATION)
c <- colnames(s)

par(mfrow=c(5,1), mar = c(1,4,3,1), las = 1) #combine 5 plots in the same window (the 5th is the legend)
for (i in seq(3, length(c), by=1)) { #in every loop a plot that referred to a metric is created
  plot(x1, s[,c[i]], xlab = "Years", ylab = c[i], main = paste("Drug spending in", c[i], sep = " "))
  axis(1, at=c(min(x1), max(x1)), lab=c(min(x1), max(x1))) #define the x-axis limits
  for (j in 1:length(loc)) { #in every loop each country takes a different color
    lines(x1[s$LOCATION==loc[j]], s[,c[i]][s$LOCATION==loc[j]], col = j, type = "b", pch = 16)
  }
}

#create the legend (as a plot)
par(mai=c(0,0,0,0))
plot.new()
legend("center", legend = loc[1:length(loc)], box.lty = 1, lty = 1, col = c(1:length(loc)), bg = "transparent", horiz = TRUE, cex = 0.75)

#Q5
bel <- Drugs[(Drugs$LOCATION == "BEL"),] #data frame "bel" contains all the data concerning the Belgium
now <- max(bel$TIME) + 1 #the year we are now

yprobs <- list() #initializes the list "yprobs"
for (i in seq(3, length(colnames(bel)), by=1)) { #loop to calculate probability starting the iterator from the column "PC_HEALTHXP"
  d <- length(which(diff(bel[,colnames(bel)[i]]) > 0)) #calculate how many years each metric is higher than the year before
  yprobs <- append(yprobs, (d/length(bel$LOCATION))) #append to the "yprobs" list the probability of drug expenditure increase for each metric of the wanted country
}
yprobs <- as.numeric(yprobs[1:4]) #convert the list into a numeric vector
names(yprobs) <- colnames(bel)[3:6] #name the variables on the vector "yprobs" as the columns of Belgium's data frame
fiveprobs <- dbinom(4, 5, yprobs[1:4]) #calculate the probability of increase in the next 5 years with the Binomial distribution [we use this distribution because the solution of the problem is based upon two outcomes (increase or decrease of drug expenditure)]

#create the requested list
belist <- list(bel, c(min(bel$TIME), max(bel$TIME)), length(unique(bel$TIME)), yprobs, fiveprobs) 
names(belist) <- c("Data", "Years", "Data.points", "YearlyProbs", "FiveYeProbs")

#Q6
#create the requested function which is called "calc" 
calc <- function(DATA = NULL, METRIC = "pc.gdp", nofY = 5){
          if("FLAG_CODES" %in% colnames(DATA) | length(unique(DATA$LOCATION)) > 1 | is.null(DATA)){ #we take as constraint and the possibility that the data frame will be null, because in that case we can't move on
            return()
          }
          if(METRIC == "pc.gdp") {
            metric <- colnames(DATA)[3]
          } else if (METRIC == "pc.tot"){
            metric <- colnames(DATA)[4]
          } else if (METRIC == "per.ca"){
            metric <- colnames(DATA)[5]
          } else if(METRIC == "total"){
            metric <- colnames(DATA)[6]
          } else {
            return()
          }
          if (nofY != 5){
            print(paste("The probability of increasing drug expenditure in at least", nofY-1, "out of the following", nofY, "years.", sep = " "))
          }
          #calculate the probability as we did in the previous question
          p <- length(which(diff(DATA[,metric]) > 0))/length(DATA[,metric]) 
          prob <- dbinom(nofY-1, nofY, p)
          if (length(unique(DATA$TIME)) < 10){
            return("Unable to calculate probability (n< 10)")
          } else{
            return(paste("Based on", length(unique(DATA$TIME)), "datapoints from years", min(DATA$TIME), "to", max(DATA$TIME), "the probability that", DATA$LOCATION[1], "will increase its drug expenditure, in terms of", metric, "in at least", nofY-1, "years in the period", max(DATA$TIME), "to", max(DATA$TIME)+1+nofY, "is", prob, ".", sep = " "))
          }
}