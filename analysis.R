install.packages("popbio")
library(MASS)
library(popbio)

setwd("~/Desktop/eleanor_2011_fundraisings")

data <- read.csv("for_analysis.csv")
drops <- c("X")
data <- data[ , !(names(data) %in% drops)]

booleans <- c(
    "is_dead",
    "government",
    "private_equity_and_venture_capital",
    "corporate",
    "angel_network",
    "private_investment_vehicle",
    "crowd_funding",
    "business_and_professional_services",
    "agriculture__forestry_and_fishing",
    "supply_chain",
    "media",
    "tradespeople",
    "transportation_operators",
    "technology_ip_based_businesses",
    "energy",
    "craft_industries",
    "industrials",
    "retail",
    "leisure_and_entertainment",
    "personal_services",
    "built_environment_and_infrastructure",
    "telecommunications_services")

# change all the True/Falses to 1/0s
data[,booleans] <- sapply(data[,booleans], function(x) {as.numeric(x) -1})

#  [1] "id"                                            "investee"                                     
#  [3] "investee_id"                                   "primary_date"                                 
#  [5] "investee__sectors"                             "investee__company__companies_house_id"        
#  [7] "investee__company__current_stage_of_evolution" "region"                                       
#  [9] "amount_gbp"                                    "government"                                   
# [11] "private_equity_and_venture_capital"            "corporate"                                    
# [13] "angel_network"                                 "private_investment_vehicle"                   
# [15] "crowd_funding"                                 "business_and_professional_services"           
# [17] "agriculture__forestry_and_fishing"             "supply_chain"                                 
# [19] "media"                                         "tradespeople"                                 
# [21] "transportation_operators"                      "technology_ip_based_businesses"               
# [23] "energy"                                        "craft_industries"                             
# [25] "industrials"                                   "retail"                                       
# [27] "leisure_and_entertainment"                     "personal_services"                            
# [29] "built_environment_and_infrastructure"          "telecommunications_services"                  
# [31] "is_dead"                                      

model <- glm(
    is_dead ~
        government +
        private_equity_and_venture_capital +
        corporate +
        angel_network +
        private_investment_vehicle +
        crowd_funding +
        business_and_professional_services +
        agriculture__forestry_and_fishing +
        supply_chain +
        media +
        tradespeople +
        transportation_operators +
        technology_ip_based_businesses +
        energy +
        craft_industries +
        industrials +
        retail +
        leisure_and_entertainment +
        personal_services +
        built_environment_and_infrastructure +
        telecommunications_services +
        region +
        amount_gbp, family=binomial(link="logit"), data=data)

aicstep <- stepAIC(model, direction="backward")
aicstep$anova
summary(aicstep)

minus_bad_sectors <- update(aicstep, . ~ . - agriculture__forestry_and_fishing - built_environment_and_infrastructure)
minus_bad_sectors_regions <- update(minus_bad_sectors, . ~ . - region)

# The following is from:
# http://www.medicine.mcgill.ca/epidemiology/joseph/courses/epib-621/logfit.pdf

#################################################
# Chi-square goodness of fit test
#################################################
chisquaredgof <- function(y, model) {
    fit <- model$fitted
    hist(fit)
    r <- (y - fit)/(sqrt(fit*(1-fit))) # e.g. y = data$is_dead
    # Sum of squares of these residuals follows a chi-square with ?? degrees of freedom
    sum(r^2)
    # Calculate the p-value from the test
    p_value <- 1 - pchisq(sum(r^2), df=model$df.residual)
    return(p_value)
}

chisquaredgof(data$is_dead, minus_bad_sectors)         # 0.2578886 !!!
chisquaredgof(data$is_dead, minus_bad_sectors_regions) # 0.4088042
chisquaredgof(data$is_dead, model)                     # 0.262208
chisquaredgof(data$is_dead, aicstep)                   # 0.3839443

# So, we cannot reject the null hypothesis that this
# model is exactly correct...but then again, we know that
# the model is wrong! So, not too useful a method!
# On to something a bit more useful.

#################################################
# Hosmer-Lemeshow tests
#################################################
# Strategy to calculate the Hosmer-Lemeshow groupings:
# Form a matrix with the outcome and fitted values,
# and re-order according to fitted value probabilities.
# Get indices of vector fit, from smallest to greatest

hosmerlemeshow <- function(y, model) {
    fit <- model$fitted
    index <- sort.list(fit)
    hosmer <- matrix(c(y[index], fit[index]), byrow=F, nrow=length(y)) # e.g. y = data$is_dead
    n_groups <- 10
    observed <- rep(NA, n_groups)
    n_per_group <- round(length(y)/n_groups)
    for (i in 1:n_groups) {
        upper_bound <- n_per_group * i
        if (upper_bound > length(hosmer[,1])) {
            upper_bound <- length(hosmer[,1])
        }
        observed[i] <- sum(hosmer[(n_per_group*(i-1)+1):upper_bound,1])/n_per_group
    }
    predicted <- rep(NA, n_groups)
    for (i in 1:n_groups) {
        upper_bound <- n_per_group * i
        if (upper_bound > length(hosmer[,2])) {
            upper_bound <- length(hosmer[,2])
        }
        predicted[i] <- sum(hosmer[(n_per_group*(i-1)+1):upper_bound,2])/n_per_group
    }
    plot(predicted, observed, type="b")
    abline(a=0, b=1)
    # how many rows make it above the 50% cutoff
    sum(hosmer[,2] > 0.5)
    table(hosmer[,1], hosmer[,2] > 0.5)
    return(hosmer)
}

# sigmoid plot
logi.hist.plot(
    data$is_dead,
    data$government,
    boxp=FALSE,
    type="hist",
    col="gray")
