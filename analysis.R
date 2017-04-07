install.packages("popbio")
install.packages("speedglm")
library(MASS)
library(popbio)
library(speedglm)

#
# An alternative stepwise thing that doesn't use AIC:
#
# http://orinanobworld.blogspot.co.uk/2011/02/stepwise-regression-in-r.html
# This is an R function to perform stepwise regression based on a "nested model" F test for inclusion/exclusion
# of a predictor.  To keep it simple, I made no provision for forcing certain variables to be included in
# all models, did not allow for specification of a data frame, and skipped some consistency checks (such as whether
# the initial model is a subset of the full model).
#
# One other note: since the code uses R's drop1 and add1 functions, it respects hierarchy in models. That is,
# regardless of p values, it will not attempt to drop a term while retaining a higher order interaction
# involving that term, nor will it add an interaction term if the lower order components are not all present.
# (You can of course defeat this by putting interactions into new variables and feeding it what looks like
# a first-order model.)
#
# Consider this to be "beta" code (and feel free to improve it).  I've done very limited testing on it.
#
# Author: Paul A. Rubin (rubin@msu.edu)
#
stepwise <- function(full.model, initial.model, alpha.to.enter, alpha.to.leave) {
  # full.model is the model containing all possible terms
  # initial.model is the first model to consider
  # alpha.to.enter is the significance level above which a variable may enter the model
  # alpha.to.leave is the significance level below which a variable may be deleted from the model
  # (Useful things for someone to add: specification of a data frame; a list of variables that must be included)
  full <- lm(full.model);  # fit the full model
  msef <- (summary(full)$sigma)^2;  # MSE of full model
  n <- length(full$residuals);  # sample size
  allvars <- attr(full$terms, "predvars");  # this gets a list of all predictor variables
  current <- lm(initial.model);  # this is the current model
  while (TRUE) {  # process each model until we break out of the loop
    temp <- summary(current);  # summary output for the current model
    rnames <- rownames(temp$coefficients);  # list of terms in the current model
    print(temp$coefficients);  # write the model description
    p <- dim(temp$coefficients)[1];  # current model's size
    mse <- (temp$sigma)^2;  # MSE for current model
    cp <- (n-p)*mse/msef - (n-2*p);  # Mallow's cp
    fit <- sprintf("\nS = %f, R-sq = %f, R-sq(adj) = %f, C-p = %f",
                   temp$sigma, temp$r.squared, temp$adj.r.squared, cp);
    write(fit, file="");  # show the fit
    write("=====", file="");  # print a separator
    if (p > 1) {  # don't try to drop a term if only one is left
      d <- drop1(current, test="F");  # looks for significance of terms based on F tests
      pmax <- max(d[-1,6]);  # maximum p-value of any term (have to skip the intercept to avoid an NA value)
      if (pmax > alpha.to.leave) {
        # we have a candidate for deletion
        var <- rownames(d)[d[,6] == pmax];  # name of variable to delete
        if (length(var) > 1) {
          # if an intercept is present, it will be the first name in the list
          # there also could be ties for worst p-value
          # taking the second entry if there is more than one is a safe solution to both issues
          var <- var[2];
        }
        write(paste("--- Dropping", var, "\n"), file="");  # print out the variable to be dropped
        f <- formula(current);  # current formula
        f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" - ")));  # modify the formula to drop the chosen variable (by subtracting it)
        current <- lm(f);  # fit the modified model
        next;  # return to the top of the loop
      }
    }
    # if we get here, we failed to drop a term; try adding one
    # note: add1 throws an error if nothing can be added (current == full), which we trap with tryCatch
    a <- tryCatch(add1(current, scope=full, test="F"), error=function(e) NULL);  # looks for significance of possible additions based on F tests
    if (is.null(a)) {
      break;  # there are no unused variables (or something went splat), so we bail out
    }
    pmin <- min(a[-1,6]);  # minimum p-value of any term (skipping the intercept again)
    if (pmin < alpha.to.enter) {
      # we have a candidate for addition to the model
      var <- rownames(a)[a[,6] == pmin];  # name of variable to add
      if (length(var) > 1) {
        # same issue with ties, intercept as above
        var <- var[2];
      }
      write(paste("+++ Adding", var, "\n"), file="");  # print the variable being added
      f <- formula(current);  # current formula
      f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" + ")));  # modify the formula to add the chosen variable
      current <- lm(f);  # fit the modified model
      next;  # return to the top of the loop
    }
    # if we get here, we failed to make any changes to the model; time to punt
    break;
  } 
}


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
# add .s into all the the region names
data$region <- make.names(data$region)
# recode the categorical region column into various binary columns
data <- cbind(data, with(data, model.matrix(~ region + 0)))

#  [1] "id"                                            "investee"                                     
#  [3] "investee_id"                                   "primary_date"                                 
#  [5] "stage_of_evolution"                            "investee__sectors"                            
#  [7] "investee__company__companies_house_id"         "investee__company__current_stage_of_evolution"
#  [9] "region"                                        "amount_gbp"                                   
# [11] "government"                                    "private_equity_and_venture_capital"           
# [13] "corporate"                                     "angel_network"                                
# [15] "private_investment_vehicle"                    "crowd_funding"                                
# [17] "agriculture__forestry_and_fishing"             "personal_services"                            
# [19] "telecommunications_services"                   "energy"                                       
# [21] "technology_ip_based_businesses"                "industrials"                                  
# [23] "business_and_professional_services"            "media"                                        
# [25] "tradespeople"                                  "retail"                                       
# [27] "leisure_and_entertainment"                     "craft_industries"                             
# [29] "supply_chain"                                  "transportation_operators"                     
# [31] "built_environment_and_infrastructure"          "is_dead"                                      
# [33] "regionEast.Midlands"                           "regionEast.of.England"                        
# [35] "regionLondon"                                  "regionNorth.East"                             
# [37] "regionNorth.West"                              "regionNorthern.Ireland"                       
# [39] "regionScotland"                                "regionSouth.East"                             
# [41] "regionSouth.West"                              "regionWales"                                  
# [43] "regionWest.Midlands"                           "regionYorkshire.and.Humberside"  
                  

model <- glm(
    data$is_dead ~
        data$government +
        data$private_equity_and_venture_capital +
        data$corporate +
        data$angel_network +
        data$private_investment_vehicle +
        data$crowd_funding +
        data$business_and_professional_services +
        data$agriculture__forestry_and_fishing +
        data$supply_chain +
        data$media +
        data$tradespeople +
        data$transportation_operators +
        data$technology_ip_based_businesses +
        data$energy +
        data$craft_industries +
        data$industrials +
        data$retail +
        data$leisure_and_entertainment +
        data$personal_services +
        data$built_environment_and_infrastructure +
        data$telecommunications_services +
        data$region +
        data$amount_gbp +
        data$stage_of_evolution +
        data$government * data$stage_of_evolution, family=binomial(link="logit"))


# data_sub <- data[,c(
#     "is_dead",
#     "government",
#     "private_equity_and_venture_capital",
#     "angel_network",
#     "business_and_professional_services",
#     "media",
#     "technology_ip_based_businesses",
#     "industrials",
#     "retail",
#     "leisure_and_entertainment",
#     "regionEast.of.England",
#     "regionLondon",
#     "regionNorth.East",
#     "regionNorth.West",
#     "regionScotland",
#     "regionSouth.East",
#     "regionSouth.West",
#     "regionWales",
#     "regionWest.Midlands",
#     "regionYorkshire.and.Humberside"
#     )]
# model <- glm(is_dead ~ .*., family=binomial(link="logit"), data=data_sub)
# summary(model)

# aicstep <- stepAIC(model, direction="backward")
# aicstep$anova
# summary(aicstep)

# Call:
# glm(formula = is_dead ~ government + private_equity_and_venture_capital + 
#     angel_network + business_and_professional_services + media + 
#     technology_ip_based_businesses + industrials + retail + leisure_and_entertainment + 
#     regionEast.of.England + regionLondon + regionNorth.East + 
#     regionNorth.West + regionScotland + regionSouth.East + regionSouth.West + 
#     regionWales + regionWest.Midlands + regionYorkshire.and.Humberside + 
#     government:angel_network + government:business_and_professional_services + 
#     government:regionEast.of.England + government:regionNorth.East + 
#     government:regionNorth.West + government:regionScotland + 
#     government:regionSouth.East + government:regionWales + government:regionWest.Midlands + 
#     government:regionYorkshire.and.Humberside + private_equity_and_venture_capital:industrials + 
#     private_equity_and_venture_capital:retail + private_equity_and_venture_capital:regionEast.of.England + 
#     private_equity_and_venture_capital:regionLondon + private_equity_and_venture_capital:regionNorth.East + 
#     private_equity_and_venture_capital:regionNorth.West + private_equity_and_venture_capital:regionScotland + 
#     private_equity_and_venture_capital:regionSouth.East + private_equity_and_venture_capital:regionWest.Midlands + 
#     angel_network:technology_ip_based_businesses + angel_network:industrials + 
#     angel_network:leisure_and_entertainment + angel_network:regionNorth.East + 
#     angel_network:regionNorth.West + angel_network:regionWales + 
#     business_and_professional_services:media + business_and_professional_services:regionEast.of.England + 
#     business_and_professional_services:regionLondon + business_and_professional_services:regionNorth.East + 
#     business_and_professional_services:regionNorth.West + business_and_professional_services:regionScotland + 
#     business_and_professional_services:regionSouth.East + business_and_professional_services:regionSouth.West + 
#     business_and_professional_services:regionWales + business_and_professional_services:regionWest.Midlands + 
#     business_and_professional_services:regionYorkshire.and.Humberside + 
#     media:retail + media:leisure_and_entertainment + media:regionNorth.West + 
#     media:regionYorkshire.and.Humberside + technology_ip_based_businesses:industrials + 
#     technology_ip_based_businesses:leisure_and_entertainment + 
#     technology_ip_based_businesses:regionEast.of.England + technology_ip_based_businesses:regionLondon + 
#     technology_ip_based_businesses:regionNorth.East + technology_ip_based_businesses:regionNorth.West + 
#     technology_ip_based_businesses:regionScotland + technology_ip_based_businesses:regionSouth.East + 
#     technology_ip_based_businesses:regionWales + technology_ip_based_businesses:regionWest.Midlands + 
#     technology_ip_based_businesses:regionYorkshire.and.Humberside + 
#     industrials:leisure_and_entertainment + industrials:regionEast.of.England + 
#     industrials:regionLondon + industrials:regionNorth.East + 
#     industrials:regionNorth.West + industrials:regionScotland + 
#     industrials:regionSouth.East + industrials:regionSouth.West + 
#     industrials:regionWales + industrials:regionWest.Midlands + 
#     industrials:regionYorkshire.and.Humberside + retail:regionYorkshire.and.Humberside, 
#     family = binomial(link = "logit"), data = data_sub)
#
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -1.79078  -0.45025  -0.29462  -0.00015   3.04915  
#
# Coefficients:
#                                                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                                         -34.9379  1203.3106  -0.029 0.976837    
# government                                                           79.8925  2666.9080   0.030 0.976101    
# private_equity_and_venture_capital                                  -34.2598  1203.3092  -0.028 0.977286    
# angel_network                                                         4.1958     1.1074   3.789 0.000151 ***
# business_and_professional_services                                   32.4858  1203.3104   0.027 0.978462    
# media                                                                 1.5315     0.4658   3.288 0.001009 ** 
# technology_ip_based_businesses                                      -45.1970  1663.8919  -0.027 0.978329    
# industrials                                                          34.2804  1203.3104   0.028 0.977273    
# retail                                                                0.8225     0.5204   1.580 0.113998    
# leisure_and_entertainment                                             0.7559     0.6876   1.099 0.271635    
# regionEast.of.England                                                29.9652  1203.3108   0.025 0.980133    
# regionLondon                                                         31.6030  1203.3107   0.026 0.979047    
# regionNorth.East                                                     34.0978  1203.3109   0.028 0.977394    
# regionNorth.West                                                     31.7043  1203.3108   0.026 0.978980    
# regionScotland                                                       31.2674  1203.3111   0.026 0.979270    
# regionSouth.East                                                     32.5663  1203.3109   0.027 0.978409    
# regionSouth.West                                                     32.2402  1203.3111   0.027 0.978625    
# regionWales                                                          34.6229  1203.3109   0.029 0.977046    
# regionWest.Midlands                                                  33.9200  1203.3110   0.028 0.977512    
# regionYorkshire.and.Humberside                                       31.0365  1203.3098   0.026 0.979423    
# government:angel_network                                           -100.3336  3687.6195  -0.027 0.978294    
# government:business_and_professional_services                         2.0122     1.1278   1.784 0.074381 .  
# government:regionEast.of.England                                    -99.5714  8516.2275  -0.012 0.990671    
# government:regionNorth.East                                         -79.4627  2666.9082  -0.030 0.976230    
# government:regionNorth.West                                         -79.8863  2666.9082  -0.030 0.976103    
# government:regionScotland                                           -76.4821  2666.9084  -0.029 0.977121    
# government:regionSouth.East                                         -99.9112  6528.3605  -0.015 0.987790    
# government:regionWales                                              -78.7116  2666.9083  -0.030 0.976455    
# government:regionWest.Midlands                                      -78.1946  2666.9088  -0.029 0.976609    
# government:regionYorkshire.and.Humberside                           -79.0667  2666.9077  -0.030 0.976348    
# private_equity_and_venture_capital:industrials                        2.1891     0.9300   2.354 0.018580 *  
# private_equity_and_venture_capital:retail                             1.3580     0.8283   1.640 0.101099    
# private_equity_and_venture_capital:regionEast.of.England             32.8952  1203.3091   0.027 0.978191    
# private_equity_and_venture_capital:regionLondon                      33.9951  1203.3091   0.028 0.977462    
# private_equity_and_venture_capital:regionNorth.East                  34.2366  1203.3095   0.028 0.977302    
# private_equity_and_venture_capital:regionNorth.West                  32.8543  1203.3096   0.027 0.978218    
# private_equity_and_venture_capital:regionScotland                    30.2318  1203.3102   0.025 0.979956    
# private_equity_and_venture_capital:regionSouth.East                  34.7342  1203.3095   0.029 0.976972    
# private_equity_and_venture_capital:regionWest.Midlands               36.2256  1203.3100   0.030 0.975983    
# angel_network:technology_ip_based_businesses                         -3.6874     1.2657  -2.913 0.003575 ** 
# angel_network:industrials                                            -2.9335     1.4018  -2.093 0.036377 *  
# angel_network:leisure_and_entertainment                              27.8715 52141.6234   0.001 0.999574    
# angel_network:regionNorth.East                                      -22.8874 10516.8084  -0.002 0.998264    
# angel_network:regionNorth.West                                      -20.2485  7446.7138  -0.003 0.997830    
# angel_network:regionWales                                           -24.3988  8016.6020  -0.003 0.997572    
# business_and_professional_services:media                             -2.7006     1.2651  -2.135 0.032791 *  
# business_and_professional_services:regionEast.of.England            -31.6304  1203.3106  -0.026 0.979029    
# business_and_professional_services:regionLondon                     -32.7215  1203.3104  -0.027 0.978306    
# business_and_professional_services:regionNorth.East                 -34.1540  1203.3112  -0.028 0.977356    
# business_and_professional_services:regionNorth.West                 -33.2372  1203.3112  -0.028 0.977964    
# business_and_professional_services:regionScotland                   -32.8865  1203.3114  -0.027 0.978197    
# business_and_professional_services:regionSouth.East                 -32.9310  1203.3109  -0.027 0.978167    
# business_and_professional_services:regionSouth.West                 -30.8451  1203.3112  -0.026 0.979550    
# business_and_professional_services:regionWales                      -53.6414  5373.1364  -0.010 0.992035    
# business_and_professional_services:regionWest.Midlands              -53.4902  5253.6037  -0.010 0.991876    
# business_and_professional_services:regionYorkshire.and.Humberside   -33.2422  1203.3103  -0.028 0.977961    
# media:retail                                                         -1.6406     0.9513  -1.725 0.084595 .  
# media:leisure_and_entertainment                                       2.4263     1.2509   1.940 0.052433 .  
# media:regionNorth.West                                              -19.9908  7677.1967  -0.003 0.997922    
# media:regionYorkshire.and.Humberside                                -33.5559  8876.0203  -0.004 0.996984    
# technology_ip_based_businesses:industrials                           -1.7249     0.8395  -2.055 0.039918 *  
# technology_ip_based_businesses:leisure_and_entertainment             -3.1858     1.4299  -2.228 0.025884 *  
# technology_ip_based_businesses:regionEast.of.England                 47.9703  1663.8924   0.029 0.977000    
# technology_ip_based_businesses:regionLondon                          46.2219  1663.8920   0.028 0.977838    
# technology_ip_based_businesses:regionNorth.East                      45.5808  1663.8921   0.027 0.978145    
# technology_ip_based_businesses:regionNorth.West                      46.6499  1663.8922   0.028 0.977633    
# technology_ip_based_businesses:regionScotland                        46.1527  1663.8923   0.028 0.977871    
# technology_ip_based_businesses:regionSouth.East                      43.7637  1663.8922   0.026 0.979016    
# technology_ip_based_businesses:regionWales                           44.0575  1663.8923   0.026 0.978876    
# technology_ip_based_businesses:regionWest.Midlands                   43.3012  1663.8926   0.026 0.979238    
# technology_ip_based_businesses:regionYorkshire.and.Humberside        47.9341  1663.8936   0.029 0.977017    
# industrials:leisure_and_entertainment                               -19.3385  4610.1222  -0.004 0.996653    
# industrials:regionEast.of.England                                   -32.5940  1203.3104  -0.027 0.978390    
# industrials:regionLondon                                            -34.7032  1203.3108  -0.029 0.976992    
# industrials:regionNorth.East                                        -34.6260  1203.3105  -0.029 0.977044    
# industrials:regionNorth.West                                        -32.5117  1203.3105  -0.027 0.978445    
# industrials:regionScotland                                          -32.2605  1203.3107  -0.027 0.978611    
# industrials:regionSouth.East                                        -34.6345  1203.3111  -0.029 0.977038    
# industrials:regionSouth.West                                        -51.3660  4835.4842  -0.011 0.991524    
# industrials:regionWales                                             -49.4743  6044.0136  -0.008 0.993469    
# industrials:regionWest.Midlands                                     -33.0589  1203.3124  -0.027 0.978082    
# industrials:regionYorkshire.and.Humberside                          -32.5527  1203.3091  -0.027 0.978418    
# retail:regionYorkshire.and.Humberside                                17.5710   850.6601   0.021 0.983520    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 732.57  on 994  degrees of freedom
# Residual deviance: 535.57  on 912  degrees of freedom
# AIC: 701.57
#
# Number of Fisher Scoring iterations: 19

# minus_bad <- update(
#     aicstep,
#     . ~ .
#         - angel_network:leisure_and_entertainment)


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
