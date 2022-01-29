setClass("covidData", 
         slots=list(totalPopulation="numeric",
                    infectedCount='numeric',
                    rO='numeric',
                    infectedPopulation='numeric',
                    susceptiblePopulation='numeric',
                    susceptiblePopulationVaccinated='numeric',
                    susceptiblePopulationUnvaccinated='numeric',
                    vaccineModifier='numeric',
                    vaccinatedPopulation='numeric',
                    incubationPeriod='numeric',
                    recoveryTime='numeric'),
         prototype=list(
           totalPopulation=5000000,
           infectedCount=100,
           rO=4.5,
           vaccineModifier=0.3,
           vaccinatedPopulation=0.9,
           incubationPeriod=2,
           recoveryTime=8
         ))


# set the values for the susceptible and infected population for further calculations
setGeneric("setPopulationProportions", function(x) standardGeneric("setPopulationProportions"))
setMethod(f = "setPopulationProportions", 
          signature="covidData",
          function(x){
            x@infectedPopulation <- x@infectedCount/x@totalPopulation
            x@susceptiblePopulation <- 1 - x@infectedPopulation
            x@susceptiblePopulationUnvaccinated <- x@susceptiblePopulation*(1 - x@vaccineModifier)
            x@susceptiblePopulationVaccinated <- x@susceptiblePopulation*x@vaccineModifier
            return(x)
          }
)

#take the current state and update for another time period (day).
setGeneric("updateCovidData", function(x) standardGeneric("updateCovidData"))
setMethod(f = "updateCovidData", 
          signature="covidData",
          function(x){
            #i dont think this is the best way to do this. I need to account for incubation in the new cases. If there are 10 new infections, then we will not see their effects for the incubation period.
            infectedPopulationCurrent <- x@infectedPopulation
            x@infectedPopulation <- x@infectedPopulation + x@susceptiblePopulationUnvaccinated*(x@infectedPopulation*x@rO*(1)/x@recoveryTime) - x@infectedPopulation*1/x@recoveryTime + x@susceptiblePopulationVaccinated*(x@infectedPopulation*x@vaccineModifier*x@rO*(1)/x@recoveryTime) - x@infectedPopulation*1/x@recoveryTime
            x@infectedCount <- x@infectedPopulation*x@totalPopulation
            x@susceptiblePopulationUnvaccinated <- x@susceptiblePopulationUnvaccinated - x@susceptiblePopulationUnvaccinated*(infectedPopulationCurrent*x@rO*(1)/x@recoveryTime) #remove the susceptible unvaccinated people from that group
            x@susceptiblePopulationVaccinated <-  x@susceptiblePopulationVaccinated - x@susceptiblePopulationVaccinated*(infectedPopulationCurrent*x@vaccineModifier*x@rO*(1)/x@recoveryTime)#remove the susceptible vaccinated people from that group
            x@susceptiblePopulation <- x@susceptiblePopulationUnvaccinated + x@susceptiblePopulationVaccinated
            return(x)
          }
)

# Functions ---------------------------------------------------------------

covidTimeDataSetup <- function(covid, timeFrame){
  mat <- matrix(nrow = (timeFrame + covid@incubationPeriod), ncol = 4)
  mat[1:(timeFrame + covid@incubationPeriod), 1] <- 1:(timeFrame + covid@incubationPeriod) 
  mat[1:covid@incubationPeriod, 2] <- covid@infectedPopulation
  mat[1:covid@incubationPeriod, 3] <- covid@susceptiblePopulation
  mat[1:covid@incubationPeriod, 4] <- covid@infectedCount
  return(mat)
}

covidSpreading <- function(covid, timeFrame, covidTimeData){

for(t in 1:timeFrame){
  # Not sure what to do about these checks
  # if((1 - covid@infectedPopulation) <= 0){#does this mean that everyone is infected? Why are we carrying on?
  #   covid@infectedPopulation <- 1
  #   spop <- 0
  #   covidTimeData[t,1] <- t
  #   covidTimeData[t,2] <- covid@infectedPopulation
  #   covidTimeData[t,3] <- spop
  #   covidTimeData[t,4] <- (spop*(1- vaccine_pop)*(infected_pop*ro*(1)/recovery) - infected_pop*1/recovery + spop*vaccine_pop*(infected_pop*vaccine_modifier*ro*(1)/recovery) - infected_pop*1/recovery)*covid@totalPopulation
  #   next
  # }
  # if((1 - infected_pop) >= 1){#Noone is infected? So we can stop?
  #   infected_pop <- 0
  #   spop <- 1
  #   covidTimeData[t,1] <- t
  #   covidTimeData[t,2] <- infected_pop
  #   covidTimeData[t,3] <- spop
  #   covidTimeData[t,4] <- (spop*(1- vaccine_pop)*(infected_pop*ro*(1)/recovery) - infected_pop*1/recovery + spop*vaccine_pop*(infected_pop*vaccine_modifier*ro*(1)/recovery) - infected_pop*1/recovery)*covid@totalPopulation
  #   next
  # }
  # covidTimeData[t,1] <- t #set the current day to the current day. Seems redundant.
  covidTimeData[(t + covid@incubationPeriod),2] <- covid@infectedPopulation #is this being updated? If so, I need to add that to a function to update infected population
  covidTimeData[(t + covid@incubationPeriod),3] <- covid@susceptiblePopulation #same as above. I assume it is 1 - infected? Or should I start to include these in vaccinated?
  #new number of infected individuals
  covid <- updateCovidData(covid)
  covidTimeData[(t + covid@incubationPeriod),4] <- covid@infectedCount
}

#I think the time vs pop (infected) is plotted by distPlot
dat <- as.data.frame(covidTimeData)
colnames(dat) <- c("time", "infected", "susceptible", "pop")
return(dat)
}


# test data ---------------------------------------------------------------

# 
# covid <- new('covidData')
# 
# covid <- setPopulationProportions(x = covid)
# 
# covid
