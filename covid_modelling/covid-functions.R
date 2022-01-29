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
            x@susceptiblePopulationUnvaccinated <- 1*(1-x@vaccinatedPopulation) - x@infectedPopulation
            x@susceptiblePopulationVaccinated <- 1 - x@infectedPopulation - x@susceptiblePopulationUnvaccinated
            return(x)
          }
)


# test data ---------------------------------------------------------------

# 
# covid <- new('covidData')
# 
# covid <- setPopulationProportions(x = covid)
# 
# covid
