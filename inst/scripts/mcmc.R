

## Basic version:

Nweeks <- 50000
current <- 10
position <- numeric(Nweeks)

for (ii in 1:Nweeks){
  position[ii] <- current

  proposal <- current + sample(c(-1,1), 1)
  if (proposal > 10) {proposal = 1}
  if (proposal < 1) {proposal = 10}

  current <- ifelse(runif(1) < proposal/current, proposal, current)

}

hist(position, breaks = (0:11) - 0.5)



################

## King jumps to any island, not just neighbouring island
Nweeks <- 50000
current <- 10
position <- numeric(Nweeks)

for (ii in 1:Nweeks){
  position[ii] <- current

  proposal <- (((current + sample(1:9, 1)) - 1) %% 10) + 1

  current <- ifelse(runif(1) < proposal/current, proposal, current)

}

hist(position, breaks = (0:11) - 0.5)


#################

## Island populations are random (not 1:10) but the king jumps to neighbouring
# islands

Nweeks <- 50000
Nislands <- 10
current <- 10
position <- numeric(Nweeks)

population <- sample(Nislands, Nislands)

for (ii in 1:Nweeks){
  position[ii] <- current

  proposal <- ((sample(c(current-1, current+1), 1) - 1) %% Nislands) + 1

  current <- ifelse(runif(1) < population[proposal]/population[current], proposal, current)

}

barplot(population)
hist(position, breaks = (0:11) - 0.5)



##################

## Only a few of the islands are populated and the king doesn't know which ones
# (MCMC fails here or is very inefficient)

# Only one in 10000 or so neighbouring islands should be populated, so when he
# chooses among neighbouring islands, most are rejected. Not sure how to
# simulate this in the island example.

Nweeks <- 50000
current <- 1
position <- numeric(Nweeks)

Npopulatedislands <- 10
ratio_unpopulated <- 1000 # 9:1 unpopulated
population <- matrix(rbind(seq(1,Npopulatedislands), matrix(0.0001,ratio_unpopulated,Npopulatedislands))) %>%
  as.numeric()

Nislands <- NROW(population)

N_rejected_proposals_limit <- 10000

barplot(population)


for (ii in 1:Nweeks){
  position[ii] <- current

  proposal <- (((current + sample(-(ratio_unpopulated + 1):(ratio_unpopulated + 1), 1)) - 1) %% Nislands) + 1

  current <- ifelse(runif(1) < population[proposal]/population[current],
                    proposal,
                    current)

  if (ii > N_rejected_proposals_limit){
    if (sum(diff(position[(ii-N_rejected_proposals_limit):ii])^2) == 0){
      print(paste(N_rejected_proposals_limit, " proposals rejected so chain is aborted"))
      break
    }
  }

}

barplot(population)
#barplot(population, xlim = c(11999, 12013))
hist(position, breaks = (0:Nislands + 1) - 0.5)


##################


