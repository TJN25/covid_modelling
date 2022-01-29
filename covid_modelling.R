##covid modelling
library(tidyverse)
library(markovchain)

total_pop <- 5000000
infected_count <- 1
ro <- 4
infected_pop <- infected_count/total_pop
spop <- 1 - infected_pop
vaccine_modifier <- 0.5
vaccine_pop <- 0.3
incubation <- 4
recovery <- 8


t <- 1
infected_pop + infected_pop*ro*t/incubation - infected_pop*t/recovery


mat <- matrix(nrow = 15, ncol = 4)
for(t in 1:15){
  if((1 - infected_pop) <= 0){
    infected_pop <- 1
    spop <- 0
    mat[t,1] <- t
    mat[t,2] <- infected_pop
    mat[t,3] <- spop
    mat[t,4] <- infected_pop*total_pop
    next
  }
  # if((1 - infected_pop) >= 1){
  #   infected_pop <- 0
  #   spop <- 1
  #   mat[t,1] <- t
  #   mat[t,2] <- infected_pop
  #   mat[t,3] <- spop
  #   mat[t,4] <- infected_pop*total_pop
  #   next
  # }
  mat[t,1] <- t
  mat[t,2] <- infected_pop
  mat[t,3] <- spop
  mat[t,4] <- infected_pop*total_pop
  
  infected_pop <- infected_pop + (infected_pop*ro*(1)/recovery - infected_pop*1/recovery)*(1- vaccine_pop) + (infected_pop*vaccine_modifier*ro*(1)/recovery - infected_pop*1/recovery)*vaccine_pop

  spop <- 1 - infected_pop
  

}

dat <- as.data.frame(mat)

colnames(dat) <- c("time", "infected", "susceptible", "pop")

ggplot() +
  geom_point(data = dat, aes(x = time, y = pop)) 


+
  scale_y_continuous(trans = "log10")













pChaser <- 0.9
pPlayer <- 0.6

pBothMove <- pPlayer*pChaser
pChaserMove <- (1 - pPlayer)*pChaser
pPlayerMove <- pPlayer*(1 - pChaser)
pNoMove <- (1 - pPlayer)*(1 - pChaser)
steps <- 1000
chaserstart <- 8
playerstart <- 5


states <- c(1:((chaserstart + 2)*(playerstart + 3)))

statesDat <- data.frame(states = states, chaser_coord = 1, player_coord = 1, game_state = "playing")

current_state <- mat[chaser, player]
i <- 0
for(chaser in chaserstart:-1){
  for(player in -1:(playerstart + 1)){
    i <-  i+1
    statesDat$chaser_coord[i] <- chaser
    statesDat$player_coord[i] <- player
  }
}

statesDat <- statesDat %>% mutate(game_state = ifelse(chaser_coord == 0, "lose", ifelse(player_coord == 0, "win", "playing")))
statesDat <- statesDat %>% mutate(labels = paste(player_coord, chaser_coord, sep = ",")) %>% 
  arrange(-player_coord, -chaser_coord)

mat <- matrix(nrow = nrow(statesDat), ncol = nrow(statesDat))

colnames(mat) <- statesDat$labels
rownames(mat) <- statesDat$labels

i <- 1
j <- 1
for(i in 1:nrow(mat)){
  current_state <- rownames(mat)[i]
  playerPos <- statesDat$player_coord[statesDat$labels == current_state]
  chaserPos <- statesDat$chaser_coord[statesDat$labels == current_state]
  if(chaserPos == 0){
    mat[i,i] <- 1
    next
  }
  if(playerPos == 0){
    mat[i,i] <- 1
    next
  }
  for(j in 1:nrow(mat)){
    change_state <- colnames(mat)[j]
    chaserChangePos  <- statesDat$chaser_coord[statesDat$labels == change_state]
    playerChangePos <- statesDat$player_coord[statesDat$labels == change_state]
    
  if(playerPos == playerChangePos){
    if((chaserPos - chaserChangePos) == 1){
      #chaser got question correct
      mat[i,j] <- pChaserMove
    }else if(chaserPos == chaserChangePos){
      #chaser got question wrong
      mat[i,j] <- pNoMove
    }
  }else if((playerPos-playerChangePos) == 1){
    if((chaserPos - chaserChangePos) == -1){
      #chaser got question wrong
      mat[i,j] <- pPlayerMove
    }else if(chaserPos == chaserChangePos){
      #chaser got question correct
      mat[i,j] <- pBothMove
      
    }
    }
  }
}

mat[is.na(mat)] <- 0
dat <- as.data.frame(mat)

dat$sums <- rowSums(dat)
dat <- dat %>% filter(sums == 1)

trans_mat <- as.matrix(dat[,-ncol(dat)])

#chaser cannot be more steps behind the player than available steps (8)
statesDat <- statesDat %>% filter(chaser_coord <= chaserstart, player_coord <=playerstart, player_coord >= 0, chaser_coord >= 0) %>% mutate(keep_label = ifelse((chaserstart - player_coord)  < chaser_coord, F, T)) %>%
filter(keep_label)

trans_mat <- trans_mat[rownames(trans_mat) %in% statesDat$labels,colnames(trans_mat) %in% statesDat$labels]
disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=statesDat$labels, name="MC 1") 


statelabels <- statesDat$labels
current_state <- statelabels
current_state[current_state == paste(playerstart, chaserstart - playerstart, sep = ",")] <- 1
current_state[current_state != "1"] <- 0
current_state <- as.numeric(current_state)
finalState<-current_state*disc_trans^steps

#get steady states 
finalstates <- steadyStates(disc_trans)
results <- colSums(finalState)
results <- as.data.frame(results)
results$labels <- rownames(results)
statesDat <- statesDat %>% full_join(results)
outcomes <- statesDat %>% select(game_state, labels, results) %>% dplyr::rename(prob = results)
probres <- outcomes %>% group_by(game_state) %>% dplyr::summarise(prob_win = sum(prob))
