server <- function(input, output, session) {
  
  # covid vaccine data ------------------------------------------------------
  
  main_data <- reactive({
    
    total_pop <- input$total_pop
    infected_count <- input$infected_count
    ro <- input$ro
    infected_pop <- infected_count/total_pop
    spop <- 1 - infected_pop
    # vaccine_modifier <- 0.15
    # vaccine_pop <- 0.7
    incubation <- input$incubation
    recovery <- input$recovery
    
    time_frame <- input$time_frame
    mat <- matrix(nrow = (time_frame + incubation), ncol = 4)
    mat[1:(time_frame + incubation), 1] <- 1:(time_frame + incubation)
    mat[1:incubation, 2] <- infected_pop
    mat[1:incubation, 3] <- spop
    mat[1:incubation, 4] <- infected_pop*total_pop
    
    for(t in 1:time_frame){
      # if((1 - infected_pop) <= 0){
      #     infected_pop <- 1
      #     spop <- 0
      #     mat[t,1] <- t
      #     mat[t,2] <- infected_pop
      #     mat[t,3] <- spop
      #     mat[t,4] <- infected_pop*total_pop
      #     next
      # }
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
      mat[(t + incubation),2] <- infected_pop
      mat[(t + incubation),3] <- spop
      mat[(t + incubation),4] <- (infected_pop + (spop*infected_pop*ro*(1)/recovery - infected_pop*1/recovery))*total_pop
      infected_pop_1 <- infected_pop
      infected_pop <-  infected_pop + spop*infected_pop*ro*(1)/recovery - infected_pop*1/recovery
      
      spop <- spop - (spop*infected_pop_1*ro*(1)/recovery)
      
      
    }
    
    dat <- as.data.frame(mat)
    
    colnames(dat) <- c("time", "infected", "susceptible", "pop")
    dat
  })
  
  vaccine_data <- reactive({
    
    total_pop <- input$total_pop
    infected_count <- input$infected_count
    ro <- input$ro
    infected_pop <- infected_count/total_pop
    spop <- 1 - infected_pop
    vaccine_modifier <- 1 - input$vaccine_modifier
    vaccine_pop <- input$vaccine_pop
    incubation <- input$incubation
    recovery <- input$recovery
    
    time_frame <- input$time_frame
    mat <- matrix(nrow = (time_frame + incubation), ncol = 4)
    mat[1:(time_frame + incubation), 1] <- 1:(time_frame + incubation)
    mat[1:incubation, 2] <- infected_pop
    mat[1:incubation, 3] <- spop
    mat[1:incubation, 4] <- infected_pop*total_pop
    
    for(t in 1:time_frame){
      if((1 - infected_pop) <= 0){
        infected_pop <- 1
        spop <- 0
        mat[t,1] <- t
        mat[t,2] <- infected_pop
        mat[t,3] <- spop
        mat[t,4] <- infected_pop*total_pop
        next
      }
      if((1 - infected_pop) >= 1){
        infected_pop <- 0
        spop <- 1
        mat[t,1] <- t
        mat[t,2] <- infected_pop
        mat[t,3] <- spop
        mat[t,4] <- infected_pop*total_pop
        next
      }
      mat[t,1] <- t
      mat[(t + incubation),2] <- infected_pop
      mat[(t + incubation),3] <- spop
      mat[(t + incubation),4] <- infected_pop*total_pop
      infected_pop_1 <- infected_pop
      infected_pop <-  infected_pop + spop*(1- vaccine_pop)*(infected_pop*ro*(1)/recovery) - infected_pop*1/recovery + spop*vaccine_pop*(infected_pop*vaccine_modifier*ro*(1)/recovery) - infected_pop*1/recovery
      # print(infected_pop_1)
      # print(spop)
      # print(ro)
      # print(recovery)
      # spop <- 1 - infected_pop
      spop <- spop - (spop*infected_pop_1*ro*(1)/recovery)
      
      # infected_pop <-  0.12 + 9.661893e-09*(0.12*4*(1)/8 - 0.12*1/8)*(1- 0.7) + 9.661893e-09*(0.12*0.1*4*(1)/8 - 0.12*1/8)*0.7
      
    }
    
    dat <- as.data.frame(mat)
    
    colnames(dat) <- c("time", "infected", "susceptible", "pop")
    dat
  })
  
  vaccine_data_2 <- reactive({
    
    covid <- new('covidData', totalPopulation = input$total_pop, infectedCount = input$infected_count, 
                 rO = input$ro, vaccineModifier = 1 - input$vaccine_modifier, incubationPeriod = input$incubation, 
                 recoveryTime = input$recovery, vaccinatedPopulation = input$vaccine_pop)
    covid <- setPopulationProportions(x = covid)
    
    covid@totalPopulation <- covid@totalPopulation
    infected_count <- covid@infectedCount
    ro <- covid@rO
    infected_pop <- covid@infectedPopulation
    spop <- covid@susceptiblePopulation
    
    vaccine_modifier <- covid@vaccineModifier
    vaccine_pop <- covid@vaccinatedPopulation
    incubation <- covid@incubationPeriod
    recovery <- covid@recoveryTime
    
    sPopUnvaccinated <- covid@susceptiblePopulationUnvaccinated
    # print(sPopUnvaccinated)
    sPopVaccinated <- covid@susceptiblePopulationVaccinated
    # print(sPopVaccinated)
    timeFrame <- input$time_frame
    covidTimeData <- covidTimeDataSetup(covid, timeFrame)
    covidSpreading(covid, timeFrame, covidTimeData)
  })
  
  output$distPlot <- renderPlot({
    if(input$show_unvaccinated == T){
      ggplot() +
        geom_point(data = main_data(), aes(x = time, y = pop))+
        geom_line(data = main_data(), aes(x = time, y = pop))+
        # geom_point(data = vaccine_data(), aes(x = time, y = pop), color = "blue")+
        # geom_line(data = vaccine_data(), aes(x = time, y = pop), color = "blue")+
        geom_point(data = vaccine_data_2(), aes(x = time, y = pop), color = "orange")+
        geom_line(data = vaccine_data_2(), aes(x = time, y = pop), color = "orange")+            # scale_y_continuous(trans = "log10") +
        theme_bw()
    }else{
      ggplot() +
        # geom_point(data = main_data(), aes(x = time, y = pop))+
        # geom_line(data = main_data(), aes(x = time, y = pop))+
        # geom_point(data = vaccine_data(), aes(x = time, y = pop), color = "blue")+
        # geom_line(data = vaccine_data(), aes(x = time, y = pop), color = "blue")+
        geom_point(data = vaccine_data_2(), aes(x = time, y = pop), color = "orange")+
        geom_line(data = vaccine_data_2(), aes(x = time, y = pop), color = "orange")+ 
        # scale_y_continuous(trans = "log10") +
        theme_bw()
    }
  })
  
  # output$datatab <- renderTable({
  #     main_data()
  # })
  output$datatab <- renderDataTable({
    
    DT::datatable( vaccine_data_2())
    
  })   
  
  
  # the chase data ----------------------------------------------------------
  
  
  chaseprobabilityByPosition <- reactive({
    pChaser <- input$chaserprob
    pPlayer <- input$playerprob
    
    pBothMove <- pPlayer*pChaser
    pChaserMove <- (1 - pPlayer)*pChaser
    pPlayerMove <- pPlayer*(1 - pChaser)
    pNoMove <- (1 - pPlayer)*(1 - pChaser)
    steps <- 1000
    chaserstart <- input$chaserstart
    playerstart <- input$playerstart
    
    states <- c(1:((chaserstart + 2)*(playerstart + 3)))
    
    statesDat <- data.frame(states = states, chaser_coord = 1, player_coord = 1, game_state = "playing")
    
    # current_state <- mat[chaser, player]
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
    statesDat <- statesDat %>% filter(chaser_coord <= chaserstart, player_coord <= (playerstart + 1), player_coord >= 0, chaser_coord >= 0) %>% mutate(keep_label = ifelse((chaserstart - player_coord)  < chaser_coord, F, T)) %>%
      filter(keep_label)
    
    trans_mat <- trans_mat[rownames(trans_mat) %in% statesDat$labels,colnames(trans_mat) %in% statesDat$labels]
    disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=statesDat$labels, name="MC 1") 
    
    getProbabilitiesFromMarkovChain <- function(disc_trans, playerstart, chaserstart, statesDat, steps = 1000){
      statelabels <- statesDat$labels
      current_state <- statelabels
      current_state[current_state == paste(playerstart, chaserstart - playerstart, sep = ",")] <- 1
      current_state[current_state != "1"] <- 0
      current_state <- as.numeric(current_state)
      finalState<-current_state*disc_trans^steps
      
      #get steady states 
      results <- colSums(finalState)
      results <- as.data.frame(results)
      results$labels <- rownames(results)
      statesDat <- statesDat %>% full_join(results)
      outcomes <- statesDat %>% select(game_state, labels, results) %>% dplyr::rename(prob = results)
      probres <- outcomes %>% group_by(game_state) %>% dplyr::summarise(prob_win = sum(prob))
      # print(probres)
      return(probres)
    }
    probres <- data.frame(game_state = NA, prob_win = NA, pos_player = NA, pos_chaser = NA)
    for(i in 1:input$playerstart){
      for(j in 1:input$chaserstart ){
        probresTmp <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = i, chaserstart = j, statesDat = statesDat)
        probresTmp <- probresTmp  %>% mutate(pos_player = i, pos_chaser = j)
        probres <- probres %>% bind_rows(probresTmp)
      }
    }
    probresSetup <- probres %>% filter(game_state == "win") %>% select(pos_player, pos_chaser, prob_win)
    probresCast <- reshape2::acast(data = probresSetup, formula = pos_player ~ pos_chaser)
    probresCast <- round(probresCast, 3)
    probresCast
  })
  
  chaseprobability <- reactive({
    pChaser <- input$chaserprob
    pPlayer <- input$playerprob
    
    pBothMove <- pPlayer*pChaser
    pChaserMove <- (1 - pPlayer)*pChaser
    pPlayerMove <- pPlayer*(1 - pChaser)
    pNoMove <- (1 - pPlayer)*(1 - pChaser)
    steps <- 1000
    chaserstart <- input$chaserstart
    playerstart <- input$playerstart
    
    states <- c(1:((chaserstart + 2)*(playerstart + 3)))
    
    statesDat <- data.frame(states = states, chaser_coord = 1, player_coord = 1, game_state = "playing")
    
    # current_state <- mat[chaser, player]
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
    statesDat <- statesDat %>% filter(chaser_coord <= chaserstart, player_coord <= (playerstart + 1), player_coord >= 0, chaser_coord >= 0) %>% mutate(keep_label = ifelse((chaserstart - player_coord)  < chaser_coord, F, T)) %>%
      filter(keep_label)
    
    trans_mat <- trans_mat[rownames(trans_mat) %in% statesDat$labels,colnames(trans_mat) %in% statesDat$labels]
    disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=statesDat$labels, name="MC 1") 
    
    getProbabilitiesFromMarkovChain <- function(disc_trans, playerstart, chaserstart, statesDat, steps = 1000){
      statelabels <- statesDat$labels
      current_state <- statelabels
      current_state[current_state == paste(playerstart, chaserstart - playerstart, sep = ",")] <- 1
      current_state[current_state != "1"] <- 0
      current_state <- as.numeric(current_state)
      finalState<-current_state*disc_trans^steps
      
      #get steady states 
      results <- colSums(finalState)
      results <- as.data.frame(results)
      results$labels <- rownames(results)
      statesDat <- statesDat %>% full_join(results)
      outcomes <- statesDat %>% select(game_state, labels, results) %>% dplyr::rename(prob = results)
      probres <- outcomes %>% group_by(game_state) %>% dplyr::summarise(prob_win = sum(prob))
      # print(probres)
      return(probres)
    }
    probresStay <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart, chaserstart = input$chaserstart, statesDat = statesDat)
    probresClose <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart + 1, chaserstart = input$chaserstart, statesDat = statesDat)
    probresAway <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart - 1, chaserstart = input$chaserstart, statesDat = statesDat)
    
    probresStay <- probresStay %>% mutate(position = "Stayed")
    probresClose <- probresClose %>% mutate(position = "Closer to Chaser")
    probresAway <- probresAway %>% mutate(position = "Further from Chaser")
    # print(probresClose)
    probres <- probresStay %>% bind_rows(probresAway) %>% bind_rows(probresClose) %>% filter(game_state != "playing")
    probres
  })
  chaseprobabilitycloser <- reactive({
    pChaser <- input$chaserprob
    pPlayer <- input$playerprob + input$skillmodifier
    
    pBothMove <- (pPlayer)*pChaser
    pChaserMove <- (1 - pPlayer)*pChaser
    pPlayerMove <- pPlayer*(1 - pChaser)
    pNoMove <- (1 - pPlayer)*(1 - pChaser)
    steps <- 1000
    chaserstart <- input$chaserstart
    playerstart <- input$playerstart
    
    states <- c(1:((chaserstart + 2)*(playerstart + 3)))
    
    statesDat <- data.frame(states = states, chaser_coord = 1, player_coord = 1, game_state = "playing")
    
    # current_state <- mat[chaser, player]
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
    statesDat <- statesDat %>% filter(chaser_coord <= chaserstart, player_coord <= (playerstart + 1), player_coord >= 0, chaser_coord >= 0) %>% mutate(keep_label = ifelse((chaserstart - player_coord)  < chaser_coord, F, T)) %>%
      filter(keep_label)
    
    trans_mat <- trans_mat[rownames(trans_mat) %in% statesDat$labels,colnames(trans_mat) %in% statesDat$labels]
    disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=statesDat$labels, name="MC 1") 
    
    getProbabilitiesFromMarkovChain <- function(disc_trans, playerstart, chaserstart, statesDat, steps = 1000){
      statelabels <- statesDat$labels
      current_state <- statelabels
      current_state[current_state == paste(playerstart, chaserstart - playerstart, sep = ",")] <- 1
      current_state[current_state != "1"] <- 0
      current_state <- as.numeric(current_state)
      finalState<-current_state*disc_trans^steps
      
      #get steady states 
      results <- colSums(finalState)
      results <- as.data.frame(results)
      results$labels <- rownames(results)
      statesDat <- statesDat %>% full_join(results)
      outcomes <- statesDat %>% select(game_state, labels, results) %>% dplyr::rename(prob = results)
      probres <- outcomes %>% group_by(game_state) %>% dplyr::summarise(prob_win = sum(prob))
      # print(probres)
      return(probres)
    }
    probresStay <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart, chaserstart = input$chaserstart, statesDat = statesDat)
    probresClose <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart + 1, chaserstart = input$chaserstart, statesDat = statesDat)
    probresAway <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart - 1, chaserstart = input$chaserstart, statesDat = statesDat)
    
    probresStay <- probresStay %>% mutate(position = "Stayed")
    probresClose <- probresClose %>% mutate(position = "Closer to Chaser")
    probresAway <- probresAway %>% mutate(position = "Further from Chaser")
    # print(probresClose)
    probres <- probresStay %>% bind_rows(probresAway) %>% bind_rows(probresClose) %>% filter(game_state != "playing")
    probres
  })
  chaseprobabilityaway <- reactive({
    pChaser <- input$chaserprob
    pPlayer <- input$playerprob - input$skillmodifier
    
    pBothMove <- pPlayer*pChaser
    pChaserMove <- (1 - pPlayer)*pChaser
    pPlayerMove <- pPlayer*(1 - pChaser)
    pNoMove <- (1 - pPlayer)*(1 - pChaser)
    steps <- 1000
    chaserstart <- input$chaserstart
    playerstart <- input$playerstart
    
    states <- c(1:((chaserstart + 2)*(playerstart + 3)))
    
    statesDat <- data.frame(states = states, chaser_coord = 1, player_coord = 1, game_state = "playing")
    
    # current_state <- mat[chaser, player]
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
    statesDat <- statesDat %>% filter(chaser_coord <= chaserstart, player_coord <= (playerstart + 1), player_coord >= 0, chaser_coord >= 0) %>% mutate(keep_label = ifelse((chaserstart - player_coord)  < chaser_coord, F, T)) %>%
      filter(keep_label)
    
    trans_mat <- trans_mat[rownames(trans_mat) %in% statesDat$labels,colnames(trans_mat) %in% statesDat$labels]
    disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=statesDat$labels, name="MC 1") 
    
    getProbabilitiesFromMarkovChain <- function(disc_trans, playerstart, chaserstart, statesDat, steps = 1000){
      statelabels <- statesDat$labels
      current_state <- statelabels
      current_state[current_state == paste(playerstart, chaserstart - playerstart, sep = ",")] <- 1
      current_state[current_state != "1"] <- 0
      current_state <- as.numeric(current_state)
      finalState<-current_state*disc_trans^steps
      # print(finalState)
      #get steady states 
      results <- colSums(finalState)
      results <- as.data.frame(results)
      results$labels <- rownames(results)
      statesDat <- statesDat %>% full_join(results)
      outcomes <- statesDat %>% select(game_state, labels, results) %>% dplyr::rename(prob = results)
      # print(outcomes)
      probres <- outcomes %>% group_by(game_state) %>% dplyr::summarise(prob_win = sum(prob))
      # print(probres)
      return(probres)
    }
    probresStay <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart, chaserstart = input$chaserstart, statesDat = statesDat)
    probresClose <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart + 1, chaserstart = input$chaserstart, statesDat = statesDat)
    probresAway <- getProbabilitiesFromMarkovChain(disc_trans = disc_trans, playerstart = input$playerstart - 1, chaserstart = input$chaserstart, statesDat = statesDat)
    
    probresStay <- probresStay %>% mutate(position = "Stayed")
    probresClose <- probresClose %>% mutate(position = "Closer to Chaser")
    probresAway <- probresAway %>% mutate(position = "Further from Chaser")
    # print(probresClose)
    probres <- probresStay %>% bind_rows(probresAway) %>% bind_rows(probresClose) %>% filter(game_state != "playing")
    probres
  })
  
  output$chasetable <- renderDataTable({
    probresstay <- chaseprobability()
    probrescloser <- chaseprobabilitycloser()
    probresaway <- chaseprobabilityaway()
    probresstay <- probresstay %>% filter(position == "Stayed")
    probrescloser <- probrescloser %>% filter(position == "Closer to Chaser")
    probresaway <- probresaway %>% filter(position == "Further from Chaser")
    probres <- probresstay %>% bind_rows(probresaway) %>% bind_rows(probrescloser)
    
    probres$prob_win <- round(probres$prob_win, 3)
    probres <- probres %>% filter(game_state == "win")
    DT::datatable(probres)
    
  })   
  output$chaseplot <-  renderPlot({
    ggplot() + 
      geom_bar(data = chaseprobability(), aes(x = position, y = prob_win, group = game_state, fill = game_state), stat = "identity", position = "dodge")
  })
  
  output$expectedReturnsHist <- renderPlot({
    probres <- chaseprobability()
    # print(probres)
    expectedreturns <- data.frame(value = input$moneystay, multiplier_close = seq(0.1, 8, 0.1), multiplier_away = seq(0.01, 0.8, 0.01))
    expectedreturns$stayP <- probres$prob_win[probres$position == "Stayed" & probres$game_state == "win"]
    expectedreturns$awayP <- probres$prob_win[probres$position == "Further from Chaser" & probres$game_state == "win"]
    expectedreturns$closeP <- probres$prob_win[probres$position == "Closer to Chaser" & probres$game_state == "win"]
    
    expectedreturns <- expectedreturns %>% mutate(stay = value*stayP)
    expectedreturns <- expectedreturns %>% mutate(away = value*awayP*multiplier_away)
    expectedreturns <- expectedreturns %>% mutate(close = value*closeP*multiplier_close)
    # print(expectedreturns)
    ggplot() + 
      geom_line(data = expectedreturns, aes(x = multiplier_close, y =  stay), color = "red") + 
      geom_line(data = expectedreturns, aes(x = multiplier_close, y =  away), color = "blue") + 
      geom_line(data = expectedreturns, aes(x = multiplier_close, y =  close), color = "green")
  })
  output$expectedReturnsOffer <- renderPlot({
    probresstay <- chaseprobability()
    probrescloser <- chaseprobabilitycloser()
    probresaway <- chaseprobabilityaway()
    probresstay <- probresstay %>% filter(position == "Stayed")
    probrescloser <- probrescloser %>% filter(position == "Closer to Chaser")
    probresaway <- probresaway %>% filter(position == "Further from Chaser")
    probres <- probresstay %>% bind_rows(probresaway) %>% bind_rows(probrescloser)
    
    winstay <- as.numeric(probres$prob_win[probres$position == "Stayed" & probres$game_state == "win"])
    winaway <- as.numeric(probres$prob_win[probres$position == "Further from Chaser" & probres$game_state == "win"])
    winclose <-  as.numeric(probres$prob_win[probres$position == "Closer to Chaser" & probres$game_state == "win"])
    # print(winstay)
    # print(winaway)
    # print(winclose)
    # print(input$moneystay)
    # print(input$moneycloser)
    # print(input$moneyaway)
    # print(probres)
    expectedreturns <- data.frame(value = c(input$moneystay, input$moneyaway, input$moneycloser), winprob = c(winstay, winaway, winclose), group = c("stay", "away from chaser", "closer to chaser"))
    # print(expectedreturns)
    
    expectedreturns <- expectedreturns %>% mutate(winvalue = value*winprob)
    ggplot() + 
      geom_bar(data = expectedreturns, aes(x = group, y =  winvalue), stat = "identity") 
  })
  output$chanceByPos <- renderDataTable({
    probres <- chaseprobabilityByPosition()
    
    
    # probres$prob_win <- round(probres$prob_win, 3)
    # probres <- probres %>% filter(game_state == "win")
    DT::datatable(probres)
  })
}