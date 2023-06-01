library(tidyverse)
library(ddpcr)

Wait<- function(description,basis=1){
  Sys.sleep(
    basis *
      switch(description
             ,short = 2
             ,medium = 3
             ,long = 5
             ,3)
  )
}

InitialiseDefaultDeck <- function(values = c(1,0,10,-1,seq(10,2,-1))){
  
  if(length(values)!=13){stop("Need to provide exactly 13 values.")}
  
  cards <- c("A","K","Q","J",seq(10,2,-1))
  suits <- c("Spades","Clubs","Hearts","Diamonds")
  
  deck <- data.frame(cards,expand.grid(values=values,suits=suits),cardID = seq(1,52,1),isSeen=0,isFlipped=0)
  deck$desc <- paste(deck$cards,"of",deck$suits)
  deck$shortDesc <- paste(deck$cards,substr(deck$suits,1,1),sep="-")
  return(deck)
}

ShuffleDeck <- function(deck){
  shuffledDeck <- deck[sample.int(nrow(deck),nrow(deck),FALSE),]
  return(shuffledDeck)
}

DrawYourHand <- function(deck,NumberOfCards = 4){
  
  Hand <- deck[c(1:NumberOfCards),]
  
  deck <- deck[-c(1:NumberOfCards),]
  
  return(list(Hand=Hand,deck=deck))
}

LookAtYourHand <- function(GameState,PeekIndex = c(1,2),graphics=TRUE){
  GameState$Hand$isSeen[PeekIndex] <- 1  
  
  if(graphics){
    Number <- menu(choices = paste(GameState$Hand$shortDesc[PeekIndex],collapse=","),graphics = TRUE,title = "Here are your cards:")
  }
  
  return(GameState)
}

FlipTopOfDeckToDiscardPile <- function(GameState){
  if(!all(c("Hand","deck") %in% names(GameState))){stop("GameState incorrect.")}
  
  if("Discard" %in% names(GameState)){
    GameState[["Discard"]] <- rbind(GameState[["deck"]][1,],GameState[["Discard"]])
  } else {
    GameState[["Discard"]] <- GameState[["deck"]][1,]
  }
  GameState[["deck"]] <- GameState[["deck"]][-1,]
  
  GameState[["Discard"]]$isSeen <- GameState[["Discard"]]$isFlipped <- 1
  
  return(GameState)
}

TakeTopDiscard <- function(GameState,HandIndex){
  if(HandIndex > nrow(GameState$Hand)){stop(paste0("Invalid HandIndex. Must be between ",1,"and",nrow(GameState$Hand),"."))}
  
  TopCard <- GameState$Discard[1,]
  HandCardToPop <- GameState$Hand[HandIndex,]
  
  GameState$Discard <- rbind(HandCardToPop,GameState$Discard[-1,])
  GameState[["Discard"]]$isSeen <- GameState[["Discard"]]$isFlipped <- 1
  
  GameState$Hand[HandIndex,] <- TopCard
  
  return(GameState)
} 

FlipACardInYourHand <- function(GameState,HandIndex){
  if(HandIndex > nrow(GameState$Hand)){stop(paste0("Invalid HandIndex. Must be between ",1,"and",nrow(GameState$Hand),"."))}
  if(length(HandIndex)>1){stop("Can only flip one card at a time.")}
  
  GameState$Hand$isSeen[HandIndex] <- 1
  GameState$Hand$isFlipped[HandIndex] <- 1
  return(GameState)
}

PrintGameState <- function(GameState,Prefix=NULL,ShowTopCardOfDeck = FALSE){
  
  if(ShowTopCardOfDeck){
    TopCardOfDeck <- paste0("\nThe next card on top of the deck would have been:\n\n",
                            as.character(GameState$deck$shortDesc[1]))
  } else {
    TopCardOfDeck <- ""
  }
  cat(Prefix,
      "In your hand:\n",
      paste(ifelse(GameState$Hand$isFlipped==1,as.character(GameState$Hand$shortDesc),ifelse(GameState$Hand$isSeen==1,paste(GameState$Hand$shortDesc,"(Face Down)"),"Unknown")),collapse="\n"),
      "\nShowing on the discard pile:\n",
      as.character(GameState$Discard$shortDesc[1]),
      TopCardOfDeck
      ,sep="\n")
}

TakeYourTurn <- function(GameState,basis=1){
  AvailableDiscard <- GameState$Discard[1,]
  Hand <- GameState$Hand
  
  Hand <- Hand %>% 
    mutate(slot = seq(1,nrow(Hand),1)) %>% 
    mutate(Reality = ifelse(isSeen==1,shortDesc,paste("Unknown at slot",slot))) %>% 
    mutate(ActionSpace = paste("Take",AvailableDiscard$shortDesc,"in exchange for",Reality))
  
  UnflippedCardsInHand <-  Hand %>% filter(isFlipped==0)
  
  userInput <- utils::select.list(choices = c(UnflippedCardsInHand$ActionSpace,"Or flip from pile?"),graphics = TRUE,multiple = FALSE,title = "Choose An Action:")
  
  if(userInput!="Or flip from pile?"){
    GameState <- TakeTopDiscard(GameState,which(Hand$ActionSpace==userInput))
    
    if(grepl(pattern = "Unknown",x = userInput)){
      Wait("short",basis)
      cat("\nYou discarded the",Hand$desc[which(Hand$ActionSpace==userInput)],"\n\n")
      Wait("short",basis)
    }
    
    GameState <- FlipTopOfDeckToDiscardPile(GameState)
    
  } else {
    
    GameState <- FlipTopOfDeckToDiscardPile(GameState)
    
    OldAvailableDiscard <- AvailableDiscard
    AvailableDiscard <- GameState$Discard[1,]
    
    cat("\nYou flipped the",AvailableDiscard$desc,"\n\n")
    Wait("medium",basis)
    
    PrintGameState(GameState,Prefix=NULL)
    
    Hand <- Hand %>% 
      mutate(ActionSpace = gsub(pattern = OldAvailableDiscard$shortDesc,replacement = AvailableDiscard$shortDesc,x = ActionSpace))
    
    UnflippedCardsInHand <-  Hand %>% filter(isFlipped==0)
    
    userInput <- utils::select.list(choices = c(UnflippedCardsInHand$ActionSpace,"Or flip one of the cards in your hand?"),graphics = TRUE,multiple = FALSE,title = "Choose An Action:")
    
    if(userInput!="Or flip one of the cards in your hand?"){
      
      GameState <- TakeTopDiscard(GameState,which(Hand$ActionSpace==userInput))
      
      if(grepl(pattern = "Unknown",x = userInput)){
        Wait("short",basis)
        cat("You flipped the",Hand$desc[which(Hand$ActionSpace==userInput)],"\n\n")
        Wait("short",basis)
      }
      
      GameState <- FlipTopOfDeckToDiscardPile(GameState)
      
    } else {
      
      userInput <- utils::select.list(choices = UnflippedCardsInHand$Reality,graphics = TRUE,multiple = FALSE,title = "Choose A Card To Show:")
      
      GameState <- FlipACardInYourHand(GameState,which(Hand$Reality==userInput))
      
      if(grepl(pattern = "Unknown",x = userInput)){
        Wait("short",basis)
        cat("You are showing the",Hand$desc[which(Hand$Reality==userInput)],"\n\n")
        Wait("short",basis)
      }
      
      GameState <- FlipTopOfDeckToDiscardPile(GameState)
    }
  }
  
  PrintGameState(GameState,Prefix="\nYour turn is over...")
  Wait("long",basis)
  cat("\014")  
  return(GameState)
  
}

CalculateYourHoleScore <- function(GameState){
  
  Hand <- GameState$Hand %>% 
    group_by(cards) %>% 
    mutate(id = row_number()) %>% 
    mutate(modulo = id %% 2) %>% 
    mutate(NewScore = ifelse(modulo == 0,-values,values)) %>%
    ungroup()
  
  sum(Hand$NewScore)
  
}

PlaySolitaire <- function(Holes = 9,NumberOfCards = 4,PeekAtCards = 2,GameplaySpeed = "normal"){
  
  basis <- switch(GameplaySpeed
                  ,slow = 1.5
                  ,normal = 1
                  ,fast = 0.5
                  ,instant = 0
                  ,1)
  
  holeScores <- c()
  
  for (hole in c(1:Holes)){
    cat("\014")
    cat("\nWelcome to hole",hole,"!\n\n")
    thisDeck <- InitialiseDefaultDeck()
    thisDeck <- ShuffleDeck(thisDeck)
    GameState<-DrawYourHand(thisDeck,NumberOfCards)
    cat("\nThe cards have been dealt!\n\n")
    Wait("short",basis)
    GameState <- LookAtYourHand(GameState,c(1:PeekAtCards))
    GameState <- FlipTopOfDeckToDiscardPile(GameState)
    PrintGameState(GameState,Prefix=NULL)
    Wait("medium",basis)
    cat("\nGet ready to play!\n\n")
    Wait("short",basis)
    
    counter<-0
    while(counter<NumberOfCards){
      if(counter > 0){PrintGameState(GameState,"\nIt's your turn...")}
      Wait("medium",basis)
      GameState <- TakeYourTurn(GameState,basis)
      counter <- counter + 1
    }
    
    holeScores <- c(holeScores,CalculateYourHoleScore(GameState))
    
    cat("\n\nYou scored",holeScores[hole],"this hole.")
    Wait("short",basis)
    cat("\n\nYour score after Hole",hole,"is",sum(holeScores),"\n")
    Wait("medium",basis)
  }
  
  Wait("long",basis)
  
  return(holeScores)
}

#set.seed(102)
#PlaySolitaire(Holes = 9,NumberOfCards = 4,PeekAtCards = 2,GameplaySpeed = "fast")


