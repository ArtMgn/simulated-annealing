# server.R
#
# End of study project
# Markowitz allocation based on simulated annealing algorithm
# @author : Arthur Magnien, INSA Lyon
# last updated: June 19, 2014 by Arthur Magnien

library(shiny)
library(quantmod)
library(GenSA)
library(TFX)

require("corpcor")
require("tawny")
require("quadprog")
require("quantmod")
require("TFX")

source("portfolio.R")

## Launch the app : runApp('markowitzSA')

### Initialization parameters
### To be downloaded one time

v <- c('EUR/USD', 'GBP/USD', 'EUR/GBP', 'USD/CAD', 'AUD/USD')
tickers <- as.character(v)

asset.names = c("EURUSD", "GBPUSD", "EURGBP", "USDCAD", "AUDUSD")

eur <<- ConnectTrueFX("EUR/USD,GBP/USD,EUR/GBP,USD/CAD,AUD/USD",
                      username="Artmmm", password="ndhxduft", format='csv', snapshot=TRUE)

month = substr(Sys.Date(),start=6, stop=10)
year = "2013"
formerDate = paste(year,month,sep="-")
currentDate = Sys.Date() - 1

## Create new dir where data will be stored

mainDir <- "C:/Users/Arthur/Google Drive/R/Sources/markowitzSA"
subDir <- paste("portfolio_", strsplit(as.character(Sys.time()), " ")[[1]][1], sep="")
newDir <- paste(mainDir, subDir, sep="/")
dir.create(file.path(mainDir, subDir))

## create storing file for portfolios

portfolios <<- paste(newDir,"/pf.csv", sep="")
if(!file.exists(portfolios))
  file.create(portfolios)
if(file.info(portfolios)$size == 0){
   sessionId <<- 1
   startPt <<- 0
}
if(file.info(portfolios)$size != 0) {
  session <- read.csv2(portfolios, header=TRUE) 
  if(exists("session")){
    sessionId <<- session$sessionId[length(session$sessionId)] + 1
  }
  startPt <<- length(session$sessionId)
}

shinyServer(function(input, output, session) {
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    as.character(Sys.time())
  })
  
  #### Affichage des métriques

  output$beta <- renderText({
    invalidateLater(1000, session)
    if(exists("betaM"))
      betaM
  })
  
  output$TRPT <- renderText({
    invalidateLater(1000, session)
    if(exists("TRPT"))
      TRPT
  })
  
  output$ExpR <- renderText({
    invalidateLater(1000, session)
    if(exists("expRet"))
      expRet
  })
  
  output$standDev <- renderText({
    invalidateLater(1000, session)
    if(exists("standDev"))
      standDev
  })
  
  ######## Options utilisateur #######################
  ####################################################
  
  output$ui <- renderUI({
    if(!input$autonom) {
      textInput("expRet", "Expected return (% if log)", "1.15")
    }
  })
  
  output$dateUi <- renderUI({
    if(!input$realtime) {
      dateInput("dateInput", "Date:", value = formerDate)
    }
  })
  
  output$optimUi <- renderUI({
    if(input$realtime) {
      checkboxInput("optim", "Try optimisation algorithm", 
                    value = TRUE)
    }
  })
  
  output$realTime <- renderUI({
    if(input$realtime) {
      textInput("freq", "Frequence of reallocation (seconds)", "30")
    }
  })
  
  output$realTime2 <- renderUI({
    if(input$realtime) {
      textInput("time", "Frequence of data fetching (smaller that frequence of asset reallocation) (ms)", "1000")
    }
  })
  
  output$ploting <- renderUI({
    if(input$realtime) {
      plotOutput(outputId= "fxplot",width = "100%", height = "800px")
    }
    else {
      plotOutput(outputId= "plot")
    }
  })
  
  output$markow <- renderUI({
    if(!input$realtime) {
      plotOutput(outputId= "markowitz")
    }
  })
  
  ############## Calcul statique du portefeuille efficient de markowitz ##########
  ################################################################################
  
  dataInput <- reactive({  
    if(input$get == 0) return(NULL)
    
    isolate({
      
      formerDate <<- input$dateInput
      
      cD <<- as.Date(as.character(currentDate))
      fD <<- as.Date(as.character(formerDate))
      
      nDay = as.POSIXct(strptime("25", format="%d"))
      
      if (cD - fD < 500) {
        getSymbols(tickers, src = "oanda", 
                   from = formerDate,
                   to = currentDate,
                   auto.assign = TRUE)
        
        EURUSD <<- EURUSD
        GBPUSD <<- GBPUSD
        EURGBP <<- EURGBP
        USDCAD <<- USDCAD
        AUDUSD <<- AUDUSD
      }
      else {
        J = as.numeric(cD - fD) %/% 400
        Jend = J + 1
        intDate = cD
        EURUSDtemp <- NULL
        GBPUSDtemp <- NULL
        EURGBPtemp <- NULL
        USDCADtemp <- NULL
        AUDUSDtemp <- NULL
        for (i in 1:Jend ) {
          if(i != Jend) {
            getSymbols(tickers, src = "oanda", 
                       from = as.character(intDate-400),
                       to = as.character(intDate),
                       auto.assign = TRUE)
            intDate = intDate - 400
          }
          else {
            getSymbols(tickers, src = "oanda", 
                       from = as.character(fD),
                       to = as.character(intDate),
                       auto.assign = TRUE)
          }
          i = i +1
          EURUSDtemp <- rbind(EURUSDtemp, EURUSD)
          GBPUSDtemp <- rbind(GBPUSDtemp, GBPUSD)
          EURGBPtemp <- rbind(EURGBPtemp, EURGBP)
          USDCADtemp <- rbind(USDCADtemp, USDCAD)
          AUDUSDtemp <- rbind(AUDUSDtemp, AUDUSD)
        }
        EURUSD <<- EURUSDtemp
        GBPUSD <<- GBPUSDtemp
        EURGBP <<- EURGBPtemp
        USDCAD <<- USDCADtemp
        AUDUSD <<- AUDUSDtemp
      }
      
      expReturn <- as.numeric(input$expRet)
      maxIt <- as.numeric(input$maxit)
      
      ### Dimension = number of assets
      dimension <- 5
      lower <- rep(0, dimension)
      upper <- rep(1, dimension) 
      
      if(input$log) {
        # Vecteur d'espérance (log)
        ErEURUSD <- mean(dailyReturn(EURUSD))
        ErGBPUSD <- mean(dailyReturn(GBPUSD))
        ErEURGBP <- mean(dailyReturn(EURGBP))
        ErUSDCAD <- mean(dailyReturn(USDCAD))
        ErAUDUSD <- mean(dailyReturn(AUDUSD))
        
        EspReturn <<- c(ErEURUSD, ErGBPUSD, ErEURGBP, ErUSDCAD, ErAUDUSD)
        names(EspReturn) = asset.names
        
        ### Tableau des 4 retours log
        ##############################
        DailyReturns <- do.call(merge, lapply(asset.names, function(x){dailyReturn(get(x))}))
        names(DailyReturns) <- paste(v,".Return",sep="")
        
        covar <<- cov.shrink(DailyReturns)
        N     <- ncol(DailyReturns)
        zeros <- array(0, dim = c(N,1))
        
        ef <<- efficient.frontier(EspReturn, covar, alpha.min=-2, alpha.max=2, nport=80)
        
        if(!input$autonom) {
          efficientFunction <<- function(w) {
            lambda <- 100
            mu <- 10
            t(w)  %*% covar   %*%  w + lambda * abs( sum(w) - 1 ) + mu * abs(t(EspReturn) %*% w - expReturn)
          }
        }
        else{
          efficientFunction <<- function(w) {
            lambda <- 100
            t(w)  %*% covar   %*%  w + lambda * abs( sum(w) - 1 ) 
          }
        }
        w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
        start.time <- Sys.time()
        out <- GenSA(w, lower = lower, upper = upper, fn = efficientFunction, control=list(maxit=maxIt,verbose=TRUE))
        end.time <- Sys.time()
        time.taken <<- end.time - start.time
        out[c("value","par","counts")]
        if(sum(out$par) != 1){
          out$par = out$par / sum(out$par)
        }
        
        gmin.port <<- getPortfolio(EspReturn, covar, out$par) 
        
      }
      ##### Calcul sans les logs
      else {      

        EsEURUSD <- mean(EURUSD)
        EsGBPUSD <- mean(GBPUSD)
        EsEURGBP <- mean(EURGBP)
        EsUSDCAD <- mean(USDCAD)
        EsAUDUSD <- mean(AUDUSD)
        
        Esp <<- c(EsEURUSD, EsGBPUSD, EsEURGBP, EsUSDCAD, EsAUDUSD)
        names(Esp) = asset.names
        
        DailyEsp <- do.call(merge, lapply(asset.names, function(x){get(x)}))
        names(DailyEsp) <- paste(v,".Return",sep="")
        
        covar <<- cov.shrink(DailyEsp)
        N     <- ncol(DailyEsp)
        zeros <- array(0, dim = c(N,1))
        
        ef <<- efficient.frontier(Esp, covar, alpha.min=-2, alpha.max=2, nport=80)
      
        if(!input$autonom) {
          efficientFunction <<- function(w) {
            lambda <- 100
            mu <- 100
            t(w)  %*% covar   %*%  w + lambda * abs( sum(w) - 1 ) + mu * abs(t(Esp) %*% w - expReturn)
          }  
        }
        else {
          efficientFunction <<- function(w) {
            lambda <- 100
            t(w)  %*% covar   %*%  w + lambda * abs( sum(w) - 1 )
          }  
        }
        
        w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
        start.time <- Sys.time()
        out <- GenSA(w, lower = lower, upper = upper, fn = efficientFunction, control=list(maxit=maxIt, verbose=TRUE))
        end.time <- Sys.time()
        time.taken <<- as.numeric(difftime(end.time,start.time,units="secs")) 
        out[c("value","par","counts")]
        
        if(sum(out$par) != 1){
          out$par = out$par / sum(out$par)
        }
      
        gmin.port <<- getPortfolio(Esp, covar, out$par)     
      }
    })
  })
  
  ############ Static assets chart ####################################
  ##############################################################
  
  output$plot <- renderPlot({
    if(input$get == 0) return(NULL) 
    
    if(!input$realtime) {
      
      if(exists("gmin.port")) {
        
        length <- min(length(EURUSD),length(GBPUSD),length(EURGBP),length(USDCAD),length(AUDUSD))
        
        markowitz <- NULL
        for (i in 1:length ) {
          x<- c(EURUSD[i],GBPUSD[i],EURGBP[i],USDCAD[i],AUDUSD[i])
          xm <- weighted.mean(x, gmin.port$weights)
          d <- xts( 
            xm, 
            as.POSIXct(
              strptime(
                as.character(index(EURUSD[i])), 
                format="%Y-%m-%d"  
              )
            ) 
          )
          markowitz <- rbind(markowitz, d)
        }        
        
        if(input$log){
          op <- par(mfrow=c(2,3))
          chartSeries(dailyReturn(EURUSD), theme = chartTheme("white"), 
                      type = "line", TA = NULL, layout=NULL)
          chartSeries(dailyReturn(GBPUSD), theme = chartTheme("white"), 
                      type = "line", TA = NULL, layout=NULL)
          chartSeries(dailyReturn(EURGBP), theme = chartTheme("white"), 
                      type = "line", TA = NULL, layout=NULL)
          chartSeries(dailyReturn(USDCAD), theme = chartTheme("white"), 
                      type = "line", TA = NULL, layout=NULL)
          chartSeries(dailyReturn(AUDUSD), theme = chartTheme("white"), 
                      type = "line", TA = NULL, layout=NULL)
          ### Rendement sur l'année du portefeuille
          chartSeries(dailyReturn(markowitz), theme = chartTheme("black", up.col='red'), 
                      type = "line", TA = NULL, layout=NULL)
          par(op)      
        }
        else {
          op <- par(mfrow=c(2,3))
          chartSeries(EURUSD, theme = chartTheme("white"), 
                      type = "line", TA=c(addVo(),addBBands()), layout=NULL)
          chartSeries(GBPUSD, theme = chartTheme("white"), 
                      type = "line", TA=c(addVo(),addBBands()), layout=NULL)
          chartSeries(EURGBP, theme = chartTheme("white"), 
                      type = "line", TA=c(addVo(),addBBands()), layout=NULL)
          chartSeries(USDCAD, theme = chartTheme("white"), 
                      type = "line", TA=c(addVo(),addBBands()), layout=NULL)
          chartSeries(AUDUSD, theme = chartTheme("white"), 
                      type = "line", TA=c(addVo(),addBBands()), layout=NULL)
          chartSeries(markowitz, theme = chartTheme("black", up.col='red'), 
                      type = "line", TA = NULL, layout=NULL)
          par(op)
        }
      }
    }
  })
  
  fetchData <- reactive({
    if(input$realtime) {
      if (!input$pause)
        invalidateLater(as.numeric(input$time)) # invalidateLater(1000) = Forces invalidation in 1000 milliseconds (1second fetching)
      
      qtf <- QueryTrueFX(eur)
    }
  })
  
  ##################################################################################
  ############## Calcul temps réel du portefeuille efficient de markowitz ##########
  ##################################################################################
  
  output$fxplot <- renderPlot({
    if(input$realtime) {
      
      input.time <- Sys.time()
      cat("\n fetching data at ",as.character(input.time)," \n")
      
      qtf <- fetchData()
    
      qtf$TimeStamp <- as.character(qtf$TimeStamp)
    
      ### Store new quote in an xts object
        
      eurusdXTS <<- xts(qtf[1,2], as.POSIXct(strptime(as.character(input.time), format="%Y-%m-%d %H:%M:%S")))
      gbpusdXTS <<- xts(qtf[2,2], as.POSIXct(strptime(as.character(input.time), format="%Y-%m-%d %H:%M:%S")))
      eurgbpXTS <<- xts(qtf[3,2], as.POSIXct(strptime(as.character(input.time), format="%Y-%m-%d %H:%M:%S")))
      usdcadXTS <<- xts(qtf[4,2], as.POSIXct(strptime(as.character(input.time), format="%Y-%m-%d %H:%M:%S")))
      audusdXTS <<- xts(qtf[5,2], as.POSIXct(strptime(as.character(input.time), format="%Y-%m-%d %H:%M:%S")))
    
      ### add the new value at the end of the whole quote dataframe
    
      if(!exists("eurusd"))
        eurusd <- NULL
      if(!exists("gbpusd"))
        gbpusd <- NULL
      if(!exists("eurgbp"))
        eurgbp <- NULL
      if(!exists("usdcad"))
        usdcad <- NULL
      if(!exists("audusd"))
        audusd <- NULL
      
      ## add value each X s
    
      eurusd <<- rbind(eurusd, eurusdXTS)
      gbpusd <<- rbind(gbpusd, gbpusdXTS)
      eurgbp <<- rbind(eurgbp, eurgbpXTS)
      usdcad <<- rbind(usdcad, usdcadXTS)
      audusd <<- rbind(audusd, audusdXTS)
      
      asset.namesRT <<- c("eurusd", "gbpusd", "eurgbp", "usdcad", "audusd")
      
      if(!exists("t1"))
        t1 <<- Sys.time()
      cat("t1 = ",as.character(t1)," \n")
      t2 <<- Sys.time()
      cat("t2 = ",as.character(t2)," \n")
      diff <- as.numeric(difftime(t2,t1,units="secs"))
      timeSpread <- as.numeric(input$freq) ## seconds that separate the creation of a new portfolio 
      
      ### compute a new markowitz portfolio every X seconds
      if(diff >= timeSpread) {
        cat("Time spread = ",as.character(timeSpread)," seconds \n")
        ###########################################
        ###### Real-time Markowitz computation ####
        ###########################################
      
        expReturn <- as.numeric(input$expRet)
        maxIt     <- as.numeric(input$maxit)
      
        ### Dimension = number of assets
        dimension <- 5
        lower     <- rep(0, dimension)
        upper     <- rep(1, dimension) 

        EsEURUSD <<- mean(eurusd)
        EsGBPUSD <<- mean(gbpusd)
        EsEURGBP <<- mean(eurgbp)
        EsUSDCAD <<- mean(usdcad)
        EsAUDUSD <<- mean(audusd)
      
        EspRT <<- c(EsEURUSD, EsGBPUSD, EsEURGBP, EsUSDCAD, EsAUDUSD)
        names(EspRT) = asset.namesRT
      
        DailyEspRT <<- do.call(merge, lapply(asset.namesRT, function(x){get(x)}))
        names(DailyEspRT) <- paste(asset.namesRT,".Return",sep="")
      
        covarRT <<- cov.shrink(DailyEspRT)
        NRT     <- ncol(DailyEspRT)
        zerosRT <- array(0, dim = c(NRT,1))
      
        efRT <<- efficient.frontier(EspRT, covarRT, alpha.min=-2, alpha.max=2, nport=80)
      
        if(!input$autonom) {
          efficientFunctionRT <<- function(w) {
            lambda <- 100
            mu <- 100
            t(w)  %*% covarRT   %*%  w + lambda * abs( sum(w) - 1 ) + mu * abs(t(EspRT) %*% w - expReturn)
          }  
        }
        else {
          efficientFunctionRT <<- function(w) {
            lambda <- 100
            t(w)  %*% covarRT   %*%  w + lambda * abs( sum(w) - 1 )
          }  
      }
      if(!input$optim){
        w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
      }
      else {
        w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
        if(file.info(portfolios)$size != 0){
          optimPf <- read.csv2(portfolios, header=TRUE)
          thePf <- optimPf[length(optimPf$sessionId),]
          w <- c(thePf$eurusdW,thePf$gbpusdW,thePf$eurgbpW,thePf$usdcadW,thePf$audusdW)
        }
      }
      start.time <- Sys.time()
      cat(" Current weights are : ", w)
      cat("\n Computing Simulated Annealing... \n")
      outRT <- GenSA(w, lower = lower, upper = upper, fn = efficientFunctionRT, control=list(maxit=maxIt, verbose=TRUE))
      end.time <- Sys.time()
      time.taken <<- end.time - start.time
      cat("\n Time used to compute SA : ", as.character(time.taken), "\n")
      outRT[c("value","par","counts")]
      
      ## Si la somme des poids n'est pas égale à 1, on rééquilibre. 
      if(sum(outRT$par) != 1){
        outRT$par = outRT$par / sum(outRT$par)
      }
      
      cat("\n Creating new portfolio... \n")
      gmin.portRT <<- getPortfolio(EspRT, covarRT, outRT$par)
      cat(" eurusdW = ",  gmin.portRT$weights[['eurusd']], " ; gbpusdW = ",gmin.portRT$weights[['gbpusd']] , " ; eurgbpW = ",gmin.portRT$weights[['eurgbp']]," ; usdcadW = ", gmin.portRT$weights[['usdcad']]," ; audusdW = ",gmin.portRT$weights[['audusd']],"\n" )
      #### create new portfolio dataframe
      ER <- gmin.portRT$er
      SD <- gmin.portRT$sd
      W  <- gmin.portRT$weights
      df <- data.frame(sessionId = sessionId,date = input.time, er = ER, sd = SD, eurusdW = W[1], gbpusdW = W[2], eurgbpW = W[3], usdcadW = W[4], audusdW = W[5])
      row.names(df) <- input.time
      ## Si le fichier possède déjà des entrées, on insère le nouveau portf à la fin de celui-ci 
      if(file.info(portfolios)$size != 0){
        cat("Reading portfolios in portfolios file... \n")
        allPortfolios <- read.csv2(portfolios, header=TRUE)
        allPortfolios$date <- as.POSIXct(strptime(as.character(allPortfolios$date), format="%Y-%m-%d %H:%M:%S"))
        allPortfolios$sessionId <- as.numeric(allPortfolios$sessionId)
        allPortfoliosNew <- rbind(allPortfolios, df)
        write.csv2(allPortfoliosNew, file=portfolios, row.names=FALSE)
        cat("\n New portfolio succesfully added to the others ! \n")
      }
      if(file.info(portfolios)$size == 0){
        write.csv2(df, file=portfolios, row.names=FALSE)
        cat("\n First portfolio succesfully added ! \n")
      }
      
      ## On met à jour le temps de référence
      t1 <<- Sys.time()
      ###########################################
      }
      
      ## Compute markowitz portfolio return
      lengthRT <- min(length(eurusd),length(gbpusd),length(eurgbp),length(usdcad),length(audusd))
      cat("there is currently : ", lengthRT , " quotes. \n")
      
      if(file.info(portfolios)$size != 0){
        
        ### Initialisation
        markowitzReturnRT   <- NULL
        markowitzReturnBest <- NULL
        markowitzLog <- NULL
        lastVector   <- NULL
        firstVector  <- NULL
        rVector  <- NULL
        iterator <- 0

        allPortfolios <- read.csv2(portfolios, header=TRUE)
        
        cat("The length of allPortfolios is currently : ",length(allPortfolios$sessionId),"\n")
        ## Main for loop
        ## @todo : éviter les boucles for en R, pas optimal. Préférer l'utilisation de apply
        for (i in 1:lengthRT ) {
          x <- c(eurusd[i],gbpusd[i],eurgbp[i],usdcad[i],audusd[i])
          
          ## On récupère les prochains cours pour calculer la moyenne de la prochaine itération
          if(i<lengthRT){
            xNext <- c(eurusd[i+1],gbpusd[i+1],eurgbp[i+1],usdcad[i+1],audusd[i+1])
          }
          else{
            xNext <- NULL
          }
          
          ## Création de currentPF et nextPF
          if(iterator == 0){
            
            currentPortfolio <- NULL
            nextPortfolio <- allPortfolios[startPt + iterator +1,]
            w <- c(0,0,0,0,0)
            
          }
          else{
            
            currentPortfolio <- allPortfolios[startPt + iterator,]
            
            if(iterator+1 <= (length(allPortfolios$sessionId) - startPt)){
              nextPortfolio <- allPortfolios[startPt + iterator+1,]
            }
            
            ## valeur du cours du portefeuille
            
            w <- c(currentPortfolio$eurusdW,currentPortfolio$gbpusdW,currentPortfolio$eurgbpW,currentPortfolio$usdcadW,currentPortfolio$audusdW)
            xm  <- weighted.mean(x, w)
            d   <- xts(xm, as.POSIXct(strptime(as.character(index(eurusd[i])), format="%Y-%m-%d %H:%M:%S")))
            markowitzReturnRT <- rbind(markowitzReturnRT, d)
            
            ## Retour en log
            if(exists("N")){
              if(i-N > 1){
                markowitzLogTemp <- ( markowitzReturnRT[[i-N]] - markowitzReturnRT[[i-N-1]] ) / markowitzReturnRT[[i-N-1]]
                markowitzLogTempXTS <- xts(markowitzLogTemp, as.POSIXct(strptime(as.character(index(eurusd[i])), format="%Y-%m-%d %H:%M:%S")))
                markowitzLog <- rbind(markowitzLog, markowitzLogTempXTS)
              }
            }
          }
            
          ## valeur du cours du portefeuille efficient sur toutes les données
          if(exists("gmin.portRT")){
            xmBestPortfolio <- weighted.mean(x, gmin.portRT$weights)
            dBestPortfolio <- xts(xmBestPortfolio, as.POSIXct(strptime(as.character(index(eurusd[i])), format="%Y-%m-%d %H:%M:%S")))
            markowitzReturnBest <- rbind(markowitzReturnBest, dBestPortfolio)
          }
          
          ## Compute next value for TRPT computation
          if(i<lengthRT){
            wNext <- c(nextPortfolio$eurusdW,nextPortfolio$gbpusdW,nextPortfolio$eurgbpW,nextPortfolio$usdcadW,nextPortfolio$audusdW)
            xmNext <- weighted.mean(xNext, wNext)        
          }
          
          ## Test sur les dates, si elles sont égales, on change de pf
          if(exists("nextPortfolio")){
            nextPortfolioDate <- as.POSIXct(strptime(as.character(nextPortfolio$date), format="%Y-%m-%d %H:%M:%S"))
          }
          assetDate <- index(eurusd[i])
          if(exists("nextPortfolioDate")){
            if(assetDate == nextPortfolioDate){
              if(iterator >= 1){
                last <- xm 
                lastVector <- rbind(lastVector, last)
                if(i<=lengthRT){
                  first <- xmNext 
                  firstVector <- rbind(firstVector, first)
                }
              }
              if(iterator >= 2){
                  if(firstVector[[iterator]] != 0){
                    r <- (lastVector[[iterator]] - firstVector[[iterator-1]]) / firstVector[[iterator-1]]
                    rVector <- rbind(rVector, r)
                  }
                }
              iterator <- iterator + 1
              if(!exists("N"))
                N <<- i
            }
          }
        } 
        
        ## Calcul du TRPT 
        if(length(rVector) > 1){
          TRPT <<- prod(rVector+1) - 1
        }
        else {
          TRPT <<- rVector[[1]]
        }
        #if(length(rVector) > 1){
        #  TRPT <- prod(rVector+1)  
        #  TRPT <- TRPT %*% (1+(x(m)- firstVector[[iterator]]) / firstVector[[iterator]]) - 1
        #}
        
      }
      if(exists("markowitzReturnBest")) {
        if(exists("markowitzReturnRT")){
          covarM <- cov(markowitzReturnRT, markowitzReturnBest[(N+1):length(markowitzReturnBest)])
          varM <- var(markowitzReturnBest)
          betaM <<- covarM / varM
          cat("\n Beta is : ", betaM, "\n")  
        }
      }
            
      ### Print des metriques : ###
      if(exists("markowitzReturnRT")){
        expRet <<- mean(markowitzReturnRT)
        cat("\n Current return of our portfolio is : ",expRet ,"\n")
      }
      if(exists("gmin.portRT")){
        standDev <<- round(gmin.portRT$sd, digits = 6)
        cat("\n Standard deviation : ", standDev , "\n")
      }
      if(exists("TRPT")){
        cat("\n Current TRPT of our portfolio is : ", TRPT ,"\n")
      }
      
      op <- par(mfrow=c(5,2))
      chartSeries(eurusd, theme = chartTheme("white"), 
                type = "line", TA=c(addVo(),addBBands()), layout=NULL)
      chartSeries(gbpusd, theme = chartTheme("white"), 
                type = "line", TA=c(addVo(),addBBands()), layout=NULL)
      chartSeries(eurgbp, theme = chartTheme("white"), 
                type = "line", TA=c(addVo(),addBBands()), layout=NULL)
      chartSeries(usdcad, theme = chartTheme("white"), 
                type = "line", TA=c(addVo(),addBBands()), layout=NULL)
      chartSeries(audusd, theme = chartTheme("white"), 
                type = "line", TA=c(addVo(),addBBands()), layout=NULL)
      if(exists("markowitzReturnRT")){
        chartSeries(markowitzReturnRT, theme = chartTheme("black", up.col='red'), 
                  type = "line", TA = NULL, layout=NULL)
      }
      if(exists("markowitzReturnBest")){
        chartSeries(markowitzReturnBest, theme = chartTheme("black", up.col='red'), 
                    type = "line", TA = NULL, layout=NULL)
      }
      if(exists("markowitzLog")){
        chartSeries(markowitzLog, theme = chartTheme("black", up.col='red'), 
                    type = "line", TA = NULL, layout=NULL)
      }
      ## On plot le dernier portfolio enregistré 
      if(exists("gmin.portRT")){
        plot(gmin.portRT, col="blue")
        plot(efRT)
        points(x = gmin.portRT$sd, y = gmin.portRT$weights %*% EspRT, type = "p", pch = 23, col="red")
      }
      par(op)
      }
    })
  
  ################# Print portfolio details ##############################
  ########################################################################
  
  output$text1 <- renderText({ 
    if(input$get == 0) return(NULL)
    if(!input$autonom){
      paste("You have selected a return of : ", input$expRet)
    }
  })
  
  output$text2 <- renderText({ 
    if(input$get == 0) return(NULL)
    paste("Consol output : ",  dataInput())
  })
  
  output$text3 <- renderText({ 
    if(input$get == 0) return(NULL)
    if(input$log) {
      if(exists("EspReturn")){
      paste("Final return of the portfolio (sum(p_i*esp_log(i))) : ",  round(gmin.port$weights %*% EspReturn, digits = 6))
      }
    }
    else {
      if(exists("Esp")){
      paste("Final return of the portfolio (sum(p_i*esp(i))) : ",  round(gmin.port$weights %*% Esp, digits = 6))
      }
    }
  })
  
  output$text4 <- renderText({ 
    if(input$get == 0) return(NULL)
    paste("Sum of weights : ",  round(sum(gmin.port$weights), digits = 6))
  })
  
  output$text5 <- renderText({ 
    if(input$get == 0) return(NULL)
    paste("Standard deviation of the founded portfolio : ",  round(gmin.port$sd, digits = 6))
  })
  
  output$text6 <- renderText({ 
    if(input$get == 0) return(NULL)
    paste("Time taken to find the portfolio (secs) : ",  time.taken)
  })
  
  ############### Markowitz allocation plot #############################
  #######################################################################
  
  output$markowitz <- renderPlot({
    if(input$get == 0) return(NULL)
        
    op <- par(mfrow=c(1,2))
    plot(dataInput(), col="blue")
    plot(ef)
    if(input$log) {
      if(exists("EspReturn")){
      points(x = gmin.port$sd, y = gmin.port$weights %*% EspReturn, type = "p", pch = 23, col="red")
      }
    }
    else {
      if(exists("Esp")){
      points(x = gmin.port$sd, y = gmin.port$weights %*% Esp, type = "p", pch = 23, col="red")
      }
    }
    par(op)
  })
})