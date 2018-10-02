#https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)
library(shinyjs)
library(ggplot2)

############  Globals (based on Egegik sockeye) ######################
ndays <<- 28

esc_lag <<- 3 #lag in days in counting escapement

abun_mean <<- 3000000. #mean annual run size
abun_sd <<- 0.75 #lognormal variation in annual run size

daily_mean <<- 15 #mean annual return date
mean_date_range <<- 6 #for adjusting mean date with uniform +/- range

arriv_spread <<- 4.#typical spread in arrival through season
daily_sd <<- 0.5 #for adjusting daily fraction with log-normal error

harv_max <<- 0.9
harv_half <<- 3 #hours to get 50% harvest rate
betab <<- 5. #controls spread of beta distributed harvest rate, higher = less spread

fore_sig <<- 0.9 #log-normal std dev in pre-season forecast

#constant arrays and indices
fishDay <<- array(data=seq(1,ndays),dim=ndays)
hours <<- array(data=rep(0,ndays),dim=ndays)
etarget <<- pnorm(fishDay,daily_mean,arriv_spread)*1.e6 #daily target
# weekly target
egoal <<- array(dim=4)
for (i in 1:4) {
  if(i>1) {
    iend <- i*7
    iprev <- (i-1)*7
    egoal[i] <<- etarget[iend]-etarget[iprev]
  } else {
    egoal[i] <<- etarget[7]
  }
  
}
elag <<-3

##### variable arrays and indices
iday <<- 1 #index of current day

arrivals <<- array(dim=ndays)
tot_esc <<- array(data=rep(0.,ndays),dim=ndays)
tot_cat <<- array(data=rep(0.,ndays),dim=ndays)
dcatch <<- array(data=rep(0.,ndays),dim=ndays) 



################# UI #################################################################
ui <- fluidPage(
  useShinyjs(),

  titlePanel("Manage this stock"),
  
  #Sidebar Layout 
  sidebarLayout(
    
    #Sidebar panel
    sidebarPanel(

      tags$button(
        id = 'close_all',
        type = "button",
        class = "btn action-button",
        onclick = "setTimeout(function(){window.close();},500);",  # close browser
        "Close window"
      ),      
      br(),
      
      actionButton("restart","Start new game"),
      h4(textOutput("preseason")), #preseason forecast
      
      #Input: Selector for number of hours to fish
     
#      numericInput("h","Hours to fish today (0-24):",min=0, max=24,step=3,value=0,width="50%"),
#      checkboxGroupInput("h","Hours to fish today (0-24):",c("0","3","6","9","12","18","24")),     
      radioButtons("h","Hours to fish today (0-24):",choices=c(0,3,6,9,12,15,18,21,24),selected=0, inline=TRUE),     
      
      #button to register decision
      icon("redo-solid.svg",lib="font-awesome"),
      actionButton("nex","Click to set hours",icon=icon("redo-solid.svg")),
      br(),br(),br(),
      img(src = "SeinersAtCannery.gif",height=200,width=300),
      
      #text for end of simulation
      h2(textOutput("simEnd")),
      h4(textOutput("catescEnd")),
      h4(textOutput("escScore")),
      br(),
      h4(textOutput("evenEnd")),
      h4(textOutput("evenScore")),
      br(),
      h4(textOutput("truth")),
      h4(textOutput("difficulty")),
      br(),br()
      

    ), #sidebarPanel
    
    #Main panel
    mainPanel(
      h4("Hours fished"),
      plotOutput("fhours",height=150),
      plotOutput("catch",height=200),
      plotOutput("escapement",height=200)
      

    ) #mainPanel  
    
  )# sidebarLayout
  
  
) #fluidPage
#########################################################################################################

##################################### server ############################################################
server <- function(input,output) {
 
# first trial (others set up by input$restart)
  alist <- run_returns()
  arrivals <<- alist$arrivals
  abun <<- alist$abun
  mean_date <<- alist$mean_date
  
  observeEvent(input$nex,{
    hours[iday] <<- as.numeric(input$h)
    nex_day(hours[iday],arrivals)
#    print(c(i,hours[i]))
    iday <<- iday+1
    
    if(iday <= ndays+1) {
      #hours plot
      output$fhours <- renderPlot({
        df <- data.frame(day=fishDay,hours=hours)
        #observe({print(df)})
        xp <- ggplot(df,aes(day,hours))+ylim(0.,24.)+geom_col()
#      smax <- max(arrivals)/24
#      xp <- xp + scale_y_continuous(sec.axis=sec_axis(~.*smax))+geom_line(aes(y=newdat/smax,col="red"))
        xp <- xp + theme(text = element_text(size=16))+ylab(NULL)
        xp
      }) #renderPlot hours
      
      #catch plot
      df2  <- data.frame(day=fishDay[1:iday-1],catch=tot_cat[1:iday-1],dcatch=dcatch[1:iday-1])
      output$catch <- renderPlot({
        if(tot_cat[iday-1]<1.e6) {
          yp <- ggplot(df2,aes(day,catch))+geom_line(col="green4",size=2)+ylim(0.,1.E6)+xlim(0.,ndays)
        } else {
          maxc <- tot_cat[iday-1]*1.2
          yp <- ggplot(df2,aes(day,catch))+geom_line(col="green4",size=2)+ylim(0.,maxc)+xlim(0.,ndays)
        }  #if else escapement over goal
        yp <- yp + labs(title="Catch")
        yp <- yp + theme(text = element_text(size=16))+ylab(NULL)
        yp <- yp + geom_col(aes(day,dcatch))
        yp
      }) #renderPlot catch
      
      #escapement plot
      output$escapement <- renderPlot({
        df4 <- data.frame(fishDay=fishDay,etarget=etarget)  

        yp <- ggplot(data=df4,aes(fishDay,etarget))+geom_line(size=1.0,col="black",linetype=3)  #reference line
 
        yp <- yp+ylim(0.,1.E6)+xlim(0.,ndays)
        if(iday>elag+1) {
          if(tot_esc[iday-elag-1]>1.e6) {
            maxe <- tot_esc[iday-elag]*1.2
            yp <- yp+ylim(0.,maxe)
        } } #if else escapement over goal
        yp <- yp + labs(title="Escapement") +ylab(NULL)
        yp <- yp + theme(text = element_text(size=16))
        
        if(iday>(elag+1)) {
          df3  <- data.frame(day=fishDay[1:(iday-1-elag)],escape=tot_esc[1:(iday-1-elag)])
          yp <- yp+geom_point(data=df3,aes(day,escape),size=3,col="red")
          }
        
        yp
      }) #renderPlot escapement

    } else { #end of simulation
      cat("End of simulation","\n")
      # evenness calculation
      obs_h <- array(data=0.,dim=4)
      for (i in 1:4) {
        i1 <- (i-1)*7+1
        i2 <- i1+6
        if(i>1) {
          obs_h[i] <- tot_esc[i2]-tot_esc[i1-1]
        } else {
          obs_h[i] <- tot_esc[i2]
        }
      }

      asize <-abun/1000000
      aesc <- tot_esc[ndays]/1000000 
      cat("timing,size,esc",mean_date,asize,aesc,"\n")
      cat("weekly harvest rates","\n") 
      cat(obs_h,"\n")
      slist <- calcScores(mean_date,asize,aesc,obs_h) #get scores
      print(slist)
      
      output$simEnd <- renderText("End of season!") 
      cf <- as.character(format(tot_cat[ndays],nsmall=0,big.mark=",",digits=3))
      ef <- as.character(format(tot_esc[ndays],nsmall=0,big.mark=",",digits=3))
      output$catescEnd <- renderText(c("Final catch was ",cf," and escapement was ",ef))
      
      #es <- as.character(format(slist$scoreE,nsmall=0,big.mark=",",digits=3))
      #output$escScore <- renderText(c("Your escapement score was ",es))
      output$escScore <- renderText(c("Your escapement score was ",slist$scoreH))
      
      fh <- as.character(format(obs_h,nsmall=0,,big.mark=",",digits=3))
      output$evenEnd <- renderText(c("weekly escapement scores were ",fh)) 

      #vs <- as.character(format(slist$scoreH,nsmall=0,big.mark=",",digits=3))
      #output$evenScore <- renderText(c("Your substock harvest score was ",vs))
      output$evenScore <- renderText(c("Your substock harvest score was ",slist$scoreE))
      
      af <- as.character(format(abun,nsmall=0,big.mark=",",digits=3))
      if(mean_date>=0) {
        mf <- as.character(format(mean_date,nsmall=0,big.mark=",",digits=3))
        output$truth <- renderText(c("The run size was ",af," and ",mf," days later than normal"))
      } else {
        mf <- as.character(format((-1*mean_date),nsmall=0,big.mark=",",digits=3))
        output$truth <- renderText(c("The run size was ",af," and ",mf," days earlier than normal"))
      } # run date (alist$mean_date)   
      
#      dd<- as.character(format(slist$difficulty,nsmall=0,big.mark=",",digits=3))
#      output$difficulty <- renderText(c("Difficulty was: ",dd))
      output$difficulty <- renderText(c("Degree of difficulty was ",slist$difficulty))
      
     
    } #if i <= ndays
  }) #observeEvent nex
  
  ### start new simulation ############################
  observeEvent(input$restart,{
    #clear text
    output$simEnd <- renderText("") 
    output$catescEnd <- renderText("")
    output$escScore <- renderText("")
    output$evenEnd <- renderText("") 
    output$evenScore <- renderText("") 
    output$truth <- renderText("")
    output$difficulty <- renderText("")
    #clear plots
    output$fhours <- renderPlot({
      df <- data.frame(day=fishDay,hours=rep(0,ndays))
      xp <- ggplot(df,aes(day,hours))+ylim(0.,24.)
      xp <- xp + theme(text = element_text(size=16))+ylab(NULL)
      xp
    }) #renderPlot hours
    output$catch <- renderPlot({
      df <- data.frame(day=fishDay,hours=rep(0,ndays))
      xp <- ggplot(df,aes(day,hours))+ylim(0.,1.e6)
      xp <- xp + labs(title="Catch")
      xp <- xp + theme(text = element_text(size=16))+ylab(NULL)
      xp
    }) #renderPlot hours
    output$escapement <- renderPlot({
      df4 <- data.frame(fishDay=fishDay,etarget=etarget)  
      yp <- ggplot(data=df4,aes(fishDay,etarget))+geom_line(size=1.0,col="black",linetype=3)  #reference line
      yp <- yp + labs(title="Escapement") +ylab(NULL)
      yp <- yp + theme(text = element_text(size=16))
      yp
    }) #renderPlot escapement
    
    
    blist <- reset_sim(ndays,tot_cat,tot_esc,arrivals,abun,mean_date)
    #print(blist)
    tot_cat <<- blist$tot_cat
    tot_esc <<- blist$tot_esc
    arrivals <<- blist$arrivals
    abun <<- blist$abun
    mean_date <<- blist$mean_date

    #pre-season forecast
    z <- rnorm(1)
    presea <- abun*exp(z*fore_sig-fore_sig*fore_sig/2.)
    output$preseason <- renderText(c("Pre-season forecast = ",format(presea,big.mark=",",nsmall=0,digits=2))) 
    
        
    #cat("observeEvent-i",iday,"\n")
    #cat("observeEvent-hours",hours,"\n")
  })
  
  # close the R session when Chrome closes
#  shinyServer(function(input, output, session){
#    session$onSessionEnded(function() {
#      stopApp()
#    })
#  })  

  server = function(input, output) {
    observe({
      if (input$close_all > 0) stopApp()                             # stop shiny
    })
  }

} #server
############################################################################################################

##################### model functions ######################################################################

#### calculate scores
calcScores <- function(timing,size,finalE,even) {
  # degree of difficulty
  sindex= max(min((log(size/3)/log(7)),1),-1) # 0 for 3, 1 for 21, -1 for 3/7
  tindex= timing/6 # 1 for 6, 0 for 0, -1 for -6
  ds <- 3*abs(tindex)+3*abs(sindex)+2*tindex*sindex  
  difficulty <- ifelse(ds>5.5,"Insane",ifelse(ds>4.0,"Very Hard",ifelse(ds>3.0,"Tough",ifelse(ds>1.5,"Moderate","Kid Stuff"))))
  #cat(timing,tindex,size,sindex,difficulty,"\n")
  # harvest rate score
  opth <- (size-1)/size
  obsh <- (size-finalE)/size
  sH <- 1.0-abs(opth-obsh)
  scoreH <- grade(sH)
  cat("opth,obsh,scoreH",opth,obsh,scoreH,"\n")
  #evenness score
  sE <- array(dim=4)
  for (i in 1:4) {
    if(even[i]>egoal[i]) {
      sE[i] <- egoal[i]/even[i]
    } else {
      sE[i] <- even[i]/egoal[i]
    }
  }
  cat("even,sE",even,sE,"\n")
  scoreE <- grade(sE)
  rlist <- list(difficulty=difficulty,scoreH=scoreH,scoreE=scoreE)
  return(rlist)
}

### grade
grade <- function(vv) {
  xx <- ifelse(vv>0.97,"A+",ifelse(vv>0.92,"A",ifelse(vv>0.899,"A-",
        ifelse(vv>0.87,"B+",ifelse(vv>0.82,"B",ifelse(vv>0.799,"B-",  
        ifelse(vv>0.77,"C+",ifelse(vv>0.72,"C",ifelse(vv>0.699,"C-",  
        ifelse(vv>0.67,"D+",ifelse(vv>0.62,"D",ifelse(vv>0.599,"D-",
        "F"))))))))))))
  return(xx)
}

#### calculate run abundance and timing for this year
run_returns <- function() {
  
  abun <- abun_mean*exp(rnorm(1,mean=0.,sd=abun_sd)) #abundance
  date_dev <-runif(1,min=-mean_date_range-1,max=mean_date_range+1) #run timing
  mean_date <- daily_mean+trunc(date_dev)
  #cat("run_returns ",abun,date_dev,"\n")

  days <- seq(1,ndays,1)
  daily_frac <- dnorm(days,mean=mean_date,sd=arriv_spread) #expected fraction
  daily_rans <- rnorm(ndays,mean=0,sd=daily_sd) #add randomness
  daily_frac <- daily_frac*exp(daily_rans)
  daily_frac <- daily_frac/sum(daily_frac)
  arrivals <- abun*daily_frac

  zz <- list(abun=abun,mean_date=trunc(date_dev),arrivals=arrivals)
  return(zz)
}

nex_day <- function(inhours,arrivals) {
  #processes result of current day's fishing
  #inputs = day of simulation and hours to fish today
  
  hours <- max(inhours,0)
  hours <- min(hours,24)

  #calulate daily catch and escapement
  hmean <- harv_max*(1-exp(-1.*hours*0.693/harv_half))
  betaa <- hmean*betab/(1-hmean)
  h <- rbeta(1,shape1=betaa,shape2=betab) #randomness in harvest rate
  catch <- arrivals[iday]*h
  escape <- arrivals[iday]-catch
  dcatch[iday] <<- catch
  tot_cat[iday] <<- catch + ifelse(iday>1,tot_cat[iday-1],0.)
  tot_esc[iday] <<- escape + ifelse(iday>1,tot_esc[iday-1],0.)

} # function nex_day

reset_sim <- function(ndays,rtot_cat,rtot_esc,rarrivals,rabun,rmean_date) {
  # declare arrays
  rhours <- array(dim=ndays)
  rtot_cat <- array(dim=ndays)
  rtot_esc <- array(dim=ndays)
  #reset all values
  iday <<- 1
  hours <<- rep(0.,ndays)
  rtot_cat <- rep(0.,ndays)
  rtot_esc <- rep(0.,ndays)
  #creat new run 
  alist <- run_returns()
  rarrivals=alist$arrivals
  rabun=alist$abun
  rmean_date=alist$mean_date
  #return a list
  ret_list <- list(tot_cat=rtot_cat,tot_esc=rtot_esc,arrivals=rarrivals,abun=rabun,mean_date=rmean_date)
  #cat("rest_sim-hours",hours,"\n")
  
  return(ret_list)
}

############################################################################################################

# Create Shiny app ----
shinyApp(ui, server)
#shinyApp(ui, server, session)
#shinyApp(ui, dummyserver)

