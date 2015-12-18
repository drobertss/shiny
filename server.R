library(shiny)
library(sqldf)
library(ggplot2)
library(scales)

# #source("helpers.R")

# # # # # # # print("Reading csv")
          f <- file("data/hubway_trips.csv") 
          df <- sqldf("select * from f", dbname=tempfile(), file.format = list(header=T, row.names=F)) # Imports data into SQLite then pull to R
          df <- subset(df, select = -c(2,3)) #removes hubway id, status and zip code columns from dataframe
# # # # # # # #  print("File successfuly read")
# # # # # # # #  # # splits start date column to separate cols for time
# # # # # # # # # 
#  # # # #  #  #print("Editing start time/date columns")
          df$start_time <- sapply(df$start_date, FUN = function(x) {strsplit(x, split=' ')[[1]][2]}) 
          df$start_date <- sapply(df$start_date, FUN = function(x) {strsplit(x, split=' ')[[1]][1]}) 
# # # # # #  #  # # do same for end date
# # # # # # # # # # print("Editing end time/date columns")
# # # # # # # # #  
          df$end_time <- sapply(df$end_date, FUN = function(x) {strsplit(x, split=' ')[[1]][2]}) 
         df$end_date <- sapply(df$end_date, FUN = function(x) {strsplit(x, split=' ')[[1]][1]}) 
#  # #  # # #  # # year the bike was rented (taken from start_date)
# # # # # # # # print("Editing year columns")
# # # # # # # # #  
        df$year <- sapply(df$start_date,FUN = function(x){strsplit(x, split='/')})[[1]][3]
# # # # # # # #  # # column with age of renter
# # # # # # # # print("Editing age time/date columns")
# # # # # # # # #  
          df$age <- as.numeric(df$year) - as.numeric(df$birth_date)
         df$weekday <- weekdays(df$start_date)
# # #      
# # # print("Everything done")
# # # #df3 <- df


shinyServer(function(input, output) {
  print("Outer shinyServer function")
  output$plot <- renderPlot({
    print("entering outtput plot")
  
    if(input$graphType=="histogram"){
      print("Printing histogram")
      if(input$zip == TRUE){
        print("Printing zip code histogram")
        p <- ggplot(df, aes(x=zip_code)) + geom_bar(aes(y = (..count..)/sum(..count..)), width = .25, position=position_dodge(0.35)) + 
          scale_y_continuous(labels = percent) 
        p <- p + ggtitle("Casual vs. Registered Users") + theme(axis.ticks = element_blank()) + labs(x="User Type", y="")
        print(p)
        print("Printed zip code histogram")
      }else{
      if(input$hist == TRUE){
        if(input$color == TRUE){
          p <- ggplot(df, aes(x=subsc_type, fill=subsc_type)) + geom_bar(aes(y = (..count..)/sum(..count..)), width = .25, position=position_dodge(0.35)) + 
            scale_y_continuous(labels = percent) 
          p <- p + ggtitle("Casual vs. Registered Users") + theme(axis.ticks = element_blank()) + labs(x="User Type", y="")
          print(p)
        }else if(input$color == FALSE){
        p <- ggplot(df, aes(x=subsc_type)) + geom_bar(aes(y = (..count..)/sum(..count..)), width = .25, position=position_dodge(0.35)) + 
          scale_y_continuous(labels = percent) 
        p <- p + ggtitle("Casual vs. Registered Users") + theme(axis.ticks = element_blank()) + labs(x="User Type", y="")
        print(p)
        }
        }else{
          if(input$color==TRUE){
            print("entering gender hist graph type")
            df2 <- df$gender
            df2 <- as.data.frame(df2)
            df2[df2==""] <- NA
            df2 <- na.omit(df2)
            print("gender dataframe created")
            p <- ggplot(df2, aes(x = df2, fill =df2)) +  
              geom_bar(aes(y = (..count..)/sum(..count..)), width = .25, position=position_dodge(0.35)) + 
              scale_y_continuous(labels = percent) 
            p <- p + ggtitle("Male vs. Female Bike Rentals")+ theme(axis.ticks = element_blank()) + labs(x="Sex", y="")
            print(p)
            print("Histogram: After printing x with ggtitle + labels")
          }
          if(input$color==FALSE){
      print("entering gender hist graph type")
      df2 <- df$gender
      df2 <- as.data.frame(df2)
      df2[df2==""] <- NA
      df2 <- na.omit(df2)
      print("gender dataframe created")
      p <- ggplot(df2, aes(x = df2)) 
      p <- p + geom_bar(aes(y = (..count..)/sum(..count..)), width = .25, position=position_dodge(0.35)) + 
        scale_y_continuous(labels = percent) 
      p <- p + ggtitle("Male vs. Female Bike Rentals")+ theme(axis.ticks = element_blank()) + labs(x="Sex", y="")
      print(p)
      print("Histogram: After printing x with ggtitle + labels")
          }
        }
      }
      
    } else if(input$graphType == "ageVsDuration"){
      print("entering scatterplot for age vs duration")
      min <- input$age[1]
      max <- input$age[2]
      time <- input$time
      print(min)
      print(max)
      print(time)
      if(input$smooth == TRUE && input$reverse == TRUE){
        x <- ggplot(subset(df, age>min & age<max & duration<time*3600), aes(x=duration/3600, y=age)) 
        x <- x + geom_point() + ggtitle("Age of Renter vs. Duration of Rent (in hours)") + theme(axis.ticks = element_blank())+
          labs(x="Hours", y="Age") + geom_smooth(method=lm)
        print(x)
      }else if(input$smooth == TRUE && input$reverse == FALSE){
      x <- ggplot(subset(df, age>min & age<max & duration<time*3600), aes(x=age, y=duration/3600)) 
      x <- x + geom_point() + ggtitle("Age of Renter vs. Duration of Rent (in hours)") + theme(axis.ticks = element_blank())+
        labs(x="Age", y="Hours") + geom_smooth(method=lm)
      print(x)
      print("ageVsDuration: After printing x with ggtitle + labels")
      
      }else if(input$smooth == FALSE && input$reverse == TRUE){
        x <- ggplot(subset(df, age>min & age<max & duration<time*3600), aes(x=duration/3600, y=age)) 
        x <- x + geom_point() + ggtitle("Age of Renter vs. Duration of Rent (in hours)") + theme(axis.ticks = element_blank())+
          labs(x="Hours", y="Age") 
        print(x)
        print("ageVsDuration: After printing x with ggtitle + labels")
      }else if(input$smooth == FALSE && input$reverse == FALSE){
        x <- ggplot(subset(df, age>min & age<max & duration<time*3600), aes(x=age, y=duration/3600)) 
        x <- x + geom_point() + ggtitle("Age of Renter vs. Duration of Rent (in hours)") + theme(axis.ticks = element_blank())+
          labs(x="Age", y="Hours")
        print(x)
        print("ageVsDuration: After printing x with ggtitle + labels")
      
    } 
      }else if(input$graphType == "boxWhisker"){
      print("Server.R: Entering boxWhisker")
      if(input$outliers == TRUE && input$color == TRUE){
        a <- ggplot(subset(df, duration<3600 & duration > 0),aes(x=subsc_type, y = duration, fill=subsc_type)) + geom_boxplot()
        b <- ggplot(subset(df, duration<3600 & duration > 0),aes(x=subsc_type, y = duration/60, fill=subsc_type)) + geom_boxplot()
        c <- ggplot(subset(df, duration<3600 & duration > 0),aes(x=subsc_type, y = duration/3600, fill=subsc_type)) + geom_boxplot()
        
      } else if(input$outliers == TRUE && input$color == FALSE){
        a <- ggplot(subset(df, duration<3600 & duration > 0),aes(x=subsc_type, y = duration)) + geom_boxplot()
        b <- ggplot(subset(df, duration<3600 & duration > 0),aes(x=subsc_type, y = duration/60)) + geom_boxplot()
        c <- ggplot(subset(df, duration<3600 & duration > 0),aes(x=subsc_type, y = duration/3600)) + geom_boxplot()
        
      }else if(input$outliers == FALSE && input$color == TRUE){
        a <- ggplot(df,aes(x=subsc_type, y = duration, fill=subsc_type)) + geom_boxplot()
        b <- ggplot(df,aes(x=subsc_type, y = duration/60, fill=subsc_type)) + geom_boxplot()
        c <- ggplot(df,aes(x=subsc_type, y = duration/3600, fill=subsc_type)) + geom_boxplot()
        
      }else if(input$outliers ==FALSE && input$color == FALSE){
        a <- ggplot(df,aes(x=subsc_type, y = duration)) + geom_boxplot()
        b <- ggplot(df,aes(x=subsc_type, y = duration/60)) + geom_boxplot()
        c <- ggplot(df,aes(x=subsc_type, y = duration/3600)) + geom_boxplot()
        
        
      }
      if(input$timeUnit == "second"){
           print("BoxWhisker: After storing boxwhisker ggplot in variable a")
           a <- a +  ggtitle("Box and Whisker Plot of User Subscription Type vs. Duration (in seconds)") + theme(axis.ticks = element_blank())+
             labs(x="Subscription Type", y="Duration")
           print(a)
           print("After printing x with ggtitle + labels")
      } else if (input$timeUnit == "minute"){
          print("BoxWhisker: After storing boxwhisker ggplot in variable a MINUTE")
          b <- b +  ggtitle("Box and Whisker Plot of User Subscription Type vs. Duration (in minutes)") + theme(axis.ticks = element_blank())+
            labs(x="Subscription Type", y="Duration")
          print(b)
          print("After printing x with ggtitle + labels MINUTE")
      } else if (input$timeUnit == "hours"){
          print("BoxWhisker: After storing boxwhisker ggplot in variable a HOUR")
          c <- c +  ggtitle("Box and Whisker Plot of User Subscription Type vs. Duration (in hours)") + theme(axis.ticks = element_blank())+
            labs(x="Subscription Type", y="Duration")
          print(c)
          print("After printing x with ggtitle + labels HOUR")
      }
    } else if(input$graphType == "weekday"){
      if (input$percent == TRUE && input$color == TRUE){
        print("Weekday grah percent == TRUE AND color TRUE")
        w <- ggplot(df,aes(x=weekday, color, fill=weekday)) + theme(axis.ticks = element_blank())
        w <- w + geom_bar(aes(y = (..count..)/sum(..count..)), width = .25, position=position_dodge(0.35)) + 
          scale_y_continuous(labels = percent) + ggtitle("Rentals on Days of the Week (as percentage)") + labs(x="Weekday", y="")
        print(w)
        print("after plotting weekday percent == TRUE ---------- COLOR TRUE")
      } else if(input$percent == TRUE && input$color == FALSE){
        print("Weekday grah percent == TRUE AND color FALSE")
        w <- ggplot(df,aes(x=weekday)) + theme(axis.ticks = element_blank())
        w <- w + geom_bar(aes(y = (..count..)/sum(..count..)), width = .25, position=position_dodge(0.35)) + 
          scale_y_continuous(labels = percent) + ggtitle("Rentals on Days of the Week") + labs(x="Weekday", y="")
        print(w)
        print("after plotting weekday percent == TRUE---- COLOR FALSE")
      } else if(input$percent == FALSE && input$color == TRUE){
        print("Weekday grah percent == TRUE AND color TRUE")
        w <- ggplot(df,aes(x=weekday, fill=weekday)) + theme(axis.ticks = element_blank())
        w <- w + geom_bar() + ggtitle("Rentals on Days of the Week") + labs(x="Weekday", y="")
        print(w)
        print("after plotting weekday percent == TRUE ---------- COLOR TRUE")
        
      } else if(input$percent == FALSE && input$color == FALSE){
        print("Entering weekday plot BOTH FALSE")
      w <- ggplot(df, aes(x=weekday)) + geom_bar()
      w <- w + ggtitle("Frequency of Rentals by Day of the Week") + theme(axis.ticks = element_blank())+
        labs(x="Weekday", y="Count")
      print(w)
      print("after printing weekday plot BOTH FALSE")

      }
      }
  })
  
}
)
