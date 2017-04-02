library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(ggplot2)
library(googleVis)
library(scales)
library(gtrendsR)
library(reshape)
library(stats)
library(rjson)
library(RColorBrewer)
library(lubridate)
library(shinythemes)


#To Deploy
#library(rsconnect)
#rsconnect::deployApp('/Users/mariahharvey/Documents/shinyApp/')

  ########################
  #                       #
  #       Data            #
  #                       #
  ########################
set.seed(123)
#traffic datasets
df<-data.frame(dates=sample(seq(as.Date("2016/10/1"), as.Date("2017/3/31"), "days"),75000, replace=TRUE), location=sample(c("register", "right front", "left front"), replace=TRUE, 75000), 
           age=sample(c("Gen-Z", "Millennial", "Gen-X", "Baby Boomer", "Silent Gen."), prob=c(.10,.34,.41,.12, .13), replace=TRUE, 75000), gender=sample(c("Female", "Male"), replace=TRUE, prob=c(.22, .78), 75000), emotion=sample(c("neutral", "negative", "positive"), prob=c(.6,.18,.22), replace=TRUE, 75000),
           hour=sample(8:21, replace=TRUE, 75000), Device2=sample(0:15, 75000, replace=TRUE), duration=sample(50:5000, replace=TRUE, 75000), duration_store=sample(5:45, replace=TRUE, 75000), time=sample(c("Morning", "Afternoon", "Evening"), 75000, replace=TRUE), 
           week=(sample(c("Weekend", "Weekday"), 75000, replace=TRUE)), visited=sample(c("Just Once", "VIP Shoppers (2+ Visits)"), replace=TRUE, 75000, prob=c(.76,.24)), distance=sample(.5:10, replace=TRUE, 75000))

df$duration2<- cut(df$duration_store, 
             breaks = c(-Inf, 10, 20, 30, Inf), 
             labels = c("< 10 min", "10min-20min", "20min-30min", ">30min"), 
             right = FALSE)
df$Device<- cut(df$Device2, 
                   breaks = c(-Inf, 5, 10, 15, Inf), 
                   labels = c("< 5 sec", "5 to 10 secs", "10 to 15 secs", "> 15 seconds"), 
                   right = FALSE)
from<-c("Front Left(1)","Front Left(1)", "Front Left(1)","Front Left(1)","Front Right(1)", "Front Right(1)", "Front Right(1)", "Front Right(1)",
        "Register(2)", "Back Right(2)", "Back Left(2)", "Back Right(2)", "Back Right(2)", "Back Left(2)", "Register(2)", "Back Left(2)",
        "Back Left(3)", "Back Left(3)", "Back Right(3)", "Back Left(3)","Front Right(3)", "Front Left(3)", "Front Left(3)", "Back Right(3)")
to<-c("Register(2)", "Back Right(2)", "Back Left(2)", "Back Right(2)","Back Right(2)", "Back Left(2)", "Register(2)", "Back Left(2)",
      "Back Left(3)", "Back Left(3)", "Back Right(3)", "Back Left(3)","Front Right(3)", "Front Left(3)", "Front Left(3)", "Back Right(3)",
      "Register(4)", "Leave Store(4)", "Register(4)", "Leave Store(4)", "Register(4)", "Register(4)", "Leave Store(4)", "Leave Store(4)")
n<-c(100, 200, 400, 150, 120, 90, 150, 300,
     100, 200, 400, 150, 120, 90, 150, 300,
     100, 200, 400, 150, 120, 90, 150, 300)

df_sankey<-cbind.data.frame(from,to,n)



  ########################
 #                        #
 #          UI            #
 #                        #
  ########################

ui<-dashboardPage(skin = "black",
  dashboardHeader(title = tags$a(href='https://eyeqinsights.shinyapps.io/shinyapp/',
                                 tags$img(src='logo.png')),
                  dropdownMenuOutput("logoutMenu")),
  dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("area-chart")),
    menuItem("Compare Time Ranges", tabName = "compare", icon = icon("calendar-check-o")),
    menuItem("Compare Locations", tabName = "compare_loc", icon = icon("map-o")),
    menuItem("Demographics", tabName = "demographics", icon = icon("users")),
    menuItem("Traffic", tabName = "traffic", icon = icon("line-chart")),
    menuItem("Duration", tabName = "duration", icon = icon("hourglass-end")),
    menuItem("Store Flow", tabName = "store_flow", icon = icon("arrows"))
     )),
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-black .main-header .logo {
          background-color: #D3D3D3;
        }
        .skin-black .main-header .logo:hover {
          background-color: #D3D3D3;
        }
        .skin-black .main-header >.navbar {
          background-color: #D3D3D3;
        }
      '))),
    
    fluidRow(
    tabItems(
      #Overview
      tabItem(tabName = "overview",
              fluidPage(
              titlePanel(""),
              fluidRow(
                column(3,wellPanel(h3("Q1 2017 Report"),
                                   dateRangeInput("daterange", 
                                                  "Date Range",
                                                  start = "2017-02-01", 
                                                  end = as.character(Sys.Date())),
                                   checkboxGroupInput("loc_overview", label = "Select Location", 
                                                      choices = list("Register" = "register", "Left Front"="left front", "Right Front"="right front"),
                                                      selected = "register"))),
                valueBoxOutput("trafficBox"),
                valueBoxOutput("durationBox"),
                valueBoxOutput("storeBox"),
                valueBoxOutput("notsureBox"),
              mainPanel(plotOutput(outputId="trafficPlot", width="1200px",height="300px"),
              br(),
              column(4,plotOutput(outputId="agePlot", width="400px",height="300px")),
              column(4, plotOutput(outputId="genPlot", width="400px",height="300px")),
              column(4, plotOutput(outputId="emoPlot", width="400px",height="300px")), width=12)
              
      ))),
      
      #Compare time
      tabItem(tabName = "compare",
              fluidPage(
                titlePanel(""),
                sidebarPanel(
                              h3("Q1 2017 Report"),
                              checkboxGroupInput("loc_compare", label = "Select Location", 
                                                 choices = list("Register" = "register", "Left Front"="left front", "Right Front"="right front"),
                                                 selected = "register"),
                             dateRangeInput("daterange1", 
                                            "First Date Range",
                                            start = "2017-02-01", 
                                            end = as.character(Sys.Date())),
                             dateRangeInput("daterange2", 
                                            "Second Date Range",
                                            start = "2017-01-01", 
                                            end = "2017-01-31")),
                mainPanel(plotOutput(outputId="traffic_comparePlot", width="750px",height="350px"),
                          br(),
                          fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                          plotOutput(outputId="agePlot1", width="350px",height="350px"),
                          plotOutput(outputId="agePlot2", width="350px",height="350px"))),
                          br(),
                          fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                          plotOutput(outputId="genPlot1", width="350px",height="350px"),
                          plotOutput(outputId="genPlot2", width="350px",height="350px"))))
                )
            
      ),
      #compare locations 
      tabItem(tabName = "compare_loc",
              fluidPage(
                titlePanel(""),
                sidebarPanel(
                  h3("Q1 2017 Report"),
                  dateRangeInput("daterange_loc1", 
                                 "Date Range",
                                 start = "2017-02-01", 
                                 end = as.character(Sys.Date()))),
                mainPanel(plotOutput(outputId="trafficlocPlot", width="700px",height="500px"),
                          br(),
                           plotOutput(outputId="agelocPlot", width="700px",height="350px"))
                           #plotOutput(outputId="genlocPlot", width="700px",height="350px"))
              )
            ),        
      #Demographics
      tabItem(tabName = "demographics",
              fluidPage(
                titlePanel(""),
                sidebarPanel(h3("Q1 2017 Report"), 
                          dateRangeInput("daterange_demo", 
                                      "Date Range",
                                      start = "2017-02-01", 
                                      end = as.character(Sys.Date())),
                          checkboxGroupInput("loc_demographics", label = "Select Location", 
                                              choices = list("Register" = "register", "Left Front"="left front", "Right Front"="right front"),
                                              selected = "right front"),
                          checkboxGroupInput("gen_demographics", label = "Select Gender", 
                                             choices = list("Male"="Male","Female"="Female"),
                                             selected = "Female")),
          mainPanel(plotOutput(outputId="timeweekPlot", width="600px",height="400px"),
                    br(),
                    plotOutput(outputId="timedayPlot", width="600px",height="400px"),
                    br(),
                    plotOutput(outputId="age_durationPlot", width="600px",height="400px"),
                    br(),
                    plotOutput(outputId="distancePlot", width="600px",height="400px"),
                    br(),
                    plotOutput(outputId="emotion_agePlot", width="600px",height="400px")) 
              )
      ),
      
      #Traffic
      tabItem(tabName = "traffic",
              fluidPage(
                titlePanel(""),
                sidebarPanel(
                  h3("Q1 2017 Report"), 
                             dateRangeInput("daterange_traf", 
                                            "Date Range",
                                            start = "2017-02-01", 
                                            end = as.character(Sys.Date())),
                  checkboxGroupInput("loc_traffic", label = "Select Location", 
                                     choices = list("Register" = "register", "Left Front"="left front", "Right Front"="right front"),
                                     selected = "register"),
                  checkboxGroupInput("gen_traffic", label = "Select Gender", 
                                     choices = list("Male"="Male","Female"="Female"),
                                                    selected = "Male"),
                  checkboxGroupInput("age_traffic", label ="Select Age Group",
                                          choices = list("Gen-Z" = "Gen-Z", "Millennial" = "Millennial",
                                                         "Gen-X"="Gen-X", "Baby Boomer"="Baby Boomer",
                                                         "Silent Gen."="Silent Gen."),
                                          select="Gen-X")),
              mainPanel(plotOutput(outputId="calPlot", width="600px",height="400px"),
                        br(),
                        plotOutput(outputId="hourPlot", width="600px",height="400px"),
                        br(),
                        plotOutput(outputId="DevicePlot", width="600px",height="400px"))
      )),
      
      #Duration
      tabItem(tabName = "duration",
              fluidPage(
                titlePanel(""),
                sidebarPanel(h3("Q1 2017 Report"), 
                            dateRangeInput("daterange_dur", 
                                            "Date Range",
                                            start = "2017-02-01", 
                                            end = as.character(Sys.Date())),
                            checkboxGroupInput("loc_duration", label = "Select Location", 
                                               choices = list("Register" = "register", "Left Front"="left front", "Right Front"="right front"),
                                               selected = "register"),
                            checkboxGroupInput("gen_duration", label = "Select Gender", 
                                               choices = list("Male"="Male","Female"="Female"),
                                                              selected = "Female"),
                            checkboxGroupInput("age_duration", label ="Select Age Group",
                                          choices = list("Gen-Z" = "Gen-Z", "Millennial" = "Millennial",
                                                         "Gen-X"="Gen-X", "Baby Boomer"="Baby Boomer",
                                                         "Silent Gen."="Silent Gen."),
                                          select="Gen-X")),
              mainPanel(plotOutput(outputId="dur1Plot", width="600px",height="400px"),
                        br(),
                        plotOutput(outputId="dur2Plot", width="600px",height="400px"),
                        br(),
                        plotOutput(outputId="dur3Plot", width="600px",height="400px"))
      )),
      
      #Store Flow
      tabItem(tabName = "store_flow",
              mainPanel(
                htmlOutput("flow")))
    )
)))

   ########################
  #                        #
  #       Server           #
  #                        #
  ########################

server <- function(input, output) {
#+++++++++++++++++++++++++++++++++++++++Menu+++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  output$logoutMenu <-renderMenu({
    sidebarMenu(
      menuItem("Logout", icon=icon("cog"), 
               href ="https://eyeqinsights.shinyapps.io/shinyapp/__logout__/")
    )
  })

#++++++++++++++++++++++++++++++++++++++++Overview++++++++++++++++++++++++++++++++++++++++++++++++
  #boxes
  usrCount <- reactive({
    df=df[df$location%in%input$loc_overview,]
    df=df %>%
      filter(dates>=input$daterange[1], dates<=input$daterange[2])%>%
      tally()
    return(as.list(df))})
  
  output$trafficBox<-renderValueBox({
    valueBox(
      usrCount(),
      "Visitors Detected",
      icon = icon("map-o"),
      color="blue"
    )
  })
  
  durationAvg <- reactive({
    df=df[df$location%in%input$loc_overview,]
    df=df%>%filter(dates>=input$daterange[1], dates<=input$daterange[2])%>%
            summarise(mean(duration/1000))
    return(as.list(round(df,2)))})
  
  output$durationBox<-renderValueBox({
    valueBox(
      durationAvg(),
      "Avg. Duration in Front of Device (seconds)",
      icon = icon("hourglass-end"),
      color="blue"
    )
  })

  storeAvg <- reactive({
    df=df[df$location%in%input$loc_overview,]
    df<-df %>%
      filter(dates>=input$daterange[1], dates<=input$daterange[2])%>%
      summarise(mean(duration_store))
    return(as.list(round(df,2)))})
  
  output$storeBox<-renderValueBox({
    valueBox(
      storeAvg(),
      "Avg. Duration in Store (minutes)",
      icon = icon("hourglass"),
      color="blue"
    )
  })
  
  returned <- reactive({
    df=df[df$location%in%input$loc_overview,]
    f<-as.list(df %>%
        filter(dates>=input$daterange[1], dates<=input$daterange[2])%>%
          group_by(visited)%>%summarise(n = n()) %>%
          mutate(freq = n / sum(n)))
    return(paste(round(100*f$freq[2],2),"%", sep=""))
})
  
  output$notsureBox<-renderValueBox({
    valueBox(
      returned(),
      "Percent of Visitors That Returned",
      icon = icon("reply"),
      color="blue"
    )
  })
  
  #traffic plot
  output$trafficPlot <- renderPlot({
      dataInput <- reactive({
        df=df[df$location%in%input$loc_overview,]
        df=df %>%
      filter(dates>=input$daterange[1], dates<=input$daterange[2])
      return(df)})
    
    traffic<-ggplot(dataInput(), aes(x=dates))+
      geom_line(stat="count", colour="#E41A1C")+
      theme(plot.title = element_text(size=22))+
      labs(title="Total Store Visitor Traffic Over Time", x="Dates", y="Total Traffic")
    print(traffic)
  }
)
  
  #pie chart gender
    output$genPlot <- renderPlot({
      genInput <- reactive({
        df=df[df$location%in%input$loc_overview,]
        df=df %>%
          filter(dates>=input$daterange[1], dates<=input$daterange[2])%>%
          group_by(gender) %>% tally()%>%
          arrange(desc(n))
        return(df)})
      
      gen<-ggplot(genInput(), aes(x="", y=n, fill=gender))+
        geom_bar(width = 1, stat = "identity")+
        coord_polar("y", start=0)+
        scale_fill_brewer(palette="Set1")+
        theme_minimal()+
        theme(axis.text.x=element_blank()) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank())+
        geom_text(aes(y =cumsum(n)-n/2, 
                      label = scales::percent(n/sum(n))), size=5)+
        theme(plot.title = element_text(size=22))+
        labs(title="Visitors by Gender", fill='Gender')
      print(gen)
})
    
  #age bar chart
    output$agePlot <- renderPlot({
    ageInput <- reactive({
      df=df[df$location%in%input$loc_overview,]
      df=df %>%
          filter(dates>=input$daterange[1], dates<=input$daterange[2])%>%
          group_by(age) %>% tally()
      return(df)})
      
    age<-ggplot(ageInput(), aes(x=factor(age)))+
      geom_bar(stat="identity", fill=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"), aes(y=(n/sum(n))))+
      scale_y_continuous(labels=percent)+
      theme(plot.title = element_text(size=22))+
      labs(title="Visitors by Age Group", x="Age Group", y="Percent")
  print(age)
})
    
    
  #emotion loc
    output$emoPlot <- renderPlot({
    emoInput <- reactive({
      df=df[df$location%in%input$loc_overview,]
      df=df %>%
          filter(dates>=input$daterange[1], dates<=input$daterange[2])%>%
          group_by(dates, emotion)%>%tally()
      return(df)})
      
    emo<-ggplot(emoInput(), aes(x=dates, y=n, fill=emotion))+
      geom_bar(stat="identity")+
      theme(plot.title = element_text(size=22))+
      scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"))+
      labs(y=" Visitor's Emotion Count", x="Dates", title="Visitor's Emotion over Time", fill="Emotion Type")
  print(emo)
  })
    
#++++++++++++++++++++++++++++++++++++++++Compare Time++++++++++++++++++++++++++++++++++++++++++++++++++
#boxes

#traffic line
    #traffic plot
    output$traffic_comparePlot <- renderPlot({
      dataInput1 <- reactive({
        df=df[df$location%in%input$loc_compare, ]
        df=df %>%
          filter(dates>=input$daterange1[1], dates<=input$daterange1[2])
        return(df)
        })
      dataInput2 <- reactive({
        df=df[df$location%in%input$loc_compare, ]
        df=df %>%
          filter(dates>=input$daterange2[1], dates<=input$daterange2[2])
        return(df)
        })
      
      traffic<-ggplot()+
        geom_line(data = dataInput1(), stat="count", aes(x = dates, color ="#E41A1C"))+
        geom_line(data = dataInput2(), stat="count", aes(x = dates, color = "#377EB8"))+
        theme(plot.title = element_text(size=22))+
        scale_colour_discrete(name  ="Date Range",
                              breaks=c("#E41A1C", "#377EB8"),
                              labels=c("Date Range 1", "Date Range 2"))+
        labs(title="Total Store Visitor Traffic Over Time", x="Dates", y="Total Traffic")
      print(traffic)
    })

#gen chart 1
    output$genPlot1 <- renderPlot({
      genInput1 <- reactive({
        df=df[df$location%in%input$loc_compare, ]
        df=df %>%
          filter(dates>=input$daterange1[1], dates<=input$daterange1[2])%>%
          group_by(gender) %>% tally()%>%
          arrange(desc(n))
        return(df)
      })
      
    ggplot(genInput1(), aes(x="", y=n, fill=gender))+
        geom_bar(width = 1, stat = "identity")+
        coord_polar("y", start=0)+
        scale_fill_brewer(palette="Set1")+
        theme_minimal()+
        theme(axis.text.x=element_blank()) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank())+
        geom_text(aes(y =cumsum(n)-n/2, 
                      label = scales::percent(n/sum(n))), size=5)+
        theme(plot.title = element_text(size=22))+
        labs(title="Visitors by Gender \n(Date Range 1)", fill='Gender')

    })   

#gen chart 2
    output$genPlot2 <- renderPlot({
      
      genInput2 <- reactive({
        df=df[df$location%in%input$loc_compare, ]
        df=df %>%
          filter(dates>=input$daterange2[1], dates<=input$daterange2[2])%>%
          group_by(gender) %>% tally()%>%
          arrange(desc(n))
        return(df)})
      
     ggplot(genInput2(), aes(x="", y=n, fill=gender))+
        geom_bar(width = 1, stat = "identity")+
        coord_polar("y", start=0)+
        scale_fill_brewer(palette="Set1")+
        theme_minimal()+
        theme(axis.text.x=element_blank()) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank())+
        geom_text(aes(y =cumsum(n)-n/2, 
                      label = scales::percent(n/sum(n))), size=5)+
        theme(plot.title = element_text(size=22))+
        labs(title="Visitors by Gender\n (Date Range 2)", fill='Gender')

    }) 
    
    #age bar chart 1
    output$agePlot1 <- renderPlot({
      
      ageInput1 <- reactive({
        df=df[df$location%in%input$loc_compare, ]
        df=df %>%
          filter(dates>=input$daterange1[1], dates<=input$daterange1[2])%>%
          group_by(age) %>% tally()
        return(df)})
      
   ggplot(ageInput1(), aes(x=factor(age)))+
        geom_bar(stat="identity", fill=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"), aes(y=(n/sum(n))))+
        scale_y_continuous(labels=percent)+
        theme(plot.title = element_text(size=22))+
        labs(title="Visitors by Age Group\n (Date Range 1)", x="Age Group", y="Percent")

    })  
    
    #age bar chart 2
    output$agePlot2 <- renderPlot({

      ageInput2 <- reactive({
        df=df[df$location%in%input$loc_compare, ]
        df=df %>%
          filter(dates>=input$daterange2[1], dates<=input$daterange2[2])%>%
          group_by(age) %>% tally()
        return(df)})
      
      ggplot(ageInput2(), aes(x=factor(age)))+
        geom_bar(stat="identity", fill=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"), aes(y=(n/sum(n))))+
        scale_y_continuous(labels=percent)+
        theme(plot.title = element_text(size=22))+
        labs(title="Visitors by Age Group\n (Date Range 2)", x="Age Group", y="Percent")

    })
#+++++++++++++++++++++++++++++++++++++++++++++Compare Locations++++++++++++++++++++++++++++++++++++++++++++++++        
#compare loc
    output$agelocPlot <- renderPlot({
      ageloc1Input <- reactive({
            df %>%
            filter(dates>=input$daterange_loc1[1], dates<=input$daterange_loc1[2])%>%
        group_by(location, age) %>% tally()
    })

      ggplot(ageloc1Input(), aes(x=factor(x=age)))+
        geom_bar(stat="identity", aes(y=n/sum(n)))+
        facet_wrap(~location)+
        scale_y_continuous(labels=scales::percent)+
        theme(plot.title = element_text(size=22))+
        labs(title="Visitors by Age Group", x="Age Group", y="Percent")
    })

# #gender loc
#     output$genlocPlot <- renderPlot({
# 
#       genlocInput <- reactive({df %>%
#                                   filter(dates>=input$daterange_loc1[1], dates<=input$daterange_loc1[2])%>%
#                                   group_by(location, gender) %>% tally()%>%
#                                   arrange(desc(n))})
# 
#      ggplot(genlocInput(), aes(x="", y=n, fill=gender))+
    #     geom_bar(width=1, stat ="identity")+
    #     coord_polar("y", start=0)+
    #     scale_fill_brewer(palette="Set1")+
    #     theme_minimal()+
    #     theme(axis.text.x=element_blank()) +
    #     theme(
    #       axis.title.x = element_blank(),
    #       axis.title.y = element_blank(),
    #       panel.border = element_blank(),
    #       panel.grid=element_blank(),
    #       axis.ticks = element_blank())+
    #     theme(plot.title = element_text(size=22))+
    #     facet_wrap(~location)+
    #     labs(title="Visitors by Gender", fill='Gender')
    # 
    # })

    #traffic plot compare
    output$trafficlocPlot <- renderPlot({
      traffic_loc<-reactive({df %>% filter(dates>=input$daterange_loc1[1], dates<=input$daterange_loc1[2])
            })

      traffic<-ggplot(traffic_loc())+
        geom_line(stat="count", aes(x = dates, color ="#E41A1C"))+
        theme(plot.title = element_text(size=22))+
        facet_wrap(~location, ncol=1)+
        scale_colour_discrete(name="",
                              breaks="#E41A1C",
                              labels="Daily Traffic")+
        labs(title="Total Store Visitor Traffic Over Time", x="Dates", y="Total Traffic")
      print(traffic)
    })
#++++++++++++++++++++++++++++++++++++++++Demographics+++++++++++++++++++++++++++++++++++++++++++++
#time of week


    gender <- reactive({
      df=df[df$location%in%input$loc_demographics,]
      df=df[df$gender%in%input$gen_demographics,]
      df=df%>%filter(dates>=input$daterange_demo[1], dates<=input$daterange_demo[2])%>%
                 group_by(age, time) %>% tally()
      return(df)})

    output$timedayPlot <- renderPlot({
      time<-ggplot(gender(), aes(x=age, y=n, fill=time)) +
        geom_bar(position = "fill",stat = "identity") +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_brewer(palette="Set1")+
        theme(plot.title = element_text(size=22))+
        labs(x="Gender Group", y="Percent(%)", fill="Time of Day", title="Visitor Traffic by Time of Day")
      print(time)
   })
    
    gender2 <- reactive({
      df=df[df$location%in%input$loc_demographics,]
      df=df[df$gender%in%input$gen_demographics,]
      df=df %>%
        filter(dates>=input$daterange_demo[1], dates<=input$daterange_demo[2])%>%
        group_by(age, week) %>% tally()
      return(df)})
    
    output$timeweekPlot <- renderPlot({
      time<-ggplot(gender2(), aes(x=age, y=n, fill=week)) +
        geom_bar(position = "fill",stat = "identity") +
        scale_y_continuous(labels = percent_format()) +
        theme(plot.title = element_text(size=22))+
        scale_fill_brewer(palette="Set1")+
        labs(x="Gender Group", y="Percent(%)", fill="Time of Week", title="Weekday vs. Weekend Traffic")
      print(time)
    })

    #duration in front of Device
  output$age_durationPlot<-renderPlot({
    age_duration<-reactive({
      df=df[df$location%in%input$loc_demographics,]
      df=df[df$gender%in%input$gen_demographics,]
      df=df %>%
        filter(dates>=input$daterange_demo[1], dates<=input$daterange_demo[2])%>%
        group_by(age) %>% summarise(n=mean(Device2))
      return(df)
    })
      ggplot(age_duration(), aes(x = age, y = n, ordered=TRUE, fill = age)) + 
      geom_bar(width = 0.85, stat="identity")+scale_fill_brewer(palette = "Set1")+
      coord_polar(theta = "y") +    
      xlab("Time (in Seconds)") +ylab("") + ggtitle("Average Time People Stand In Front \n of the Device by Age Group")+
      ylim(c(0,15)) + 
      theme(plot.title = element_text(size=22))+
      geom_text(data = age_duration(), hjust = 1, size = 3, aes(x = age, y = 0, label = ""))+
      theme(axis.text.y = element_blank() , axis.ticks = element_blank())+
      geom_text(aes( label = format(n, digits=2, drop0trailing=TRUE),
                     y=n ), stat= "identity", vjust = -.5)
  })
  
  #distance from Device
  output$distancePlot<-renderPlot({
    age_distance<-reactive({
      df=df[df$location%in%input$loc_demographics,]
      df=df[df$gender%in%input$gen_demographics,]
      df=df %>%
        filter(dates>=input$daterange_demo[1], dates<=input$daterange_demo[2])
      return(df)})
   
  fun_median <- function(x){
    return(data.frame(y=median(x),label=paste(round(median(x,na.rm=T),2), "ft", seq="")))}
  
  ggplot(age_distance(), aes(x=factor(age), y=distance, fill=age))+
    geom_boxplot()+
    theme(plot.title = element_text(size=22))+
    stat_summary(fun.data = fun_median, geom="text", hjust=-0.25)+
    coord_flip()+
    scale_fill_brewer(palette="Set1")+
    labs(fill="Age Group", y="Distance from Device (in Feet)", x="", title="How Close Different Age Groups Stand\n in Front of the Device")
})
  
  #emotion age
  output$emotion_agePlot<-renderPlot({
    age_emotion<-reactive({
      df=df[df$location%in%input$loc_demographics,]
      df=df[df$gender%in%input$gen_demographics,]
      df=df %>%
        filter(dates>=input$daterange_demo[1], dates<=input$daterange_demo[2])
      return(df)})
  
  ggplot(age_emotion(), aes(factor(age), fill = emotion)) + 
    geom_bar(position="fill") + 
    coord_flip()+
    labs(x="Age", y="Percent(%)", title="Visitors by Emotion and Age")+
    scale_fill_brewer(palette = "Set1")+
    theme(plot.title = element_text(size=22))+
    scale_y_continuous(labels = percent_format()) +
    theme(axis.text.x=element_text(size=6))+
    theme(legend.position="bottom")
  })  
#++++++++++++++++++++++++++++++++++++++Traffic++++++++++++++++++++++++++++++++++++++++++++++++++++
    traffic_month <- reactive({
      df=df[df$location%in%input$loc_traffic,]
      df=df[df$gender%in%input$gen_traffic,]
      df=df[df$age%in%input$age_traffic,]
      traffic_cal<-df %>%
        filter(dates>=input$daterange_traf[1], dates<=input$daterange_traf[2])%>%
        group_by(dates) %>% tally()
    
      traffic_cal$year  <- year(traffic_cal$dates)
      traffic_cal$month <- month(traffic_cal$dates)
      traffic_cal$cmonth<- month(traffic_cal$dates,label=T)
      traffic_cal$day   <- mday(traffic_cal$dates)
      traffic_cal$cdow  <- wday(traffic_cal$dates,label=T)
      traffic_cal$dow   <- wday(traffic_cal$dates)
    
    wom <- function(date) { # week-of-month
      first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
      return((mday(date)+(first-2)) %/% 7+1)
    }
    
    traffic_cal$week  <- wom(traffic_cal$dates)
    return(traffic_cal)
})
  
    output$calPlot <- renderPlot({
      cal<-ggplot(traffic_month(), aes(x=cdow,y=-week))+
        geom_tile(aes(fill=n,colour="grey50"))+
        geom_text(aes(label=day),size=3,colour="grey20")+
        facet_wrap(~cmonth, ncol=3)+
        scale_fill_gradient(low = "yellow", high = "#F6511D", na.value="white")+
        scale_color_manual(guide=F,values="grey50")+
        scale_x_discrete(labels=c("S","M","T","W","Th","F","S"))+
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        theme(plot.title = element_text(size=14))+
        theme(axis.title.x=element_text(size=10))+
        theme(panel.grid=element_blank())+
        theme(plot.title = element_text(size=22))+
        labs(x="",y="", title="Traffic Count by Hour and Day", fill="Traffic Count")+
        coord_fixed()
    print(cal)
    })
  
    #day hour plot
    traffic_day <- reactive({
    df$year<-as.numeric(as.POSIXlt(df$dates)$year+1900)
    df$month<-as.numeric(as.POSIXlt(df$dates)$mon+1)
    df$weekdayf <- wday(df$dates, label=TRUE)
    
    df=df[df$location%in%input$loc_traffic,]
    df=df[df$gender%in%input$gen_traffic,]
    df=df[df$age%in%input$age_traffic,]
    
    dayhour<-df %>%
      filter(dates>=input$daterange_traf[1], dates<=input$daterange_traf[2])%>%
      group_by(weekdayf, hour) %>% tally()
    return(dayhour)
})
    output$hourPlot <- renderPlot({
    location_traffic<-ggplot(traffic_day(), aes(x=hour,y=n)) +
      geom_bar(stat="identity", colour="#00A6ED", fill="#0D2C54") +
      facet_wrap(~weekdayf, ncol=7)+
      labs(x="Hour of the Day", y = "", title="Traffic Count by Hour")+
      theme(plot.title = element_text(size=14))+
      theme(axis.title.x=element_text(size=10))+
      theme(text = element_text(size=7))+
      scale_x_continuous(breaks=c(10,15,20), labels=c("10am", "3pm", "8pm"))+
      theme(plot.title = element_text(size=22))+
      theme(axis.text.y = element_blank(), axis.ticks.y= element_blank())
    print(location_traffic)
    })
    
    #percent by Device
    output$DevicePlot <- renderPlot({
      Device <- reactive({
        df=df[df$location%in%input$loc_traffic,]
        df=df[df$gender%in%input$gen_traffic,]
        df=df[df$age%in%input$age_traffic,]
        df=df%>%filter(dates>=input$daterange_traf[1], 
                  dates<=input$daterange_traf[2])%>%
                  group_by(dates) %>% tally()
        return(df)})
      
      Device_plot<-ggplot(Device(), aes(x=dates,y=n/100)) + geom_area(fill="#E41A1C")+
        scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
        theme(plot.title = element_text(size=22))+
        labs(title="Percent of Total Store Traffic That Passes by Device", x="Dates", y="Percent(%)")
      print(Device_plot)
    })   
    
#+++++++++++++++++++++++++++++++++++++++Duration++++++++++++++++++++++++++++++++++++++++++++++++++
  
    #duration store
    output$dur1Plot <- renderPlot({
      tbl_duration <- reactive({
      
      df=df[df$location%in%input$loc_duration,]
      df=df[df$gender%in%input$gen_duration,]
      df=df[df$age%in%input$age_duration,]
      df=df%>%
      filter(dates>=input$daterange_dur[1], 
            dates<=input$daterange_dur[2])%>%    
      group_by(visited,duration2) %>% 
      summarise(n=n()) %>% 
      group_by(visited) %>% 
      mutate(perc=n/sum(n))
      return(df)})
    
    dur1<-ggplot(tbl_duration(), aes(x=duration2, fill=duration2))+
      geom_bar(stat="identity", aes(y = perc)) + 
      facet_wrap(~visited, nrow=2)+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette="Set1")+
      theme(plot.title = element_text(size=22))+
      labs(x="Duration", fill="Duration", y="Percent(%)", title="Time Spent in the Store by Frequency of Visit")+
      theme(axis.text=element_text(size=8))
    print(dur1)
})
    
  #duration Device
    
    output$dur2Plot <- renderPlot({
    tbl_duration2 <- reactive({
      df=df[df$location%in%input$loc_duration,]
      df=df[df$gender%in%input$gen_duration,]
      df=df[df$age%in%input$age_duration,]
      df=df%>%
          filter(dates>=input$daterange_dur[1], 
                 dates<=input$daterange_dur[2])%>%
      count(dates, Device) %>%
      group_by(dates)%>%
      mutate(per=round(n/sum(n), 2)) %>% 
      ungroup
    return(df)})
    
    ggplot(tbl_duration2(), aes(x=dates, y=per, colour=(Device)))+geom_line(linetype=5)+
      scale_color_discrete(breaks=c("< 5 sec", "5 to 10 secs", "10 to 15 secs", "> 15 seconds"))+
      scale_y_continuous(labels = percent_format())+
      theme(plot.title = element_text(size=22))+
      labs(y="Percent(%) of Visitors", x="Dates", title="Percent of Visitors by Dwell Time in Front of Device")+
      theme(legend.title=element_blank())
    })
    
    #duration emotion
    output$dur3Plot <- renderPlot({
      tbl_duration3 <- reactive({
        df=df[df$location%in%input$loc_duration,]
        df=df[df$gender%in%input$gen_duration,]
        df=df[df$age%in%input$age_duration,]
        df=df%>%
          filter(dates>=input$daterange_dur[1], 
                 dates<=input$daterange_dur[2])%>%
          group_by(emotion)%>%summarise(n=mean(Device2))
        return(df)
          })
      
    ggplot(tbl_duration3(), aes(x = emotion, y =n , ordered=TRUE, fill = emotion)) + 
      geom_bar(width = 0.85, stat="identity")+scale_fill_brewer(palette = "Set1")+
      coord_polar(theta = "y") +    
      xlab("Time (in Seconds)") + ylab("") + ggtitle("Average Time A Person Stands In Front \n of the Device by their Emotion")+
      ylim(c(0,15)) + 
      theme(plot.title = element_text(size=22))+
      geom_text(data = tbl_duration3(), hjust = 1, size = 3, aes(x = emotion, y = 0, label = ""))+
      theme(axis.text.y = element_blank() , axis.ticks = element_blank())+
      geom_text(aes( label = format(n, digits=2, drop0trailing=TRUE),
                     y= n ), stat= "identity", vjust = -.5)
      })
#+++++++++++++++++++++++++++++++++++++++Store Flow++++++++++++++++++++++++++++++++++++++++++++++++    
#flow traffic chart
    output$flow <- renderGvis({
      gvisSankey(df_sankey, from='from', to='to', weight='n',
                      options=list(height=800, width=850))})
}

shinyApp(ui, server)
