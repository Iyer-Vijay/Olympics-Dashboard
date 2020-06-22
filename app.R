library(plotly)
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(stringr)

df <- read_csv("athlete_events.csv")

events <- unique(df$Event)
year <- unique(sort(df$Year))


df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], 
                                     function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

df$Medal <- ifelse(is.na(df$Medal),"Did not win", df$Medal)

df$Sex[df$Sex=='M'] <- 'Male'
df$Sex[df$Sex=='F'] <- 'Female'

df_new <- df %>% filter(Medal != "Did not win")

# Assign ui function

ui <- dashboardPage(
    dashboardHeader(title = "120 years of Olympic history: Country and Medal count Dashboard",
                    titleWidth = 800),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            box(title = "Leaderboard for gold medal (Top 10 countries only)", 
                plotlyOutput("goldMedal"),width = 3),
            box(title = "Leaderboard for silver medal (Top 10 countries only)", 
                plotlyOutput("silverMedal"),width = 3),
            box(title = "Leaderboard for bronze (Top 10 countries only)", 
                plotlyOutput("bronzeMedal"),width = 3),
            box(
                selectInput("gender", "Select gender",choices = c("Male","Female"),selected = "Male"),
                selectInput("event","Select event",choices = NULL),
                p("*Note: Some events happened in only few olympics, so the leaderboard will show 
          few countries instead of top 10."),
                width = 3)
        ),
        fluidRow(
            p("Data Reference:"),
            p("Kaggle.",em("120 years of Olympic history: athletes and results")),
            a(href="https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results",
              "https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results")
        )
    ))

#Assign Server Function
server <- function(input,output,session){
    
    observe({
        if(input$gender == "Male"){
            events <- df_new$Event[df_new$Sex=='Male']
        }
        else{
            events <- df_new$Event[df_new$Sex=='Female']
        }
        updateSelectInput(session,inputId = "event",choices = events,selected = events[1])
    })
    
    output$goldMedal <- renderPlotly({
        df_filtered <- df %>% filter(Event == input$event & Sex == input$gender)
        gold_count <- df_filtered %>% filter(Medal == "Gold") %>% count(NOC)
        gold_count_top_10 <- gold_count %>% arrange(desc(n)) %>% slice(1:10)
        fig <- ggplot(gold_count_top_10,aes(x=NOC,y=n))+
            geom_bar(position = "stack",stat="identity",fill="#FFDF00") +
            labs(x="Countries",y="Number of medal") +
            theme_minimal() +
            coord_flip()
        ggplotly(fig)
    })
    
    output$silverMedal <- renderPlotly({
        df_filtered <- df %>% filter(Event == input$event & Sex == input$gender)
        silver_count <- df_filtered %>% filter(Medal == "Silver") %>% count(NOC)
        silver_count_top_10 <- silver_count %>% arrange(desc(n)) %>% slice(1:10)
        fig <- ggplot(silver_count_top_10,aes(x=NOC,y=n))+
            geom_bar(position = "stack",stat="identity",fill="#C0C0C0") +
            labs(x="Countries",y="Number of medal") +
            theme_minimal() +
            coord_flip()
        ggplotly(fig)
    })
    
    output$bronzeMedal <- renderPlotly({
        df_filtered <- df %>% filter(Event == input$event & Sex == input$gender)
        bronze_count <- df_filtered %>% filter(Medal == "Bronze") %>% count(NOC)
        bronze_count_top_10 <- bronze_count %>% arrange(desc(n)) %>% slice(1:10)
        fig <- ggplot(bronze_count_top_10,aes(x=NOC,y=n))+
            geom_bar(position = "stack",stat="identity",fill="#cd7f32") +
            labs(x="Countries",y="Number of medal") +
            theme_minimal() +
            coord_flip()
        ggplotly(fig)
    })
}

shinyApp(ui = ui, server = server)

# library(rsconnect)
# deployApp("C:\\Users\\iyerv\\Documents\\Olympics")
