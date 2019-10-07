library(shiny)
library(shinybulma)
library(grid)
library(png)
library(bs4Dash)
library(rsconnect)
library(shinyFeedback)
library(shinyalert)
basic_cards_tab <- bs4TabItem(
  tabName = "linear",
  fluidRow(
    bs4Card(
      title = "INPUT", 
      "FYI: 1 kg = 2.205 lbs. 1 inch = 2.54 cm",
      width = 3,
      height = 500,
      status = "danger", 
      closable = FALSE,
      maximizable = T, 
      collapsible = FALSE,
      # sliderInput("abdomen", "Abdomen 2 circumference (cm):",
      #             min = 60, max = 200, value = 80
      # ),
      # sliderInput("weight", "Weight (lbs):",
      #             min = 100, max = 400, value = 150
      # )
      useShinyFeedback(), # include shinyFeedback
      numericInput(inputId="abdomen",
        "Abdomen 2 circumference (cm):", 
        value = 100
      ),
      numericInput(inputId="weight",
        "Weight (lbs):", 
        value = 150
      ),
      snackbarDanger(
        id = "dangerSnackbar",
        message = "Please enter another set of numbers."
      )
    ),  
    bs4Card(
      width = 9,
      height = 500,
      plotOutput("bigPlot"))
  ),
  fluidRow(
  useShinyalert(),  # Set up shinyalert
  actionButton(inputId="detail", "DETAIL")
  
  )
  
  )

info_tab<- bs4TabItem(
  tabName = "info",
  fluidRow( bs4Card(
    title ="Data cleaning summary",
    mainPanel(
      div("Record 182 is filtered out because it has 0 body fat and there's no way to fix that."),
      div("The HEIGHT of record 42 is fixed according to the weight and adiposity."),
      div("The body fat of record 48 and 76 are fixed according to the density."),
      div("The adiposity of record 163, 220, and 234 are fixed according to the weight and height.")),width=10
  )),
  fluidRow( bs4Card(
    title ="Model summary",
    "In both of our model selection methods, we find that ABDOMEN is the most important variable among 14 predictors. WEIGHT ranks high in both methods comparing with other variables. WRIST is significant in linear stepwise model and ADIPOSITY scores high in nonlinear model. Considering the rule of thumb, we select ABDOMEN as our first variable in predicting body fat. Also, to increase the accuracy we choose another variable among WRIST, WEIGHT, ADIPOSITY as our second variable. The number of predictors are too small to use any tree model or ensemble method in machine learning. We still choose normal linear regression as our model. The collinearity are not significant between two variables, so we may not add regulization to our model. After comparing three models, we choose ABDOMEN and WEIGHT as our predictors, because it has highest R-square 0.72.",width=10
  )),  fluidRow( bs4Card(
    title ="Model(For male)",
      img(src="model.png"),width=10
  ))
  )

mem_tab<- bs4TabItem(
  tabName = "members",
  fluidRow(  bs4Card(
    title = "Members", 
    valueBox(value = "Jiawen Chen", subtitle = "jchen778@wisc.edu",icon = "github",width=8 ),
    valueBox(value = "Chunyuan Jin", subtitle = "cjin46@wisc.edu",icon = "github",width=8 ),
    valueBox(value = "Han Liao", subtitle = "hliao27@wisc.edu",icon = "github",width=8 ),width=10)
  ),
  fluidRow(  
    bs4Card(
      title = "Contact", 
      mainPanel(
        div("Project Website: https://github.com/JiawenChenn/STAT628Module2_G9"),
        div("Question about data cleaning: Han Liao."),
        div("Question about model building: Chunyuan Jin."),
        div("Question about shiny app buiding: Jiawen Chen. "))
    ,width=10)))


ui = bs4DashPage(
  sidebar_collapsed = TRUE,
  enable_preloader = TRUE,
  loading_duration = 3,
  navbar = bs4DashNavbar(
    status = "white"
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "STAT628 GROUP9",
    src = "true.png",
    brandColor = "gray-light",
    #url = "https://github.com/JiawenChenn/haha",
    elevation = 3,
    opacity = 0.8,
    bs4SidebarMenu(
      id = "current_tab",
      bs4SidebarMenuItem(
        "Basic information",
        tabName = "info",
        icon = "book"
      ),
      bs4SidebarMenuItem(
        text = "Body Fat Calculator",
        icon = "calculator",
        startExpanded = FALSE,
        bs4SidebarMenuSubItem(
          text = "Linear regression",
          tabName = "linear",
          icon = "chart-line"
        )
      ),
      bs4SidebarMenuItem(
        "Members",
        tabName = "members",
        icon = "user-astronaut"
      )
      
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      info_tab,
      basic_cards_tab,
      mem_tab
    )
  ),
  footer = bs4DashFooter(
    copyrights = a(
      href = "https://github.com/JiawenChenn/STAT628Module2_G9", 
      target = "_blank", "@Jiawen Chen @Chunyuan Jin @Han Liao"
    ),
    right_text = "2019 Fall"
  )
)