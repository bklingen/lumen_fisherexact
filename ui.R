library("shiny")
library("shinyMatrix")
library("shinyWidgets")

shinyUI(
  navbarPage(
    title=HTML("<b style='color:black;'>Fisher's Exact Test for 2 x 2 Contingency Tables</b>"),
    header = tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      .rounded-corners {
        border-radius: 8px;
      }
      .column1 {
        float: left;
        width: 100px;
        padding: 2px;
        margin-left: 15px;
        margin-bottom: 6px;
      }
      .column2 {
        float: left;
        width: 130px;
        margin-top: 4px;
        padding: 2px;
      }
      .image2 {
         margin-top: 5px;
      }
      .custom-hr {
      border: 0;
      border-top: 1px solid #808080;
      margin: 10px 0;
      }
      
      "))
    ),
    # Application title
    windowTitle="Fisher Exact",
    tabPanel("Contingency Table and P-value", 
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, selectInput("how", "Enter Data:", 
                                list("From Textbook"="textb", "Contingency Table" = "table"), selectize=FALSE)),
          column(6, 
                 conditionalPanel(condition="input.how=='textb'", 
                                  selectInput("dataset", "Dataset:", 
                                              list(
                                                "Lady Tasting Tea" = "tea",
                                                "Hero or Villain" = "snowden",
                                                "Quizzes" = "quiz"
                                              ), selectize=FALSE))
          )
        ),
        # fluidRow(
        #   column(6, textInput("lsamp1", "Row Label:", value="Actually Poured First")),
        #   column(6, textInput("lsamp2", "Column Label:", value="Dr. Bristol's Guess Poured First"))
        # ),
        conditionalPanel(condition = "input.how == 'textb'", 
                         uiOutput(("textbText")),
        ),
        h5(tags$b(tags$u("Contingency Table:"))),
        conditionalPanel(condition = "input.how == 'textb'",
                         tags$style("#tbl0 {background-color:beige;}"),
                         matrixInput("tbl0",
                                     value = matrix(c(3,1,1,3), nrow=2, ncol=2, byrow=TRUE, dimnames=list(Actual=c("Actual Milk First", "Actual Tea First"), Guess=c("Guessed Milk First", "Guessed Tea First"))),               
                                     rows = list(names =TRUE, editableNames=TRUE),
                                     cols = list(names = TRUE, editableNames=TRUE),
                                     class = "numeric",
                                     cells = list(editableCells = TRUE))
        ),
        conditionalPanel(condition = "input.how == 'table'",
          helpText("Enter numbers for the 4 empty cells of the contingency table. Click on row or column headers to change labels."),
          tags$style("#tbl {background-color:beige;}"),
          matrixInput("tbl",
                    value = matrix(c(NA, NA, NA, NA), nrow=2, ncol=2, byrow=TRUE, dimnames=list(Var1=c("Row 1", "Row 2"), Var2=c("Col 1","Col 2"))),            
                    rows = list(names =TRUE, editableNames=TRUE),
                    cols = list(names = TRUE, editableNames=TRUE),
                    class = "numeric",
                    cells = list(editableCells = TRUE))
        ),
        #br(),
        actionButton("go", "Run Fisher's Exact Test"),
        br(),
        br(),
        selectInput("alternative", "Alternative Hypothesis:",
                    list("Odds Ratio ≠ 1 (Association)" = "two.sided", "Odds Ratio > 1 (Positive Association)" = "greater", "Odds Ratio < 1 (Negative Association)" = "less")),
        #h5(tags$b(tags$u("Options:"))),
        fluidRow(
          column(6, awesomeCheckbox("or", "Show Odds Ratio")),
          column(6, awesomeCheckbox("sampDist", "Sampling Distribution"))
        ),
        downloadButton("save", "Download Graph", style = "font-size: 12px; padding: 2px 10px;"),
        tags$hr(class = "custom-hr"),
        #tags$br(),
        h5(tags$b("Available on mobile:")),
        div(class="row",
            # div(class="column1",
            #     a(img(src="IconArtofStat.png", width = "85px", class = "rounded-corners"), 
            #       href='https://artofstat.com/mobile-apps', 
            #       target="_blank"),
            # ),
            div(class="column1",
                a(img(src="IconInference512.png", width = "85px", class = "rounded-corners"), 
                  href='https://artofstat.com/mobile-apps', 
                  target="_blank"),
            )
            # div(class="column2",
            #     a(img(src="AppStoreLogoApple.png", width="125px"), 
            #       href='https://apps.apple.com/us/app/art-of-stat-explore-data/id1599474757?platform=iphone', 
            #       target="_blank"),
            #     br(),
            #     a(img(src="AppStoreLogoAndroid1.png",width="125px",class="image2"), 
            #       href='https://play.google.com/store/apps/details?id=com.artofstat.exploredata', 
            #       target="_blank"
            #       ),
            # )
        ),
        tags$p(
          "More information ",
          tags$a(href = "https://artofstat.com/mobile-apps", "here.", target="_blank")
        )
    ),  #end sidebarpanel
    mainPanel(
      fluidRow(
        column(12,
               HTML("<b> <u> <span style='color:#000000'> Contingency Table:  </u> </b>"),
               tableOutput("tab")
        ),
        # column(4,
        #        plotOutput("barchart1", height=175, width="90%"),
        # )
      ),
      tableOutput("mytest"),
      plotOutput("fisherplot", height = "320px"),
      br(),
      textOutput("explain1"),
      textOutput("explain2"),
      br(),
      conditionalPanel('input.or',
                       tableOutput("or")
      ),
      conditionalPanel('input.sampDist',
                       h5(tags$u(tags$b("Sampling Distribution of First Cell Count under the Null Hypothesis:"))),
                       tableOutput("sampDist")
      )
    )
  )) #end sidebarlayout
) #end navbarpage
) #end shinyUI