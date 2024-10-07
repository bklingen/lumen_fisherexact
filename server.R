library("shiny")
library("ggplot2")
library("grid")
library("shinyMatrix")

PvalRound <- function(p, plotLabel=FALSE) {
  p0 = unlist(p)
  if(is.null(p0) || is.na(p0)) return(NA)
  if(!is.finite(p0)) return(NA)
  if(p0 > 0.9999) return("1.000")
  else if(p0 < 0.0001) return("< 0.0001")
  else return(format(round(p0 + 10^(-12), 4), nsmall=4))
}

shinyServer(function(input, output, session) {
  
  # ## Initial Pop-up
  # observe({
  #   showModal(
  #     modalDialog(
  #       title = HTML("
  #         <h4>Conduct Inference on your phone with the <b>Art of Stat: Inference</b> mobile app!</h4>
  #         "),
  #       tags$div(
  #         style = "display: flex; flex-wrap: wrap; gap: 5px;",
  #         
  #         tags$div(
  #           style = "flex: 1;",
  #           tags$a(href = 'https://artofstat.com/mobile-apps',
  #                  tags$img(src = "app-inference.png", width = "140", class = "rounded-corners")),
  #           tags$br(),
  #           tags$br(),
  #           tags$a(href = 'https://apps.apple.com/us/app/art-of-stat-resampling/id6448982230?platform=iphone',
  #                  tags$img(src = "AppStoreLogoApple.png", width = "140")),
  #           tags$br(),
  #           tags$br(),
  #           tags$a(href = 'https://play.google.com/store/apps/details?id=com.artofstat.resampling',
  #                  tags$img(src = "AppStoreLogoAndroid1.png", width = "140"))
  #         ),
  #         
  #         tags$div(
  #           style = "flex: 1.2;",
  #           tags$a(href = 'https://artofstat.com/mobile-apps',
  #                  tags$img(src = "IMG-1508.PNG", width = "190", class = "rounded-corners"))
  #         ),
  #         
  #         tags$div(
  #           style = "flex: 1.2;",
  #           tags$a(href = 'https://artofstat.com/mobile-apps',
  #                  tags$img(src = "IMG-1519.PNG", width = "190", class = "rounded-corners"))
  #         )
  #       ),
  #       footer = tagList(
  #         HTML("<big>New: Upload CSV files to app. Search for <b>Art of Stat</b> in the App Store.<br>For more information, including screenshots, <a href=https://artofstat.com/mobile-apps>check here</a>.</big> <br>"), 
  #         tags$br(),
  #         modalButton("Dismiss")
  #       ),
  #       size = "m", #c("m", "s", "l"),
  #       easyClose = TRUE,
  #       fade = TRUE
  #     )
  #   )
  # })  
  
  tableComplete <- reactiveVal(FALSE)

  observeEvent(list(input$how, input$dataset), {
    tableComplete(FALSE)
    if(input$how == "textb") {
      if(input$dataset=='tea'){
        # updateTextInput(session,"lsamp1", value="Belief in Heaven")
        # updateTextInput(session,"lsamp2", value="Belief in Hell")
        m <- matrix(c(3,1,1,3), nrow=2, ncol=2, byrow=TRUE, dimnames=list(Actual=c("Actual Milk First", "Actual Tea First"), Guess=c("Guessed Milk First", "Guessed Tea First")))          
        updateMatrixInput(session, "tbl0", value=m)
      }
      if(input$dataset=='snowden'){
        # updateTextInput(session,"lsamp1", value="First Purchase")
        # updateTextInput(session,"lsamp2", value="Second Purchase")
        m <- matrix(c(1, 9, 5, 2), nrow=2, ncol=2, byrow=TRUE, dimnames=list(Student=c("U.S. Student", "International Student"), Second=c("Hero", "Criminal")))          
        updateMatrixInput(session, "tbl0", value=m)
      }
      if(input$dataset=='quiz'){
        # updateTextInput(session,"lsamp1", value="Belief in Heaven")
        # updateTextInput(session,"lsamp2", value="Belief in Hell")
        m <- matrix(c(5,0,6,6), nrow=2, ncol=2, byrow=TRUE, dimnames=list(Course=c("Stats", "CS"), CS=c("Change","No Change")))          
        updateMatrixInput(session, "tbl0", value=m)
      }
    }
  }, ignoreInit = FALSE)
  
  output$textbText <- renderText({
    switch(input$dataset,
           "tea" = {
             HTML('R.A. Fisher asked Dr. Bristol to taste
eight cups of tea, four of which had milk added first and four of which
had tea added first. She was told there were four cups of each type. The order of presenting
the eight cups to her was randomized. Suppose Dr. Bristol correctly identified 3 cups.')
           },
           "snowden" = {
             HTML('In a 2014 survey at
a U.S. College, 10 randomly selected U.S. students and 7 randomly selected
international students were asked whether Edward Snowden should
be considered a hero or a criminal. The results of the survey are shown in the table below.')
           },
           "quiz" = {
             HTML('To help students gauge their understanding, short online quizzes 
             were administered in a statistics and a computer science
  class. For those
  students who realized they knew less than they thought
  after seeing their performance on the quizz, did this change their study habits?')
           },
    )
  })
  
  
  
  myTable <- reactive({
    if(input$how == 'textb') mytable <- input$tbl0
    else mytable <- input$tbl
  })
  
  observeEvent(list(input$go), {
    mytab = myTable()
    if(any(is.na(mytab)) | any(is.null(mytab)) | any(!is.numeric(mytab))) {
      showModal(
        modalDialog(
          title = "Attention !",
          "Please enter four valid cell counts in order to run the test.",
          easyClose = FALSE, # Allows closing the modal by clicking outside
          footer = tagList(
            #actionButton("confirm", "Yes"),
            modalButton("OK") # Predefined button to close the modal
          )
        )
      )
      tableComplete(FALSE)
    } else {
      tableComplete(TRUE)
    }
  }, ignoreInit = TRUE)
  
  
  output$tab <- renderTable({
    mytab = myTable()
    tab <- addmargins(mytab)
    tab <- apply(tab,c(1,2), function(x) format(x, big.mark = ","))
    rnames <- dimnames(tab)[[1]]
    rnames <- paste0("<b>", rnames, "</b>")
    rownames(tab) <- c(rnames[1:2], "<b> Total </b>")
    colnames(tab)[3] <- "<b> Total </b>"
    return(tab)
  },
  rownames=TRUE,
  colnames=TRUE,
  digits = 0,
  sanitize.text.function = function(x) x,
  align = "r"
  #add.to.row <- list(pos=list(0,0), command=c("& \\multicolumn{2}{c}{Sample 2} \\\\\n",
  #                                            "Sample 1 & Success & Failure \\\\\n"))
  )
  
  # stat1 <- reactive({
  #   df <- mytab()
  #   if(all(is.na(df))) return(NULL)
  #   lgroups <- names(dimnames(df))
  #   m1 <- margin.table(df,1)
  #   n <- sum(m1)
  #   prop1 <- m1[1]/n
  #   m2 <- margin.table(df,2)
  #   prop2 <- m2[1]/n
  #   df <- data.frame(Sample = c(1,2), Label=lgroups, n=c(n,n), y=c(m1[1],m2[1]), prop=c(prop1,prop2), stringsAsFactors = FALSE)
  #   return(df)  
  # })
  
  fishertest <- reactive({
    if(!tableComplete()) return(NULL)
    mytab = myTable()
    return(fisher.test(mytab, alternative=input$alternative))
  })

  fisher.df  <- reactive({
    if(!tableComplete()) return(NULL)
    mytable <- myTable()
    n1. <- sum(mytable[1,])
    n.1 <- sum(mytable[,1])
    n <- sum(mytable)
    n1 <- seq(max(0,n.1-(n-n1.)),min(n1.,n.1))
    probs <- dhyper(n1, n.1, n-n.1, n1.) #could also use odds ratio as extremeness criterion
    df <- data.frame(x=n1, 
                  prob=probs, 
                  extreme=switch(input$alternative,
                        "greater" = (n1>=mytable[1,1]),
                        "less" = (n1<=mytable[1,1]),
                        "two.sided" = (probs<=dhyper(mytable[1,1], n.1, n-n.1, n1.))
                        )
                )
    df$extreme <- factor(df$extreme,levels=c("TRUE","FALSE"),order=TRUE)
    return(df)
  })
   
  output$mytest <- renderTable({
    if(!tableComplete()) return(NULL)
    mytable <- myTable()
    oddsRatio = mytable[1,1]*mytable[2,2]/(mytable[1,2]*mytable[2,1])
    df <- data.frame(
      param = "Odds Ratio", 
      null = HTML("Odds Ratio = 1<br>(Independence)"),
      alth = switch(input$alternative,
          "greater" = HTML("Odds Ratio > 1<br>(Positive Association)"),
          "two.sided" = HTML("Odds Ratio ≠ 1<br>(Association)"),
          "less" = HTML("Odds Ratio < 1<br>(Negative Association)")
      ),
      tstat = mytable[1,1], 
      # tstat = switch(input$alternative,
      #     "greater" = oddsRatio,
      #     "two.sided" = mytable[1,1],
      #     "less" = oddsRatio),
      or = signif(oddsRatio,2),
      Pval = fishertest()$p.value
    )
    df$tstat = format(round(df$tstat, 2))
    df$Pval = PvalRound(df$Pval)
    colnames(df) <- c(
      HTML("Population<br>Parameter"), 
      HTML("Null<br>Hypothesis"), 
      HTML("Alternative<br>Hypothesis"), 
      switch(input$alternative,
        "greater" = HTML("Test Statistic<br>(Odds Ratio)"),
        "two.sided" = HTML("Test Statistic<br>(First Cell)"),
        "less" = HTML("Test Statistic<br>(Odds Ratio)")
      ),
      "Odds Ratio",
      "P-value"
    )
    if(!input$or) return(df[,-5]) else return(df)
  },
  rownames=FALSE,
  caption = "<b> <u> <span style='color:#000000'> Fisher's Exact Test:  </u> </b>",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL),
  sanitize.text.function = function(x) x,
  align = "c",
  striped = TRUE,
  bordered = TRUE
  )
  
  output$sampDist <- renderTable({
    if(!input$sampDist) return(NULL)
    if(!tableComplete()) return(NULL)
    df <- fisher.df()[,c(1,2)]
    colnames(df) = c('First Cell Count', 'Null Probability')
    return(df)
  },
  # caption = "<b> <u> <span style='color:#000000'> Sampling Distribution of First Cell Count under the Null Hypothesis:  </u> </b>",
  # caption.placement = getOption("xtable.caption.placement", "top"),
  # caption.width = '100%',
  rownames=FALSE,
  sanitize.text.function = function(x) x,
  align = "c",
  digits = 5,
  striped = TRUE,
  bordered = FALSE
  )
  
  
  output$explain1 <- renderText({
    if(!tableComplete()) return(NULL)
    mytable <- myTable()
    Pval = PvalRound(fishertest()$p.value)
    HTML(paste0("The P-value of ", Pval, " is the sum of the probabilities of those tables (shown in red) with first cell count as or more extreme
          than the observed cell count of ", mytable[1,1],":"))
  })
  
  output$explain2 <- renderText({
    if(!tableComplete()) return(NULL)
    Pval = PvalRound(fishertest()$p.value)
    pterm <- paste(format(round(fisher.df()$prob[fisher.df()$extreme=="TRUE"], 5), scientific=FALSE), collapse=" + ")
    HTML(paste0("P-value = ", Pval, " = ", pterm))
  })

  output$fisherplot <- renderPlot({
    if(!tableComplete()) return(NULL)
    mytable <- myTable()
    #t <- input$tbl
      #plot.maintitle <- substitute(a, list(a="Distribution of first cell count"))
      #plot.subtitle <- substitute(a, list(a="The P-value is the sum of the table probabilities for those tables with first cell count \n as or more extreme than the observed cell count."))
      #plot.subtitle2 <- substitute(a * b, list(a="Observed cell count:", b=t[1,1]))

      fisher.plot <- ggplot(data=fisher.df(), aes(x=x, y=prob, color=factor(extreme,labels=c("FALSE","TRUE"),order=TRUE))) +
        geom_segment(aes(xend=x, yend=0, color=extreme), stat="identity", linewidth=4, alpha=0.5) +
        #geom_point(size=4, alpha=0.5) +
        theme_classic() +
        scale_y_continuous(limits=c(0,1.05*max(fisher.df()$prob)), expand=c(0,0)) +
        scale_x_continuous(breaks=fisher.df()$x) +   #labels=c(1,2,3,4,5,6,7,8)
        scale_color_manual(name="Table Probability of Tables that are:  ", labels=c("TRUE" = "Extreme    ", "FALSE" = "Not Extreme"), values=c("TRUE" = "red", "FALSE" = "orange"), drop = FALSE) +
        #coord_cartesian(ylim=c(0,26),xlim=c(-0.5,24.5)) + #to start histogram at 0
        labs(title="Distribution of Test Statistic (First Cell)", y="Table Probability", x="Potential Values for the First Cell") +
        #ggtitle(bquote(atop(.(plot.maintitle), atop(.(plot.subtitle))))) + 
        #theme_hist_skew_right
        theme(text = element_text(size=16), 
              plot.title = element_text(colour = "black"),
              axis.title.y=element_text(vjust=1),
              legend.position = "top",
              legend.justification = "left", 
              #panel.background = element_rect(fill=gray(0.95)),
              panel.grid.major = element_line(color=gray(0.9))
              ) +
        guides(color=guide_legend(ncol=2))        
    return(fisher.plot)
# ggsave(file="C:\\Users\\Local PC Account\\Dropbox\\Agresti drafts\\Chapter 11 Updated Documents\\Artwork\\Figure_Fisher.png", plot=fisher.plot, dpi=500)
  })

  output$save <- downloadHandler(
    filename = paste('Fisher_Exact.png'),
    content = function(file) {
      png(file, height=450, width=700)
      print(last_plot())
      dev.off()
    },
    contentType = "image/png"
  )


})