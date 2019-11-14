library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(XML)
library(RCurl)
library(knitr)
library(kableExtra)
library(anytime)
library(shinyaudio)
library(shinycssloaders)
library(shinythemes)
library(svglite)
library(lattice)
library(ggplot2)
library(rvest)
library(reshape2)
library(dplyr)
library(htmlwidgets)
library(slickR)
library(heatmaply)
library(purrr)
library(tokenizers)
library(bubbles)
library(shinyBS)
library(stringr)
library(shinyjqui)
library(shinyWidgets)

options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.size = 1
)

#### Pull data from PostgreSQL database
library(RPostgres)

# Connect to my Heroku PostgreSQL database
Sys.getenv('.Renviron')

con <- dbConnect(
  Postgres(),
  dbname = "d2sjdih8tegcuc",
  host = "ec2-184-72-238-22.compute-1.amazonaws.com",
  port = '5432',
  user = "xkktypszvisogc",
  password = "35b7eb67cab6ea1750e5877d61c3415851d6f1ca9c5782ec0b6bf91ede8acbea",
  sslmode = 'require'
)

uniques <- dbGetQuery(con, "SELECT * FROM podcasts")

uniques <-
  uniques[order(as.Date(uniques$ReleaseDate), decreasing = TRUE),]


## Snippets
iframe_raindrop <- HTML('<iframe src="https://raindrop.io/collection/8841871" 
                    style="border:0px #ffffff none;" name="myiFrame" scrolling="no" 
                    frameborder="1" marginheight="0px" marginwidth="0px" height="910px" 
                    width="100%" allowfullscreen></iframe>')

about_msg <- HTML('<p style="font-size:16px; margin: 0px 15px 5px 15px;">
                  When faced with a minor inconvenience, namely too many data science podcasts to keep up with, the answer is always "can I make an app
                  for that?". And so this little dashboard is born! 
                  
                  <br><br>
                  
                  Here you will find a live database of all the podcasts on the topics of data science, artificial intelligence, and big data
                  that the iTunes store has to offer. You can browse and listen to new finds right here in the app! Or, you are feeling lucky, let us surprise you with some randomly selected podcasts on your topic of 
                  interest that you may not come across otherwise.
                  
                  <br><br>
                  
                  I hope that you can find something here to enrich your listening repertoire!
                  
                  </p>')

ir_projects <- HTML('<p style="font-size:20px; margin: 0px 15px 5px 15px;">

                    <br><br>
                    
                  Along with Mihai Chelaru, I am the co-curator of <a href="https://www.intelligencerefinery.io"><b>Intelligence Refinery</b></a>, 
                  a knowledge repository for all things data science and software development. 
                  
                  <br><br>
                  
                  Our mission at Intelligence Refinery is to assemble resources and tools that make it easier to keep up with this fast moving
                   field, as it is certainly not easy to maintain and update your professional skill set along side a busy schedule and many competing priorities. 
                  
                  <br><br>
                  
                   Therefore, in addition to this podcast explorer, I have created a variety of microlearning series for commonly used 
                   data science methods, all in accessible bite-sized chunks. You can find these and other projects at Intelligence Refinery 
                   on the left.
                  
                  <br><br>
                  
                  <center><a href="https://www.intelligencerefinery.io"><img src="full_logo.png" style="height:120px;"></a></center>
                  <br>
                  </p>')

footer_msg <- HTML('<div style="font-size:18px; margin: 0px 15px 5px 15px;"><center>I would love to hear any of your comments or suggestions! <i class="fas fa-smile"></i></center>
                  <center><a href="mailto:nancy.chelaru@gmail.com"><i class="fas fa-envelope" style="padding:10px;"></i></a>
                  <a href="https://www.intelligencerefinery.io/contact/"><i class="fas fa-globe" style="padding:10px;"></i></i></a>
                  <a href="https://github.com/nchelaru?tab=repositories"><i class="fab fa-github-alt" style="padding:10px;"></i></a>
                  <a href="https://twitter.com/n_chelaru"><i class="fab fa-twitter" style="padding:10px;"></i></a></center>

                  </div>')
                  

#### Define layout
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Welcome!",
    icon = icon("flag"),
    tabName = "welcome"
  ),
  br(),
  menuItem(
    "Look around",
    icon = icon("microphone-alt"),
    tabName = "browse"
  ),
  br(),
  menuItem(
    "Surprise Me!",
    tabName = "random",
    icon = icon("surprise")
  ),
  br(),
  menuItem(
    "About this app",
    icon = icon("question"),
    tabName = "about"
  )
))



body <- dashboardBody(tags$head(tags$style(
  HTML(
    ".main-sidebar { font-size: 18px; }
   #desc { overflow-y:scroll; max-height: 230px;}
   #random_desc { overflow-y:scroll; max-height: 230px;}
   #heatmap_item {height:600px;}
   #img {width: inherit;
         display: flex;
         vertical-align: middle;
          align-items: center;}
   .box-warning {vertical-align: middle;}
   .timeline-item {max-height: 800px;} 
    "
  )
)),
 tabItems(
  tabItem(tabName = "welcome",
          fluidPage(
            br(),
            column(5,
                   img(src='lp.png', height='950px', style='padding-left:30px;')),
            column(7, 
                   box(
                     title = HTML("<p style='font-size:24px; padding-top: 10px'>Welcome to the data science podcast explorer!</p>"),
                     solidHeader = TRUE,
                     status = "warning",
                     width = 12,
                     userPost(
                       id = 1, collapsible = FALSE,
                       src =  "http://icons.iconarchive.com/icons/google/noto-emoji-activities/256/52707-party-popper-icon.png",
                       author = h3("It's alive!"),
                       description = "September 19, 2019",
                       about_msg
                       
                     ),
                     userPost(
                       id = 2, collapsible = FALSE,
                       src = "https://i.ibb.co/qkKGVBX/logo-clean.png",
                       author = h4('Add new links to "About" page [Update]'),
                       description = "November 13, 2019",
                       HTML('<p style="font-size:16px; margin: 0px 15px 5px 15px;">
                            Add contact links and bookmark collection of Intelligence Refinery projects to the "About" page.</p>')
                   )
                   ),
                   br(),
                   box(
                     solidHeader = TRUE,
                     title = HTML("<p style='font-size:22px; padding-top: 10px'>Roadmap</p>"),
                     status = "info",
                     width = 12,
                     todoList(
                       todoListItem(
                         label = 'Return "No matching podcasts" instead of error message in random recommendations page.',
                         dashboardLabel("Bug", status = "danger")
                       ),
                       todoListItem(
                         label = "Create responsive layout",
                         dashboardLabel("Improvement", status = "success")
                       ),
                       todoListItem(
                         label = "Add recommendation system based on favourite podcasts",
                         dashboardLabel("New feature", status = "info")
                       )
                     )
                   )
                   ))),
  tabItem(tabName = "random",
          fluidPage(
            br(),
            fluidRow(
              column(3, gradientBox(
                title = "Interested in a particular topic?",
                width = 12,
                gradientColor = "purple",
                boxToolSize = "xs",
                closable = FALSE,
                collapsible = FALSE,
                footer = selectInput(
                  "select_tag",
                  label=NULL,
                  choices = c("Choose a tag" = "", sort(unique(unlist(
                    strsplit(uniques$Tags, ", ")
                  )))),
                  selected = " ",
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
                )
              ), 
              gradientBox(
                title = "Or, got a keyword?",
                width = 12,
                gradientColor = "teal",
                boxToolSize = "xs",
                closable = FALSE,
                collapsible = FALSE,
                footer = textInput(
                  "search_box",
                  label = NULL,
                  width = NULL,
                  placeholder = "Search"
                )), style='padding-top:1%;'),
              box(
                height = '330px',
                title = "You might like...",
                solidHeader = TRUE,
                width = 4,
                status = "primary",
                withSpinner(slickROutput(
                  "slick", width = '100%', height = '250px'
                ), type = 2),
                style = 'padding-top:20px'
              ),
              box(
                title = htmlOutput('random_title'),
                h4(
                  withSpinner(htmlOutput('random_desc', style = 'padding-left: 2%; padding-right: 2%;'), type=2)
                ),
                width = 5,
                solidHeader = TRUE,
                height = '330px',
                status = "warning"
              )
            ),
            fluidRow(
              box(
                title = "Most recent episodes",
                DT::dataTableOutput('random_ep', width = '100%'),
                width = 12,
                solidHeader = TRUE,
                status = 'danger'
              )
            )
          )),
  
  tabItem(tabName = "browse",
          fluidPage(
            br(),
            fluidRow(column(
              5,
              box(
                title = NULL,
                width = 12,
                accordion(
                  accordionItem(
                    id = 1,
                    title = htmlOutput('title'),
                    color = "info",
                    collapsed = FALSE,
                    withSpinner(htmlOutput('img', class = "pod-img", style='padding-left: 10%; padding-right:10%;'), type = 2),
                    bsTooltip(
                      "img",
                      "Go to podcast page in the iTunes Store",
                      placement = "bottom",
                      trigger = "hover",
                      options = NULL
                    ),
                    hr(),
                    h4(htmlOutput('desc', style = "padding:10px;"))
                    
                  ),
                  accordionItem(
                    id = 2,
                    title = "Listen to most recent episodes",
                    color = "warning",
                    collapsed = TRUE,
                    withSpinner(
                      DT::dataTableOutput('recent',  width = "100%", height = '100%'),
                      type = 2
                    )
                  )
                )
              )
            ),
            column(
              7,
              box(
                title = "Podcast listing",
                width = 12,
                solidHeader = TRUE,
                status = "danger",
                DT::dataTableOutput('tb')
              )
            ))
          )),
  tabItem(tabName = "about",
          br(),
          fluidRow(
            column(width=8,
                   iframe_raindrop),
            column(width=4,
                   widgetUserBox(
                     title = "Nancy Chelaru",
                     subtitle = "Creator",
                     type = NULL,
                     width = 12,
                     height=910,
                     src = "https://octodex.github.com/images/andycat.jpg",
                     background = TRUE,
                     backgroundUrl = "https://images.unsplash.com/photo-1560507041-ea7882a63ca1?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=634&q=80",
                     closable = FALSE,
                     collapsible = FALSE,
                     ir_projects,
                     footer = footer_msg
                   )
                   )
            
          ))
))




# We'll save it in a variable `ui` so that we can preview it in the console


ui <- dashboardPagePlus(title = 'Data science podcast round-up',
  dashboardHeader(title = "DS podcast round-up"),
  sidebar,
  body,
  skin = "green",
  collapse_sidebar = TRUE,
  HTML('<meta name="viewport" content="width=1024">')
)

# Define server logic
server <- function(input, output) {

  
  # ## Create heatmap
  # heatmap_plot <- function(df = uniques) {
  #   x <- data.frame(table(unlist(strsplit(
  #     uniques$Tags, ", "
  #   ))))
  #   
  #   df_list <- list()
  #   
  #   for (tag in x$Var1) {
  #     filtered_df <- dplyr::filter(df, grepl(tag, Tags))
  #     
  #     filtered_count <-
  #       data.frame(table(unlist(strsplit(
  #         filtered_df$Tags, ", "
  #       ))))
  #     
  #     
  #     filtered_count$Freq[is.na(filtered_count$Freq)] <- 0
  #     
  #     names(filtered_count)[2] <- tag
  #     
  #     df_list[[tag]] <- filtered_count
  #     
  #   }
  #   
  #   z <- df_list %>% reduce(full_join, by = "Var1")
  #   
  #   rownames(z) <- z$Var1
  #   
  #   z$Var1 <- NULL
  #   
  #   z[is.na(z)] <- 0
  #   
  #   z$Podcasts <- NULL
  #   z$Technology <- NULL
  #   
  #   z <- z[!rownames(z) %in% c('Podcasts', 'Technology'), ]
  #   
  #   return(heatmaply(
  #     scale(z),
  #     k_row = 3,
  #     k_col = 3,
  #     margins = c(0, 0, 30, 0)
  #   ))
  # }
  # 
  # output$heatmap <-
  #   renderPlotly(heatmap_plot() %>% layout(height = '550'))
  
  ## Parse most recent 3 episodes
  
  recent_ep <- function(url) {
    rssdoc <- xmlParse(getURL(url))
    
    titles <-
      xpathSApply(rssdoc, '//item/title', xmlValue)[1:10]
    dates <-
      xpathSApply(rssdoc, '//item/pubDate', xmlValue)[1:10]
    ep_link <-
      xpathSApply(rssdoc, "//item/link", xmlValue)[1:10]
    audio_link <-
      xpathSApply(rssdoc, "//item/enclosure[@url]", xmlGetAttr, "url")[1:10]
    
    titles <-
      sprintf("<a href='%s' target='_blank'>%s</a>", ep_link, titles)
    ep_player <-
      sprintf("<audio  src=%s  type =  'audio/mp3'  controls ></audio>",
              audio_link)
    
    df <- as.data.frame(cbind(titles, dates, ep_player))
    
    df$dates <- anydate(as.character(df$dates))
    
    colnames(df) = c('Title', 'Released', 'Player')
    
    return(df)
    
  }
  
  get_recent <- function(input, df = uniques) {
    if (length(input)) {
      Product <- reactive({
        uniques
      })
      data <- Product()[input, ]
      url <- data[, 'NewFeedURL']
      
      df <- recent_ep(url)
    } else {
      url <- uniques[1, ]$NewFeedURL
      
      df <- recent_ep(url)
    }
  }
  
  
  Product <- reactive({
    uniques
  })
  
  # Get podcast listing
  output$tb = DT::renderDataTable(
    uniques[, c('Title',
                'Creator',
                'Tags',
                'AvgRating',
                'NumRatings')],
    colnames = c(
      'Title',
      'Creator',
      'Tags',
      'Rating',
      'No. ratings'
    ),
    server = TRUE,
    selection = 'single',
    filter = 'none',
    options = list(
      searching = TRUE,
      pageLength = 15,
      scrollX = TRUE
    ),
    rownames = FALSE
  )
  
  
  # display podcast description
  output$desc = renderText({
    s = input$tb_rows_selected
    if (length(s)) {
      data <- Product()[s, ]
      desc <- data[, 'Description']
      if (length(tokenize_sentences(desc)[[1]]) <= 4) {
        desc
      } else {
        tokenize_sentences(desc)[[1]][1:3]
      }
    } else {
      data <- Product()[1, ]
      desc <- data[, 'Description']
      if (length(tokenize_sentences(desc)[[1]]) <= 4) {
        desc
      } else {
        tokenize_sentences(desc)[[1]][1:3]
      }
    }
    
  })
  
  
  # Get cover image link
  output$title = renderText({
    s = input$tb_rows_selected
    if (length(s)) {
      data <- Product()[s, ]
      url <- data$Title
    } else {
      url <- uniques[1, ]$Title
    }
  })
  
  # Show cover image
  output$img = renderText({
    s = input$tb_rows_selected
    if (length(s)) {
      data <- Product()[s, ]
      url <- data[, 3]
      link <- data[, 4]
      c(
        '<center><a href="',
        link,
        '"><img src="',
        url ,
        '" align="middle" width="70%" height="70%"></a></center>'
      )
    } else {
      url <- uniques[1, ]$ArtworkURL
      link <- uniques[1, ]$URL
      c(
        '<center><a href="',
        link,
        '"><img src="',
        url ,
        '" align="middle" width="70%" height="70%"></a></center>'
      )
    }
    
  })
  
  # Get more recent episodes
  output$recent = DT::renderDataTable(
    get_recent(input$tb_rows_selected),
    server = TRUE,
    escape = FALSE,
    options = list(
      dom = 't',
      autoWidth = TRUE,
      selection = 'single',
      scrollX = TRUE,
      scrollY = '550px',
      columnDefs = list(list(width = '30%', targets = 2))
    ),
    rownames = FALSE
  )
  
  
  # Carousel
  
  selected_tag <- reactive({
    input$select_tag
  })
  
  search_word <- reactive({
    input$search_box
  })
  
  rand_num <- sample.int(300, 1)
  
  get_filtered_df <- function (input, n = rand_num) {
    if (selected_tag()  == " " & search_word() == '') {    ## Both null
      set.seed(n)
      sampled_df <- sample_n(uniques, 3)
      return(sampled_df)
    } else if (!selected_tag() == " " & search_word() == '') {                    ## Only tag
      filtered_df <- dplyr::filter(uniques, grepl(selected_tag(), Tags))
      if (dim(filtered_df)[1] == 0) {
        return(data.frame())
      } else if (dim(filtered_df)[1] < 3) {
        return(filtered_df)
      } else {
        set.seed(n)
        sampled_df <- sample_n(filtered_df, 3)
        return(sampled_df)
      }
    } else if (selected_tag() == " " & !search_word() == '') {                       ## Only search term
      filtered_df <- uniques %>%
        select(Title, Description, Tags, ArtworkURL, NewFeedURL) %>%
        filter_all(any_vars(str_detect(., fixed(pattern = search_word(), ignore_case=TRUE))))
      
      if (dim(filtered_df)[1] == 0) {
        return(data.frame())
      } else if (dim(filtered_df)[1] < 3) {
        return(filtered_df)
      } else {
        set.seed(n)
        sampled_df <- sample_n(filtered_df, 3)
        return(sampled_df)
      }
    } else {                                                                         ## Both
      filtered_df_int <- uniques %>%
        select(Title, Description, Tags, ArtworkURL, NewFeedURL) %>%
        filter_all(any_vars(str_detect(., fixed(pattern = search_word(), ignore_case=TRUE))))
      
      filtered_df <- dplyr::filter(filtered_df_int, grepl(selected_tag(), Tags))
      
      if (dim(filtered_df)[1] == 0) {
        return(data.frame())
      } else if (dim(filtered_df)[1] < 3) {
        return(filtered_df)
      } else {
        set.seed(n)
        sampled_df <- sample_n(filtered_df, 3)
        return(sampled_df)
      }
    }
    
  }
  
  #output$selected_tag <- renderText({selected_tag()})
  
  
  
  output$slick <- renderSlickR({
    slickR(
      obj = get_filtered_df()$ArtworkURL,
      height = 500,
      width = 500,
      slideId = 'myId',
      slickOpts = list(
        list(
          slidesToShow = 1,
          arrows = T,
          fade = T,
          focusOnSelect = T,
          centerMode = T
        )
      )
    )
  })
  
  network <- shiny::reactiveValues()
  
  
  shiny::observeEvent(input$slick_current, {
    clicked_slide <- input$slick_current$.clicked
    relative_clicked <- input$slick_current$.relative_clicked
    center_slide <- input$slick_current$.center
    total_slide <- input$slick_current$.total
    active_slide <- input$slick_current$.slide
    
    
    network$clicked_slide <- clicked_slide
    network$center_slide <- center_slide
    network$relative_clicked <- relative_clicked
    network$total_slide <- total_slide
    network$active_slide <- active_slide
    
  })
  
  
  output$random_title <- renderText({
    s = input$slick_current
    if (length(s)) {
      l <- shiny::reactiveValuesToList(network)
      print(unlist(l))
      idx <- as.numeric(unlist(l)[3])
      title <- get_filtered_df()[idx, ][, 1]
    } else {
      title <- get_filtered_df()[1, ][, 1]
    }
    
  })
  
  output$random_desc <- renderText({
    s = input$slick_current
    if (length(s)) {
      l <- shiny::reactiveValuesToList(network)
      idx <- as.numeric(unlist(l)[3])
      desc <- get_filtered_df()[idx, ]$Description
      # if (length(tokenize_sentences(desc)[[1]]) <= 5) {
      #   desc
      # } else {
      #   tokenize_sentences(desc)[[1]][1:4]
      # }
    } else {
      desc <- get_filtered_df()[1, ]$Description
      # if (length(tokenize_sentences(desc)[[1]]) <= 5) {
      #   desc
      # } else {
      #   tokenize_sentences(desc)[[1]][1:4]
      # }
      
    }
    
  })
  
  
  
  get_random_recent <- function(input) {
    if (length(input)) {
      l <- shiny::reactiveValuesToList(network)
      idx <- as.numeric(unlist(l)[3])
      url <- get_filtered_df()[idx, ]$NewFeedURL
      
      df <- recent_ep(url)
      
    } else {
      url <- get_filtered_df()[1, ]$NewFeedURL
      
      df <- recent_ep(url)
      
    }
    
    
  }
  
  output$random_ep = DT::renderDataTable(
    get_random_recent(input$slick_current),
    escape = FALSE,
    server = TRUE,
    options = list(
      dom = 't',
      autoWidth = FALSE,
      selection = 'single',
      scrollX = TRUE,
      scrollY = '430px'
    )
  )
  
  get_tags <- function(uniques) {
    label <-
      data.frame(table(unlist(strsplit(
        uniques$Tags, ", "
      ))))$Var1
  }
  
  get_tag_counts <- function(uniques) {
    counts <-
      sqrt(c(data.frame(table(
        unlist(strsplit(uniques$Tags, ", "))
      ))$Freq))
  }
  
  
}


# Preview the UI in the console
shinyApp(ui = ui, server = server)