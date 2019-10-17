library(igraph); library(TDAmapper); library(shiny); library(plotly); library(shinythemes); library(shinycssloaders)
library(GGally); library(ggplot2); library(grnn); library(ggraph); library(TDAstats) #library(rgl)
library(shinyWidgets); library(cccd); library(Matrix); library(dplyr)
#### funções ----
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

distancia = function(X, M){  #retorna vetor com o quadrado da distância euclidiana de X=(x,y) ao conjunto M
  A = sqrt(colSums((t(M)-X)^2))
  return(A)
}

farthestpoints = function(M, p){  ##essa bosta dá algum pau se o input não é matrix!
  M = as.matrix(M)
  id = array(, dim = p)
  amostra = nrow(M)
  id[1] = sample(1:amostra, 1) #; FPS[1,] = M[id[1],] #escolhe ponto aleatório de M
  B = distancia(M[id[1],], M) #distância de P1 a todos os pontos de M
  id[2] = which.max(B) #; FPS[2,] = M[id[2],]
  C = distancia(M[id[2],], M)
  D = pmin(B,C)
  id[3] = which.max(D) #; FPS[3,] = M[id[3],]
  
  for (i in 4:p) {
    C = distancia(M[id[i - 1],], M)
    D = pmin(C, D)
    id[i] = which.max(D) #; FPS[i,] = M[id[i],]
  }
  return(id)
}

excentricity_filter = function(M, p){
  if (p != 0){
    apply(X = M, MARGIN = 1, FUN = function(x) (sum(x^p)/nrow(M))^(1/p))
  } else {
    apply(X = M, MARGIN = 1, FUN = max)
  }
  
}

density_filter = function(M){
  apply(X = M, MARGIN = 1, FUN = function(x) sum(exp(-x^2))/ncol(M))  
}

dtm_filter = function(M, k){ #distance to measure filter
  apply(X = M, MARGIN = 1, FUN = function(x) sqrt(sum((x[order(x)[1:k]])^2)/k))
}

# cosine_auxiliar = function(V, M){
#   apply(X = M, MARGIN = 1, FUN = function(x) sum(x*V)/(norm(x, type = "2")*norm(V, type = "2")))
# }
# 
# cosine_distance = function(M){
#   apply(X = M, MARGIN = 1, FUN = function(x) cosine_auxiliar(x, M))
# }

cosine_distance = function(M){
 n = nrow(M)
 D = matrix(0, nrow = n, ncol = n)
 for (i in 1:n){
   for (j in i:n){
     a = M[i, ]
     b = M[j, ]
     D[i,j] = sum(a*b)/(norm(a, type = "2")*(norm(b, type = "2")))
     D[j, i] = D[i, j]
   }
 }
 return(D)
}

sphere_sample = function(n){
  phi = runif(n, min = 0, max = pi)
  theta = runif(n, min = 0, max = 2*pi)
  return(data.frame(x = sin(phi)*cos(theta), y = sin(phi)*sin(theta), z = cos(phi)))
}

torus_sample = function(n){
  u = runif(n, min = 0, max = 2*pi)
  v = runif(n, min = 0, max = 2*pi)
  return(data.frame(x = (2 + cos(v))*cos(u), y = (2 + cos(v))*sin(u), z = sin(v)))
}

jet.colors <- #nice color pallete
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

coloring = function(f){ #gives the coloring of a filter
  jet.colors(100)[cut(f, breaks = 100, include.lowest = TRUE, labels = FALSE)]
}

subcoloring = function(f, V){
  n = length(V)
  c = cut(c(V, min(f), max(f)), breaks = 100, include.lowest = TRUE, labels = FALSE)
  return(jet.colors(100)[c[1:n]])
}

majority = function(x) { # function to get the majority (or ties) in the character filtering
  h = as.data.frame(table(x))
  id = which(h$Freq == max(h$Freq))
  v.mean = h$x[id]
  return(paste(sort(v.mean), collapse = "/"))
}

#palf <- colorRampPalette(c("blue",  "green", "yellow", "orange", "red"))

#### ui ----
#pdf(NULL)

ui = navbarPage(title = "Mapper", theme = shinytheme("spacelab"),
                tabPanel("Insert data",  #### insert data ----
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             selectInput(inputId = "customorexamples", label = "Custom data or examples?", 
                                         choices = c(Custom = "custom", Examples = "examples"), selected = "custom"),
                             conditionalPanel(condition = "input.customorexamples == 'custom'",
                                              tagList(
                                                fileInput("file1", "Choose CSV File", multiple = FALSE, 
                                                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                                tags$hr(),
                                                checkboxInput("header", "Header", TRUE),
                                                radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = ""),
                                                             selected = ",", inline = TRUE),
                                                radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                                                             selected = '"', inline = TRUE),
                                                radioButtons("dec", "Decimal separator", choices = c("Comma" = ",", "Dot" = "."),
                                                             selected = ".", inline = TRUE)
                                              )
                             ),
                             conditionalPanel(condition = "input.customorexamples == 'examples'",
                                              tagList(
                                                selectInput("example", "Choose example", selected = "flamingo",
                                                            choices = c(Cancer = "cancer", Diabetes = "diabetes", Elephant = "elephant", Flamingo = "flamingo", Head = "head", Sphere = "sphere",
                                                                         Torus = "torus")
                                                ),
                                                conditionalPanel(condition = "input.example == 'torus' | input.example == 'sphere'",
                                                                 numericInput("pointstorus", "Number of points", value = 1000, min = 20, max = 5000)
                                                )
                                              )
                             ),
                             conditionalPanel(condition =  "output.nrows >= 2001",
                                              tagList(
                                                checkboxInput("fps_check", "Select fewer points?", value = TRUE),
                                                conditionalPanel(condition = "input.fps_check",
                                                                 numericInput("fps_points", "Number of points", value = 2000, min = 1, max = 20000)
                                                )
                                              )
                             ),
                             tags$hr(),
                             radioButtons("disp", "Display", choices = c(Head = "head", All = "all"),
                                          selected = "head", inline = TRUE),
                             radioButtons("normalize", "Normalize", choices = c(none = "none", uniform = "uniform", zscore = "zscore"),
                                          selected = "none", inline = TRUE),
                             checkboxInput("exclude", "Exclude columns?", value = FALSE),
                             conditionalPanel(condition = "input.exclude",
                                              uiOutput("cols_for_exclusion_ui")
                             )
                           ),
                           mainPanel(h2("Data input"),
                                     plotlyOutput("plot_data") %>% withSpinner(),
                                     verbatimTextOutput("nrows"),
                                     verbatimTextOutput("summary_contents"),
                                     tableOutput("contents")
                           )
                         )
                ),
                tabPanel("mapper", #### mapper ----
                         sidebarLayout(
                           sidebarPanel(width = 2,
                                        sliderInput("overlap", "% overlap", 50, min = 1, max = 100),
                                        numericInput("num_intervals", "Number of intervals", 10, min = 1, max = 500),
                                        numericInput("bins", "Bins when clustering", 12, min = 1, max = 1000),
                                        selectInput("mapper_filter", "Filter funtion:",
                                                    c("Singular Value Decomposition" = "svd",
                                                      "Data Column" = "column", "Excentricity" = "excentricity",
                                                      "Distance to measure" = "dtm",
                                                      "Density" = "density"), selected = "svd"), 
                                        conditionalPanel(
                                          condition = "input.mapper_filter == 'svd' | input.mapper_filter == 'column' | input.mapper_filter == 'excentricity' | input.mapper_filter == 'dtm'", 
                                          uiOutput("mapper_filter_column_ui")  
                                        ),
                                        selectInput("distance", "Distance:",
                                                    c("Euclidean" = "euclidean", "Geodesic" = "geodesic", "Maximum" = "maximum", 
                                                      "Manhattan" = "manhattan", "Canberra" = "canberra", "Cosine" = "cosine"),
                                                    selected = "euclidean")
                           ),
                           mainPanel("Plot mapper", 
                                     plotOutput("plot_mapper") %>% withSpinner(), #click = "mapper_click", hover = "mapper_hover")
                                     # verbatimTextOutput("info"),
                                     # verbatimTextOutput("info2"),
                                     # verbatimTextOutput("ele"),
                                     plotlyOutput("plot_data_colored_as_mapper") %>% withSpinner(),
                                     plotOutput("ph") %>% withSpinner(),
                                     verbatimTextOutput("hover"),
                                     verbatimTextOutput("click")
                           )
                         )
                ), 
                tabPanel("coloring", #### coloring ----
                         sidebarLayout(
                           sidebarPanel(width = 2,
                                        selectInput("ffc", "Filter funtion:",
                                                    c("Singular Value Decomposition" = "svd",
                                                      "Data Column" = "column", "Excentricity" = "excentricity",
                                                      "Density" = "density"), selected = "svd"), 
                                        conditionalPanel(
                                          condition = "input.ffc == 'svd' | input.ffc == 'column' | input.ffc == 'excentricity'",
                                          uiOutput("ffc_column_ui")  
                                        )
                           ),
                           mainPanel("Coloring mapper", 
                                     plotOutput("plot_mapper_colored_as_filter") %>% withSpinner(),
                                     plotlyOutput("plot_data_colored_as_filter") %>% withSpinner(),
                                     plotOutput("plot_ggpairs", width = "auto") %>% withSpinner()
                                     #                                     verbatimTextOutput("hover"),
                                     #                                     verbatimTextOutput("click")
                           )
                         )
                )
)

server <- function(input, output, session) {
  
  #### INPUTS! ----
  
  Input_df_crude <- reactive({ #dataframe that changes when any option the first panel changes
    if (input$customorexamples == "custom") {
      req(input$file1)
      df_crude <- as.data.frame(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote, dec = input$dec))
    } else {
      switch(input$example,
             cancer = {df_crude = as.data.frame(read.csv(file = "examples/cancer.csv", header = TRUE, sep = ","))},
             diabetes = {df_crude = as.data.frame(read.csv(file = "examples/diabetes.csv", header = TRUE, sep = ","))},
             elephant = {df_crude = as.data.frame(read.csv(file = "examples/elephant-reference.csv", header = FALSE, sep = ","))},
             flamingo = {df_crude = as.data.frame(read.csv(file = "examples/flam-reference.csv", header = FALSE, sep = ","))},
             head = {df_crude = as.data.frame(read.csv(file = "examples/head-reference.csv", header = FALSE, sep = ","))},
             sphere = {df_crude = sphere_sample(input$pointstorus)},
             torus = {df_crude = torus_sample(input$pointstorus)}
      )
    }
    if (is.null(colnames(df_crude))) colnames(df_crude) = 1:ncol(df_crude)
    return(list(df_crude, nrow(df_crude), ncol(df_crude))) 
  })
  
  Input_df <- reactive({
    if (input$customorexamples == "custom") req(input$file1)
    df_temp <- Input_df_crude()[[1]]
    n = Input_df_crude()[[2]]
    
    if (input$exclude & (!is.null(input$cols_to_exclude))) {
      columns = which(colnames(df_temp) %in% input$cols_to_exclude)
      df_temp = df_temp[, -columns]
    }
    
    switch(input$normalize,
           none = {df <- df_temp},
           uniform = {df <- sapply(df_temp, normalize)},
           zscore = {df <- scale(df_temp)}
    )
    
    if (input$fps_check == TRUE & (input$fps_points < Input_df_crude()[[2]])){
      id = farthestpoints(M = df, p = input$fps_points)
      df = df[id,]
    }
    return(df)
  })
  
  #### rgn graph ----
  
  Input_rnggraph = reactive({
    if (input$customorexamples == "custom") req(input$file1)
    df = Input_df()
    if (input$distance == "geodesic"){
      grafo = rng(x = df, k = 6)
      return(grafo)}
  })
  
  #### distance ----
  Input_distance = reactive({
    if (input$customorexamples == "custom") req(input$file1)
    df = Input_df()
    if (input$distance == "geodesic") {
      grafo = Input_rnggraph()
      distance_matrix = distances(grafo)
    } else if (input$distance == "cosine") {
      distance_matrix = cosine_distance(df)
    } else {
      distance_matrix = as.matrix(dist(df, method = input$distance, diag = TRUE, upper = TRUE))
    }
    return(distance_matrix)
  })
  
  #### filters ----
  
  Input_mapper_filter = reactive({
    df = Input_df()
    i = input$mapper_filter_column
    distance_matrix = Input_distance()
    if (input$mapper_filter == "svd"){
      mapper_filter = (svd(df)$u)[, i]
    }
    if (input$mapper_filter == "column"){
      mapper_filter = df[, i]
    }
    if (input$mapper_filter == "excentricity"){
      if (input$excentricity_exponent_infinity == TRUE) p = 0 else p = input$excentricity_exponent
      mapper_filter = excentricity_filter(distance_matrix, p)
    }
    if (input$mapper_filter == "density"){
      mapper_filter = density_filter(distance_matrix)
    }
    if (input$mapper_filter == "dtm"){
      mapper_filter = dtm_filter(distance_matrix, input$dtm_k)
    }
    return(mapper_filter)
  }) 
  
  Input_ffc = reactive({
    df_crude = Input_df_crude()[[1]]
    df = Input_df()
    i = input$ffc_column
    distance_matrix = Input_distance()
    
    if (input$ffc == "svd"){
      ffc = (svd(df)$u)[, i]
    }
    if (input$ffc == "column"){
      ffc = df_crude[, i]
    }
    if (input$ffc == "excentricity"){
      if (input$ffc_excentricity_exponent_infinity == TRUE) p = 0 else p = input$ffc_excentricity_exponent
      ffc = excentricity_filter(distance_matrix, p)
    }
    if (input$ffc == "density"){
      ffc = density_filter(distance_matrix)
    }
    return(ffc)
  }) 
  
  #### mapper ----
  
  Input_mapper = reactive({
    df = Input_df()
    mapper_filter = Input_mapper_filter()
    num_intervals <- input$num_intervals
    distance_matrix = Input_distance()
    corte = cut(mapper_filter, breaks = num_intervals, labels = F)
    
    m1 <- mapper1D(
      distance_matrix = distance_matrix,
      filter_values = mapper_filter,
      num_intervals = num_intervals,
      percent_overlap = input$overlap,
      num_bins_when_clustering = input$bins)
    
    mapper_graph <- graph.adjacency(m1$adjacency, mode = "undirected")
    n <- m1$num_vertices
    v.mean <- rep(0, n)
    v.size <- rep(1, n)
    for (i in 1:n) {
      v.mean[i] <- mean((mapper_filter[m1$points_in_vertex[[i]]]))
      v.size[i] <- length((m1$points_in_vertex[[i]]))
    }
    #sum(v.size)
    V(mapper_graph)
    V(mapper_graph)$color <- subcoloring(mapper_filter, v.mean)
    if (max(v.size) == min(v.size))  {V(mapper_graph)$size <- rep(10, times = n) 
    } else {
      V(mapper_graph)$size <- ((v.size-min(v.size))/(max(v.size) - min(v.size))*12) + 10
    } 
    V(mapper_graph)$label <- v.size
    l <- as.data.frame(layout_with_fr(mapper_graph))
    colnames(l) = c("x", "y")
    return(list(mapper_graph = mapper_graph, num_vertices = n, vertex_size = v.size, layout = l,
                points_in_vertex = m1$points_in_vertex, vertex_mean = v.mean))
  })
  
  #### *CLICKS! ----
  selected_points <- reactiveValues(pts = NULL)
  hover_points <- reactiveValues(pts = NULL)
  
  observeEvent(input$mapper_click, {
    x <- nearPoints(df = l, input$mapper_click)
    selected_points$pts <- x
  })
  
  observeEvent(input$mapper_hover, {
    x <- nearPoints(df = l, input$mapper_hover)
    hover_points$pts <- x
  })
  
  output$ele <- renderPrint({
    (Input_mapper())$layout
  })
  
  output$info <- renderPrint({
    selected_points$pts
  })
  
  output$info2 <- renderPrint({
    hover_points$pts
  })
  
  #### OUTPUTS! ----
  
  output$nrows <- reactive({
    return(Input_df_crude()[[2]])
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  output$contents <- renderTable(digits = 5, rownames = TRUE, {
    if (input$customorexamples == "custom") req(input$file1)
    df = Input_df()
    if(input$disp == "head") {return(head(df))} else {return(df)}
  })
  
  output$summary_contents <- renderPrint({
    if (input$customorexamples == "custom")  req(input$file1)
    summary(Input_df())
  })
  
  #### UIS! ----
  
  output$cols_for_exclusion_ui = renderUI({
    n = Input_df_crude()[[3]]
    selectInput("cols_to_exclude", "Columns to exclude", choices = colnames((Input_df_crude())[[1]]), multiple = TRUE)
  }) 
  
  output$mapper_filter_column_ui = renderUI({
    require(input$file1)
    n = ncol(Input_df())
    df = Input_df()
    if (input$mapper_filter == "svd") {
      numericInput("mapper_filter_column", "Choose column", value = 1, min = 1, max = n)
    } 
    else if (input$mapper_filter == "column") {
      selectInput("mapper_filter_column", "Choose column", choices = colnames(df), selected = colnames(df)[1], multiple = FALSE)
    } else if (input$mapper_filter == "dtm"){
      numericInput("dtm_k", "Choose k", value = 2, min = 1, max = nrow(df))
    } else {
      tagList(
        numericInput("excentricity_exponent", "Exponent", value = 2, min = 1, step = 0.1),
        checkboxInput("excentricity_exponent_infinity", "Infinity?")
      )
    }
  })
  
  output$ffc_column_ui = renderUI({
    require(input$file1)
    n = ncol(Input_df())
    df = Input_df()
    df_crude = Input_df_crude()[[1]]
    
    if (input$ffc == "svd") {
      numericInput("ffc_column", "Choose column", value = 1, min = 1, max = n)
    } 
    else if (input$ffc == "column") {
      selectInput("ffc_column", "Choose column", choices = colnames(df_crude), selected = colnames(df)[1], multiple = FALSE)
    } else {
      tagList(
        numericInput("ffc_excentricity_exponent", "Exponent", value = 2, min = 1, step = 0.1),
        checkboxInput("ffc_excentricity_exponent_infinity", "Infinity?")
      )
    }
  })
  
  #### PLOTING! ----
  
  output$plot_data <- renderPlotly({
    if (input$customorexamples == "custom")  req(input$file1)
    df = Input_df()
    plot_ly(x = df[,1], y = df[,2], z = df[,3], size = 3)
  })
  
  output$plot_mapper <- renderPlot({
    L = Input_mapper()
    mapper_graph = L$mapper_graph
    n = L$num_vertices
    v.size = L$vertex_size
    l = L$layout
    v.mean = L$vertex_mean
    l[,1] = l[,1]*2
    ggraph(mapper_graph, layout = l) + 
      geom_edge_link() + geom_node_point(aes(size = v.size, color = v.mean)) +
      scale_color_gradientn(colours = jet.colors(n)) + scale_radius(range=c(3,10))
  })
  
  #### ggpairs ----
  output$plot_ggpairs <- renderPlot({
    df = as.data.frame(Input_df())
    ffc = Input_ffc()
    ggpairs(df[, 1:3]) #, col = coloring(ffc))
  })
  
  #### mapper plots ----
  output$plot_mapper_colored_as_filter <- renderPlot({
    L = Input_mapper()
    mapper_graph = L$mapper_graph
    n = L$num_vertices
    v.size = L$vertex_size
    l = L$layout
    points_in_vertex = L$points_in_vertex
    num_intervals = input$num_intervals
    ffc = Input_ffc()
    v.mean <- rep(0, n)
    if (is.numeric(ffc)) {
      for (i in 1:n) {
        v.mean[i] <- mean((ffc[points_in_vertex[[i]]]))
      }
      V(mapper_graph)$color <- subcoloring(ffc, v.mean)  #y.mean.vertex.grey
      c = cut(c(v.mean, min(ffc), max(ffc)), breaks = 100, include.lowest = TRUE, labels = FALSE)
      c = c[1:n]
      
      ggraph(mapper_graph, layout = l) + 
        geom_edge_link() + geom_node_point(aes(size = v.size, color = v.mean)) +
        scale_color_gradientn(colours = jet.colors(100)[min(c):max(c)]) + scale_radius(range=c(3,10))
    } else {
      for (i in 1:n) {
        v.mean[i] <- majority((ffc[points_in_vertex[[i]]]))
      }
      ggraph(mapper_graph, layout = l) + 
        geom_edge_link() + geom_node_point(aes(size = v.size, color = as.factor(v.mean))) + scale_radius(range=c(3,10))
    }
    
  })
  
  output$plot_data_colored_as_mapper <- renderPlotly({
    if (input$customorexamples == "custom") req(input$file1)
    df = Input_df()
    mapper_filter = Input_mapper_filter()
    if (input$distance == "geodesic" & ncol(df) == 3){
      grafo = Input_rnggraph()
      arestas = arestas = get.edges(grafo, 1:ecount(grafo))
      df1 = df[arestas[,1], ]
      df2 = df[arestas[,2], ]
      df1$nonono = 1:nrow(df1)
      df2$nonono = 1:nrow(df2)
      ms = list(df1, df2)
      m <- ms %>%
        bind_rows() %>%
        group2NA("nonono")
      plot_ly() %>%
        add_markers(data = df, x = df[,1], y = df[,2], z = df[,3], 
                    color = mapper_filter, colors = jet.colors(100), size = 1) %>%
        add_paths(x = m[,1], y = m[,2], z = m[,3], mode = 'lines', 
                  line = list(color = "black", width = 1))
    } else {
      plot_ly(x = df[,1], y = df[,2], z = df[,3], 
              color = mapper_filter, colors = jet.colors(100), size = 3)
    }
    
  })
  
  #### persistent homology ----
  
  # output$ph = renderPlot({
  #   distance_matrix = Input_distance()
  #   p = calculate_homology(mat = distance_matrix, dim = 1, format = "distmat")
  #   plot_persist(p)
  # })
  
  output$plot_data_colored_as_filter <- renderPlotly({
    if (input$customorexamples == "custom") req(input$file1)
    df = Input_df()
    ffc = Input_ffc()
    if (!is.character(ffc)){
      plot_ly(x = df[,1], y = df[,2], z = df[,3], 
              color = ffc, colors = jet.colors(100), size = 3)  
    } else {
      
    }
    
  })
}

shinyApp(ui, server)

