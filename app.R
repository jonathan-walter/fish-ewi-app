


# Setup -------------------------------------------------------------------------------------------
library(shiny)
library(bslib)
library(viridis)

abund <- readRDS("./data/sppabund.RDS")
ewi <- readRDS("./data/sppewi.RDS")
trends <- readRDS("./data/trends.RDS")
scores <- readRDS("./data/scores.RDS")
years <- 1980:2023
srownames <- rownames(abund[[1]])

getEWI <- function(species, sunit, metrics){
  out <- NULL
  if("Spatial synchrony" %in% metrics){
    out <- rbind(out, ewi[[species]]$spatsync)
  }
  if("Temporal CV" %in% metrics){
    out <- rbind(out, ewi[[species]]$cv.time[srownames==sunit,])
  }
  if("Lag-1 autocorrelation" %in% metrics){
    out <- rbind(out, ewi[[species]]$ar1[srownames==sunit,])
  }
  rownames(out) <- metrics
  return(out)
}

pal <- c("#377EB8", "#4DAF4A","#E41A1C") #from color brewer Set 1

q1 <- rownames(scores$ewi)[scores$ewi[,"Combined"]>=median(scores$ewi[,"Combined"]) & scores$confidence[,"Combined"]>=median(scores$confidence[,"Combined"])]
q2 <- rownames(scores$ewi)[scores$ewi[,"Combined"]>=median(scores$ewi[,"Combined"]) & scores$confidence[,"Combined"]<median(scores$confidence[,"Combined"])]
q3 <- rownames(scores$ewi)[scores$ewi[,"Combined"]<median(scores$ewi[,"Combined"]) & scores$confidence[,"Combined"]<median(scores$confidence[,"Combined"])]
q4 <- rownames(scores$ewi)[scores$ewi[,"Combined"]<median(scores$ewi[,"Combined"]) & scores$confidence[,"Combined"]>=median(scores$confidence[,"Combined"])]

# UI ----------------------------------------------------------------------------------------------
ui <- page_navbar(
  id = "page",
  title = "Central California fishes EWIs",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "About", 
            h2("About this web app"),
            p("The purpose of this web app is to provide an interactive platform for examining statistical evidence of loss of population stability and potential early warnings of regime shifts in a suite of fishes inhabiting the San Francisco Estuary and Sacramento and San Joaquin River systems."),
            p("Early warning indicator (EWI) metrics provide generic statistics for describing the stability of populations and other ecological variables based on properties of their time series and/or spatial distributions. In addition to their use in measuring stability, EWIs are so named because they may exhibit distinct increases prior to a regime shift, or abrupt state change. In wildife populations, regime shifts can include population collapses as well as population irruptions in invasive species."),
            p("The fish population data presented here is supplied by three long-term monitoring studies: the San Francisco Bay Study, the Fall Midwater Trawl survey, and the Delta Juvenile Fish Monitoring Program, as compiled in https://doi.org/10.6073/pasta/a29a6e674b0f8797e13fbc4b08b92e5b."),
            p("The information provided here is based on research by Jonathan Walter, Levi Lewis, James Hobbs, and Andrew Rypel and detailed methods are described in https://doi.org/10.32942/X29G9N."),
            p("Recorded presentations describing this research (https://youtu.be/f3OoJfJ0-9E) and the app functionality (https://youtu.be/h_9RW7tAoTg) are available."),
            p("For background on statistical early warnings of regime shifts, a recommended resource is https://www.nature.com/articles/nature08227."),
            p("Research and app development were supported by the California Department of Fish and Wildlife."),
            h2("Instructions for use"),
            p("This app supports exploration of time series data, directional trends in early warning indicators (EWIs), and composite scores describing the evidence (relative to other species) for loss of population stability; use the navigation bar in the top right to select an option or return to this page."),
            p("In the 'Time series' tab, the left sidebar provides clickable options to control what information is plotted. Choose a species and spatial unit to display the CPUE time series; choose an EWI metric to display its time series as a scaled value overlaying the CPUE time series."),
            p("Here, spatial units correspond to combinations of sub-region and sampling method (midwater trawl, beach seine), since different methods sample different habitats (e.g., portion of the water column)."),
            h2("Open data and code"),
            p("Analysis code used to quantify EWIs and compute the EWI index is available at https://doi.org/10.5281/zenodo.14907908."),
            p("Code for this Shiny app is available at https://github.com/jonathan-walter/fish-ewi-app."),
            p("Numerical data on EWI trends used to produce the figures in the 'EWI trends' tab are available at https://doi.org/10.6073/pasta/aa552e1f82f95a33c2ea657e3c0706e4."),
            p("Last update: 2025-02-18")
            ),
  nav_panel(title = "Time series", 
            layout_sidebar(sidebar = sidebar(selectInput("species","Species", 
                                                         choices=c("American shad", "Bay pipefish", "Bluegill", "Channel catfish",         
                                                                   "Chinook salmon", "Delta smelt", "Fathead minnow", "Jacksmelt",               
                                                                    "Longfin smelt", "Mississippi silverside", "Northern anchovy", "Pacific herring",         
                                                                    "Pacific lamprey", "Pacific pompano", "Pacific staghorn sculpin", "Plainfin midshipman",     
                                                                    "Red shiner", "River lamprey", "Sacramento splittail", "Sardine",                 
                                                                    "Shimofuri goby", "Shiner perch", "Starry flounder", "Striped bass",            
                                                                    "Threadfin shad", "Topsmelt", "Tule perch", "White catfish",           
                                                                    "White croaker", "White sturgeon", "Yellowfin goby"), 
                                                         multiple=FALSE),
                                             selectInput("sunit","Spatial unit", 
                                                                choices=c("South Bay-MWT", "Central Bay-BS", "Central Bay-MWT",
                                                                          "San Pablo Bay-BS", "San Pablo Bay-MWT", "Napa River-MWT",
                                                                          "Suisun Bay-MWT", "Confluence-BS", "Confluence-MWT",
                                                                          "South Delta-BS", "South Delta-MWT", "North Delta-BS",
                                                                          "North Delta-MWT", "San Joaquin River-BS", "Sacramento River-BS"),
                                                                multiple=FALSE),
                                            checkboxGroupInput("metrics","EWI metric", 
                                                               choices=c("Spatial synchrony", "Temporal CV", "Lag-1 autocorrelation"))
                           ),
                           #textOutput("test")
                           plotOutput("tsplot")
                           ),
            p("Spatial unit codes: MWT = midwater trawl, BS = beach seine.")
            ),
  nav_panel(title = "EWI trends",
              card(card_header("Trends in spatial synchrony (t-statistic)"),
                plotOutput("synctrends"),
                min_height="300px"
              ),
              card(card_header("Trends in temporal CV (t-statistic)"),
                plotOutput("cvtrends"),
                min_height="500px"
              ),
              card(card_header("Trends in lag-1 autocorrelation (t-statistic)"),
                 plotOutput("artrends"),
                 min_height="500px"
              ),
            p("Numeric data represented in these plots are available at https://doi.org/10.6073/pasta/aa552e1f82f95a33c2ea657e3c0706e4.")
            ),
  nav_panel(title = "Composite scores",
              card(card_header("Relative EWI score"),
                plotOutput("ewiscore"),
                min_height="400px"
              ),
              card(card_header("Relative confidence score"),
                 plotOutput("confidence"),
                 min_height="400px"
              ),
              card(card_header("Considering EWI & confidence scores together"),
                   plotOutput("biplot"),
                   min_height="400px")
            )
)


# Server ------------------------------------------------------------------------------------------
server <- function(input, output) {

  output$test <- renderText({print(input$species)})
  
  abunddat <- reactive({abund[[input$species]][srownames==input$sunit,]})
  ewidat <- reactive({getEWI(input$species, input$sunit, input$metrics)})

  ## Time series plotting
  observe({
    d1 <- abunddat()
    d2 <- ewidat()
    
    if(all(is.na(d1))){
      output$tsplot <- renderPlot({
        plot(years, d1, type="b", lwd=1.5, ylim=c(0,1),
             xlab="Year", ylab="CPUE")
        mtext("No data for selected species and spatial unit")
      })
    }
    else{
      output$tsplot <- renderPlot({
        plot(years,d1, type="b", lwd=1.5, xlab="Year", ylab="CPUE")
        if(!is.null(d2)){
          for(ii in 1:nrow(d2)){
            par(new=TRUE, xpd=TRUE)
            plot(years, scale(c(d2[ii,])), col=pal[ii], xaxt="n", yaxt="n", type="b", lwd=1.5,
                 xlab="", ylab="")
          }
          legend("top", legend = c("CPUE", rownames(d2)), pch=1, col=c("black", pal[1:nrow(d2)]), ncol=nrow(d2)+1, 
                 bty="n", inset=-0.1)
        }
      })
    }
  })
  ## EWI trend plotting
  output$synctrends <- renderPlot({
    layout(matrix(1:2, nrow=2), heights=c(0.2,0.7))
    par(mar=c(1.2,1,0.5,1))
    image(matrix(1:50), col=turbo(50), yaxt="n", xaxt="n")
    axis(1, at=c(0,0.25,0.5,0.75,1), labels=c(-9,-4.5,0,4.5,9))
    par(mar=c(10,1,1,1))
    image(x=1:length(trends$spatsync), z=matrix(trends$spatsync), col=turbo(50), yaxt="n", xaxt="n",
          xlab="", zlim=c(-9, 9))
    axis(1, at=1:length(trends$spatsync), labels=names(trends$spatsync), las=2)
  })
  
  output$cvtrends <- renderPlot({
    layout(matrix(1:2, nrow=2), height=c(0.1,0.9))
    par(mar=c(1.2,9,1,1))
    image(matrix(1:50), col=turbo(50), yaxt="n", xaxt="n")
    axis(1, at=c(0,0.25,0.5,0.75,1), labels=c(-10,-5,0,5,10))
    par(mar=c(10,9,1,1))
    image(x=1:nrow(trends$cv.time), y=1:ncol(trends$cv.time), z=trends$cv.time, col=turbo(50), yaxt="n", xaxt="n",
          xlab="", zlim=c(-10, 10), ylab="")
    axis(1, at=1:nrow(trends$cv.time), labels=rownames(trends$cv.time), las=2)
    axis(2, at=1:ncol(trends$cv.time), labels=colnames(trends$cv.time), las=2)
  })
  
  output$artrends <- renderPlot({
    layout(matrix(1:2, nrow=2), height=c(0.1,0.9))
    par(mar=c(1.2,9,1,1))
    image(matrix(1:50), col=turbo(50), yaxt="n", xaxt="n")
    axis(1, at=c(0,0.25,0.5,0.75,1), labels=c(-10,-5,0,5,10))
    par(mar=c(10,9,1,1))
    image(x=1:nrow(trends$ar1), y=1:ncol(trends$ar1), z=trends$ar1, col=turbo(50), yaxt="n", xaxt="n",
          xlab="", zlim=c(-10, 10), ylab="")
    axis(1, at=1:nrow(trends$ar1), labels=rownames(trends$ar1), las=2)
    axis(2, at=1:ncol(trends$ar1), labels=colnames(trends$ar1), las=2)
    
  })
  
  ## Composite score plotting
  
  output$ewiscore <- renderPlot({
    layout(matrix(1:2, nrow=2), height=c(0.18,0.9))
    par(mar=c(1.2,9.2,1,1))
    image(matrix(1:50), col=rev(rocket(50)), yaxt="n", xaxt="n")
    axis(1)
    par(mar=c(9.2,9.2,1,1))
    image(x=1:nrow(scores$ewi), y=1:4, z=scores$ewi, col=rev(rocket(50)), yaxt="n", xaxt="n",
          xlab="", zlim=c(0, 1), ylab="")
    axis(1, at=1:nrow(scores$ewi), labels=rownames(scores$ewi), las=2, cex.axis=0.9)
    axis(2, at=1:4, labels=colnames(scores$ewi), las=2, cex.axis=0.9)
    abline(h=3.5, lwd=3, col="white")
  })
  
  output$confidence <- renderPlot({
    layout(matrix(1:2, nrow=2), height=c(0.18,0.9))
    par(mar=c(1.2,8.5,1,1))
    image(matrix(1:50), col=viridis(50), yaxt="n", xaxt="n")
    axis(1)
    par(mar=c(9.2,8.5,1,1))
    image(x=1:nrow(scores$confidence), y=1:5, z=scores$confidence, col=viridis(50), yaxt="n", xaxt="n",
          xlab="", zlim=c(0, 1), ylab="")
    axis(1, at=1:nrow(scores$confidence), labels=rownames(scores$confidence), las=2, cex.axis=0.9)
    axis(2, at=1:5, labels=colnames(scores$confidence), las=2, cex.axis=0.9)
    abline(h=4.5, lwd=3, col="white")
  })
  
  output$biplot <- renderPlot({
    par(mfrow=c(1,2), mar=c(3.1,3.1,0.6,0.6), mgp=c(2,0.7,0))
    plot(scores$confidence[,"Combined"], scores$ewi[,"Combined"], xlab="Relative confidence", ylab="Relative EWI score", pch=16, cex=0.75)
    abline(h=median(scores$ewi[,"Combined"]), col="grey")
    abline(v=median(scores$confidence[,"Combined"]), col="grey")
    text(x=1, y=median(scores$ewi[,"Combined"]) + 0.025, "I", cex=0.9)
    text(x=0, y=median(scores$ewi[,"Combined"]) + 0.025, "II", cex=0.9)
    text(x=0, y=median(scores$ewi[,"Combined"]) - 0.025, "III", cex=0.9)
    text(x=1, y=median(scores$ewi[,"Combined"]) - 0.025, "IV", cex=0.9)
    text(0.01,1,"a)")
    
    par(mgp=c(1,0.7,0))
    plot(NA, NA, ylim=c(0,1), xlim=c(0,1), xlab="Relative confidence", ylab="Relative EWI score",
         xaxt="n", yaxt="n")
    abline(h=0.5, col="grey")
    abline(v=0.5, col="grey")
    text(x=par("usr")[1]+0.75*diff(par("usr")[1:2]), 
         y=par("usr")[3]+0.75*diff(par("usr")[3:4]), 
         paste(q1, collapse="\n"),
         cex=0.7)
    text(x=par("usr")[1]+0.25*diff(par("usr")[1:2]),
         y=par("usr")[3]+0.75*diff(par("usr")[3:4]), 
         paste(q2, collapse="\n"),
         cex=0.7)
    text(x=par("usr")[1]+0.25*diff(par("usr")[1:2]), 
         y=par("usr")[3]+0.25*diff(par("usr")[3:4]), 
         paste(q3, collapse="\n"),
         cex=0.7)
    text(x=par("usr")[1]+0.75*diff(par("usr")[1:2]), 
         y=par("usr")[3]+0.25*diff(par("usr")[3:4]), 
         paste(q4, collapse="\n"),
         cex=0.7)
    text(0.01,1,"b)")
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
