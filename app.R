


# Setup -------------------------------------------------------------------------------------------
library(shiny)
library(bslib)

abund <- readRDS("./data/sppabund.RDS")
ewi <- readRDS("./data/sppewi.RDS")
#ewtrends <- 
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

# UI ----------------------------------------------------------------------------------------------
ui <- page_navbar(
  id = "page",
  title = "Central California fishes EWIs",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "About", 
            h2("About this web app"),
            p("The goal of this web app is to provide an interactive platform for examining statistical evidence of loss of population stability and potential early warnings of regime shifts in a suite of fishes inhabiting the San Francisco Estuary and Sacramento and San Joaquin River systems."),
            p("The information provided here is based on research by Jonathan Walter, Levi Lewis, James Hobbs, and Andrew Rypel and detailed methods are described in <url>."),
            p("Research and app development were supported by the California Department of Fish and Wildlife"),
            h2("Instructions for use"),
            p("This app supports exploration of time series data and composite scores describing the evidence (relative to other species) for loss of population stability; use the navigation bar in the top right to select an option or return to this page."),
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
                                                                          "South Delta-BS", "South Delta-KT", "South Delta-MWT",
                                                                          "North Delta-BS","North Delta-KT","North Delta-MWT",
                                                                          "San Joaquin River-BS", "Sacramento River-BS"),
                                                                multiple=FALSE),
                                            checkboxGroupInput("metrics","EWI metric", 
                                                               choices=c("Spatial synchrony", "Temporal CV", "Lag-1 autocorrelation"))
                           ),
                           #textOutput("test")
                           plotOutput("tsplot")
                           ),
            p("Spatial unit codes: MWT = midwater trawl, KT = kodiak trawl, BS = beach seine.")
            ),
  nav_panel(title = "Composite scores",
            layout_sidebar(sidebar = sidebar(checkboxGroupInput("sunits","Spatial unit",
                                                                choices=c("South Bay-MWT", "Central Bay-BS", "Central Bay-MWT",
                                                                          "San Pablo Bay-BS", "San Pablo Bay-MWT", "Napa River-MWT",
                                                                          "Suisun Bay-MWT", "Confluence-BS", "Confluence-MWT",
                                                                          "South Delta-BS", "South Delta-KT", "South Delta-MWT",
                                                                          "North Delta-BS","North Delta-KT","North Delta-MWT",
                                                                          "San Joaquin River-BS", "Sacramento River-BS")
                                                                )
                                             ),
                           #plotOutput()

                           ),
            p("Spatial unit codes: MWT = midwater trawl, KT = kodiak trawl, BS = beach seine.")
            )
)


# Server ------------------------------------------------------------------------------------------
server <- function(input, output) {

  output$test <- renderText({print(input$species)})
  
  abunddat <- reactive({abund[[input$species]][srownames==input$sunit,]})
  ewidat <- reactive({getEWI(input$species, input$sunit, input$metrics)})

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

}

# Run the app ----
shinyApp(ui = ui, server = server)
