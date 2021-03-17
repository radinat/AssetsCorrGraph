

# Load libraries ----
library(readxl)
library(quantmod)
library(igraph)
library(dplyr)
library(lubridate)
library(shiny)
library(imputeTS)
library(tidyverse)

setwd("//Users//radinatalanova//Downloads")
# # Import the available list of tickers ----
equity.tickers = read_excel("List_of_assets.xlsx", sheet = 1)
fixed.income.tickers = read_excel("List_of_assets.xlsx", sheet = 2)
commodities.tickers = read_excel("List_of_assets.xlsx", sheet = 3)
#
# # Create Environments
equity = list()
equity$tickers = equity.tickers$Ticker
equity$env = new.env()

fixed_income = list()
fixed_income$tickers = fixed.income.tickers$Ticker
fixed_income$env = new.env()

commodities = list()
commodities$tickers = commodities.tickers$Ticker
commodities$env = new.env()

# Download historical information ----
# For stocks:
getSymbols(
    equity$tickers,
    env = equity$env,
    from = "2006-01-01",
    "getSymbols.warning4.0" = FALSE
)
rm(.getSymbols, envir = equity$env)
save(list = ls(equity$env),
     file = 'equity.RData',
     envir = equity$env)

# There is no data for ticker AGN in yahoo finance. Removed it from Excel
# list of assets, because it brings an error when we load the data.

# For fixed income ETFs:
getSymbols(
    fixed_income$tickers,
    env = fixed_income$env,
    from = "2006-01-01",
    "getSymbols.warning4.0" = FALSE
)
rm(.getSymbols, envir = fixed_income$env)
save(
    list = ls(fixed_income$env),
    file = 'fixed_income.RData',
    envir = fixed_income$env
)

# For commodity ETFs:
getSymbols(
    commodities$tickers,
    env = commodities$env,
    from = "2006-01-01",
    "getSymbols.warning4.0" = FALSE
)
rm(.getSymbols, envir = commodities$env)
save(
    list = ls(commodities$env),
    file = 'commodities.RData',
    envir = commodities$env
)


# # Load data (not downloading it every time) ----
#load("equity.RData", envir=equity$env)
#load('fixed_income.RData', envir=fixed_income$env)
#load('commodities.RData', envir=commodities$env)

# Create data frames with adjusted prices ----
eq = lapply(names(equity$env), get, envir = equity$env)
fi = lapply(names(fixed_income$env), get, envir = fixed_income$env)
com = lapply(names(commodities$env), get, envir = commodities$env)

# Equity data frame
eq_dd = list()
for (i in 1:length(eq)) {
    eq_dd[[i]] = eq[[i]][, 6]
}
eq_dd = do.call(merge, eq_dd)
colnames(eq_dd) = gsub(".Adjusted", "", colnames(eq_dd))
eq_dd = as.data.frame(eq_dd)

# Fixed income ETFs data frame
fi_dd = list()
for (i in 1:length(fi)) {
    fi_dd[[i]] = fi[[i]][, 6]
}
fi_dd = do.call(merge, fi_dd)
colnames(fi_dd) = gsub(".Adjusted", "", colnames(fi_dd))
fi_dd = as.data.frame(fi_dd)

# Commodities data frame
com_dd = list()
for (i in 1:length(com)) {
    com_dd[[i]] = com[[i]][, 6]
}
com_dd = do.call(merge, com_dd)
colnames(com_dd) = gsub(".Adjusted", "", colnames(com_dd))
com_dd = as.data.frame(com_dd)

rm(eq, fi, com, i)
rm(commodities.tickers, equity.tickers, fixed.income.tickers)

# Bind data frames
all_dd = cbind(eq_dd, fi_dd, com_dd)

all_dd$row_names = rownames(all_dd)
all_dd$date = as.Date(all_dd$row_names)
all_dd = all_dd[,!(names(all_dd) %in% c("row_names"))] # remove row_names column

rownames(all_dd) = all_dd$date

testdates = all_dd
all_dd$date = as.Date(all_dd$date)
all_dd = all_dd %>%
    complete(date = seq(min(all_dd$date), max(all_dd$date), by = "day"))

# #inpute missing dates
testdates$date = as.Date(testdates$date)
testdates = testdates %>%
    complete(date = seq(min(testdates$date), max(testdates$date), by = "day"))
# missing values interpolation
testdates <- na_interpolation(testdates, option = "linear")
testdates = as.data.frame(testdates)
rownames(testdates) = as.character(testdates$date)
#remove date from the set
final = testdates[, !names(testdates) %in% c("date")]
rownames(final) = rownames(testdates)

# Perform rolling sample estimation of the correlation matrix ---
date = ymd(rownames(final))
rdate = date[-1]

# Function for calculation of returns
rets = function(x) {
    lx = dplyr::lag(x)
    r = (x - lx) / lx
    return (r[-1])
}

rr = as.data.frame(sapply(final, rets))
rownames(rr) = rdate


n = 120 # length of the sliding window
test_cc = list()
suppressWarnings(for (i in 1:(nrow(rr) - n + 1)) {
    test_cc[[i]] = cor(rr[i:(i + n - 1), ], use = "pairwise.complete.ob")
})
names(test_cc) = as.character(date[n:nrow(rr)])



# R Shiny ----
tweaks <-
    list(tags$head(tags$style(
        HTML(
            "
                                 .multicol1 {
                                   height: 500px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */
                                   -moz-column-count: 5;    /* Firefox */
                                   column-count: 6;
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 };
                                 .multicol2 {
                                   height: 200px;
                                   -webkit-column-count: 1; /* Chrome, Safari, Opera */
                                   -moz-column-count: 1;    /* Firefox */
                                   column-count: 1;
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 }
                                 "
        )
    )))

# Get ticker names
eqnames = sort(colnames(eq_dd))
finames = sort(colnames(fi_dd))
comnames = sort(colnames(com_dd))

names = colnames(all_dd[,!(names(all_dd) %in% c("date"))])
names = sort(names)

# Create check boxes
controls = list(
    h3("Tickers:"),
    tags$div(
        align = 'left',
        class = 'multicol1',
        checkboxGroupInput(
            inputId  = 'tSelector1',
            label    = "Equities:",
            choices  = eqnames,
            selected = c(
                "AMZN",
                "ALB",
                "AMAT",
                "NKE",
                "BBY",
                "BSX",
                "CMG",
                "COST",
                "DPZ",
                "EA",
                "EXAS",
                "EOG",
                "IBM",
                "GILD",
                "INTC",
                "JNPR",
                "MPC",
                "TAK",
                "TSLA",
                "XLNX",
                "VZ",
                "VALE",
                "TWOU",
                "WMT",
                "HUM",
                "ALB",
                "MAT",
                "PEP",
                "PLD",
                "CVS"
            ),
            inline   = TRUE
        )
    ),
    tags$div(
        align = 'left',
        class = 'multicol2',
        checkboxGroupInput(
            inputId  = 'tSelector2',
            label    = "Fixed Income:",
            choices  = finames,
            selected = c("JNK", "TIP"),
            inline   = TRUE
        )
    ),
    tags$div(
        align = 'left',
        class = 'multicol2',
        checkboxGroupInput(
            inputId  = 'tSelector3',
            label    = "Commodities:",
            choices  = comnames,
            selected = c("GLD"),
            inline   = TRUE
        )
    )
    
)

ui <- fluidPage(
    tags$head(tags$style(type = "text/css", ".irs {max-width: 1500px;}")),
    titlePanel("Assets Correlation Graph"),
    tweaks,
    sidebarLayout(
        sidebarPanel(
            fluidRow(column(width = 12, controls),),
            sliderInput(
                "slider",
                "Time",
                min = as.Date("2007-01-01", "%Y-%m-%d"),
                max = as.Date("2021-01-04", "%Y-%m-%d"),
                value = as.Date("2014-05-13", timeFormat = "%Y-%m-%d"),
                step = 1,
                animate = TRUE,
                width = 1000
            )
        ),
        
        mainPanel(plotOutput(
            "clPlot", width = 1000, height = 900
        ),)
    )
)

server <- shinyServer(function(input, output, session) {
    set.seed(123)
    plot_data <- reactive(input$tSelector1)
    plot_data <- reactive(input$tSelector2)
    plot_data <- reactive(input$tSelector3)
    
    output$SliderText <- renderText({
        input$slider
    })
    
    currentFib <- reactive({
        if (length(c(input$tSelector1, input$tSelector2, input$tSelector3)) < 2) {
            p = "Choose at least 4 assets"
            
        } else {
            # take data for the selected tickers only
            selected = all_dd
            selected = selected[, c(input$tSelector1,
                                    input$tSelector2,
                                    input$tSelector3,
                                    'date')]
            
            inp = c(input$tSelector1,
                    input$tSelector2,
                    input$tSelector3)
            dateinp = input$slider
            temp = as.data.frame(test_cc[as.character(dateinp)])
            
            colnames(temp) = rownames(temp)
            temp = temp[, inp]
            
            sel_cc = data.frame()
            
            for (i in inp) {
                sel = data.frame(temp[i, ])
                sel_cc = rbind(sel_cc, sel)
            }
            
            # filter
            inpdate = input$slider
            daydata = selected %>%
                filter(selected$date == inpdate)
            
            if ((sum(is.na(daydata) == FALSE)) == 1) {
                inpdate = as.Date(input$slider) - 3
                daydata = selected %>%
                    filter(selected$date == inpdate)
            }
            
            torem = which(colSums(is.na(daydata)) > 0)
            if (length(torem) > 0) {
                sel_cc = sel_cc[-torem, -torem]
                rr = rr[, -torem]
            }
            
            if (length(sel_cc) > 1) {
                # Extract correlations only above the main diagonal
                ww = list()
                n = nrow(sel_cc) - 1
                for (i in 1:n) {
                    ww[[i]] = sel_cc[-c(1:i), i]
                }
                ww = unlist(ww) # vector of all correlations
                
                # Graph theory application
                g = graph.full(length(sel_cc),
                               directed = F,
                               loops = F)
                V(g)$name = colnames(sel_cc)
                V(g)$color = "#A8C3F6"
                
                E(g) # all possible connections
                
                # Set threshold - plot connections with cor higher than 0.3
                thr = 0.3
                wwt = ifelse(abs(ww) < thr, 0, ww)
                E(g)$weight = wwt
                
                E(g)[weight >= 0.7]$width = 3
                E(g)[weight >= 0.7]$color = "black"
                E(g)[weight <= -0.3]$color = "brown3"
                E(g)[weight <= -0.3]$width = 3
                E(g)[weight > -0.3 & weight < 0.7]$width = 0.5
                E(g)[weight > -0.3 & weight < 0.7]$color = "grey"
                
                plot(
                    g,
                    layout = layout.fruchterman.reingold,
                    vertex.color = "bisque2",
                    vertex.label = V(g)$number,
                    vertex.frame.color = "bisque2",
                    vertex.label.color = "black",
                    vertex.size = 12,
                    vertex.label.cex = 1,
                    vertex.label.family = "Helvetica",
                    vertex.label.font = 2
                )
            }
            
            
        }
    })
    
    
    set.seed(123)
    output$plot <- renderPlot(plot(
        x = plot_data(),
        y = plot_data(),
        pch = 7,
        cex = 2,
        xlim = c(1, 25),
        ylim = c(1, 25)
    ))
    
    output$clPlot <- renderPlot({
        set.seed(123)
        currentFib()
        
    })
})


shinyApp(ui = ui, server = server)
