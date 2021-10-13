library(shiny)
library(shinydashboard)
library(DT)
library(htmltools)


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Background", tabName = "bkg"),
        menuItem("Gene Lookup", tabName = "glu"),
        menuItem("Gene Set Enrichment", tabName="gse")
    )
)

# Menu Tab "Introduction"
bkgDescp <- tabItem(
    tabName="bkg",
    column(12, wellPanel(tabsetPanel(tabPanel("Introduction", h1("What are we investigating?"),
                                              img(src = "www/intro1.png", width="70%"),
                                              img(src = "www/intro2.png", width="70%"),
                                              img(src = "www/intro3.png", width="70%"),
                                              img(src = "www/intro4.png", width="70%")),
                                     tabPanel("Datasets", h1("How did we get the dataset?"),
                                              img(src = "www/mouse_puberty_timeline.png", width="70%")),
                                     tabPanel("Output", h1("How do we interpret the output plots and tables?")),
                                     tabPanel("Credit", h1("Funded by, Lab members ....")))))
) 

# Menu Tab "Gene Lookup"
gluUserInput <- textInput(inputId = "gluGeneNames", label = "Enter your gene of interest:", width = "70%")

geneLookUp <- tabItem(
    tabName="glu",
    column(3, wellPanel(fluidRow(gluUserInput,
                                 p("For multiple genes, separate them with comma"),
                                 fluidRow(id = "alertMsgs"),
                                 actionButton(inputId = "submitGene", "Add Gene", width = "100%",
                                              style="background-color: green; border-color: green; color: white")),
                        style="padding: 50px; border-color: green; background-color: white")),

    column(9, wellPanel(tabsetPanel(tabPanel("Plots",
                                             fluidRow(plotOutput="normExp"),
                                             # Dustin's take on the multi-plot issue
                                             lapply(seq(1,4,2), function(x){
                                                 
                                                 fluidRow(column(6, h3("Hypothalamus", align="center"),
                                                                 plotOutput(outputId = paste0("gene",x))),
                                                          column(6, h3("Pituitary Gland", align="center"),
                                                                 plotOutput(outputId = paste0("gene",x+1))))
                                                 
                                                
                                             }),
                                             
                                             #fluidRow(column(6, h3("Hypothalamus", align="center"),
                                             #                plotOutput(outputId = "gene1")),
                                             #         column(6, h3("Pituitary Gland", align="center"),
                                             #                plotOutput(outputId = "gene2"))),
                                             style="padding: 30px"),
                                    tabPanel("Table", DT::dataTableOutput(outputId = "pvalTable"),
                                             style="padding: 30px",
                                             downloadButton("gluDownloadTable", "Download .csv"))),
    style = "border-color: #4dfff9; background-color: white"))
)

# miRNAtab <- tabItem(
#     tabName="mirna",
#     column(3, wellPanel(p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi sapien nulla, laoreet in euismod eget, hendrerit ut mauris. Aliquam mauris nisi, ultrices quis eros eget, auctor sollicitudin justo. Nullam elementum in justo iaculis vulputate. Nullam imperdiet posuere rutrum. Nulla tortor justo, iaculis sit amet consequat ut, suscipit non lacus. Praesent nisi mauris, dictum finibus nibh ut, rhoncus bibendum felis. In ac sagittis lacus. Phasellus vehicula velit et diam sagittis porta. Vivamus at ex iaculis sem condimentum fermentum. Donec tempor scelerisque sem, nec pellentesque nisi tempus eget. Integer tellus diam, aliquet sit amet lacinia at, fermentum mollis sapien. Nullam vitae laoreet augue, non rhoncus nunc. Vestibulum luctus, nulla sit amet placerat dapibus, odio lectus molestie ipsum, at scelerisque nisl lectus ut ante."))),
#     column(9, wellPanel(p("Proin id nisl lorem. Quisque tincidunt gravida purus, at tincidunt nibh finibus porttitor. Quisque luctus dolor ac turpis pulvinar, non imperdiet metus iaculis. Mauris nulla dui, aliquam a venenatis ac, ullamcorper vitae erat. Vivamus ac magna viverra mauris tempus maximus nec et mauris. Sed et odio malesuada tellus iaculis auctor. Phasellus imperdiet lectus libero, et rutrum mauris pulvinar sit amet. Integer volutpat at sapien ut consectetur. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Aenean dignissim, enim in euismod viverra, nisi tortor dapibus eros, et porta metus orci vel ligula. Nullam malesuada, neque eu luctus rhoncus, est augue vulputate dolor, non ullamcorper metus arcu sit amet purus. Maecenas finibus eu arcu sed porta. Vestibulum ornare lacinia orci sed pretium.
# 
# Ut efficitur fermentum tortor, pharetra euismod massa malesuada nec. Donec eu laoreet elit. Morbi massa risus, cursus id varius eget, ornare sed leo. Etiam in felis ac dui fermentum malesuada. Sed sit amet neque at mauris cursus convallis ac non elit. Suspendisse consectetur ac ex a feugiat. Vestibulum semper commodo dictum. Vestibulum pellentesque, mi vel convallis pretium, eros enim ultrices lorem, non lobortis purus lorem quis purus. Nullam posuere fringilla metus, nec luctus turpis hendrerit eu.
# 
# Fusce dapibus velit a ante laoreet cursus. Sed ullamcorper suscipit lorem quis consectetur. Vivamus volutpat, nunc ut ornare commodo, elit tellus pretium felis, non pulvinar diam tortor quis nulla. Quisque suscipit blandit purus quis aliquet. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Suspendisse potenti. Etiam euismod condimentum dolor id iaculis. Sed felis augue, auctor ut tortor eu, commodo mattis ex. Donec scelerisque tortor quam, vitae congue felis condimentum sit amet. Nulla vitae sem eu diam ultricies varius.
# 
# Vestibulum in ante pulvinar, feugiat mi at, vehicula dui. Mauris at laoreet ante, id sollicitudin dolor. Nunc elementum ligula ac erat ornare, vel rhoncus libero feugiat. Quisque et lectus felis. Nulla facilisi. Integer volutpat tellus ullamcorper lorem efficitur dapibus. Aliquam in sollicitudin magna. Mauris laoreet volutpat dolor non rutrum. Nunc sed arcu quam.")))
# )


# Menu Tab "Gene Set Enrichment"

gseUpload <- fileInput("gseGeneNamesUpload", NULL, accept = c(".txt"))
geneSetEnrichment <- tabItem(
    tabName="gse",
    column(3, wellPanel(fluidRow(p("Upload .txt"), gseUpload),
                        style="padding: 50px; border-color: green; background-color: white")),
    
    column(9, wellPanel(tabsetPanel(tabPanel("Plots",
                                             fluidRow(column(12, plotOutput(outputId = "gseNormExp"))),
                                             style = "padding: 30px"),
                                    tabPanel("Table", DT::dataTableOutput(outputId = "gseTable"),
                                             style = "padding: 30px",
                                             downloadButton("gseDownloadTable", "Download .csv"))),
                        style = "border-color: #4dfff9; background-color: white"))
)

## Body ##
body <- dashboardBody(
    tabItems(
        bkgDescp,
        geneLookUp,
        geneSetEnrichment
        # miRNAtab
    ),
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }')))
)

## UI Page ##
ui <- dashboardPage(
    dashboardHeader(title="Mouse Puberty Gene Expression"),
    sidebar,
    body
)






# 
# tabName = "gse",
# column(4, wellPanel(fileInput("gseGeneNamesUpload", NULL, accept = c(".txt"))),
#        style = "padding: 30px; background-color: white"),
# column(8, wellPanel(tabsetPanel(tabPanel("Plots",
#                                          fluidRow(column(12, plotOutput(outputId = "gseNormExp")))),
#                                 tabPanel("Table", 
#                                          fluidRow(DT::dataTableOutput(outputId = "gseTable"),
#                                                   style = "padding: 30px"),
#                                          downloadButton("gseDownloadTable", "Download .csv")))),
#        style = "padding: 30px; background-color: white")

# column(8, wellPanel(tabsetPanel(tabPanel("Plots",
#                                          fluidRow(column(12, plotOutput(outputId = "gseNormExp")))),
#                                 tabPanel("Table",
#                                          DT::dataTableOutput(outputId = "gseTable"),
#                                          style="padding: 30px",
#                                          downloadButton("gluDownloadTable", "Download .csv"))),
#                     style = "padding: 30px; background-color: white"))
# column(4, wellPanel(fluidRow(p("Input file format .txt"),
#                              fileInput("gseGeneNamesUpload", NULL, accept=c(".txt"))),
#                     style = "padding: 30px; background-color: white")),

