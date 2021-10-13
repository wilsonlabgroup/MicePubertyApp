library(shiny)
library(DT)
library(reshape)
library(ggplot2)

source("script/factor_plot.R")
source("script/script.R")
source("script/sunyun_puberty_applet_internal.R")

load("data/Counts/Hypothalamus/hypo_RNAseq_counts.RData") #norm_counts
load("data/Counts/Pituitary/Pituitary_RNAseq_Counts.rda") #norm_df
load("data/Gene_lists/Hypothalamus/hypo_differential_mRNA.Rdata") #puberty_sigDEGs
pitu_DEGs <- readRDS("data/Gene_lists/Pituitary/pit_differential_mRNA.rds")

all_DEGs <- readRDS("data/Gene_lists/Puberty_all_DEGs.rds")
# Hypothalamus normalized countsDlk1,
colnames(norm_counts) <- paste0("WL000_",colnames(norm_counts))
norm_counts_log_hypo <- log2(norm_counts)
# Pituitary normalized counts
norm_counts_log_pitu <- log2(norm_df)

all_DEGs_hypo <- get_DEGs_bind(puberty_sigDEGs, type="Hypo")
all_DEGs_pitu <- get_DEGs_bind(pitu_DEGs, type="Pitu")

all_DEGs <- get_DEGs_bind(all_DEGs)


server <- function(input, output) {
    rv <- reactiveValues(gluGeneNames=c(), gluTable=NULL, gseTable=NULL)
    
    observeEvent(input$submitGene, {
        # removeUI(
        #     selector = "div:has(> #alertMsgs)"
        # )
        # New gene names list due to new input value
        rv$gluGeneNames <- lapply(strsplit(input$gluGeneNames, ","), trimws)[[1]]
        genes_not_found <- c()
        # Create new plot (single value)
        # TODO: include all values
        count <- 1
        for (i in 1:length(rv$gluGeneNames)) {
            geneName <- rv$gluGeneNames[i]

            # Collect gene names that are not in both hypothalamus and pituitary DEGs
            if (!(geneName %in% rownames(norm_counts_log_hypo)) &&
                !(geneName %in% rownames(norm_counts_log_pitu))) {
                genes_not_found <- append(genes_not_found, geneName)
            }
            output[[paste0("gene",count)]] <- renderPlot({
                factor_plot(norm_counts_log_hypo, rv$gluGeneNames[i], name = TRUE)
            })
            output[[paste0("gene",count+1)]] <- renderPlot({
                factor_plot(norm_counts_log_pitu, rv$gluGeneNames[i], name = TRUE)
            })
            count <- count+2
            
        }


        # Insert alert message for genes not found
        if (length(genes_not_found) > 0) {
            insertUI(
                selector="#alertMsgs",
                where="afterEnd",
                ui=p(sprintf("Gene(s) '%s' cannot be found", paste(genes_not_found, collapse="', '")),
                     style="color: red;")
            )
        }

        # Create new table with all values
        output$pvalTable <- DT::renderDataTable({
            # table_hypo <- all_DEGs_hypo[all_DEGs_hypo$gene_name %in% rv$gluGeneNames,]
            # table_pitu <- all_DEGs_pitu[all_DEGs_pitu$genename %in% rv$gluGeneNames,][c("genename", "PValue", "logFC", "Study")]
            # colnames(table_pitu) <- c("Gene Names", "padj", "log2FC", "Study")
            # colnames(table_hypo) <- c("Gene Names", "padj", "log2FC", "Study")
            # table <- rbind(table_hypo, table_pitu)
            # table$padj <- sprintf("%.4e", table$padj)
            # print(table)
            # colnames(table) <- c("Gene Names", "p-value", "log2FC", "Study")
            # rv$gluTable <- table
            # DT::datatable(table, rownames=FALSE)
            
            table <- all_DEGs[all_DEGs$gene_name %in% rv$gluGeneNames,]
            rv$gluTable <- table
            DT::datatable(table, rownames=FALSE)
        })
    }) # Observe event input submit gene ends
    
    output$gluDownloadTable <- downloadHandler(
        filename = "gene_expression.csv",
        content = function(file) {
            write.csv(rv$gluTable, file = file, row.names = FALSE)
        }
    )

    observeEvent(input$gseGeneNamesUpload, {
        geneNames <- read.table(input$gseGeneNamesUpload$datapath, header = FALSE,as.is=TRUE,sep="\t")
        if(ncol(geneNames) > 1) {
            geneNames <- read.table(input$gseGeneNamesUpload$datapath, header = TRUE,as.is=TRUE,sep="\t")

            rownames(geneNames) <- geneNames$gene_symbol
        } else {
            geneNames <- geneNames$V1
        }
        # TODO: read.table

        outputGSE <- puberty_AP_wrapper(geneNames, ordered = FALSE)

        # Create Plots
        output$gseNormExp <- renderPlot({
            outputGSE$plot
        })
        
        # Create Table
        output$gseTable <- DT::renderDataTable({
            rv$gseTable <- data.frame(outputGSE$table)
            DT::datatable(rv$gseTable, rownames =  FALSE)
        })
    })
    
    output$gseDownloadTable <- downloadHandler(
        filename = "gene_set_enrichment.csv",
        content = function(file) {
            write.csv(rv$gseTable, file = file, row.names = FALSE)
        }
    )
}



# else {
#     hypo_name <- paste0("normExpHypo", i)
#     pitu_name <- paste0("normExpPitu", i)
# 
#     print(hypo_name)
#     print(pitu_name)
#     
#     hypo_plot <- factor_plot(norm_counts_log_hypo, rv$geneNames[i], name = TRUE)
#     pitu_plot <- factor_plot(norm_counts_log_pitu, rv$geneNames[i], name = TRUE)
#     print(hypo_plot)
#     print(pitu_plot)
#     
#     insertUI(
#         selector="#normExp ", where="afterEnd",
#         ui= fluidRow(
#             # column(6, "aslkdjflsadkjflaskjdf"),
#             # column(6, "aslkdjflsadkjflaskjdf")
#             column(6, hypo_plot),
#             column(6, pitu_plot)
#             # column(6, plotOutput(outputId = "normExpHypo1")),
#             # column(6, plotOutput(outputId = "normExpPitu1"))
#         )
#     )
# 
#     output$"normExpHypo1" <- renderPlot({
#         factor_plot(norm_counts_log_hypo, rv$geneNames[i], name = TRUE)
#     })
#     output$"normExpPitu1" <- renderPlot({
#         factor_plot(norm_counts_log_pitu, rv$geneNames[i], name = TRUE)
#     })
#     }



