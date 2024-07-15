# Shiny App - Customer Segmentation ----

library(shiny)
library(bslib)
library(crosstalk)
library(plotly)
library(reactable)
library(DT)
library(tidyverse)
library(timetk)
library(openxlsx)

# Load Data
customer_predictions_tbl <- readRDS("data/customer_predictions_tbl.rds")
invoice_lines_tbl <- readRDS("data/invoice_lines_tbl.rds")
invoices_tbl <- readRDS("data/invoices_tbl.rds")

# UI
ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "sandstone"), #flatly, lux, sandstone, cosmo
    
    titlePanel("Customer Segmentation Dashboard - Which Customers Purchase again within 90 Days?"),
    
    sidebarLayout(
        sidebarPanel(
            width = 2,
            conditionalPanel(
                condition = "input.tabs == 'Segmentation'",
                plotlyOutput("purchase_time_graph")
            )
        ),
        
        mainPanel(
            tabsetPanel(
                id = "tabs",
                tabPanel("Segmentation",
                         fluidRow(
                             column(5,
                                    div(
                                        style = "border: 1px solid #ccc; padding: 10px;",
                                        plotlyOutput("segmentation_plot", height = "700px")
                                    )
                             ),
                             column(7,
                                    div(
                                        style = "border: 1px solid #ccc; padding: 10px;",
                                        plotlyOutput("customer_habits_plot", height = "755px")
                                    )
                             )
                         )
                ),
                tabPanel("Data Tables",
                         fluidRow(
                             column(6,
                                    downloadButton("export_customers_csv", "Export Customers CSV"),
                                    downloadButton("export_customers_xlsx", "Export Customers XLSX"),
                                    reactableOutput("customer_table", height = "calc(100vh - 200px)")
                             ),
                             column(6,
                                    downloadButton("export_invoices_csv", "Export Invoices CSV"),
                                    downloadButton("export_invoices_xlsx", "Export Invoices XLSX"),
                                    reactableOutput("invoice_lines_table", height = "calc(100vh - 200px)")
                             )
                         )
                )
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    
    # Reactive value to store selected customer_id
    selected_customer_id <- reactiveVal(NULL)
    
    # Update selected_customer_id when a point is clicked
    observe({
        click_data <- event_data("plotly_click", source = "seg_plot")
        if (!is.null(click_data)) {
            selected_customer_id(click_data$key)
        } else {
            selected_customer_id(1)
        }
    })
    
    # Purchases over time graph
    output$purchase_time_graph <- renderPlotly({
        plot_data <- if (is.null(selected_customer_id())) {
            invoices_tbl# %>%
                #group_by(InvoiceDate) %>%
                #summarize(total_purchases = sum(Total))
        } else {
            invoices_tbl %>%
                filter(CustomerId == selected_customer_id()) #%>%
                #group_by(InvoiceDate) %>%
                #summarize(total_purchases = sum(Total))
        }
        
        p <- plot_data %>%
            ggplot(aes(x = InvoiceDate, y = Total)) +
            geom_line(color = "darkblue") +
            labs(title = if (is.null(selected_customer_id())) "Purchases Over Time" else paste("Purchases Over Time for Customer", selected_customer_id()), x = "Date", y = "Total Purchases") +
            theme_minimal() +
            theme(
                axis.title = element_text(size = 6),
                axis.text = element_text(size = 4)
            )
        
        ggplotly(p)
    })
    
    # Segmentation plot
    output$segmentation_plot <- renderPlotly({
        p <- customer_predictions_tbl %>%
            ggplot(aes(x = artist_UMAP19, y = song_len_q50, color = .pred_1, key = CustomerId)) +
            geom_point() +
            scale_color_gradient(low = "darkblue", high = "yellow") +
            labs(title = "Customer Segmentation", x = "Segmentation Variable 1", y = "Segmentation Variable 2", color = "prediction") +
            theme_minimal() +
            theme(
                axis.title = element_text(size = 10),
                axis.text = element_text(size = 8),
                legend.text = element_text(size = 4)
            )
        
        ggplotly(p, source = "seg_plot") %>%
            layout(dragmode = "zoom") %>%  # Changed dragmode to "zoom" to remove brush color menu
            highlight(on = "plotly_click", off = "plotly_doubleclick", dynamic = TRUE, opacityDim = 0.1) %>%
            config(displayModeBar = TRUE, modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    })
    
    # Customer habits plot
    output$customer_habits_plot <- renderPlotly({
        plot_data <- if (is.null(selected_customer_id())) {
            invoice_lines_tbl %>%
                group_by(ArtistName, GenreName) %>%
                summarize(n = sum(Quantity))
        } else {
            invoice_lines_tbl %>%
                filter(CustomerId == selected_customer_id()) %>%
                group_by(ArtistName, GenreName) %>%
                summarize(n = sum(Quantity))
        }
        
        p <- plot_data %>%
            ggplot(aes(x = GenreName, y = ArtistName, fill = n)) +
            geom_tile() +
            scale_fill_gradient(low = "darkblue", high = "yellow") +
            labs(title = if (is.null(selected_customer_id())) "Customer Purchase Habits" else paste("Purchase Habits for Customer", selected_customer_id()), x = "Genre", y = "Artist") +
            theme_minimal() +
            theme(
                axis.title = element_text(size = 10),
                axis.text.x = element_text(size = 5, angle = 45),
                axis.text.y = element_text(size = 5)
            )
        
        ggplotly(p)
    })
    
    # Customer table
    output$customer_table <- renderReactable({
        reactable(
            customer_predictions_tbl,
            searchable = TRUE,
            resizable = TRUE,
            pagination = TRUE,
            outlined = TRUE,
            columns = list(
                .pred_1 = colDef(name = "Prediction Yes"), .pred_0 = colDef(name = "Prediction No")
            )
        )
    })
    
    # Invoice lines table
    output$invoice_lines_table <- renderReactable({
        reactable(
            invoice_lines_tbl,
            searchable = TRUE,
            resizable = TRUE,
            pagination = TRUE,
            outlined = TRUE
            #columns = list(
            #    n = colDef(name = "Number of Purchases")
            #)
        )
    })
    
    # Export customers to CSV
    output$export_customers_csv <- downloadHandler(
        filename = function() { "data/exports_customers.csv" },
        content = function(file) {
            write.csv(customer_predictions_tbl, file)
        }
    )
    
    # Export customers to XLSX
    output$export_customers_xlsx <- downloadHandler(
        filename = function() { "data/exports_customers.xlsx" },
        content = function(file) {
            write.xlsx(customer_predictions_tbl, file)
        }
    )
    
    # Export invoices to CSV
    output$export_invoices_csv <- downloadHandler(
        filename = function() { "data/exports_invoices.csv" },
        content = function(file) {
            write.csv(invoice_lines_tbl, file)
        }
    )
    
    # Export invoices to XLSX
    output$export_invoices_xlsx <- downloadHandler(
        filename = function() { "data/exports_invoices.xlsx" },
        content = function(file) {
            write.xlsx(invoice_lines_tbl, file)
        }
    )
    
}

shinyApp(ui, server)
