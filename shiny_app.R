# load library
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(readr)       
library(readxl)
library(DT)

library(tidyverse)
library(plotly)
library(padr)
library(scales)
library(zoo)
library(glue)

library(rnaturalearth)
library(leaflet)
library(ggrepel)
library(GGally)
library(gridExtra)
library(ggpubr)
library(sf)      
library(tigris)

library(forecast)


# Data
dat <- read_csv("data.csv")
dat <- dat %>% dplyr::select(-...1)
dat <- dat %>% dplyr::select(-KOTA, -`TIPE CUSTOMER`, -`KODE SALESMAN`)
dat <- dat %>% mutate(KATEGORI = replace_na(KATEGORI,"AKSESORIS"))
dat <- dat %>% drop_na(`NAMA SALESMAN`)
dat <- dat %>% distinct()
dat <- dat %>% mutate(QUANTITY = abs(QUANTITY),
                      `TOTAL RUPIAH` = abs(`TOTAL RUPIAH`))

# format_rupiah
format_rupiah <- function(amount) {
  format(amount, big.mark = ".", decimal.mark = ",", scientific = FALSE, prefix = "Rp ")
}

# Trend Distributor
distributor_rupiah <- dat %>% group_by(`KODE DISTRIBUTOR`,`TOTAL RUPIAH`) %>% 
  summarise(urut = min(TANGGAL))

penjualan_distributor <- distributor_rupiah %>% 
                         mutate(y_m = as.yearmon(urut)) %>% 
                         group_by(`KODE DISTRIBUTOR`,y_m) %>% 
                         summarize(total = sum(`TOTAL RUPIAH`)) %>% 
                         mutate(rupiah = format_rupiah(total),
                                detail = glue("Bulan: {y_m}
                                               Penjualan: {rupiah}"))

grafik_distributor <- ggplot(penjualan_distributor,
                             aes(x = y_m,
                                 y = total,
                                 group = 1,
                                 colour = `KODE DISTRIBUTOR`)) +
  geom_line(aes(text = detail)) +
  theme_get() +
  labs(x = "Bulan",
       y = "Total (Rupiah)",
       title = "Penjualan Per Distributor") +
  scale_y_continuous(label = scales::format_format(big.mark = ".",
                                                   decimal.mark = ",",
                                                   scientific = F))
## hasil
ggplotly(grafik_distributor, tooltip = "text")


# Trend Subkategori
subkategori_rupiah <- dat %>% group_by(SUBKATEGORI,`TOTAL RUPIAH`) %>% 
  summarise(urut = min(TANGGAL))

penjualan_subkategori <- subkategori_rupiah %>% 
  mutate(y_m = as.yearmon(urut)) %>% 
  group_by(SUBKATEGORI,y_m) %>% 
  summarize(total = sum(`TOTAL RUPIAH`)) %>% 
  mutate(rupiah = format_rupiah(total),
         detail = glue("Bulan: {y_m}
                        Subkategori: {SUBKATEGORI}
                        Penjualan: {rupiah}"))

grafik_subkategori <- ggplot(penjualan_subkategori,
                             aes(x = y_m,
                                 y = total,
                                 group = 1,
                                 colour = SUBKATEGORI)) +
  geom_line(aes(text = detail)) +
  theme_get() +
  labs(x = "Bulan",
       y = "Total (Rupiah)",
       title = "Penjualan Per Subkategori") +
  scale_y_continuous(label = scales::format_format(big.mark = ".",
                                                   decimal.mark = ",",
                                                   scientific = F))

##hasil
ggplotly(grafik_subkategori, tooltip = "text")


# Trend Produk
produk_rupiah <- dat %>% group_by(`NAMA PRODUK`,`TOTAL RUPIAH`) %>% 
  summarise(urut = min(TANGGAL))

penjualan_produk <- produk_rupiah %>% 
  mutate(y_m = as.yearmon(urut)) %>% 
  group_by(`NAMA PRODUK`,y_m) %>% 
  summarize(total = sum(`TOTAL RUPIAH`)) %>% 
  mutate(rupiah = format_rupiah(total),
         detail = glue("Bulan: {y_m}
                                             Produk: {`NAMA PRODUK`}
                                             Penjualan: {rupiah}"))

grafik_produk <- ggplot(penjualan_produk,
                        aes(x = y_m,
                            y = total,
                            group = 1,
                            colour = `NAMA PRODUK`)) +
  geom_line(aes(text = detail)) +
  theme_get() +
  labs(x = "Bulan",
       y = "Total (Rupiah)",
       title = "Penjualan Per Produk") +
  scale_y_continuous(label = scales::format_format(big.mark = ".",
                                                   decimal.mark = ",",
                                                   scientific = F))
## Hasil
ggplotly(grafik_produk, tooltip = "text") %>% 
  layout(showlegend = FALSE)


# Trend Salesman
sales_rupiah <- dat %>% group_by(`NAMA SALESMAN`,`TOTAL RUPIAH`) %>% 
  summarise(urut = min(TANGGAL))

penjualan_sales <- sales_rupiah %>% 
  mutate(y_m = as.yearmon(urut)) %>% 
  group_by(`NAMA SALESMAN`,y_m) %>% 
  summarize(total = sum(`TOTAL RUPIAH`)) %>% 
  mutate(rupiah = format_rupiah(total),
         detail = glue("Bulan: {y_m}
                                             Sales: {`NAMA SALESMAN`}
                                             Penjualan: {rupiah}"))

grafik_sales <- ggplot(penjualan_sales,
                       aes(x = y_m,
                           y = total,
                           group = 1,
                           colour = `NAMA SALESMAN`)) +
  geom_line(aes(text = detail)) +
  theme_get() +
  labs(x = "Bulan",
       y = "Total (Rupiah)",
       title = "Penjualan Per Salesman") +
  scale_y_continuous(label = scales::format_format(big.mark = ".",
                                                   decimal.mark = ",",
                                                   scientific = F))
## hasil
ggplotly(grafik_sales, tooltip = "text") 


# Persebaran Provinsi Distributor
prov_distributor <- dat %>% 
                    dplyr::select(PROVINSI, `KODE DISTRIBUTOR`) %>% 
                    distinct() %>% 
                    group_by(PROVINSI,`KODE DISTRIBUTOR`) %>% 
                    summarise(total = n()) %>% 
                    mutate(detail = glue("Distributor: {`KODE DISTRIBUTOR`}"))
prov_distributor <- prov_distributor %>% 
                    mutate(PROVINSI = case_when(
                           PROVINSI == "DKI JAKARTA" ~ "Jakarta Raya",
                           TRUE ~ PROVINSI),
                           PROVINSI = case_when(
                           PROVINSI == "JAWA TENGAH" ~ "Jawa Tengah",
                           TRUE ~ PROVINSI))


indonesia <- st_read("gadm41_IDN_shp/gadm41_IDN_1.shp")
indonesia <-  indonesia %>% dplyr::select(NAME_1)

map_dist <- left_join(indonesia, prov_distributor, by = c("NAME_1"="PROVINSI"))

grafik_map_dist <- ggplot(map_dist,aes(colour = NAME_1)) +
                   geom_sf(aes(text = detail)) +
                   scale_fill_gradient(low = "white", high = "red", name = "Jumlah Distributor") +
                   labs(title = "Persebaran Distributor di Indonesia") +
                   theme_minimal()

## hasil
ggplotly(grafik_map_dist, tooltip = "text")


# Persebaran Provinsi Jumlah Customer
prov_cust <- dat %>% 
             dplyr::select(PROVINSI, `KODE CUSTOMER`) %>% 
             distinct() %>% 
             group_by(PROVINSI) %>% 
             summarise(total = n()) %>% 
             mutate(detail = glue("Provinsi: {PROVINSI}
                                  Jumlah Customer: {total}"))
prov_cust <- prov_cust %>% 
             mutate(PROVINSI = case_when(
                    PROVINSI == "DKI JAKARTA" ~ "Jakarta Raya",
                    TRUE ~ PROVINSI),
                    PROVINSI = case_when(
                    PROVINSI == "JAWA TENGAH" ~ "Jawa Tengah",
                    TRUE ~ PROVINSI))

map_cust <- left_join(indonesia, prov_cust, by = c("NAME_1"="PROVINSI"))

grafik_map_cust <- ggplot(map_cust,aes(colour = NAME_1)) +
  geom_sf(aes(text = detail)) +
  scale_fill_gradient(low = "white", high = "red", name = "Jumlah Customer") +
  labs(title = "Persebaran Customer di Indonesia") +
  theme_minimal()

## hasil
ggplotly(grafik_map_cust, tooltip = "text")



# Top 5 produk dengan porsi penjualan tertinggi 
high_sales <- dat %>% group_by(`NAMA PRODUK`) %>% 
  summarize(sales = sum(`TOTAL RUPIAH`)) %>% 
  arrange(desc(sales)) %>%
  head(5) %>%
  mutate(Produk = as.factor(`NAMA PRODUK`),
         Produk = reorder(Produk,sales))

grafik_top5 <- ggplot(high_sales, 
                      aes(x = Produk,
                          y = sales)) +
  geom_bar(stat = "identity",
           show.legend = F,
           aes(fill = Produk,
               text = sales)) +
  labs(title = "Top 5 Produk Dengan Penjualan Tertinggi",
       x = "") +
  coord_flip() + 
  theme_get()


## hasil
ggplotly(grafik_top5, tooltip = "text") %>%
  layout(showlegend = F)



## prediksi total omset 3 bulan kedepan
omset <- dat %>% group_by(`TOTAL RUPIAH`) %>% 
  summarise(urut = min(TANGGAL))

omset <- omset %>% 
  mutate(y_m = as.yearmon(urut)) %>% 
  group_by(y_m) %>% 
  summarize(total = sum(`TOTAL RUPIAH`)) %>% 
  mutate(data = "Actual")

omset <- omset %>% arrange((y_m))


ts_data <- ts(omset$total, start = c(2022, 1), frequency = 12)


fit <- auto.arima(ts_data)
fit <- forecast(fit,h=3)
fit <- data.frame(y_m = as.yearmon(c("Aug 2023", "Sep 2023", "Oct 2023")),
                  total = as.numeric(fit$mean))
fit <- fit %>% mutate(data = "Predicted")
combined_data <- rbind(omset, fit)

pred <- ggplot(combined_data, aes(x = y_m)) +
        geom_line(aes(y = total, color = data))
        labs(title = "Actual vs. Predicted Values", x = "y_m", y = "total") +
        scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

## hasil
ggplotly(pred)




ui <- shinyUI(
  
  dashboardPage(
    skin = "black-light",
    title = "Visualisation Dashboard",
    dashboardHeader(
      
      title = "Visualisation Dashboard"
      
    ),
    dashboardSidebar(
      
      sidebarMenu(
        menuItem(
          text = "Overview",
          tabName = "page1",
          badgeLabel = "new",
          badgeColor = "green",
          icon = icon("gear")
        ),
        menuItem(
          text = "Persebaran Provinsi",
          tabName = "page2",
          icon = icon("gear")
        ),
        menuItem(
          text = "Prediction",
          tabName = "page3",
          icon = icon("gear")
        )
      )
    ),
    body = dashboardBody(
      tabItems(
      tabItem(
        tabName = "page1",
        
        
        fluidRow(
          
          box(
            title = "Select Product Name", 
            closable = TRUE, 
            enable_label = TRUE,
            label_status = "danger",
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            width = 4,
            height = "600px",
            
            selectInput(
              inputId = "prodname", 
              label = "Select Product Name", 
              choices = c(high_sales$`NAMA PRODUK`),
              selected = "Sportwear"
            ),
            
            dataTableOutput(
              outputId = "highsales"
            )
          ),
          
          box(
            title = "Top 5 Product based on Sales", 
            closable = TRUE, 
            enable_label = TRUE,
            label_status = "danger",
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            width = 8,
            height = "600px",
            
            plotlyOutput(
              outputId = "graph1"
            )
          )
        ),
        fluidRow(
          
          box(
            title = "Trend Distributor", 
            closable = TRUE, 
            enable_label = TRUE,
            label_status = "danger",
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            width = 6,
            height = "600px",
            
            plotlyOutput(
              outputId = "graph2"
            )
          ),
          box(
            title = "Trend Subkategori", 
            closable = TRUE, 
            enable_label = TRUE,
            label_status = "danger",
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            width = 6,
            height = "600px",
            
            plotlyOutput(
              outputId = "graph3"
            ) 
            
          )
        ),
        fluidRow(
          
          box(
            title = "Trend Produk", 
            closable = TRUE, 
            enable_label = TRUE,
            label_status = "danger",
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            width = 6,
            height = "600px",
            
            plotlyOutput(
              outputId = "graph4"
            )
          ),
          box(
            title = "Trend Salesman", 
            closable = TRUE, 
            enable_label = TRUE,
            label_status = "danger",
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            width = 6,
            height = "600px",
            
            plotlyOutput(
              outputId = "graph5"
            ) 
            
          )
        )
      ),
      tabItem(
        tabName = "page2",
        fluidRow(
          box(
            title = "Distributor Name", 
            closable = TRUE, 
            enable_label = TRUE,
            label_status = "danger",
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            width = 4,
            height = "600px",
            
            selectInput(
              inputId = "distname", 
              label = "Distributor Name", 
              choices = c(prov_distributor$`KODE DISTRIBUTOR`)
            ),
            
            dataTableOutput(
              outputId = "distmap"
            )
          ),
          box(
            title = "Persebaran Provinsi Berdasarkan Distributor", 
            closable = TRUE, 
            enable_label = TRUE,
            label_status = "danger",
            status = "primary", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            width = 8,
            height = "600px",
            
            plotlyOutput(
              outputId = "graph6"
            )
          )
      ),
      fluidRow(
        box(
          title = "Total Customer", 
          closable = TRUE, 
          enable_label = TRUE,
          label_status = "danger",
          status = "primary", 
          solidHeader = FALSE, 
          collapsible = TRUE,
          width = 4,
          height = "600px",
          
          selectInput(
            inputId = "cust", 
            label = "Total Customer", 
            choices = c(prov_cust$PROVINSI)
          ),
          
          dataTableOutput(
            outputId = "custmap"
          )
        ),
        box(
          title = "Persebaran Provinsi Berdasarkan Customer", 
          closable = TRUE, 
          enable_label = TRUE,
          label_status = "danger",
          status = "primary", 
          solidHeader = FALSE, 
          collapsible = TRUE,
          width = 8,
          height = "600px",
          
          plotlyOutput(
            outputId = "graph7"
          )
        )
      )
    ),
    tabItem(
      tabName = "page3",
      fluidRow(
        box(
          title = "Prediksi Total Omset 3 Bulan Kedepan", 
          closable = TRUE, 
          enable_label = TRUE,
          label_status = "danger",
          status = "primary", 
          solidHeader = FALSE, 
          collapsible = TRUE,
          width = 12,
          height = "600px",
          
          plotlyOutput(
            outputId = "graph8"
          )
        )
      )
    )
   )
  )
)
)



server <- function(input, output, session) {
  
  output$highsales <- renderDataTable({
    
    highsales <- high_sales %>% dplyr::select(-Produk) %>% 
                 mutate(sales = format_rupiah(sales))
    datatable(highsales)
    
  })
  
  output$graph1 <- renderPlotly(
    
    ggplotly(grafik_top5, tooltip = "text") %>%
      layout(showlegend = F)
  )
  
  output$graph2 <- renderPlotly(
    
    ggplotly(grafik_distributor, tooltip = "text")
    
  )
  
  output$graph3 <- renderPlotly(
    
    ggplotly(grafik_subkategori, tooltip = "text")
  )
  
  output$graph4 <- renderPlotly(
    
    ggplotly(grafik_produk, tooltip = "text") %>% 
      layout(showlegend = FALSE)
  )
  
  output$graph5 <- renderPlotly(
    
    ggplotly(grafik_sales, tooltip = "text") 
  )
  
  output$distmap <- renderDataTable({
    
    distmap <- prov_distributor %>% dplyr::select(-total,-detail) 
    datatable(distmap)
    
  })
  
  output$graph6 <- renderPlotly(
    
    ggplotly(grafik_map_dist, tooltip = "text") 
  )
  
  output$custmap <- renderDataTable({
    
    custmap <- prov_cust %>% dplyr::select(PROVINSI,total) 
    datatable(custmap)
    
  })
  
  output$graph7 <- renderPlotly(
    
    ggplotly(grafik_map_cust, tooltip = "text") 
  )
  
  output$graph8 <- renderPlotly(
    
    ggplotly(pred)
  )
  
}

shinyApp(ui, server)

















