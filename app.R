library(httr) 
library(jsonlite) 
library(tidyverse)
library(readr)
library(stringr)
library(rvest)
library(countrycode)
library(priceR)
library(shiny)
library(scales)

data<-GET("https://api.worldbank.org/v2/country?region=SSA&format=json")
mydata<-content(data,as="text")
mydata<-fromJSON(mydata)
mydata<-as.data.frame(mydata)

mydata$income_level<-mydata$incomeLevel$id

mydata <- mydata %>%
    mutate(countrycode=countrycode(mydata$name, "country.name", destination = "iso3c")) %>%
    select(countrycode,name,income_level)

##############Per Capita#########################################################
mydata2<-read_html("https://en.wikipedia.org/wiki/List_of_African_countries_by_GDP_(PPP)_per_capita#cite_note-1")
mydata2<-html_table(mydata2)
mydata2<-data.frame(mydata2)
names(mydata2)<-c("name","Per_capita")

mydata2$countrycode<-countrycode(mydata2$name, "country.name", destination = "iso3c")

mydata2= mydata2 %>%
    select(countrycode,Per_capita)

##################Ease of doing business##################
mydata3<-read_html("https://tradingeconomics.com/country-list/ease-of-doing-business?continent=africa")
mydata3<-html_table(mydata3)
mydata3<-data.frame(mydata3)

mydata3= mydata3 %>%
    mutate(countrycode=countrycode(mydata3$Country, "country.name", destination = "iso3c")) %>%
    select(countrycode,Last)
names(mydata3)=c("countrycode","EDB")

#############################################################################

mydata4=read.csv("https://raw.githubusercontent.com/stanleynyoro/Web-Scrapping-Nairobi-House-Prices/master/fdi_data.csv")

names(mydata4)=c("country","FDI_2020")

mydata4= mydata4 %>%
    mutate(countrycode=countrycode(mydata4$country, "country.name", destination = "iso3c")) %>%
    select(countrycode,FDI_2020)

convert.brackets <- function(x){
    if(grepl("\\(.*\\)", x)){
        paste0("-", gsub("\\(|\\)", "", x))
    } else {
        x
    }
}

mydata4$FDI_2020=sapply(mydata4$FDI_2020, convert.brackets, USE.NAMES = F)

mydata4$FDI_2020 <- str_replace_all(mydata4$FDI_2020, " ", "")

mydata4$FDI_2020<-parse_number(mydata4$FDI_2020)

newdata=mydata %>%
    inner_join(mydata3,by="countrycode") %>%
    inner_join(mydata2,by="countrycode") %>%
    inner_join(mydata4,by="countrycode") %>%
    distinct(name, .keep_all= TRUE) %>%
    select(-countrycode)
newdata$Per_capita=parse_number(newdata$Per_capita)
newdata$Per_capita=as.numeric(newdata$Per_capita)
newdata$EDB=as.numeric(newdata$EDB)
newdata$income_level=as.factor(newdata$income_level)
newdata$income_level=recode_factor(newdata$income_level,LMC="Low Middle Income",
                                   LIC="Low Income Country",
                                   UMC="Upper Middle Income")

ui <- fluidPage(
    theme=shinythemes::shinytheme('cerulean'),
    titlePanel("Africa Countries by Economic Performance"),
    HTML("<p>The data has been sourced from the World Bank <a href='https://api.worldbank.org/v2/country?region=SSA&format=json'>API</a> and
         <a href='https://tradingeconomics.com/country-list/ease-of-doing-business?continent=africa'> Trading Economics</a>. Joining was done through R dplyr.</p>"),
    sidebarLayout(
        sidebarPanel(
    selectInput("income_level","Country Income Level",unique(newdata$income_level))),
    mainPanel(
        tabsetPanel(
            tabPanel("Classification table",tableOutput("table.income")),
            tabPanel("Ease of doing business in 2019 (available)",plotOutput("edb")),
            tabPanel("FDI Earned in 2020",plotOutput("fdi"))
        )
    )
)
)

server <- function(input, output) {
    income.country=reactive({
        newdata %>%
            filter(income_level==input$income_level) 
    })
    output$table.income=renderTable({
        income.country() %>%
            select(name,income_level)
    })
    
    output$edb=renderPlot({
        first.plot=income.country() 
        ggplot(first.plot,aes(x = reorder(name,-EDB),EDB))+
            theme_classic()+
            xlab("Country Name") + ylab("Position on EDB") +
            geom_bar(stat = 'identity',fill = "#FF6666") +
            ylim(0, 200) +
            coord_flip()
    })
    output$fdi=renderPlot({
        second.plot=income.country()
        options(scipen = 999)
        ggplot(second.plot,aes(x = reorder(name,FDI_2020),FDI_2020))+
            theme_classic()+
            xlab("Country Name") + ylab("FDI Amt in USD") +
            geom_bar(stat = 'identity',fill = "#FF6666") +
            scale_y_continuous(labels = comma) +
            coord_flip()
    })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
