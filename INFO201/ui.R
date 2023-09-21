library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)
library(bslib)
library(maps)
library(scales)

source("server.R")

#Data
college_df <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-mandyrwu/main/Data-Table%201.csv", stringsAsFactors = FALSE)

# Data-Yuchen
state_shape <- map_data("state")
state_enrollment_data <- college_df %>% select(FIPS.state.code, Undergraduate.enrollment, Graduate.enrollment)
state_enrollment_data$FIPS.state.code <- tolower(state_enrollment_data$FIPS.state.code)

undergraduate_enrollment_1 <- state_enrollment_data %>%
  group_by(FIPS.state.code) %>%
  summarize(undergraduate_enrollment = sum(Undergraduate.enrollment, na.rm = TRUE))

graduate_enrollment_1 <- state_enrollment_data %>%
  group_by(FIPS.state.code) %>%
  summarize(graduate_enrollment = sum(Graduate.enrollment, na.rm = TRUE))

stat_enrollment_polygon <- left_join(state_shape, undergraduate_enrollment_1, by = c("region" = "FIPS.state.code"))
stat_enrollment_polygon <- left_join(stat_enrollment_polygon , graduate_enrollment_1, by = c("region" = "FIPS.state.code"))



#UI

my_theme <- bs_theme(bg = "#f2f2f2", #background color
                     fg = "#16508a", #foreground color
                     primary = "#c3eefa", # primary color
) 

#Evelyn
intro_tab <- tabPanel("Introduction",
                      titlePanel("Welcome to College Admission Study App!"),
                      mainPanel(
                        h3("Overview"),
                        p("Welcome to our Shiny App on the Factors that Influence College Admissions in the United States."),
                        img(src = "https://images.squarespace-cdn.com/content/v1/5c70bcfc16b6403c514e5a3f/94037304-28b2-432c-b172-8573c2c977a4/stand+out+in+college+admissions", height = "300px", width = "400px"),
                        p("Our research project aims to answer several major questions related to the college admissions process, including how a student's demographics, academic performance, standardized test scores, and extracurricular activities impact their chances of being admitted to college, what the demographic composition of college students in the US is and how it has changed over time, and how the graduation rate of colleges varies based on student demographics."),
                        p("To answer these questions, we are using a dataset on US colleges and universities from the National Center for Education Statistics (NCES). The dataset includes information on over 7,000 institutions and covers a wide range of variables related to enrollment, admissions, financial aid, faculty, and degrees awarded. You can access the original dataset at "),
                        a("https://public.opendatasoft.com/explore/dataset/us-colleges-and-universities/table/", href = "https://public.opendatasoft.com/explore/dataset/us-colleges-and-universities/table/", style = "color: red;"),
                        p("While this dataset provides valuable insights into the factors that influence college admissions, it also raises important ethical questions and limitations to consider. For example, the dataset includes sensitive information on race and ethnicity that could be subject to discrimination, and the data may not be complete or up-to-date for all institutions. As such, we are taking care to use the data responsibly and to acknowledge its limitations in our analysis."),
                        p("We hope that our Shiny App provides a useful and informative tool for exploring the factors that influence college admissions in the United States. Thank you for your interest in our research project!")
                      )
)


#Mandy's tab
chart_1_tab <- tabPanel("Graduation Rates vs Selectivity",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput(inputId = "select", 
                                  label = h4("Select a state"), 
                                  choices = c(unique(college_df$State.abbreviation)), 
                                  selected = "Alabama"),
                      
                      radioButtons(inputId ="grad_year", 
                                   label = h4("Select the time frame for graduating with a Bachelor's degree"),
                                   choices = c("Within 4 Years" = "Graduation.rate...Bachelor.degree.within.4.years..total", "Within 5 Years" = "Graduation.rate...Bachelor.degree.within.5.years..total", "Within 6 Years" = "Graduation.rate...Bachelor.degree.within.6.years..total"), 
                                   selected = NULL)
                    ),
                    
                    mainPanel(
                      plotlyOutput(outputId = "plot_1"),
                      br(),
                      h4("Chart Description"),
                      p("This chart attempts to understand whether the selectivity of a college plays a role in the undergrad graduation rate. To do this, the chart examines a college's admission rate, and maps that against the college's graduation rate for a bachelor's degree. The graph allows you to see how the mapping changes when the time frame for graduating with a bachelor's is 4 years, 5 years, or 6 years for all 50 states and DC. Each data point represents a college, and the color indicates whether it is a private or public institution. From looking at the chart, a general trend is that private institutions tend to have higher graduation rates than public institutions, with prominent examples being colleges in California and Indiana.")
                    )
                 )
                  
)

#Bonnie's tab

select_widget <- selectInput(
  inputId = "geographic_select",
  label = h4("Select a geographic location"),
  choices = college_df$Degree.of.urbanization.Urban.centric.locale,
  selectize = TRUE,
  multiple = TRUE,
  selected = "City: Large"
)

chart_2_tab <- tabPanel("Undergrad Enrollment by Geographic Location",
                        sidebarLayout(
                          sidebarPanel(
                            select_widget,
                          ),
                          mainPanel(
                            plotlyOutput(outputId = "plot_2"),
                            br(),
                            h4("Chart Description"),
                            p("This bar chart highlights the comparison of college undergraduate enrollment in each geographic location as selected with the widgets on the left panel. In addition, for each geographic location, the numbers are present as a sum of all selected geographic locations. We can use this chart to find which geographic location has the highest and lowest undergraduate enrollment numbers, and we can make further analyses and comparisons based on the chart.")
                          )
                        )
)

#Yuchen's tab

select_widget_Yuchen <- selectInput(
  inputId = "state_selection",
  label = h4("Select a state"),
  choices = unique(stat_enrollment_polygon$region),
  selectize = TRUE,
  multiple = TRUE,
  selected = "california"
)

radio_widget_Yuchen <- radioButtons(
  inputId = "type_selection",
  label = h4("Select a degree"),
  choices = c( "Undergradaute" = "undergraduate_enrollment", "Graduate" = "graduate_enrollment"),
  selected = NULL
)

main_panel_plot <- mainPanel(
  plotlyOutput(outputId = "plot_3"),
  br(),
  h4("Chart Description"),
  p("This map chart helps to visualize and analyze the university enrollments across the United States. By presenting the total university enrollments in the form of map, the graph allows you to visually see the distribution of enrollments in each state. To do this, the chart firstly examines a degree choice, bachelor's degree or master's degree, from users. Then the chart examines the states that users are interested in. The color of each state indicates the number of students. Red color represents high population and blue represents low population. The map chart reveals the states with highest number of students enrolled are California, Texas, and New York. The states with the lowest number of students are Alaska, Wyoming, and Vermont." )
)

chart_3_tab <- tabPanel("University enrollment by state",
                        sidebarLayout(
                          sidebarPanel(
                            radio_widget_Yuchen,
                            select_widget_Yuchen),
                          main_panel_plot
                        )
)


#Evelyn
conclusion_tab <- tabPanel("Conclusion",
                           mainPanel(
                             p("Based on the description of the Graduation Rates vs Selectivity chart, one major takeaway from our analysis is that private institutions tend to have higher graduation rates than public institutions. This trend is consistent across the majority of states in the US, as shown in the chart. It is worth noting that this finding is not surprising, as private institutions typically have higher tuition fees and are more selective in their admissions process. This means that students attending private institutions are more likely to have higher academic achievement and financial resources, which can lead to higher graduation rates.")
                           )
                           )


my_ui <- navbarPage(theme = my_theme,
                    "College Data in the US", 
                    intro_tab,
                    chart_1_tab,
                    chart_2_tab,
                    chart_3_tab,
                    conclusion_tab)
