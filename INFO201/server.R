library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)

#data
college_df <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-mandyrwu/main/Data-Table%201.csv", stringsAsFactors = FALSE)

#Data-Yuchen
state_shape <- map_data("state")

# Filter data
state_enrollment_data <- college_df %>% select(FIPS.state.code, Undergraduate.enrollment, Graduate.enrollment)
state_enrollment_data$FIPS.state.code <- tolower(state_enrollment_data$FIPS.state.code)

undergraduate_enrollment_1 <- state_enrollment_data %>%
  group_by(FIPS.state.code) %>%
  summarize(undergraduate_enrollment = sum(Undergraduate.enrollment, na.rm = TRUE))

graduate_enrollment_1 <- state_enrollment_data %>%
  group_by(FIPS.state.code) %>%
  summarize(graduate_enrollment = sum(Graduate.enrollment, na.rm = TRUE))

stat_enrollment_polygon <- left_join(state_shape, undergraduate_enrollment_1, by = c("region" = "FIPS.state.code"))
stat_enrollment_polygon <- left_join(stat_enrollment_polygon, graduate_enrollment_1, by = c("region" = "FIPS.state.code"))




# Define server logic
my_server <- function(input, output) {
  
  #Mandy's plot
  output$plot_1 <- renderPlotly({
    
    admissions_data <- college_df %>%
      select(Name, Applicants.total, Admissions.total, input$grad_year, Control.of.institution, State.abbreviation) %>%
      filter(State.abbreviation == input$select)
    
    admissions_data <- mutate(admissions_data, "Acceptance.rate" = (Admissions.total/Applicants.total) * 100)
    
    units <- if(input$grad_year == "Graduation.rate...Bachelor.degree.within.4.years..total") "Grad Rate with Bachelor's within 4 Years (%)"
    else if(input$grad_year == "Graduation.rate...Bachelor.degree.within.5.years..total") "Grad Rate with Bachelor's within 5 Years (%)"
    else if(input$grad_year == "Graduation.rate...Bachelor.degree.within.6.years..total") "Grad Rate with Bachelor's within 6 Years (%)"
    
    college_info = paste0("College: ", admissions_data$Name, "\n", "Acceptance Rate: ", format(round(admissions_data$Acceptance.rate, 3), nsmall = 3), "%")
    
    scatterplot <- ggplot(data = admissions_data) +
      geom_point(mapping = aes_string(x = "Acceptance.rate", 
                                      y = input$grad_year, 
                                      color = "Control.of.institution",
                                      text = "college_info")) +
      labs(title = paste0("Colleges in ", input$select, ": Acceptance Rate vs Undergrad Graduation Rate"),
           x = "Acceptance Rate (%)",
           y = units, 
           col = "Institution Type") +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
    
    return(ggplotly(scatterplot, tooltip = "text"))
  })
  
  
  #Bonnie's plot
  output$plot_2 <- renderPlotly({
    
    location_college_df <- college_df %>%
      group_by(Degree.of.urbanization..Urban.centric.locale.) %>%
      filter(Degree.of.urbanization..Urban.centric.locale. %in% input$geographic_select) %>%
      summarize(enrollment = sum(Undergraduate.enrollment))
    
    barchart <- ggplot(data = location_college_df) +
      geom_col(mapping = aes(
        x = enrollment,
        y = Degree.of.urbanization..Urban.centric.locale.,
        text = paste("</br> Undergraduate erollment: ", enrollment),
        fill = Degree.of.urbanization..Urban.centric.locale.
      ), stat = "identity") +
      labs(
        title = "Undergrad Enrollment Percentage by Geographic Location",
        x = "Undergraduate Erollment",
        y = "Geographic Location"
      )
    ggplotly(barchart, tooltip = "text")
  })
  
  #Yuchen's plot
  
  output$plot_3<- renderPlotly({
    filtered_data <- stat_enrollment_polygon %>%
      select(long, lat, group, order, region, subregion, input$type_selection) %>%
      filter(region %in% input$state_selection)
    
    map_plot <- ggplot(data = state_shape) +
      geom_polygon(aes(
        x = long,
        y = lat,
        group = group,
      )) +
      geom_polygon(
        data = filtered_data,
        aes(
          x = long,
          y = lat,
          group = group,
          fill = eval(as.name(input$type_selection))
        )
      ) +
      scale_fill_continuous(
        low = "blue",
        high = "red",
        labels = label_number_si()
      ) +
      coord_map() +
      labs(
        title = "University enrollments across United States",
        fill = "Number of Students",
        x = "Longitude",
        y = "Latitude",
      )
    return(ggplotly(map_plot))
  })

  
  
}
