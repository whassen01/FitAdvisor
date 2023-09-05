
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)

# Interface utilisateur (UI)
ui <- dashboardPage(
  dashboardHeader(title = "FitAdvisor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "tab1", icon = icon("home")),
      menuItem("Fitness", tabName = "tab2"),
      menuItem("Nutrition", tabName = "tab3"),
      menuItem("Graphics", tabName = "tab4"),
      menuItem("Map", tabName = "tab5")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab2",
              fluidPage(
                titlePanel(HTML("BMI, BFP, MAS <br/>My Fitness Plan")),
                sidebarLayout(
                  sidebarPanel(
                    # Entrées utilisateur
                    selectInput("sexe", "Sex :", choices = c("Male", "Female")),
                    numericInput("age", "Age :", value = 30, min = 1, max = 100),
                    numericInput("taille", "Height (cm) :", value = 170, min = 50, max = 300),
                    numericInput("poids", "Weight (kg) :", value = 70, min = 1, max = 500),
                    selectInput("niveau_activite", "Level of activity :", choices = c("Sedentary", "Lightly Active", "Moderately Active", "Very Active")),
                    numericInput("distance_12min", "Distance covered in 12 minutes (km) :", value = 2, min = 0, max = 6, step = 0.1),
                    actionButton("calculer_feedback", "My BMI, BFP, MAS"),
                    actionButton("afficher_programme", "My Fitness plan")
                  ),
                  
                  mainPanel(
                    h3("Results :"),
                    uiOutput("imc_output"),
                    uiOutput("imc_feedback"),
                    uiOutput("img_output"),
                    uiOutput("img_feedback"),
                    uiOutput("vma_output"),
                    uiOutput("vma_feedback"),
                    h4("What are your objectives?"),
                    selectInput("objectif", "Objetive :", choices = c("Weight loss", "Cardio", "Muscle Toning")),
                    conditionalPanel(
                      condition = "input.objectif == 'Muscle Toning'", 
                      selectInput("niveau_condition_physique", "Fitness level :", choices = c("Beginner", "Intermediate", "Advanced"))
                    ),
                    uiOutput("programme_sportif_output"),
                    p("Note: Repeat the same workout each week, but slightly increase the intensity or duration of the sessions if possible.
        Be sure to consult a health professional or sports coach before starting any exercise program, especially if you have health problems or are just starting out.")
                  )
                )
              )
      ),
      tabItem(tabName = "tab1",
              fluidPage(

                tags$img(src='https://i.pinimg.com/originals/63/dc/95/63dc95d6ac2d13ed84b2824774fefd81.jpg', height=200, width=200),
                
                
                HTML("
                <h1>FitAdvisor<h1>
           <h3>Discover a Healthier Version of Yourself!</h3>
           
           <p>Welcome to our all-in-one health and fitness application, a user-friendly platform designed to support you on your journey to a healthier lifestyle. Leveraging the versatility of R Shiny, our application offers a multitude of features tailored to your unique health goals.</p>
           
           <hr>
           
           <h4>Feature Highlights</h4>
           
           <strong>Customized Workout Plans</strong>: Based on your fitness level and goals, our application crafts engaging and effective workout routines. Your journey to physical fitness just got upgraded with a personal trainer!<br><br>
           
           <strong>Personalized Nutritional Recommendations</strong>: Enter your details to receive personalized nutritional advice based on your daily caloric intake, helping you make informed choices daily.<br><br>
           
           <strong>Health Calculators</strong>: With our integrated calculators, easily monitor your Body Mass Index (BMI), Body Fat Percentage (BFP), and Maximum Aerobic Speed (MAS).<br><br>
           
           <strong>Sports Facilities Locator</strong>: Find nearby sports facilities with our interactive map. Whether it's a gym, swimming pool, or other, we've got you covered, facilitating the maintenance of an active lifestyle with convenience.<br><br>
           
           <hr>
           
           <h4>Take Control of Your Well-being</h4>
           
           <p>In the modern world, taking control of your health and well-being has never been more critical. Our application aims to provide individuals with the tools and knowledge necessary to lead a balanced and healthy life. With a focus on personalization, we aspire to be your trusted companion in your quest for health and happiness.</p>
           
           <p>Join us in this approach to well-being. Your path to a healthier and happier life starts here!</p>
           "),
          
                
                tags$img(src = "https://media.giphy.com/media/xT5LMBc1W9lVSheDnO/giphy.gif", height = "200px", width = "300px")

              )
              ), 
      tabItem(tabName = "tab4",
              fluidPage(
                titlePanel("Graphics"),
                fluidRow(
                  column(6, plotOutput("plot_hommes")),
                  column(6, plotOutput("plot_femmes")),
                  column(6, plotOutput("pie_hommes")),
                  column(6, plotOutput("pie_femmes"))
                )
              )
      )
      ,
      tabItem(tabName = "tab5",
              fluidPage(
                titlePanel("Map of sports facilities in Geneva "),
                leafletOutput("map")
              )
      ),
      tabItem(tabName = "tab3",
              fluidPage(
                titlePanel("Daily calorie intake calculator"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("sexe_cal", "Sex :", choices = c("Male", "Female")),
                    numericInput("age_cal", "Age :", value = 30, min = 1, max = 100),
                    numericInput("taille_cal", "Height (cm) :", value = 170, min = 50, max = 300),
                    numericInput("poids_cal", "Weight (kg) :", value = 70, min = 1, max = 500),
                    selectInput("activite_cal", "Level of activity :", choices = c("Sedentary", "Lightly Active", "Moderately Active", "Very Active")),
                    actionButton("calculer_calorique", "Calculate calorie intake")
                  ),
                  mainPanel(
                    h3("Results :"),
                    uiOutput("calorique_output"),
                    p("Note: Please note that this estimate is for guidance only and may differ slightly from your actual energy requirements. It's important to adjust your calorie intake according to your individual needs. Other factors, such as sleep quality and stress management, also play an important role in your overall health.")
                  )
                ) 
              )
      )
    )
  )
)


# Fonction pour calculer l'IMC
calculate_imc <- function(poids, taille) {
  imc <- poids / ((taille/100)^2)
  return(imc)
}

# Fonction pour donner le feedback de l'IMC
get_imc_feedback <- function(imc) {
  if (imc < 16.5) {
    return("Skinny")
  } else if (imc >= 16.5 & imc < 18.5) {
    return("Skinny")
  } else if (imc >= 18.5 & imc < 25) {
    return("Normal")
  } else if (imc >= 25 & imc < 30) {
    return("Overweight")
  } else if (imc >= 30 & imc < 35) {
    return("Moderate obesity")
  } else if (imc >= 35 & imc < 40) {
    return("Severe Obesity")
  } else {
    return("Severe Obesity")
  }
}

# Fonction pour calculer l'IMG
calculate_img <- function(poids, taille, age, sexe) {
  img <- 1.20 * calculate_imc(poids, taille) + 0.23 * age - 10.8 * sexe - 5.4
  return(img)
}

# Fonction pour donner le feedback de l'IMG
get_img_feedback <- function(img) {
  if (img < 15) {
    return("Low")
  } else if (img >= 15 & img < 25) {
    return("In good health")
  } else if (img >= 25 & img < 30) {
    return("Overweight")
  } else {
    return("Obese")
  }
}

# Fonction pour calculer la VMA
calculate_vma <- function(distance_km) {
  vma <- distance_km / 2 * 10
  return(vma)
}

# Fonction pour donner le feedback de la VMA
get_vma_feedback <- function(vma) {
  if (vma < 10) {
    return("Bad")
  } else if (vma >= 10 & vma < 12) {
    return("Mediocre")
  } else if (vma >= 12 & vma < 14) {
    return("Low")
  } else if (vma >= 14 & vma < 16) {
    return("Average")
  } else if (vma >= 16 & vma < 18) {
    return("Good")
  } else if (vma >= 18 & vma < 20) {
    return("Very good")
  } else {
    return("Excellent")
  }
}

calculate_apport_calorique <- function(poids, taille, age, sexe, activite) {
  poids <- as.numeric(poids)
  taille <- as.numeric(taille)/100
  age <- as.numeric(age)
  
  
  activite_coeff <- c("Sedentary" = 1, "Lightly Active" = 1.375, "Moderately Active" = 1.55, "Very Active" = 1.725)
  
  
  # Calcul de l'énergie de base 
  if (sexe == "Femme") {
    mb <- (0.963 * poids^0.48 * taille^0.50 * age^-0.13) * (1000/4.1855)
  } else {
    mb <- (1.083 * poids^0.48 * taille^0.50 * age^-0.13) * (1000/4.1855)
  }
  
  # Calcul de l'apport énergétique journalier 
  apport_journalier <- mb * activite_coeff[activite]
  
  return(round(apport_journalier, -2))
}


# Serveur
server <- function(input, output) {
  # Calculateur d'IMC, d'IMG et de VMA
  observeEvent(input$calculate, {
    weight <- input$weight
    height <- input$height / 100  
    age <- input$age
    sex <- ifelse(input$sex == "Man", 1, 0)
    distance <- input$distance
    
    # Calcul de l'IMC, l'IMG et la VMA 
    imc <- calculate_imc(weight, height)
    img <- calculate_img(weight, height, age, sex)
    vma <- calculate_vma(distance) 
    
    # Feedback 
    feedback_imc <- get_imc_feedback(imc)
    feedback_img <- get_img_feedback(img)
    feedback_vma <- get_vma_feedback(vma)
    
    # Afficher résultats
    output$imc <- renderText(paste("IMC :", round(imc, 1)))
    output$img <- renderText(paste("IMG :", round(img, 1)))
    output$vma <- renderText(paste("VMA :", round(vma, 1)))
    output$feedback_imc <- renderText(paste("IMC :", feedback_imc))
    output$feedback_img <- renderText(paste("IMG :", feedback_img))
    output$feedback_vma <- renderText(paste("VMA :", feedback_vma))
  })
  
  observeEvent(input$calculer_feedback, {
    imc <- calculate_imc(input$poids, input$taille)
    img <- calculate_img(input$poids, input$taille, input$age, ifelse(input$sexe == "Male", 1, 0))
    vma <- calculate_vma(input$distance_12min)
    
    output$imc_output <- renderUI({
      tags$p(HTML(paste("BMI (Body Mass Index) : <b style='color:red'>", round(imc, 2), "</b>")))
    })
    output$imc_feedback <- renderUI({
      tags$p(HTML(paste("Interpretation of your BMI : <b>", get_imc_feedback(imc), "</b>")))
    })
    output$img_output <- renderUI({
      tags$p(HTML(paste("BFP (Body Fat Percentage) : <b style='color:red'>", round(img, 2), "%</b>")))
    })
    output$img_feedback <- renderUI({
      tags$p(HTML(paste("Interpretation of your BFP : <b>", get_img_feedback(img), "</b>")))
    })
    output$vma_output <- renderUI({
      tags$p(HTML(paste("MAS (Maximum Aerobic Speed) : <b style='color:red'>", round(vma, 2), "km/h</b>")))
    })
    output$vma_feedback <- renderUI({
      tags$p(HTML(paste("Interpretation of your MAS : <b>", get_vma_feedback(vma), "</b>")))
    })
    
    
    
    
  })
  
  output$programme_sportif_output <- renderUI({
    if (input$afficher_programme && input$objectif == "Muscle Toning") {
      if (input$niveau_condition_physique == "Beginner") {
        return(
          tags$div(
            tags$p("Strength Training Workout Program (Beginner) (2-3 times a week):"),
            tags$ul(
              tags$li("Push-ups (4 sets of 12 repetitions)"),
              tags$li("Pull-ups (4 sets of 12 repetitions)"),
              tags$li("Dumbbell press (4 sets of 12 repetitions)"),
              tags$li("Squat(4 sets of 12 repetitions)"),
              tags$li("Floor crunch (4 sets of 10 repetitions) + Plank 3*1 minute")
            )
          )
        )
      } else if (input$niveau_condition_physique == "Intermediate") {
        return(
          tags$div(
            tags$p("Strength Training Workout Program (Intermediate) (3-4 times a week):"),
            tags$ul(
              tags$li("Neck bar Squats (4 sets of 12 repetitions)"),
              tags$li("Bench Press (4 sets of 10 repetitions)"),
              tags$li("Deadlifts (4 sets of 8 repetitions)"),
              tags$li("Crunches (3 sets of 20 repetitions)"),
              tags$li("Pull-Ups (4 sets of 6 repetitions)")
            ) 
          )
        )
      } else {
        return(
          tags$div(
            tags$p("Strength Training Workout Program (Advanced) (4-5 times a week):"),
            tags$ul(
              tags$li("Squats (5 sets of 12 repetitions)"),
              tags$li("Bench Press (5 sets of 10 repetitions)"),
              tags$li("Deadlifts (5 sets of 8 repetitions)"),
              tags$li("Crunches (4 sets of 25 repetitions)"),
              tags$li("Pull-Ups (5 sets of 8 repetitions)")
            )
          )
        )
      }
    } else if (input$afficher_programme && input$objectif == "Weight loss") {
      return(
        list(
          tags$p(strong("Here are three workouts you can incorporate into your weight loss fitness program: Swimming, Running, or Boxing")),
          tags$p("Swimming", style = "font-size: 1.5em;"),
          tags$p("This program can be performed 3 to 4 times a week."),
          tags$p("Warm-Up (5-10 minutes):"),
          tags$p("Start with a light warm-up by swimming a few lengths at a comfortable pace. You can alternate between different strokes, such as breaststroke, freestyle, or backstroke."),
          tags$p("Cardio Training (20-30 minutes):"),
          tags$p("Perform interval series by swimming at moderate to intense intensity for a certain time, followed by a period of active recovery (slow swimming). For example, swim 1 to 2 lengths at high speed, then recover by swimming gently for 30 seconds to 1 minute. Repeat this for 20 to 30 minutes."),
          tags$p("Strength Training (15-20 minutes):"),
          tags$p("Use swimming accessories such as kickboards or fins to target the upper body and leg muscles more. For example, perform leg kick sets with a board or arm movements with fins."),
          tags$p("Endurance Training (20-30 minutes):"),
          tags$p("Swim continuously at a moderate to fast pace for 20 to 30 minutes. You can choose a specific stroke or alternate between different strokes throughout the session."),
          tags$p("Stretching (5-10 minutes):"),
          tags$p("Finish the session with stretches to promote flexibility and reduce the risk of soreness."),
          tags$p("Running", style = "font-size: 1.5em;"),
          tags$p("Day 1: Fast walk for 5 minutes to warm up, then alternate 1 minute of slow running followed by 1 minute of walking for 20 minutes. Finish with a 5-minute walk to recover."),
          tags$p("Day 2: Rest or low-intensity activity such as walking."),
          tags$p("Day 3: Fast walk for 5 minutes to warm up, then run at a comfortable pace for 25 minutes. Finish with a 5-minute walk to recover."),
          tags$p("Day 4: Rest or low-intensity activity such as walking."),
          tags$p("Day 5: Fast walk for 5 minutes to warm up, then alternate 1 minute of slow running followed by 1 minute of walking for 20 minutes. Finish with a 5-minute walk to recover."),
          tags$p("Day 6: Rest."),
          tags$p("Day 7: Rest."),
          tags$p("Boxing", style = "font-size: 1.5em;"),
          tags$p("This program can be performed 3 to 4 times a week."),
          tags$p("Warm-Up (5-10 minutes):"),
          tags$p("Start with a light warm-up, such as jumping rope or jogging in place, to raise the body temperature and prepare your body for the effort."),
          tags$p("Technical Training (20-30 minutes):"),
          tags$p("Work on the basic techniques of boxing, including punches (jab, hook, uppercut) and foot movements (shuffles, dodges, etc.). You can do this using a punching bag or performing shadow boxing exercises (air boxing)."),
          tags$p("Cardio Training (20-30 minutes):"),
          tags$p("Alternate fast and intense striking series on the punching bag or in the air with periods of active recovery (light walking or jogging in place). For example, perform 1 minute of intense strikes followed by 30 seconds of recovery, repeat this for 20 to 30 minutes."),
          tags$p("Strength Training (15-20 minutes):"),
          tags$p("Perform strength exercises such as push-ups, squats, lunges, and burpees. These exercises help to build strength and boost metabolism."),
          tags$p("Abdominal Training (10-15 minutes):"),
          tags$p("Finish the session with exercises targeting the abdominal muscles, like crunches, planks, and leg raises."),
          tags$p("Stretching (5-10 minutes):"),
          tags$p("Finish the session with stretches to promote flexibility and reduce the risk of soreness."),
          tags$p("The most important thing is to practice regularly and adapt the training to your level and objectives. Don't hesitate to consult a professional to personalize your program.")
        )
        )
    } else if (input$afficher_programme && input$objectif == "Cardio") {
      vma <- calculate_vma(input$distance_12min)
      return(
        tags$div(
          tags$p("Workout Program (Cardio):"),
          
          tags$p("SESSION 1 - MAS 1h10:"),
          tags$ul(
            tags$li(paste("30-minute jog at fundamental endurance pace.")),
            tags$li(paste("2 sets of 10 x 30 seconds of effort at", round(vma, 2), "km/h, with 30 seconds of recovery between each effort.")),
            tags$li("3 minutes of recovery between sets."),
            tags$li("End the session with 5 to 10 minutes of slow jogging.")
          ),
          
          tags$p("SESSION 2 - JOGGING 45':"),
          tags$ul(
            tags$li(paste("45-minute jog at a fundamental endurance pace at", round(vma*0.65, 2), "km/h."))
          ),
          
          tags$p("SESSION 3 - MAS 1h10:"),
          tags$ul(
            tags$li(paste("20 to 30 minutes of jogging at fundamental endurance pace at", round(vma*0.65, 2), "km/h.")),
            tags$li(paste("2 sets of 5 x 400m at", round(vma*0.95, 2), "km/h, with 1 minute and 15 seconds of recovery between each 400m.")),
            tags$li("3 minutes of recovery between sets."),
            tags$li("End the session with 5 to 10 minutes of slow jogging.")
          ),
          
          tags$p("SESSION 4 - JOGGING 60':"),
          tags$ul(
            tags$li(paste("60-minute jog at fundamental endurance pace at", round(vma*0.65, 2), "km/h."))
          )
        ))
    }
  })
  
  
  #nutrition
  
  observeEvent(input$calculer_calorique, {
    poids <- as.numeric(input$poids_cal)
    taille <- as.numeric(input$taille_cal)
    age <- as.numeric(input$age_cal)
    sexe <- input$sexe_cal
    activite <- input$activite_cal
    
    apport_calorique <- calculate_apport_calorique(poids, taille, age, sexe, activite)
    
    output$calorique_output <- renderUI({
      tags$div(
        tags$p(tags$b("Your daily energy intake is", tags$span(apport_calorique, style = "color:red;"), "Kcal.")),
        tags$p("To maintain or lose weight, establish a nutritional plan that does not exceed this level. Adhere to this macronutrient ratio for weight management:"),
        tags$ul(
          tags$li("30% for proteins"),
          tags$li("40% for carbohydrates"),
          tags$li("30% for fats")
        ),
        tags$p(""), 
        tags$p(style = "text-align: center; font-weight: bold; padding-bottom: 10px;", "Example of daily plan: 2200 kcal"),
        tags$p(style = "font-weight: bold;", "Breakfast"),
        tags$p("250g of 0% fat cottage cheese"),
        tags$p("80g of strawberries, fresh or frozen"),
        tags$p("Sprinkle with cinnamon to add flavor"),
        tags$p("1 slice of whole grain bread accompanied by 85g of thinly sliced turkey escalope"),
        
        tags$p(style = "font-weight: bold;", "Morning Snack"),
        tags$p("125g of 0% yogurt with a touch of vanilla"),
        
        tags$p(style = "font-weight: bold;", "Lunch: Composed Salad"),
        tags$p("Leafy green vegetables (lettuce, spinach) – as much as you want"),
        tags$p("160g of mixed sliced vegetables (carrots, peppers, tomato)"),
        tags$p("170g of grilled chicken escalope"),
        tags$p("150g of cooked white beans"),
        tags$p("2 tablespoons (30g) of low-fat salad dressing"),
        tags$p("A light dessert: 1 mandarin"),
        
        tags$p(style = "font-weight: bold;", "Afternoon Snack"),
        tags$p("30g of soybeans for snacking"),
        tags$p("Carrot and celery sticks for a crunchy touch"),
        
        tags$p(style = "font-weight: bold;", "Dinner"),
        tags$p("200g of grilled salmon seasoned with a splash of lemon"),
        tags$p("160g of steamed green beans, topped with a touch of garlic"),
        tags$p("300g of cooked brown rice to accompany"),
        tags$p("Various salad leaves – as much as you want"),
        tags$p("¼ of a normal sized avocado to garnish the salad"),
        tags$p("2 tablespoons (30g) of low-fat salad dressing"),
        
        tags$p(style = "font-weight: bold;", "Evening Snack"),
        tags$p("1 orange for a sweet note at the end of the day"),
        
        tags$p(""),
        tags$p("Be sure to stay hydrated throughout the day."),
        tags$p("Adjust the quantities according to your personal needs and preferences."),
        
        tags$p(tags$b(HTML("<a href='https://www.weightwatchers.com/ch/fr/recette'>Find more healthy recipe ideas here</a>")))
        
      )
    })
    
    
  })
  
  # Graphiques
  output$plot_hommes <- renderPlot({
    data <- data.frame(
      Age = c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
      Surpoids_Homme = c(20.3, 33.7, 39.9, 42.9, 43.5, 48.3, 43.7),
      Obesite_Homme = c(5.1, 9.2, 11, 14.4, 17.4, 17.7, 11.7),
      Surpoids_Femme = c(11.8, 16.9, 19.3, 24, 25.8, 32.5, 32.2),
      Obesite_Femme = c(3, 5.7, 9.1, 12.1, 15.3, 14.1, 12.5)
    )
    
    ggplot(data, aes(x = Age)) +
      annotate("text", x = Inf, y = Inf, label = "Source: OFS 2020", 
               hjust = 1.1, vjust = 2, size = 3, color = "black") +
      geom_bar(aes(y = Surpoids_Homme, fill = "Overweight (Men)"), stat = "identity", width = 0.4) +
      geom_text(aes(y = Surpoids_Homme, label = sprintf("%.1f", Surpoids_Homme)), vjust = 1.5, color = "white") +
      geom_bar(aes(y = Obesite_Homme, fill = "Obesity (Men)"), stat = "identity", width = 0.4) +
      geom_text(aes(y = Obesite_Homme, label = sprintf("%.1f", Obesite_Homme)), vjust = 1.5, color = "white") +
      labs(title = "Overweight and obesity among Swiss men by age group",
           x = "Age group",
           y = "Percentage",
           fill = "Category") +
      scale_fill_manual(values = c("#e41a1c", "#377eb8"),
                        labels = c("Obesity (Men)", "Overweight (Men)")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.text = element_text(size = 10))
  })
  
  
  output$plot_femmes <- renderPlot({
    data <- data.frame(
      Age = c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
      Surpoids_Homme = c(20.3, 33.7, 39.9, 42.9, 43.5, 48.3, 43.7),
      Obesite_Homme = c(5.1, 9.2, 11, 14.4, 17.4, 17.7, 11.7),
      Surpoids_Femme = c(11.8, 16.9, 19.3, 24, 25.8, 32.5, 32.2),
      Obesite_Femme = c(3, 5.7, 9.1, 12.1, 15.3, 14.1, 12.5)
    )
    
    ggplot(data, aes(x = Age)) +
      annotate("text", x = Inf, y = Inf, label = "Source: OFS 2020", 
               hjust = 1.1, vjust = 2, size = 3, color = "black")  +
      geom_bar(aes(y = Surpoids_Femme, fill = "Overweight (Women)"), stat = "identity", width = 0.4) +
      geom_text(aes(y = Surpoids_Femme, label = sprintf("%.1f", Surpoids_Femme)), vjust = 1.5, color = "white") +
      geom_bar(aes(y = Obesite_Femme, fill = "Obesity (Women)"), stat = "identity", width = 0.4) +
      geom_text(aes(y = Obesite_Femme, label = sprintf("%.1f", Obesite_Femme)), vjust = 1.5, color = "white") +
      labs(title = "Overweight and obesity among Swiss women by age group",
           x = "Age group",
           y = "Percentage",
           fill = "Category") +
      scale_fill_manual(values = c("#4daf4a", "#984ea3"),
                        labels = c("Obesity (Women)", "Overweight (Women)")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.text = element_text(size = 10))
  })
  
  # Données pour les hommes
  data_hommes <- data.frame(
    Groupe = c("Inactive", "Partially active", "Active enough", "Trained"),
    Pourcentage = c(7.2, 15, 42.8, 35),
    Genre = "Men"
  )
  
  # Données pour les femmes
  data_femmes <- data.frame(
    Groupe = c("Inactive", "Partially active", "Active enough", "Trained"),
    Pourcentage = c(9.1, 17.4, 46.7, 26.8),
    Genre = "Femmes"
  )
  
  # Combinaison des données
  data_combined <- rbind(data_hommes, data_femmes)
  
   
  # Création du pie chart pour les hommes
  pie_chart_hommes <- ggplot(data_combined[data_combined$Genre == "Men", ], aes(x = "", y = Pourcentage, fill = Groupe)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(round(Pourcentage, 1), "%")), position = position_stack(vjust = 0.5)) +
    labs(title = "Physical activity rates for men in Switzerland (OFS 2018)",
         fill = "Activity group",
         x = NULL,
         y = NULL) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.text = element_text(size = 12)
    )
  
  # Création du pie chart pour les femmes
  pie_chart_femmes <- ggplot(data_combined[data_combined$Genre == "Femmes", ], aes(x = "", y = Pourcentage, fill = Groupe)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(round(Pourcentage, 1), "%")), position = position_stack(vjust = 0.5)) +
    labs(title = "Physical activity rates for women in Switzerland (OFS 2018)",
         fill = "Activity group",
         x = NULL,
         y = NULL) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.text = element_text(size = 12)
    )
  output$pie_hommes <- renderPlot({
    print(pie_chart_hommes)
  })
  
  output$pie_femmes <- renderPlot({
    print(pie_chart_femmes)
  })
  
  
  
  
  
  # Carte de Genève
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 6.1432, lat = 46.2044, zoom = 12) %>%
      addTiles() %>%
      addMarkers(lng = 6.1056217, lat = 46.2099606,
                 popup = paste0("<b>Centre sportif du Bois-des-Frères</b><br>",
                                "Chemin de l'Ecu 22<br>",
                                "1219 Châtelaine<br>",
                                "Tel. +41 22 418 87 00<br>",
                                "<a href='https://www.geneve.ch/fr/centre-sportif-bois-freres'>More information</a>")
      ) %>%
      addMarkers(lng = 6.1567152, lat = 46.1806627,
                 popup = paste0("<b>Centre sportif du Bout-du-Monde</b><br>",
                                "Route de Vessy 12<br>",
                                "1206 Genève<br>",
                                "Tel. +41 22 418 48 50<br>",
                                "Fax + 41 22 418 48 51<br>",
                                "<a href='https://www.geneve.ch/fr/centre-sportif-bout-monde'>More information</a>")
      ) %>%
      addMarkers(lng = 6.1309869, lat = 46.1963385,
                 popup = paste0("<b>Centre sportif de la Queue-d'Arve</b><br>",
                                "Rue François-DUSSAUD 12<br>",
                                "1227 Les Acacias<br>",
                                "Tel. +41 22 418 44 44<br>",
                                "<a href='https://www.geneve.ch/fr/actualites/reservez-terrains-sportifs-queue-arve-clic'>More information</a>")
      ) %>%
      addMarkers(lng = 6.1370498, lat = 46.2186461,
                 popup = paste0("<b>Centre sportif de Varembé</b><br>",
                                "Rue de Vermont 33<br>",
                                "1202 Genève<br>",
                                "Tel. +41 22 418 93 33<br>",
                                "<a href='https://www.geneve.ch/fr/centre-sportif-varembe'>More information</a>")
      ) %>%
      addMarkers(lng = 6.1623639, lat = 46.1761702,
                 popup = paste0("<b>Centre sportif de Vessy</b><br>",
                                "Route de Vessy 31<br>",
                                "1234 Vessy<br>",
                                "Tel. +41 22 418 48 70<br>",
                                "Fax +41 22 418 48 71<br>",
                                "<a href='https://www.geneve.ch/fr/centre-sportif-vessy'>More information</a>")
      ) %>%
      addMarkers(lng = 6.0917993, lat = 46.188765,
                 popup = paste0("<b>Centre intercommunal de sports, loisirs et nature des Evaux</b><br>",
                                "Chemin François-CHAVAZ 110<br>",
                                "1213 Onex<br>",
                                "Tel. +41 22 879 85 85<br>",
                                "info@evaux.ch<br>",
                                "<a href='https://www.evaux.ch/'>More information</a>")
      ) %>%
      addMarkers(lng = 6.1692478, lat = 46.2085361,
                 popup = paste0("<b>Tennis club de Genève - Eaux-Vives</b><br>",
                                "Parc des Eaux-Vives<br>",
                                "1207 Genève<br>",
                                "Tel. +41 22 735 53 50<br>")
      ) %>%
      addMarkers(lng = 6.1433362, lat = 46.1904507,
                 popup = paste0("<b>Salle de tennis de table des Minoteries</b><br>",
                                "Rue de Carouge 100<br>",
                                "1205 Genève<br>",
                                "Tel. +41 22 320 62 93<br>")
      ) %>%
      addMarkers(lng = 6.1622096, lat = 46.2045212,
                 popup = paste0("<b>Salle de boxe de la rue de l'Avenir</b><br>",
                                "Rue de l'Avenir 34<br>",
                                "1207 Genève<br>",
                                "Tel. +41 78 878 22 23<br>",
                                "info@obcgeneve.ch<br>")
      ) %>%
      addMarkers(lng = 6.1017751, lat = 46.187152,
                 popup = paste0("<b>Piscine d'Onex-Parc</b><br>",
                                "Rue des Bossons 5<br>",
                                "1213 Onex<br>",
                                "Tel. +41 22 792 93 56<br>",
                                "<a href='https://www.onex.ch/mes-loisirs/sport/installations-sportives/piscine-618'>More information</a>")
      ) %>%
      addMarkers(lng = 6.136718, lat = 46.1870477,
                 popup = paste0("<b>Piscine des Pervenches</b><br>",
                                "Avenue de la Praille 20<br>",
                                "1227 Carouge GE<br>",
                                "Tel. +41 22 307 93 15<br>",
                                "<a href='https://www.carouge.ch/piscine-des-pervenches'>More information</a>")
      ) %>%
      addMarkers(lng = 6.1007482, lat = 46.2041746,
                 popup = paste0("<b>Piscine du Lignon</b><br>",
                                "Route du Bois-des-Frères 30<br>",
                                "1219 Le Lignon<br>",
                                "Tel. +41 22 306 07 70<br>",
                                "<a href='https://www.vernier.ch/lieux/piscine-du-lignon'>More information</a>")
      ) 
  })
}


shinyApp(ui, server)



