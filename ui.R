
  ## ui.R  - CVD Predictor, an app programmed by Colin M. Wright
  
library(shiny)

shinyUI(navbarPage("Coursera Data Products Project",
      tabPanel("Cardiovascular Disease (CVD) Predictor",
          pageWithSidebar(
             headerPanel("Input Your Data:"),
               
             sidebarPanel(
               numericInput('age', 'enter your age:', 0, min = 10, max = 100, step = 5),
               numericInput('tc', 'total cholesterol (mg/dl)', 200, min = 100, max = 300, step = 5),
               numericInput('hdl', 'hdl (mg/dl)', 50, min = 10, max = 100, step = 5),
               numericInput('sbp', 'systolic blood pressure (mm/Hg)', 125, min = 90, max = 300, step = 5),
               radioButtons('gender', "Gender", c("female" = 0,"male" = 1), inline=TRUE, selected=0),
               radioButtons("tbp", "Treated blood pressure?", c("yes" = "1","no" = "0"), inline=TRUE, selected="0"),
               radioButtons("diabetic", "diabetic?", c("yes" = "1","no" = "0"), inline=TRUE, selected="0"),
               radioButtons("smoker", "do you currently smoke?", c("yes" = "1","no" = "0"),selected="0", inline=TRUE),
               submitButton('Submit')
             ),
             mainPanel(
               h2('Predict your risk of Heart Attack or Stroke'),
               h5('This app accepts as input your health data and computes your risk of a first cardiovascular event. Graphs are also presented 
                  which show how your risk would change if you improved your blood pressure and cholesterol levels. You can even see how your risk improves
                  by changing your smoking or diabetes status. Simply enter your age and change the default parameters, such as whether or not you are on 
                  blood pressure medication, as needed on the panel on the left. Your systolic blood pressure in the upper number of your blood pressure, 
                  e.g. 130 from 130/80. Hdl is your high density cholesterol.'),
               h3('Results of prediction'),
               h5('Your percent 10 year risk of a CVD event (coronary, cerebrovascular, or peripheral arterial disease or heart failure) is:'),
               #verbatimTextOutput("inputAge"),
               verbatimTextOutput("prediction"),
               #TextOutput("prediction"),
               plotOutput('plot1')
              )
        ) ),
      tabPanel("More graphs & info",
               pageWithSidebar(
                 headerPanel("More info"),
                 sidebarPanel(
                   p('The algorithm is based on :'),
                   a("the Framington Heart Study (2008).", href="http://circ.ahajournals.org/content/117/6/743.full"),
                   br(), br(),
                   p( 'The authors used Cox proportional-hazards regression to evaluate the 
                      risk of developing a first CVD event in 8491 Framingham study participants 
                      (mean age, 49 years; 4522 women) who attended a routine examination 
                      between 30 and 74 years of age and were free of CVD. Sex-specific multivariable 
                      risk functions ("general CVD" algorithms) were derived that incorporated age, 
                      total and high-density lipoprotein cholesterol, systolic blood pressure, 
                      treatment for hypertension, smoking, and diabetes status. They assessed the 
                      performance of the general CVD algorithms for predicting individual CVD events 
                      (coronary heart disease, stroke, peripheral artery disease, or heart failure). 
                      Over 12 years of follow-up, 1174 participants (456 women) developed a first CVD 
                      event.' ),
                   br(),
                   p('If you have a 5% risk, this means that 5 people out of 100 (with your parameters) 
                     will have a CVD event within 10 years.'),
                   br(),
                   p('For information into how to improve your health, check out '),
                   a("Nutrition Facts", href="http://nutritionfacts.org/video/how-to-prevent-high-blood-pressure-with-diet/")
                   ),                
               mainPanel(
                 h3('Graphs of blood pressure and HDL vs Risk'),
                 plotOutput('plot2'),
                 plotOutput( 'plot3')
               )
          ))
))


