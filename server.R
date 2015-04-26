## server.r  - CVD Predictor
 
library(shiny)

CVD_Risk <- function(gender, age, tc, hdl, tbp, sbp, smoker, diabetic) {

  if ( gender == '0') {  # females

  if ( tbp == 1 )          { beta_sbp <- 2.82263 } # treated bp
  else if ( tbp == 0 )      { beta_sbp <- 2.76157 } # untreated bp
  
  sum1 <- 2.32888*log( age )+1.20904*log( tc )-0.70833*log( hdl )+    beta_sbp*log( sbp ) + 0.52873*as.numeric(smoker)+0.69154*as.numeric(diabetic)
  
  sum2 <- 2.32888*3.8686+1.20904*5.3504-0.70833*4.0176+2.76157*4.24+2.82263*0.5826+0.52873*0.3423+0.69154*0.0376
  
  phat <- 1 - 0.95012^exp(sum1-sum2) 
  phat <- round(phat*100,2)
  return( phat )
}

else if ( gender == 1 ) {   # males}  
    
  if ( tbp == 1)          { beta_sbp <- 1.99881 } # treated bp
  else if ( tbp == 0 )      { beta_sbp <- 1.93303 } # untreated bp
  
  sum1 <- 3.06117*log( age )+1.12370*log( tc )-0.93263*log( hdl )+ beta_sbp*log( sbp ) + 0.65451*as.numeric(smoker)+0.57367*as.numeric(diabetic)
  
  sum2 <- 3.06117*3.8560+1.12370*5.3420-0.93263*3.7686+1.93303*4.3544 +1.99881*0.5019+0.65451*0.3522+0.57367*0.0650
  
  phat <- 1 - 0.88936^exp(sum1-sum2)
  phat <- round(phat*100,2)
  return( phat )
 }
}

plot_tc <- function(gender, age, tc, hdl, tbp, sbp, smoker, diabetic) { 
  
  x<- seq(tc,120,-10)
  y<- CVD_Risk(gender, age, x, hdl, tbp, sbp, smoker, diabetic)
  plot(x,y,xlim=c(tc,120),type='l',xlab='tc (mg/dl)', ylab='10 year risk (%)',
       main='your risk vs total cholesterol', sub='lower cholesterol decreases risk',col='red',lwd=4)
}

plot_bp <- function(gender, age, tc, hdl, tbp, sbp, smoker, diabetic) { 
  
  x<- seq(sbp,100,-10)
  y<- CVD_Risk(gender, age, tc, hdl, tbp, x, smoker, diabetic)
  plot(x,y,xlim=c(sbp,100),type='l',xlab='sbp (mmHg)', ylab='10 year risk (%)',
       main='your risk vs systolic blood pressure',col='red',lwd=4)
}

plot_hdl <- function(gender, age, tc, hdl, tbp, sbp, smoker, diabetic) { 
  
  x<- seq(hdl,100,10)
  y<- CVD_Risk(gender, age, tc, x, tbp, sbp, smoker, diabetic)
  plot(x,y,xlim=c(hdl,100),type='l',xlab='HDL (mg/dl)', ylab='10 year risk (%)',
       main='your risk vs HDL',col='red',lwd=4)
}
shinyServer(
  function(input, output) {
    output$inputAge <- renderPrint({input$age})
    
    output$prediction <- renderPrint({CVD_Risk(input$gender, input$age, 
                            input$tc, input$hdl, input$tbp, input$sbp,
                            input$smoker, input$diabetic)})
    output$plot1 <- renderPlot({plot_tc(input$gender, input$age, 
                                        input$tc, input$hdl, input$tbp, input$sbp,
                                        input$smoker, input$diabetic)})
    output$plot2 <- renderPlot({plot_bp(input$gender, input$age, 
                                        input$tc, input$hdl, input$tbp, input$sbp,
                                        input$smoker, input$diabetic)})
    output$plot3 <- renderPlot({plot_hdl(input$gender, input$age, 
                                        input$tc, input$hdl, input$tbp, input$sbp,
                                        input$smoker, input$diabetic)})
    
  }
)

