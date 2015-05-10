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
CVD_pts<-function(gender, age, tc, hdl, tbp, sbp, smoker, diabetic) {
  pts<-0
  risk<-0.0
  heart<-0  # heart age
  
  if(gender==1){ #male
    if( age>=35 & age<40) {pts<-pts+2}
    if( age>=40 & age<45) {pts<-pts+5}
    if( age>=45 & age<50) {pts<-pts+6}
    if( age>=50 & age<55) {pts<-pts+8}
    if( age>=55 & age<60) {pts<-pts+10}
    if( age>=60 & age<65) {pts<-pts+11}
    if( age>=65 & age<70) {pts<-pts+12}
    if( age>=70 & age<75) {pts<-pts+14}
    if( age>=75         ) {pts<-pts+15}
    
    if( tc>=160 & tc<200) {pts<-pts+1}
    if( tc>=200 & tc<239) {pts<-pts+2}
    if( tc>=240 & tc<279) {pts<-pts+3}
    if( tc>=280         ) {pts<-pts+4}
    
    if( hdl>=60         ) {pts<-pts-2}
    if( hdl>=50 & hdl<60) {pts<-pts-1}
    if( hdl>=45 & hdl<50) {pts<-pts}
    if( hdl>=35 & hdl<45) {pts<-pts+1}
    if( hdl<35          ) {pts<-pts+2}
    
    if( sbp<120 & tbp==0) pts<-pts-2
    if( sbp>=120 & sbp<130 & tbp==0) pts<-pts
    if( sbp>=130 & sbp<140 & tbp==0) pts<-pts+1
    if( sbp>=140 & sbp<160 & tbp==0) pts<-pts+2
    if( sbp>=160 &           tbp==0) pts<-pts+3
    
    if( sbp>=120 & sbp<130 & tbp==1) pts<-pts+2
    if( sbp>=130 & sbp<140 & tbp==1) pts<-pts+3
    if( sbp>=140 & sbp<160 & tbp==1) pts<-pts+4
    if( sbp>=160 &           tbp==1) pts<-pts+5
    
    if( smoker==1) pts<-pts+4
    if( diabetic==1) pts<-pts+3
    
  
    if(pts<=-3) risk<-1.0
    if(pts==-2) risk<-1.1
    if(pts==-1) risk<-1.4
    if(pts==0)  risk<-1.6
    risk<-switch(pts, 1.9,2.3,2.8,3.3,3.9,4.7,5.6,6.7,7.9,9.4,11.2,13.2,15.6,
                 18.4,21.6,25.3,29.4)
    if(pts>=18) risk<-30
    
    
    if(pts<=0) heart<-30
    heart<-switch(pts,32,34,36,38,40,42,45,48,51,54,57,60,64,68,72,76)
    if(pts>=17) heart<-80
   # print(heart)
    
  }  
  if(gender==0){ #female
    if( age>=35 & age<40) {pts<-pts+2}
    if( age>=40 & age<45) {pts<-pts+4}
    if( age>=45 & age<50) {pts<-pts+5}
    if( age>=50 & age<55) {pts<-pts+7}
    if( age>=55 & age<60) {pts<-pts+8}
    if( age>=60 & age<65) {pts<-pts+9}
    if( age>=65 & age<70) {pts<-pts+10}
    if( age>=70 & age<75) {pts<-pts+11}
    if( age>=75         ) {pts<-pts+12}
    
    if( tc>=160 & tc<200) {pts<-pts+1}
    if( tc>=200 & tc<239) {pts<-pts+3}
    if( tc>=240 & tc<279) {pts<-pts+4}
    if( tc>=280         ) {pts<-pts+5}
    
    if( hdl>=60         ) {pts<-pts-2}
    if( hdl>=50 & hdl<60) {pts<-pts-1}
    if( hdl>=45 & hdl<50) {pts<-pts}
    if( hdl>=35 & hdl<45) {pts<-pts+1}
    if( hdl<35          ) {pts<-pts+2}
    
    if( sbp<120 & tbp==0) pts<-pts-3
    if( sbp>=120 & sbp<130 & tbp==0) pts<-pts
    if( sbp>=130 & sbp<140 & tbp==0) pts<-pts+1
    if( sbp>=140 & sbp<150 & tbp==0) pts<-pts+2
    if( sbp>=150 & sbp<160 & tbp==0) pts<-pts+4
    if( sbp>=160 &           tbp==0) pts<-pts+5
    
    if( sbp>=120 & sbp<130 & tbp==1) pts<-pts+2
    if( sbp>=130 & sbp<140 & tbp==1) pts<-pts+3
    if( sbp>=140 & sbp<150 & tbp==1) pts<-pts+5
    if( sbp>=150 & sbp<160 & tbp==1) pts<-pts+6
    if( sbp>=160 &           tbp==1) pts<-pts+7
    
    if( smoker==1) pts<-pts+3
    if( diabetic==1) pts<-pts+4
    
   
    if(pts==-2) risk<-1.0
    if(pts==-1) risk<-1.0
    if(pts==0)  risk<-1.2
    risk<-switch(pts, 1.5,1.7,2.0,2.4,2.8,3.3,3.9,4.5,5.3,6.3,7.3,8.6,10.0,11.7,13.7,15.9,
                 18.5,21.5,24.8,28.5)
    if(pts>=21) risk<-30
       
    if(pts<1) heart<-30
    heart<-switch(pts,31,34,36,39,42,45,48,51,55,59,64,68,73,79,80)
    if(pts>15) heart<-80
      
  }
 return(heart) 
}

plot_tc <- function(gender, age, tc, hdl, tbp, sbp, smoker, diabetic) { 
  
  x<- seq(tc,120,-10)
  y<- CVD_Risk(gender, age, x, hdl, tbp, sbp, smoker, diabetic)
  plot(x,y,xlim=c(tc,120),type='l',xlab='tc (mg/dl)', ylab='10 year risk (%)', 
       main='your risk vs total cholesterol', sub='',col='red',lwd=4)
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
    output$heart <- renderPrint({CVD_pts(input$gender, input$age, 
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

