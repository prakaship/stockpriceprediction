library(shiny)
comp<-read.csv("company.csv")

mypredict<-function(st,horl){
  lm.st<-lm(formula = horl~dateno,data = st)
  
  predict.lm(lm.st,data.frame(dateno=(((tail(st,1))[,1])+1)))
}
doublepredict<-function(st,horl){
  price<-horl
  
  no<-st[,1]
  ReturnOnAssets <- (round((st[,5]/st[,6])*100))
  ProfitMargin <- (round((st[,5]/st[,7])*100))
 
  l<-lm(price~ no+ReturnOnAssets+ProfitMargin)

  predict.lm(l,data.frame((no=(((tail(st,1))[,1])+1)),ReturnOnAssets=0,ProfitMargin=0))
 
  }
singlepredict<-function(st,sing,horl){
  if(sing=="2"){
    price<-horl
    no<-st[,1]
    ReturnOnAssets <- (round((st[,5]/st[,6])*100))
    
    l<-lm(price~ no+ReturnOnAssets)
    predict.lm(l,data.frame((no=(((tail(st,1))[,1])+1)),ReturnOnAssets=0))
  }
  else if(sing=="3"){
    price<-horl
    no<-st[,1]
    ProfitMargin <- (round((st[,5]/st[,7])*100))
    
    l<-lm(price~ no+ProfitMargin)
    predict.lm(l,data.frame((no=(((tail(st,1))[,1])+1)),ProfitMargin=0))
  }
}
high<-function(st,leng,roavar,profitmarginvar,horl){
  if(leng==0){
    mypredict(st,horl)
    
  }
  else {
  
    if(toString(roavar)!="NA"&&toString(profitmarginvar)!="NA"){
      doublepredict(st,horl)
    }
    else if(toString(roavar)!="NA"){
      
      singlepredict(st,roavar,horl)
    }
    else if(toString(profitmarginvar)!="NA"){
      singlepredict(st,profitmarginvar,horl)
    }
    
  }
}

shinyServer(function(input, output) {
  output$data_table <- renderDataTable({
    X <- subset(comp, company==input$sin, select = c(company, link))
    st<-read.csv(toString(X[,2]))
    data.frame(st)
  })
  
  output$graph<-renderPlot({
    X <- subset(comp, company==input$sin, select = c(company, link))
    st<-read.csv(toString(X[,2]))
    lm.st<-lm(formula = high~dateno,data = st)
    lm.low<-lm(formula = low~dateno,data = st)
    plot(high~date,data=st,xlab="date",ylab="price")
    
    abline(lm.st)
    abline(lm.low)
    })
  
  output$highvar <- renderText( {
    X <- subset(comp, company==input$sin, select = c(company, link))
    
    st<-read.csv(toString(X[,2]))
    h<-st[,3]
  
     high(st,length(input$predictors),input$predictors[1],input$predictors[2],h)
    })
  output$lowvar <- renderText( {
    X <- subset(comp, company==input$sin, select = c(company, link))
    
    st<-read.csv(toString(X[,2]))
    h<-st[,4]
    
    high(st,length(input$predictors),input$predictors[1],input$predictors[2],h)
  })
  
})
