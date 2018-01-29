shinyServer(function(input, output, session) {
  
  observe({
    input$btn
    session$sendCustomMessage(type = "resetFileInputHandler", "file1") 
  })
  
})