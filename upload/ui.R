shinyUI(bootstrapPage(
  
  fileInput('file1', 'Choose File'),
  actionButton("btn", "Trigger server to reset file input"),
  
  tags$script('
    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
        var id = "#" + x + "_progress";      # name of progress bar is file1_progress
        var idBar = id + " .bar";  
        $(id).css("visibility", "hidden");   # change visibility
        $(idBar).css("width", "0%");         # reset bar to 0%
    });
  ')
))