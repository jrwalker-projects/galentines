library(shiny); library(praise); library(rcorpora); library(magick); library(magrittr); library(colourpicker)

ui <- fluidPage(
  titlePanel("Galentine's Card Generator"),
  
  fluidRow(
    column(4, wellPanel(
      
      radioButtons("pic", "Background photo?:",
                   inline = TRUE,
                   selected = "No",
                   choices = list("Yes", "No")
      ),
      colourInput("bcol", "Background colour", "pink"),
      actionButton("backg", "New Background", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      colourInput("rcol", "Border colour", "pink"),
      colourInput("tcol", "Text colour", "purple"),
      colourInput("xcol", "Text box colour", "yellow"),
      actionButton("ntext", "New Message", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      
    )),
    column(4,
           imageOutput("image1", height = 400, width = 600), 
           radioButtons("adj", "Adjectives:",
                        inline = TRUE,
                        selected = "3",
                        choiceNames = list("1", "2", "3", "Random"),
                        choiceValues = list(1,2,3,"Random")
           ),
           radioButtons("fnt", "Font:",
                        inline = TRUE,
                        choices = list("mono", "Trebuchet", "Palatino"),
                        selected = "Trebuchet"
           ),
           helpText("Press the 'New Background' button when you're happy with your background choices"),
           helpText("and the 'New Message' button to see a new message"),
           
           helpText("------------------------------------------------------------------------------"),
           helpText("Inspiration & most of the code is from the marvelous Maëlle Salmon see her blog http://www.masalmon.eu in the post 'Galentine's day cards'")
           
    )
  )
)

compliments_text <- function(nbr){
  if(nbr == "Random"){nbr <- sample(3,1)} 
  else {nbr <- as.numeric(nbr)}
  praise::praise(paste("You", paste(rep("${adjective}", nbr), sep="", collapse = ", "),
                       sample(rcorpora::corpora(which = "animals/common")$animals, size = 1)))
}

image_names <-  c("balloons.jpg","beach.jpeg","coco.jpeg","daisy.jpeg","hands.jpg","hearttree.jpeg","wooden.jpg")
readpic <- function(name){
  magick::image_read(paste0("images/", name))
}
bpics <- purrr::map(image_names, readpic)
my_text <- compliments_text(3)
my_background <- magick::image_blank(400, 600, color = "pink")

create_background <- function(pic, height, width, bcol){
  if(pic == "Yes"){
    bpics[[sample(1:length(bpics), size=1)]]
  } else {
    magick::image_blank(width, height, color = bcol)
  }
  
}
create_card <- function(height, width, adj_nbr, pic, file, bcol, rcol, tcol, xcol, fnt){
  my_background %>%
    magick::image_border(rcol, "20x10") %>%
    magick::image_annotate(text = my_text,
                           color = tcol,
                           font = fnt,
                           location = "+50+100",
                           boxcolor = xcol,
                           size = 40) %>%
    magick::image_annotate(text = "Love You",
                           color = tcol,
                           font = fnt,
                           location = "+200+300",
                           boxcolor = xcol,
                           size = 30)%>%
    magick::image_annotate(text = "HAPPY GALENTINE'S DAY!",
                           boxcolor = xcol,
                           location = "+100+200",
                           color = tcol,
                           font = fnt,
                           size = 30) %>%
    magick::image_write(file)
}

server <- function(input, output, session) {
  
  new_text <- eventReactive(input$ntext, {
    my_text <<- compliments_text(input$adj)
  })
  
  new_backg <- eventReactive(input$backg, {
    my_background <<- create_background(input$pic, session$clientData$output_image1_height, 
                                        session$clientData$output_image1_width, input$bcol)
  })
  
  # image1 creates a new PNG file when create_card is called
  output$image1 <- renderImage({
    # Get width and height of UI image1
    width  <- session$clientData$output_image1_width
    height <- session$clientData$output_image1_height
    outfile <- tempfile(fileext = '.png')
    
    new_backg()
    new_text()
    
    # Return a list containing information about the image
    create_card(session$clientData$output_image1_height, session$clientData$output_image1_width, 
                input$adj, input$pic, outfile, input$bcol, input$rcol, input$tcol, input$xcol, input$fnt)
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "Happy Galentine's Day!")
  }, deleteFile = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

