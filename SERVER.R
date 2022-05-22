options(shiny.maxRequestSize = 200*1024^2)
server <- function(input, output,session) {
  observe({
    if(input$submit1 > 0){
      isolate({
        lollipop.height <<- input$lollipopHeight
        lollipop.width <<- input$lollipopWidth
  data_info <- file.info(input$Upload_data$datapath)
          if(data_info$size == 0) {
            sendSweetAlert(
              session = session,
              title = "Error !!",
              text = "The file can't be empty.",
              type = "error"
            )
          } else {
            data <- read.table(input$Upload_data$datapath,  sep = "\t", head = T, as.is = T)
            data <- as.data.frame(data)
  if(dim(data)[2] != 3) {
              sendSweetAlert(
                session = session,
                title = "Data formatting error !", type = "error",
                text = "Please check the input data format!"
              )
            } 
            }
 
  
    gg <- function(){ 
         if (input$lollipop_color == "1") {
            if(input$line_Type =="1"){
            p1<- ggplot(data) +
            geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color="grey",linetype="solid") +
            geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
            geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
            scale_colour_manual(values = c("orange","green"),
                                guide = guide_legend(),
                                name = "")+
            theme_light() +
            theme(
              axis.text.y = element_text(size = 5,hjust = 1),
              legend.position = "right",
              panel.border = element_blank(), )+
            labs(title = input$lollipop_Title)+
            xlab(input$lollipop_xTitle) +
            ylab(input$lollipop_yTitle)
          return(p1)}
            if(input$line_Type =="2"){
              p1<- ggplot(data) +
                geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color="grey",linetype="dashed") +
                geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
                geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
                scale_colour_manual(values = c("orange","green"),
                                    guide = guide_legend(),
                                    name = "")+
                theme_light() +
                theme(
                  axis.text.y = element_text(size = input$Abscissa_Size,hjust = 1),
                  legend.position = "right",
                  panel.border = element_blank(), )+
                labs(title = input$lollipop_Title)+
                xlab(input$lollipop_xTitle) +
                ylab(input$lollipop_yTitle)
              return(p1)}
            if(input$line_Type =="3"){
              p1<- ggplot(data) +
                geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color="grey",linetype="dotted") +
                geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
                geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
                scale_colour_manual(values = c("orange","green"),
                                    guide = guide_legend(),
                                    name = "")+
                theme_light() +
                theme(
                  axis.text.y = element_text(size = 5,hjust = 1),
                  legend.position = "right",
                  panel.border = element_blank(), )+
                labs(title = input$lollipop_Title)+
                xlab(input$lollipop_xTitle) +
                ylab(input$lollipop_yTitle)
              return(p1)}
            if(input$line_Type =="4"){
              p1<- ggplot(data) +
                geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color="grey",linetype="dotdash") +
                geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
                geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
                scale_colour_manual(values = c("orange","green"),
                                    guide = guide_legend(),
                                    name = "")+
                theme_light() +
                theme(
                  axis.text.y = element_text(size = 5,hjust = 1),
                  legend.position = "right",
                  panel.border = element_blank(), )+
                labs(title = input$lollipop_Title)+
                xlab(input$lollipop_xTitle) +
                ylab(input$lollipop_yTitle)
              return(p1)}
            if(input$line_Type =="5"){
              p1<- ggplot(data) +
                geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color="grey",linetype="longdash") +
                geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
                geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
                scale_colour_manual(values = c("orange","green"),
                                    guide = guide_legend(),
                                    name = "")+
                theme_light() +
                theme(
                  axis.text.y = element_text(size = 5,hjust = 1),
                  legend.position = "right",
                  panel.border = element_blank(), )+
                labs(title = input$lollipop_Title)+
                xlab(input$lollipop_xTitle) +
                ylab(input$lollipop_yTitle)
              return(p1)}
            if(input$line_Type =="6"){
              p1<- ggplot(data) +
                geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color="grey",linetype="twodash") +
                geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
                geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
                scale_colour_manual(values = c("orange","green"),
                                    guide = guide_legend(),
                                    name = "")+
                theme_light() +
                theme(
                  axis.text.y = element_text(size = 5,hjust = 1),
                  legend.position = "right",
                  panel.border = element_blank(), )+
                labs(title = input$lollipop_Title)+
                xlab(input$lollipop_xTitle) +
                ylab(input$lollipop_yTitle)
              return(p1)}
            }
    
         if (input$lollipop_color == "2") {  
            if(input$line_Type =="1"){
            p1<- ggplot(data) +
            geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color=input$inputcolor1,linetype="solid") +
            geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
            geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
              scale_colour_manual(values = c(input$inputcolor2,input$inputcolor3),
                                guide = guide_legend(),
                                name = "")+
            theme_light() +
            theme(
              axis.text.y = element_text(size = 5,hjust = 1),
              legend.position = "right",
              panel.border = element_blank(), )+
            labs(title = input$lollipop_Title)+
            xlab(input$lollipop_xTitle) +
            ylab(input$lollipop_yTitle)
          return(p1)}
            if(input$line_Type =="2"){
            p1<- ggplot(data) +
              geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color=input$inputcolor1,linetype="dashed") +
              geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
              geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
              scale_colour_manual(values = c(input$inputcolor2,input$inputcolor3),
                                  guide = guide_legend(),
                                  name = "")+
              theme_light() +
              theme(
                axis.text.y = element_text(size = 5,hjust = 1),
                legend.position = "right",
                panel.border = element_blank(), )+
              labs(title = input$lollipop_Title)+
              xlab(input$lollipop_xTitle) +
              ylab(input$lollipop_yTitle)
            return(p1)}
            if(input$line_Type =="3"){
            p1<- ggplot(data) +
              geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color=input$inputcolor1,linetype="dotted") +
              geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
              geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
              scale_colour_manual(values = c(input$inputcolor2,input$inputcolor3),
                                  guide = guide_legend(),
                                  name = "")+
              theme_light() +
              theme(
                axis.text.y = element_text(size = 5,hjust = 1),
                legend.position = "right",
                panel.border = element_blank(), )+
              labs(title = input$lollipop_Title)+
              xlab(input$lollipop_xTitle) +
              ylab(input$lollipop_yTitle)
            return(p1)}
            if(input$line_Type =="4"){
            p1<- ggplot(data) +
              geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color=input$inputcolor1,linetype="dotdash") +
              geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
              geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
              scale_colour_manual(values = c(input$inputcolor2,input$inputcolor3),
                                  guide = guide_legend(),
                                  name = "")+
              theme_light() +
              theme(
                axis.text.y = element_text(size = 5,hjust = 1),
                legend.position = "right",
                panel.border = element_blank(), )+
              labs(title = input$lollipop_Title)+
              xlab(input$lollipop_xTitle) +
              ylab(input$lollipop_yTitle)
            return(p1)}
            if(input$line_Type =="5"){
            p1<- ggplot(data) +
              geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color=input$inputcolor1,linetype="longdash") +
              geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
              geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
              scale_colour_manual(values = c(input$inputcolor2,input$inputcolor3),
                                  guide = guide_legend(),
                                  name = "")+
              theme_light() +
              theme(
                axis.text.y = element_text(size = 5,hjust = 1),
                legend.position = "right",
                panel.border = element_blank(), )+
              labs(title = input$lollipop_Title)+
              xlab(input$lollipop_xTitle) +
              ylab(input$lollipop_yTitle)
            return(p1)}
            if(input$line_Type =="6"){
            p1<- ggplot(data) +
              geom_segment( aes(y=data[,1], yend=data[,1], x=data[,2], xend=data[,3]), color=input$inputcolor1,linetype="twodash") +
              geom_point( aes(y=data[,1],x=data[,2],colour = colnames(data)[2]), size=input$point_Size1) +
              geom_point( aes(y=data[,1],x=data[,3],colour = colnames(data)[3]), size=input$point_Size2) +
              scale_colour_manual(values = c(input$inputcolor2,input$inputcolor3),
                                  guide = guide_legend(),
                                  name = "")+
              theme_light() +
              theme(
                axis.text.y = element_text(size = 5,hjust = 1),
                legend.position = "right",
                panel.border = element_blank(), )+
              labs(title = input$lollipop_Title)+
              xlab(input$lollipop_xTitle) +
              ylab(input$lollipop_yTitle)
            return(p1)}
        }}
        output$lollipopplot <- renderPlot({ 
         gg()
        })
      })      
    }else{NULL}
  observe({
   
    output$downloadlollipop.pdf <- downloadHandler(
      filename = function(){paste('lollipop.pdf')},
      content = function(file){
        pdf(file,height = lollipop.height,width = lollipop.width)
        p2 <- gg()
        grid.draw(p2)
        dev.off()
        },contentType = 'application/pdf'
    )
    output$downloadlollipop.svg <- downloadHandler(
      filename = function(){paste('lollipop.svg')},
      content = function(file){
        svg(file,height = lollipop.height,width = lollipop.width)
        p2 <- gg()
        grid.draw(p2)
        dev.off()
      },contentType = 'image/svg'
    )
  })
  })
}


