extrafont::loadfonts(device = "win")
library(ggplot2)
library(grid)

#------------------------------------------------------------------------------------- #
   base_theme <- function (){
     # ------------------------------------------------------------------------------- #
     # Function to apply base theme                                                    #  
     # Use this if you don't want to make a ton of theme customizations to your graph  #
     # These theme adjustments will work very nicely with MC headers, etc.             #
     # ------------------------------------------------------------------------------- #
    theme(
     legend.position = "none",
     panel.grid.minor.y = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.grid.major.y = element_line(color = "#CCCCCC", linetype = "dotted"),
     axis.ticks.y = element_blank(),
     axis.ticks.x = element_line(color = "#CCCCCC", linewidth = .5),
     axis.title.x = element_blank(),
     axis.title.y = element_blank(),
     axis.text = element_text(size = 12, face = "bold", family = "Arial"),
     axis.text.y = element_text(margin = margin(r = 10), family = "Arial"),
     axis.text.x = element_text(margin = margin(t = 10), family = "Arial"),
     panel.background = element_rect(fill = "white", color = "black"),
     plot.background = element_rect(colour = "white", fill="white", size=.5),
     plot.margin = margin(t = .8, r = .2, l = .2, b = .5, unit = "in"),
     plot.title.position = "plot",
     plot.caption.position = "plot")
     }


#------------------------------------------------------------------------------------- #  
   graph_meta_Style1 <- function(theColor,theTitle,theSubtitle,theFooter) {
     # ------------------------------------------------------------------------------- #
     # Function to apply standardized grid elements to figure                          #
     # ------------------------------------------------------------------------------- #
         if(theColor == "mayoBlue"){themeColor = "#0057B8"}
     else if(theColor == "mayoBlack"){themeColor = "#000000"}
        else{print("Error: Indicate a theme color")}
  
          # The top box
            grid.rect(x = 0, y = dev.size("in")[2], width = dev.size("in")[1], height = 0.6, just = c("left", "top"), gp = gpar(fill = themeColor, col = themeColor, lwd = 0), default.units = "in")
          
          # The title
            grid.text(theTitle, x = .1, y = dev.size("in")[2] - .12, just = c("left", "top"), gp = gpar(col = "#FFFFFF", fontsize = 14, fontface = "bold", fontfamily = "Arial"), default.units = "in")

          # The subtitle
            grid.text(theSubtitle, x = .1, y = dev.size("in")[2] - .325, just = c("left", "top"), gp = gpar(fontsize = 10, col = "#FFFFFF", fontfamily = "Arial"), default.units = "in")          
         
          # The bottom line
            grid.lines(x = c(0,dev.size("in")[1]), y = .3, gp = gpar(col = themeColor, lwd = 2), default.units = "in")
          
          # The footer text
            grid.text(theFooter, x = 0, y = 0.12, just = c("left", "bottom"), gp = gpar(col = "#666666", fontsize = 9, fontfamily = "Arial"), default.units = "in")
            }


#------------------------------------------------------------------------------------- #  
   graph_meta_style1_theme <- function(margin_top = .8, margin_right = .2, margin_left=.2, margin_bottom = .5){
     # ------------------------------------------------------------------------------- #
     # Function that applies margins to ggplot to prepare it for grid elements         #
     # ------------------------------------------------------------------------------- #
    theme(plot.margin = margin(t = margin_top, r = margin_right, l = margin_left, b = margin_bottom, unit = "in"))
     }


#------------------------------------------------------------------------------------- #
   new_graph_window <- function(windowChoice, w = 6.5845, h = 5.083335){
     # ------------------------------------------------------------------------------- #
     # Function that invokes a plot window of desired size                             #
     # ------------------------------------------------------------------------------- # 
    if(windowChoice == "standard"){
       dev.new(width =  6.5845, height = 5.083335, unit = "in", noRStudioGD = TRUE) # standard (A width of 6.5845 is perfect for Word docs with 1" margins)
       }
    else if(windowChoice == "squatty"){  
       dev.new(width =  6.5845, height = 4.000000, unit = "in", noRStudioGD = TRUE) # squatty
       }
    else if(windowChoice == "tall"){
       dev.new(width =  6.5845, height = 7.000000, unit = "in", noRStudioGD = TRUE) # tall
       }
    else if(windowChoice == "wide"){
       dev.new(width = 10.0000, height = 5.083335, unit = "in", noRStudioGD = TRUE) # wide (Great for PowerPoint)
       }
    else if(windowChoice == "custom"){
       dev.new(width = w, height = h, unit = "in", noRStudioGD = TRUE) # custom
       }
    else { print("Error: Please select 'standard','squatty','tall', or 'wide'")}
     }


#------------------------------------------------------------------------------------- #   
   save_graph <- function(graph_name, type = "png"){
     # ------------------------------------------------------------------------------- #
     # Function to save plot in desired format (default is hi-res PNG)                 #
     # ------------------------------------------------------------------------------- #
    if(type == "svg"){
       dev.copy(svg,paste0(graph_name,".svg"),width = dev.size("in")[1], height = dev.size("in")[2])
       dev.off()}
  
    else if(type == "png"){
       dev.copy(png,paste0(graph_name,".png"),width = dev.size("in")[1], height = dev.size("in")[2], units = "in", res = 600)
       dev.off()}

    else{
       dev.copy(png,paste0(graph_name,".png"),width = dev.size("in")[1], height = dev.size("in")[2], units = "in", res = 600)
       dev.off()}}

   
#------------------------------------------------------------------------------------- #   
   plotframe <- function( 
     # ------------------------------------------------------------------------------- #
     # Function to for creating customizable frame (in conjunction with)               #
     # ------------------------------------------------------------------------------- #  
                          topbar = TRUE,
                        sidebars = TRUE,
                       bottombar = TRUE,
  
                     topbarcolor = "#000000",
                    sidebarcolor = "#000000",
                  bottombarcolor = "#000000",
                       
                    topbarheight = 0.6,
                    sidebarwidth = 0.2,
                 bottombarheight = 0.2,

                       titletext = "",
                   titletextsize = 14,
                  titletextcolor = "#FFFFFF",

                    subtitletext = "",
                subtitletextsize = 10,
                   subtitlecolor = "#FFFFFF",

                      footertext = "",
                  footertextsize =  9,
                     footercolor = "#FFFFFF",
  
                    left_padding =  0,
                   title_padding =  0
                                     ){

                    #topbar
                    if(topbar == TRUE){
                    grid.rect( 
                      x = 0,
                      y = dev.size("in")[2],
                      width = dev.size("in")[1],
                      height = topbarheight,
                      just = c("left", "top"),
                      gp = gpar(fill = topbarcolor, col = topbarcolor, lwd = 0),
                      default.units = "in"
                      )}
                    
                    #leftbar
                    if(sidebars == TRUE){
                    grid.rect(
                      x = 0,
                      y = 0,
                      width = sidebarwidth,
                      height = dev.size("in")[2],
                      just = c("left", "bottom"),
                      gp = gpar(fill = sidebarcolor, col = sidebarcolor, lwd = 0),
                      default.units = "in"
                      )}          
                    
                    #rightbar
                    if(sidebars == TRUE){
                    grid.rect(
                      x = dev.size("in")[1],
                      y = 0,
                      width = sidebarwidth,
                      height = dev.size("in")[2],
                      just = c("right", "bottom"),
                      gp = gpar(fill = sidebarcolor, col = sidebarcolor, lwd = 0),
                      default.units = "in"
                      )}
                    
                    #bottombar
                    if(bottombar == TRUE){
                    grid.rect(
                      x = 0,
                      y = 0,
                      width = dev.size("in")[1],
                      height = bottombarheight,
                      just = c("left", "bottom"),
                      gp = gpar(fill = bottombarcolor, col = bottombarcolor, lwd = 0),
                      default.units = "in"
                      )}

                    # The title
                    grid.text(titletext, 
                      x = sidebarwidth + .03 + left_padding, 
                      y = dev.size("in")[2] - (topbarheight/2) + .035 + title_padding,
                      just = c("left", "bottom"),
                      gp = gpar(col = titletextcolor, fontsize = titletextsize, fontface = "bold", fontfamily = "Arial"),
                      default.units = "in"
                      )
          
                    # The subtitle
                    grid.text(subtitletext, 
                      x = sidebarwidth + .03 + left_padding, 
                      y = dev.size("in")[2]  - (topbarheight/2) - .035 - title_padding,
                      just = c("left", "top"),
                      gp = gpar(fontsize = subtitletextsize, col = subtitlecolor, fontfamily = "Arial"),
                      default.units = "in"
                      )          
                   
                    # The footer text
                    grid.text(footertext, 
                      x = sidebarwidth + .03 + left_padding, 
                      y = bottombarheight/2, 
                      just = c("left", "center"),
                      gp = gpar(col = footercolor, fontsize = footertextsize, fontfamily = "Arial"),
                      default.units = "in"
                      )
   }

      
cat("\014")

writeLines(
"---------------------------------------------------------------------------
\nAvailable functions:\n
             base_theme() #A nice base theme.
      graph_meta_Style1() #Add after (and separate from) your ggplot object.
graph_meta_style1_theme() #Add to theme to create header/footer.
       new_graph_window() #This calls a dev window of specified size.
             save_graph() #Saves the graph in the active dev window.
              plotframe() #A function to create custom plot framings.\n
---------------------------------------------------------------------------\n\n")

