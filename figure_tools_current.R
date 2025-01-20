extrafont::loadfonts(device = "win")
library(ggplot2)
library(grid)

#------------------------------------------------------------------------------------- #
   plot_window <- function(windowChoice = "letterhalf", w = 6.5845, h = 5.083335){
     # ------------------------------------------------------------------------------- #
     # Function that invokes a plot window of desired size                             #
     # ------------------------------------------------------------------------------- # 
            if(windowChoice == "letterhalf"){
            dev.new(width =  6.5845, height = 5.083335, unit = "in", noRStudioGD = TRUE) # standard (A width of 6.5845 is perfect for Word docs with 1" margins)
            }
            else if(windowChoice == "letterfull"){  
            dev.new(width =  6.5845, height = 9, unit = "in", noRStudioGD = TRUE) # squatty
            }
            else if(windowChoice == "pptx43"){
            dev.new(width =  10, height = 7.5, unit = "in", noRStudioGD = TRUE) # tall
            }
            else if(windowChoice == "pptx169"){
            dev.new(width = 13.3333, height = 7.5, unit = "in", noRStudioGD = TRUE) # wide (Great for PowerPoint)
            }
            else if(windowChoice == "custom"){
            dev.new(width = w, height = h, unit = "in", noRStudioGD = TRUE) # custom
            }
            else { print("Error: Please indicate 'letterhalf','letterfull','pptx43', 'pptx169', or 'custom and provide width and height'")}
            }

#------------------------------------------------------------------------------------- #  
   plot_margins <- function(margin_top = .8, margin_right = .2, margin_left=.2, margin_bottom = .5){
     # ------------------------------------------------------------------------------- #
     # Function that applies a margin theme to ggplot to prepare it for grid elements  #
     # ------------------------------------------------------------------------------- #
            theme(plot.margin = margin(t = margin_top, r = margin_right, l = margin_left, b = margin_bottom, unit = "in"))
            }

#------------------------------------------------------------------------------------- #  
   plot_frame_quick <- function(theColor = "black") {
     # ------------------------------------------------------------------------------- #
     # Function to apply standardized grid elements to figure                          #
     # ------------------------------------------------------------------------------- #
          # The top box
            grid.rect(x = 0, y = dev.size("in")[2], width = dev.size("in")[1], height = 0.6, just = c("left", "top"), gp = gpar(fill = theColor, col = theColor, lwd = 0), default.units = "in")
          # The bottom line
            grid.lines(x = c(0,dev.size("in")[1]), y = .3, gp = gpar(col = theColor, lwd = 2), default.units = "in")
            }

#------------------------------------------------------------------------------------- #  
   plot_captions <- function(titletext = "", 
                             subtitletext = "", 
                             footertext = "", 
                             titletextcolor = "#FFFFFF", 
                             subtitlecolor = "#FFFFFF", 
                             footercolor = "#222222", 
                             topbarheight = .6, 
                             sidebarwidth = 0,
                             bottombarheight = .3,
                             left_padding = .04, 
                             title_padding = .04,
                             titletextsize = 14,
                             subtitletextsize = 10,
                             footertextsize = 9) {
     # ------------------------------------------------------------------------------- #
     # Function to add captions to the plotframe                                       #
     # ------------------------------------------------------------------------------- # 
          # The title
            grid.text(titletext, 
            x = sidebarwidth + .03 + left_padding, 
            y = dev.size("in")[2] - (topbarheight/2) + .035 + title_padding,
            just = c("left", "bottom"),
            gp = gpar(col = titletextcolor, fontsize = titletextsize, fontface = "bold", fontfamily = "Arial"),
            default.units = "in")
          # The subtitle
            grid.text(subtitletext, 
            x = sidebarwidth + .03 + left_padding, 
            y = dev.size("in")[2]  - (topbarheight/2) - .035 - title_padding,
            just = c("left", "top"),
            gp = gpar(fontsize = subtitletextsize, col = subtitlecolor, fontfamily = "Arial"),
            default.units = "in")          
          # The footer text
            grid.text(footertext, 
            x = sidebarwidth + .03 + left_padding, 
            y = bottombarheight/2, 
            just = c("left", "center"),
            gp = gpar(col = footercolor, fontsize = footertextsize, fontfamily = "Arial"),
            default.units = "in")
            }

#------------------------------------------------------------------------------------- #   
   plot_save <- function(graph_name, type = "png"){
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
   plot_frame_custom <- function( 
     # ------------------------------------------------------------------------------- #
     # Function to for creating customizable frame (in conjunction with)               #
     # ------------------------------------------------------------------------------- #  
                          topbar = TRUE,
                        sidebars = TRUE,
                       bottombar = TRUE,
                      bottomline = TRUE,
  
                     topbarcolor = "#222222",
                    sidebarcolor = "#222222",
                  bottombarcolor = "#222222",
                 bottomlinecolor = "#222222",
                       
                    topbarheight = 0.6,
                    sidebarwidth = 0.005,
                 bottombarheight = 0.3,

                       titletext = "",
                   titletextsize = 14,
                  titletextcolor = "#FFFFFF",

                    subtitletext = "",
                subtitletextsize = 10,
                   subtitlecolor = "#FFFFFF",

                      footertext = "",
                  footertextsize =  9,
                     footercolor = "#FFFFFF",
  
                    left_padding =  .04,
                   title_padding =  .04
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
                      default.units = "in")}
                    #leftbar
                    if(sidebars == TRUE){
                    grid.rect(
                      x = 0,
                      y = dev.size("in")[2],
                      width = sidebarwidth,
                      height = dev.size("in")[2] - bottombarheight,
                      just = c("left", "top"),
                      gp = gpar(fill = sidebarcolor, col = sidebarcolor, lwd = 0),
                      default.units = "in")}        
                    #rightbar
                      if(sidebars == TRUE){
                      grid.rect(
                      x = dev.size("in")[1],
                      y = dev.size("in")[2],
                      width = sidebarwidth,
                      height = dev.size("in")[2] - bottombarheight,
                      just = c("right", "top"),
                      gp = gpar(fill = sidebarcolor, col = sidebarcolor, lwd = 0),
                      default.units = "in")}
                    #bottombar
                      if(bottombar == TRUE){
                      grid.rect(
                      x = 0,
                      y = 0,
                      width = dev.size("in")[1],
                      height = bottombarheight,
                      just = c("left", "bottom"),
                      gp = gpar(fill = bottombarcolor, col = bottombarcolor, lwd = 0),
                      default.units = "in")}
                    #bottomline
                      if(bottomline == TRUE){
                      grid.rect(
                      x = 0,
                      y = bottombarheight,
                      width = dev.size("in")[1],
                      height = .005,
                      just = c("left", "bottom"),
                      gp = gpar(fill = bottomlinecolor, col = bottomlinecolor, lwd = 0),
                      default.units = "in")}
                    #The title
                      grid.text(titletext, 
                      x = sidebarwidth + .03 + left_padding, 
                      y = dev.size("in")[2] - (topbarheight/2) + .035 + title_padding,
                      just = c("left", "bottom"),
                      gp = gpar(col = titletextcolor, fontsize = titletextsize, fontface = "bold", fontfamily = "Arial"),
                      default.units = "in")
                    #The subtitle
                      grid.text(subtitletext, 
                      x = sidebarwidth + .03 + left_padding, 
                      y = dev.size("in")[2]  - (topbarheight/2) - .035 - title_padding,
                      just = c("left", "top"),
                      gp = gpar(fontsize = subtitletextsize, col = subtitlecolor, fontfamily = "Arial"),
                      default.units = "in")          
                    #The footer text
                      grid.text(footertext, 
                      x = sidebarwidth + .03 + left_padding, 
                      y = bottombarheight/2, 
                      just = c("left", "center"),
                      gp = gpar(col = footercolor, fontsize = footertextsize, fontfamily = "Arial"),
                      default.units = "in")

   }

instructions <- function(set = "general"){
  
if(set == "general") {
writeLines(
"---------------------------------------------------------------------------
\nOrder of functions()\n
window -> plot -> margins -> frame -> captions -> save\n
1).............plot_window() - This calls a dev window of specified size
2)..................ggplot() - The ggplot object  
3)............plot_margins() - Append to the ggplot object (+)
4)........plot_frame_quick() - Or, plot_frame_custom (with additional parameters)
5)...........plot_captions() - Title, subtitle, footer (not used if plot_frame_custom() is used, which has it's own captions arguments)
6)...............plot_save() - Save the plot.\n
---------------------------------------------------------------------------\n")}
else if(set == "plot_captions") {
writeLines(
"---------------------------------------------------------------------------
\nplot_captions()\n
Defaults:
  titletext = '', 
  subtitletext = '', 
  footertext = '', 
  titletextcolor = '#FFFFFF', 
  subtitlecolor = '#FFFFFF', 
  footercolor = '#222222', 
  topbarheight = .6, 
  sidebarwidth = 0,
  bottombarheight = .3,
  left_padding = .04, 
  title_padding = .04,
  titletextsize = 14,
  subtitletextsize = 10,
  footertextsize = 9\n
Notes:
  - You don't need to supply caption text, but it's silly not to.
  - If you use plot_frame_custom() and adjust things like the topbarheight, you'll need to adjust here.\n
---------------------------------------------------------------------------\n")}
else if(set == "plot_frame_custom") {
writeLines(
"---------------------------------------------------------------------------
\nplot_frame_custom()\n
Defaults:
  topbar = TRUE,
  sidebars = TRUE,
  bottombar = TRUE,
  bottomline = TRUE,
  ------------------------  
  topbarcolor = '#222222',
  sidebarcolor = '#222222',
  bottombarcolor = '#222222',
  bottomlinecolor = '#222222',
  ------------------------
  topbarheight = 0.6,
  sidebarwidth = 0.005,
  bottombarheight = 0.3,
  ------------------------
  titletext = '',
  titletextsize = 14,
  titletextcolor = '#FFFFFF',
  ------------------------
  subtitletext = '',
  subtitletextsize = 10,
  subtitlecolor = '#FFFFFF',
  ------------------------
  footertext = '',
  footertextsize =  9,
  footercolor = '#FFFFFF',
  ------------------------
  left_padding =  .04,
  title_padding =  .04\n
Notes:
  - The defaults are acceptably good, but highly customizable
  - You'll need to specify title, subtitle, and footer text within the function\n
---------------------------------------------------------------------------\n")}
else if(set == "plot_frame_quick") {
writeLines(
"---------------------------------------------------------------------------
\nplot_frame_quick()\n
Defaults:
  theColor = 'black'\n
Notes:
  - The defaults are acceptably good; the only passable argument is the frame color
  - You'll need to specify title, subtitle, and footer text within the plot_captions() function\n
---------------------------------------------------------------------------\n")}
else if(set == "plot_margins") {
writeLines(
"---------------------------------------------------------------------------
\nplot_margins()\n
Defaults:
  margin_top = .8, 
  margin_right = .2, 
  margin_left=.2, 
  margin_bottom = .5\n
Notes:
  - This is a theme function that must be added to the ggplot object (with +)
  - The defaults will work well, unless you alter the size of the header / footer
    using plot_frame_custom(). Then you'll need to experiement with optimal widths.\n
---------------------------------------------------------------------------\n")}
else if(set == "plot_save") {
writeLines(
"---------------------------------------------------------------------------
\nplot_save()\n
Defaults:
  graph_name, 
  type = 'png'\n
Notes:
  - File type options are png and svg.
  - Saves active dev window to the current working directory
  - graph_name argument does not include file extension\n
---------------------------------------------------------------------------\n")}
else if(set == "plot_window") {
writeLines(
"---------------------------------------------------------------------------
\nplot_window()\n
Defaults:
  windowChoice = 'letterhalf', 
  w = 6.5845, 
  h = 5.083335\n
Notes:
  - Options for windowChoice argument are letterhalf, letterfull,
    pptx43, pptx169, and custom.
  - If a custom window size is used, then w and h may be specified.\n
---------------------------------------------------------------------------\n")}
}  
cat("\014")
instructions()

