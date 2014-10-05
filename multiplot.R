multiplot = function(..., plotlist=NULL, cols=1, layout=NULL) {
        # puts multiple ggplot2 plots on one page
        # 
        # Args:
        #       ...      : ggplot2 objects
        #       plotlist : a list of ggplot2 objects
        #       cols     : number of columns in layout
        #       layout   : a matrix specifying the layout. If present, 'cols'
        #                  is ignored. If the layout is something like
        #                  matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1
        #                  will go in the upper left, 2 will go in the upper 
        #                  right, and 3 will go all the way across the bottom.
        # Returns:
        #       NULL. It renders multiple plots on one page
        if (!require(grid))
                install.packages("grid")
        
        # make a list from the ... arguments and plotlist
        plots = c(list(...), plotlist)
        num.plots = length(plots)
        
        # if layout is NULL, use 'cols' to determine layout
        if (is.null(layout)) {
                # ncol: number of columns
                # nrow: number of rows needed, calculated from # of cols
                layout = matrix(seq(1, cols * ceiling(num.plots/cols)),
                                ncol = cols, nrow = ceiling(num.plots/cols))
        }
        
        if (num.plots==1) {
                print(plots[[1]])
                
        } else {
                # set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                                           ncol(layout))))
                
                # make each plot, in the correct location
                for (i in 1:num.plots) {
                        # get the i,j matrix positions of the regions that 
                        # contain this subplot
                        matchidx = as.data.frame(which(layout == i, 
                                                       arr.ind = TRUE))
                        
                        print(plots[[i]], 
                              vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
                }
        }
}

