for(pack in c("png", "extrafont")) {
    if(!(pack %in% installed.packages()[,1])) {
        install.packages(pack)  
    }
    library(pack, character.only = T)
}
# might need loading fonts from the extrafont package first
# alternatively, it is possible to change the fontfamily variable in the function


makecomment <- function(filename, imgname, name, comment) {
    helper <- function(lines = 2) {
        fontfamily <- "Arial"
        textsize <- 24
        textcol <- "#141823"
        background <- "#eeeff4"
        namecol <- "#3b5998"
        linesize <- 32
        
        height <- ifelse(lines > 2, lines*linesize + 12, 2*linesize + 12)
        width <- 940
        
        ypix <- 1/height
        xpix <- 1/width
        
        lineheight <- linesize/textsize
        
        png(file = filename, width = width, height = height, pointsize = textsize)
        icoors <- c(xpix*6, 1-ypix*70, xpix*70, 1-ypix*6)
        
        par(mar=c(0,0,0,0))
        plot(0,0, xlim = c(0,1), ylim = c(0,1), type = "n", axes = F, xlab = "", ylab = "",
             xaxs = "i", yaxs = "i")
        rect(0, 0, 1, 1, col = background, border = background)
        picture <- readPNG(imgname)
        rasterImage(picture, icoors[1], icoors[2], icoors[3], icoors[4])
        
        namewidth <- strwidth(name, font = 2)
        
        xtext <- icoors[3] + xpix*16
        ytext <- icoors[4] - textsize*ypix*(lineheight-1)/2
        
        words <- strsplit(comment, " ")
        current <- ""
        curline <- 1
        for(word in unlist(words)) {
            left <- ifelse(curline == 1, namewidth + xtext + xpix*2, xtext)
            if(strwidth(paste(current, word)) + left + xpix*6 > 1) {
                text(left,
                     ytext - (curline-1)*textsize*ypix*lineheight, 
                     current, adj = c(0,1), col = textcol, family = fontfamily)
                current <- word
                curline <- curline + 1
            } else {
                current <- paste0(current, " ", word)
            }
        }
        if(nchar(current) > 0) {
            text(left,
                 ytext - (curline-1)*textsize*ypix*lineheight, 
                 current, adj = c(0,1), col = textcol, family = fontfamily)    
        }
        text(xtext, 
             ytext, 
             name, adj = c(0,1), col = namecol, family = fontfamily, font = 2)
        dev.off()
        if(curline > lines) {
            helper(curline)
        }
    }
    helper()
}

# example
imgname <- "img1.png"
url <- "http://wiki.inf.utfsm.cl/images/e/e8/Beastie.png"
download.file(url, imgname, mode = "wb")
name <- "bahniks"
comment <- 'Such an awesome code Stepan! This can be so useful for creating stimuli for some of mine experiments!'
filename <- "comm1.png"
makecomment(filename, imgname, name, comment)
