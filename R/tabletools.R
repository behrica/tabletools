#' Converts any html into a png file
#' @param html HTML text to convert
#' @param zoom Zoomfactor
#' @param rotate Rotation 0 - 360
#' @return A file name of the temporary png file created
#' @export
htmlToPng <- function(html,zoom = 1,rotate = 0,vwidth = 992,vheight = 74,cliprect=NULL,selector=NULL) {
    htmlFile <- tempfile(fileext = ".html")
    pngFile <- tempfile(fileext = ".png")
    writeLines(html,htmlFile)
    webshot::webshot(htmlFile,pngFile,zoom=zoom,vwidth = vwidth,vheight = vheight,cliprect = cliprect,selector)
    if (rotate!=0) {
        magick::image_read(pngFile) %>%
            magick::image_rotate(rotate) %>%
                magick::image_write(pngFile)
    }
    pngFile

}

##' Converts a FlexTable into a png file
#' @param ft The FlexTable object to convert
#' @param zoom Zoomm level
#' @param rotate Rotation 0 - 360
#' @importFrom magrittr %>%
#' @return Path of png file created
#' @export
ftToPng <- function(ft,zoom = 1,rotate=0,vwidth = 992,vheight = 744,cliprect=NULL,selector=NULL) {
    ft %>% ReporteRs::as.html() %>%
        htmlToPng(zoom,rotate,vwidth,vheight,cliprect,selector)
}
