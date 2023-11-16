save_pheatmap_pdf <- function(x, filename, width=7, height=7) {
    stopifnot(!missing(x))
    stopifnot(!missing(filename))
    pdf(filename, width=width, height=height)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
}


plotNMF <- function(nmf_df) {

    #Create heatmap
    df2 <- nmf_df
    df3 <- as.data.frame(t(df2))
    df3 <- df3 |> dplyr::rename(Component_1 = V1,
                          Component_2 = V2)
    df4 <- df3
    rownames(df4) <- seq.int(nrow(df4))

    #Save heatmap
    xx <- pheatmap::pheatmap(df4,
                   main = "CNV",
                   fontsize = 8,
                   show_row_dend=FALSE)

    return(xx)

}
