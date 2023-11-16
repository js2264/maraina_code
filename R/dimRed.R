dimred <- function(df_rare_c2, ndim, out) {

    df_rare_c2 <- as.data.frame(df_rare_c2)
    df2 <- as.data.frame(t(df_rare_c2))

    df3 <- df2[ , grepl( "V" , names( df2) ) ]
    df3 <- df3 |> dplyr::mutate_if(is.character, as.integer)
    df_rare_c2 <- as.data.frame(df3)

    df_rare_c2 <- df_rare_c2 |> dplyr::select(dplyr::where(~ any(. != 0)))
    df_rare_c2[is.na(df_rare_c2)] <- 0

    #Run NMF
    model <- RcppML::nmf(df_rare_c2, ndim, verbose = FALSE)

    nmf_df <- as.data.frame(model$h)
    colnames(nmf_df) <- colnames(df_rare_c2)

    write.table(nmf_df, row.names=FALSE, col.names = F, quote=FALSE, sep = '\t', file = out)
    return(nmf_df)

}

