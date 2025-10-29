## Define the ggRDA function
ggRDA <-
  function(rda_obj,
           sp_size = 4,
           arrow_txt_size = 4,
           envfit_df) {
    # fortify rda to data.frame
    fmod <- fortify(rda_obj)
    # get biplot arrow multiplier
    basplot <- plot(rda_obj)
    mult <- attributes(basplot$biplot)$arrow.mul

    # normalize column names
    if ("score" %in% names(fmod)) names(fmod)[names(fmod) == "score"] <- "Score"
    if ("label" %in% names(fmod)) names(fmod)[names(fmod) == "label"] <- "Label"

    # detect axis columns (first two numeric columns) for overall plot
    f_axes <- names(fmod)[vapply(fmod, is.numeric, logical(1))]
    if (length(f_axes) < 2) stop("Unable to detect ordination axes from fortify(rda_obj)")
    f_axis1 <- f_axes[1]
    f_axis2 <- f_axes[2]

    # subsets
    fmod_biplot <- dplyr::filter(fmod, Score == "biplot")
    fmod_species <- dplyr::filter(fmod, Score == "species")

    # base plot
    p <- ggplot(fmod, aes(x = .data[[f_axis1]], y = .data[[f_axis2]])) +
      coord_fixed()

    # build arrow data from fortify biplot rows (robust across versions)
    if (nrow(fmod_biplot) > 0) {
      arrow_df <- fmod_biplot
      # detect axis columns in biplot subset to ensure presence
      bi_axes <- names(arrow_df)[vapply(arrow_df, is.numeric, logical(1))]
      if (length(bi_axes) >= 2) {
        bi_axis1 <- bi_axes[1]
        bi_axis2 <- bi_axes[2]
        # compute arrow ends and label positions using biplot axes
        arrow_df$xend <- mult * arrow_df[[bi_axis1]]
        arrow_df$yend <- mult * arrow_df[[bi_axis2]]
        arrow_df$x_label <- (mult + mult/10) * arrow_df[[bi_axis1]]
        arrow_df$y_label <- (mult + mult/10) * arrow_df[[bi_axis2]]
        # mark significance if envfit_df provided
        if (missing(envfit_df)) {
          arrow_df <- dplyr::mutate(arrow_df, bold = 'sig')
        } else {
          arrow_df <- dplyr::left_join(arrow_df, envfit_df, by = c('Label' = 'factor'))
          arrow_df <- dplyr::mutate(arrow_df, bold = ifelse(stringr::str_detect(sig, stringr::fixed('*')), 'sig', 'ns'))
        }
        # add arrow layers
        p <- p +
          geom_segment(
            data = arrow_df,
            aes(
              x = 0,
              y = 0,
              xend = xend,
              yend = yend,
              size = bold,
              color = bold,
              linetype = bold
            ),
            arrow = arrow(length = unit(0.25, "cm"))
          ) +
          geom_text(
            data = arrow_df,
            aes(x = x_label, y = y_label, label = Label),
            size = arrow_txt_size,
            hjust = 0.5
          )
      }
      # if bi_axes < 2, skip arrows gracefully
    }

    # species labels always
    p +
      geom_text(
        data = fmod_species,
        aes(colour = "species", label = Label),
        size = sp_size
      )
  }