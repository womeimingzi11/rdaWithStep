## Define the ggRDA function
ggRDA <-
  function(rda_obj,
           sp_size = 4,
           arrow_txt_size = 4,
           envfit_df) {
    # 检查ggvegan包是否可用
    if (!requireNamespace('ggvegan', quietly = TRUE)) {
      # 如果ggvegan不可用，提供一个简单的替代方案
      warning("ggvegan包不可用，使用基础vegan函数替代。某些可视化功能可能受限。")
      
      # 提取物种和环境变量分数
      species_scores <- scores(rda_obj, display = "species", choices = c(1, 2))
      site_scores <- scores(rda_obj, display = "sites", choices = c(1, 2))
      biplot_scores <- tryCatch({
        scores(rda_obj, display = "biplot", choices = c(1, 2))
      }, error = function(e) {
        NULL
      })
      
      # 创建数据框
      fmod <- data.frame()
      
      # 添加物种数据
      if (!is.null(species_scores)) {
        species_df <- as.data.frame(species_scores)
        names(species_df) <- c("RDA1", "RDA2")
        species_df$Score <- "species"
        species_df$Label <- rownames(species_df)
        fmod <- rbind(fmod, species_df)
      }
      
      # 添加位点数据
      if (!is.null(site_scores)) {
        site_df <- as.data.frame(site_scores)
        names(site_df) <- c("RDA1", "RDA2")
        site_df$Score <- "sites"
        site_df$Label <- rownames(site_df)
        fmod <- rbind(fmod, site_df)
      }
      
      # 添加双标图数据
      if (!is.null(biplot_scores)) {
        biplot_df <- as.data.frame(biplot_scores)
        names(biplot_df) <- c("RDA1", "RDA2")
        biplot_df$Score <- "biplot"
        biplot_df$Label <- rownames(biplot_df)
        fmod <- rbind(fmod, biplot_df)
      }
    } else {
      # 如果ggvegan可用，使用它的fortify函数
      fmod <- ggvegan::fortify(rda_obj)
    }
    # initialize multiplier (will compute adaptively)
    mult <- 1

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
        # --- adaptive multiplier (synced with repro_ggRDA.R) ---
        num_cols <- names(arrow_df)[vapply(arrow_df, is.numeric, logical(1))]
        if (length(num_cols) >= 2) {
          b1 <- num_cols[1]; b2 <- num_cols[2]
          fnum_cols <- names(fmod_species)[vapply(fmod_species, is.numeric, logical(1))]
          s1 <- fnum_cols[1]; s2 <- fnum_cols[2]
          sp_rx <- max(abs(fmod_species[[s1]]), na.rm = TRUE)
          sp_ry <- max(abs(fmod_species[[s2]]), na.rm = TRUE)
          bp_rx <- max(abs(arrow_df[[b1]]), na.rm = TRUE)
          bp_ry <- max(abs(arrow_df[[b2]]), na.rm = TRUE)
          m_x <- if (bp_rx > 0) sp_rx / bp_rx else NA_real_
          m_y <- if (bp_ry > 0) sp_ry / bp_ry else NA_real_
          mult <- suppressWarnings(min(m_x, m_y, na.rm = TRUE))
          if (!is.finite(mult) || is.na(mult)) mult <- 1
        }
        # --- end adaptive multiplier ---

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