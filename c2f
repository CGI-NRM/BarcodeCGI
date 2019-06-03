c2f <-function (user = FALSE, Labels = NULL, name = "LabelsOut", type = "matrix", Text = NULL,
                ErrCorr = "H", Fsz = 5, Across = TRUE, ERows = 0, ECols = 0, 
                trunc = TRUE, numrow = 20, numcol = 4, page_width = 8.5, 
                page_height = 11, width_margin = 0.25, height_margin = 0.5, 
                label_width = NA, label_height = NA, x_space = 0, y_space = 0.5) 
{
  if (length(Labels) == 0) 
    stop("Labels do not exist. Please pass in Labels")
  if (class(Labels) %in% c("character", "integer", "numeric", 
                           "factor")) {
    Labels <- Labels
  }
  else if (class(Labels) == "data.frame") {
    if (any(tolower(names(Labels)) == "label")) {
      Labels <- Labels[, "label"]
    }
    else {
      warning("Cannot find a label column. Using first column as label input.")
      Labels <- Labels[, 1]
    }
  }
  else {
    stop("Label input not a vector or a data frame. Please check your input.")
  }
  if (any(unlist(lapply(c(numcol, numrow, Fsz, ERows, ECols, 
                          trunc, page_width, page_height, height_margin, width_margin, 
                          x_space, y_space), class)) != "numeric") == TRUE) {
    stop("One or more numerical parameters are not numeric")
  }
  labelLength <- max(nchar(paste(Labels)))
  if (x_space > 1 | x_space < 0) 
    stop("ERROR: x_space value out of bounds. Must be between 0 and 1")
  if (y_space < 0 | y_space > 1) 
    stop("ERROR: y_space value out of bounds. Must be between 0 and 1")
  on.exit(grDevices::dev.off())
  if (user == TRUE) {
    inputCheck <- c("T", "t", "F", "f")
    yesNo <- c("Y", "N")
    name <- readline(paste0("Please enter name for PDF output file: "))
    Fsz <- noquote(as.numeric(readline("Please enter a font size ")))
    while (Fsz <= 0) {
      noquote(print("Invalid input, please specify a font size greater than 0"))
      Fsz <- noquote(as.numeric(readline("Please enter a font size greater than 0: ")))
    }
    ErrCorr <- noquote(toupper(readline("Specify an error correction - L, M, Q, H: ")))
    errCheck <- c("L", "l", "M", "m", "Q", "q", "H", "h")
    while ((ErrCorr %in% errCheck) == FALSE) {
      noquote(print("Invalid input, please only enter what is specified"))
      ErrCorr <- noquote(toupper(readline("Specify an error correction - L, M, Q, H: ")))
    }
    Advanced <- noquote(toupper(readline("Edit advanced parameters? (Y/N): ")))
    while ((Advanced %in% yesNo) == FALSE) {
      noquote(print("Invalid input"))
      Advanced <- noquote(toupper(readline("Edit advanced parameters? (Y/N): ")))
    }
    if (Advanced == "Y") {
      type <- noquote(toupper(readline("Linear barcodes ( Y = Linear, N = matrix): ")))
      while (type %in% yesNo) {
        noquote(print("Invalid input"))
        type <- noquote(toupper(readline("Linear barcodes ( Y = Linear, N = matrix): ")))
      }
      Across <- noquote(toupper(readline("Please enter T or F to print across: ")))
      while ((Across %in% inputCheck) == FALSE) {
        noquote(print("Invalid input"))
        Across <- noquote(toupper(readline("Please enter T or F to print across: ")))
      }
      trunc <- noquote(toupper(readline("Do you want to split text into rows? (T/F): ")))
      while ((trunc %in% inputCheck) == FALSE) {
        noquote(print("Invalid input"))
        trunc <- noquote(toupper(readline("Do you want to split text into rows? (T/F): ")))
      }
      ERows <- noquote(as.numeric(readline("Number of rows to skip? (enter 0 for default): ")))
      ECols <- noquote(as.numeric(readline("Number of cols to skip? (enter 0 for default): ")))
      numrow <- as.numeric(readline("# of rows per page: "))
      while (is.numeric(numrow) == FALSE) {
        noquote(print("Invalid input"))
        numrow <- as.numeric(readline("# of rows per page: "))
      }
      numcol <- as.numeric(readline("# of col per page: "))
      while (is.numeric(numcol) == FALSE) {
        noquote(print("Invalid input"))
        numcol <- as.numeric(readline("# of col per page: "))
      }
      height_margin <- as.numeric(readline("Please enter the height margin of page (in inch): "))
      while (is.numeric(height_margin) == FALSE) {
        noquote(print("Invalid input"))
        height_margin <- as.numeric(readline("Please enter the height margin of page (in inch): "))
      }
      width_margin <- as.numeric(readline("Please enter the width margin of page (in inch): "))
      while (is.numeric(width_margin) == FALSE) {
        noquote(print("Invalid input"))
        width_margin <- as.numeric(readline("Please enter the width margin of page (in inch): "))
      }
      label_width <- as.numeric(readline("Please enter the width of the label (in inch): "))
      while (is.numeric(label_width) == FALSE) {
        noquote(print("Invalid input"))
        label_width <- as.numeric(readline("Please enter the width of the label (in inch): "))
      }
      label_height <- as.numeric(readline("Please enter the height of the label (in inch): "))
      while (is.numeric(label_height) == FALSE) {
        noquote(print("Invalid input"))
        label_height <- as.numeric(readline("Please enter the height of the label (in inch): "))
      }
      space <- toupper(readline("change distance between qrcode and label? (y/n): "))
      while ((space %in% yesNo) == FALSE) {
        noquote(print("Invalid input"))
        space <- toupper(readline("change distance between qrcode and label? (y/n): "))
      }
      x_space <- 0
      y_space <- 0.5
      if (space == "Y") {
        x_space <- as.numeric(readline("Please enter a distance between 0 and 1: "))
        while ((x_space < 1 | x_space > 0)) {
          noquote(print("Invalid input"))
          x_space <- as.numeric(readline("Please enter a distance between 0 and 1: "))
        }
        y_space <- as.numeric(readline("Please enter a value between 0 and 1: "))
        while ((x_space < 0 | x_space > 1)) {
          noquote(print("Invalid input"))
          y_space <- as.numeric(readline("Please enter a distance between 0 and 1:"))
        }
      }
    }
  }
  width_margin <- page_width - width_margin * 2
  height_margin <- page_height - height_margin * 2
  if (is.na(label_width)) {
    label_width <- width_margin/numcol
  }
  if (is.na(label_height)) {
    label_height <- height_margin/numrow
  }
  if (type == "linear" & label_width/labelLength < 0.03) 
    warning("Linear barcodes created will have bar width smaller than 0.03 inches which may be unreadable by some barcode scanners.")
  if (!is.numeric(c(label_width, label_height))) 
    stop("label_width and label_height should be set to NULL or a numeric value.")
  column_space <- (width_margin - label_width * numcol)/(numcol - 
                                                           1)
  row_space <- (height_margin - label_height * numrow)/(numrow - 
                                                          1)
  barcode_layout <- grid::grid.layout(numrow, numcol, widths = grid::unit(c(rep(label_width + 
                                                                                  column_space, numcol - 1), label_width), "in"), heights = grid::unit(c(rep(label_height + 
                                                                                                                                                               row_space, numrow - 1), label_height), "in"))
  if (type == "linear") {
    code_vp <- grid::viewport(x = grid::unit(0.05, "npc"), 
                              y = grid::unit(0.8, "npc"), width = grid::unit(0.9 * 
                                                                               label_width, "in"), height = grid::unit(0.8 * 
                                                                                                                         label_height, "in"), just = c("left", "top"))
    text_height <- ifelse(Fsz/72 > label_height * 0.3, label_height * 
                            0.3, Fsz/72)
    label_vp <- grid::viewport(x = grid::unit(0.5, "npc"), 
                               y = grid::unit(1, "npc"), width = grid::unit(1, "npc"), 
                               height = grid::unit(text_height, "in"), just = c("centre", 
                                                                                "top"))
    Fsz <- ifelse(Fsz/72 > label_height * 0.3, label_height * 
                    72 * 0.3, Fsz)
    label_plots <- sapply(as.character(Labels), baRcodeR::code_128_make, 
                          USE.NAMES = T, simplify = F)
    names(label_plots) <- paste("DNA-prov rovdjur", Text, names(label_plots), sep = ":")
  }
  else if (type == "matrix") {
    code_vp <- grid::viewport(x = grid::unit(0.05, "npc"), 
                              y = grid::unit(0.8, "npc"), width = grid::unit(0.3 * 
                                                                               label_width, "in"), height = grid::unit(0.6 * 
                                                                                                                         label_height, "in"), just = c("left", "top"))
    label_vp <- grid::viewport(x = grid::unit((0.4 + 0.6 * 
                                                 x_space) * label_width, "in"), y = grid::unit(y_space, 
                                                                                               "npc"), width = grid::unit(0.4, "npc"), height = grid::unit(0.8, 
                                                                                                                                                           "npc"), just = c("left", "center"))
    label_plots <- sapply(as.character(Labels), qrcode_make, 
                          ErrCorr = ErrCorr, USE.NAMES = T, simplify = F)
  }
  x_pos <- ERows + 1
  y_pos <- ECols + 1
  oname <- paste0(name, ".pdf")
  grDevices::pdf(oname, width = page_width, height = page_height, 
                 onefile = TRUE, family = "Helvetica")
  bc_vp = grid::viewport(layout = barcode_layout)
  grid::pushViewport(bc_vp)
  for (i in 1:length(label_plots)) {
    Xsplt <- names(label_plots[i])
    if (trunc == TRUE) {
      if (nchar(Xsplt) > 15) {
        #Xsplt <- paste0(substring(Xsplt, seq(1, nchar(Xsplt), 
        #                                     15), seq(15, nchar(Xsplt) + 15 - 1, 15)), collapse = "\n")
        Xsplt <- paste(unlist(strsplit(Xsplt, ":")), collapse = "\n")
      }
    }
    if (x_pos > numcol | y_pos > numrow) {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(width = grid::unit(page_width, 
                                                           "in"), height = grid::unit(page_height, "in")))
      grid::pushViewport(bc_vp)
      x_pos = 1
      y_pos = 1
    }
    grid::pushViewport(grid::viewport(layout.pos.row = y_pos, 
                                      layout.pos.col = x_pos))
    
    grid::pushViewport(code_vp)
    
    grid::grid.draw(label_plots[[i]])
    
    grid::popViewport()
    grid::pushViewport(label_vp)
    if (type == "linear") {
      grid::grid.rect(gp = grid::gpar(col = NA, fill = "WHITE"), height = 4)
    }
    grid::grid.text(label = Xsplt, gp = grid::gpar(fontsize = Fsz, 
                                                   lineheight = 0.8, fontface = "bold", lineheight = 1.1))
    grid::popViewport(2)
    if (Across == "T" | Across == TRUE) {
      x_pos <- x_pos + 1
      if (x_pos > numcol) {
        x_pos <- 1
        y_pos <- y_pos + 1
      }
    }
    else {
      y_pos <- y_pos + 1
      if (y_pos > numrow) {
        y_pos <- 1
        x_pos <- x_pos + 1
      }
    }
  }
}
