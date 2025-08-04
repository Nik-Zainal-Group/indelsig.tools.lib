palettes <- list()

palettes$set1 <- c('#D55E00','#0072B2','#CC79A7','#E69F00','#56B4E9','#009E73','#c90c2e')
names(palettes$set1) <- c( '[+T]','[+C]','[+>1]','[-T]','[-C]','[->1]','[Com]')

palettes$set2 <- c('#D55E00','#0072B2','#CC79A7','#E69F00','#56B4E9','#009E73','#9966ff','#c90c2e')
names(palettes$set2) <- c( '[+T]','[+C]','[+>1]','[-T]','[-C]','[->1]','[±]Mh','[Com]')

palettes$set3 <- c('#87CEEB','#5478E4','#FFD03E','#FF8C00','#FF4C4C','#4aa342','#FF77AA','#993299','#6d0a23')
names(palettes$set3) <- c('[M=C,S=0]OR≤4','[M=C,S=0]OR≥5',
                                '[M=T,S=0]OR≤4','[M=T,S=0]OR≥5',
                                '[M≥2,S=0]',
                                '[S≥1]OR=0',
                                '[S=1]',
                                '[S≥2]',
                                '[Com]')
palettes$set4 <- c(
  '#4aa342','#4aa342',
  '#FF77AA','#FF77AA',
  '#993299', '#993299',
  '#FF4C4C',
  '#6d0a23')
names(palettes$set4) <- c(
  'N-Mer insertions','N-Mer deletions',
  'A-periodic insertions',
  'A-periodic deletions',
  'Longer A-periodic insertions',
  'Longer A-periodic deletions',
  'Non-repeating',
  'Complex')


palettes$set7 <- c('#87CEEB','#5478E4','#FFD03E','#FF8C00',
                         '#87CEEB','#5478E4','#FFD03E','#FF8C00',
                         '#4aa342','#4aa342',
                         '#FF77AA','#FF77AA',
                         '#993299', '#993299',
                         '#FF4C4C',
                         '#6d0a23')
names(palettes$set7) <- c(
  '+C Short','+C Long','+T Short','+T Long',
  '-C Short','-C Long','-T Short','-T Long',

  'N-Mer insertions','N-Mer deletions',
  'A-periodic insertions','A-periodic deletions',
  'Longer A-periodic insertions','Longer A-periodic deletions',
  'Non-repeating',
  'Complex')


palettes$glodzik_default <-c('#87CEEB','#FFD03E')
names(palettes$glodzik_default) <- c('Insertion','Deletion')


palettes$range <- c('#87CEEB','#FFD03E','#FF8C00',
                    '#5478E4','#4aa342','#FF77AA',
                    '#993299','#FF4C4C',"#d1d0e2",
                    "#264481","#008080",'#6d0a23')


## new functions for working with signatures
signature_barplot_formatting <- ggplot2::theme(axis.text.x= ggplot2::element_text(angle=90, vjust=0.5,hjust = 1, size=10,colour = "black"),
                                               axis.text.y= ggplot2::element_text(size=10,colour = "black"),
                                               axis.title =  ggplot2::element_text(size=15),
                                               #axis.ticks.x.bottom = ggplot2::element_blank(),
                                               axis.ticks.y.left= ggplot2::element_line(colour = 'black'),
                                               axis.line.y.left = ggplot2::element_line(colour = 'black'),
                                               axis.line.x.bottom = ggplot2::element_line(color = 'black'),
                                               legend.position='none',
                                               plot.title =  ggplot2::element_text(size=10),
                                               panel.grid = ggplot2::element_blank(),
                                               panel.grid.major.y = ggplot2::element_line(color = 'grey'),
                                               panel.background = ggplot2::element_rect(fill = "white"),
                                               panel.border =  ggplot2::element_rect(colour = "white", fill=NA),
                                               strip.background = ggplot2::element_rect(colour = 'white',fill='white'))


### plotting function

signature_barplot <- function(mat,channel_set=NULL,text_size=2,ymax=NULL){

  ## transform the matrix into a df
  df <- mat %>%
    as.data.frame %>%
    tibble::rownames_to_column()

  sample_name <-colnames(df)[2]

  colnames(df) <- c('rowname','y')

  ## if there's a defined channel set
  ## re-level the factors based on the channel set to ensure plotting correction
  ## and add a fill column, otherwise fill column is just rowname
  if(!is.null(channel_set)){
    df$rowname <- factor(df$rowname,levels = ch[[channel_set]]$channels)

    df$fill <- factor(ch[[channel_set]]$assigned_type,ch[[channel_set]]$type)

    ## Add a type for splitting the channels into two subplots
    ## we know the type based on the channel set assigned type and therefore know the length
    blocks <- data.frame(
      fill=factor(ch[[channel_set]]$type,levels = ch[[channel_set]]$type),
      labels=ch[[channel_set]]$type_alias,
      label_colors=ch[[channel_set]]$type_color,
      palette=ch[[channel_set]]$palette,
      xmin=cumsum(c(0,ch[[channel_set]]$channels_per_type[-length(ch[[channel_set]]$channels_per_type)]))+0.5,
      xmax=cumsum(c(ch[[channel_set]]$channels_per_type))+0.5
    )


    if(!is.null(ymax)){
      max_param <- ymax
    }else{
      max_param <- max(df[['y']])
    }

    blocks$ymin <- max_param*1.08
    blocks$ymax <- max_param*1.2

  } else{

    df$fill <- df$rowname
  }



  P <- df %>%
    ggplot(aes(x=rowname,y=y,fill=fill)) +
    geom_bar(stat='identity',width =0.7 ) +
    ggtitle(sample_name)+
    labs(x='',y='',fill='')


  if(!is.null(channel_set)){
    P <- P+scale_fill_manual(values = ch[[channel_set]]$palette)

    P <- P+
      ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,
                                                     ymin=ymin,
                                                     xmax=xmax,
                                                     ymax=ymax,
                                                     fill=fill),
                         inherit.aes = F)+
      ggplot2::geom_text(data=blocks,
                         ggplot2::aes(x=(xmax+xmin)/2,
                                      y=(ymax+ymin)/2,
                                      label=labels,color=fill,fontface='bold'),
                         size=text_size,inherit.aes = F,)+
      scale_color_manual(values=blocks$label_colors)


    ## Alter the scale if neccesary
    if(all(df[['y']] < 1)){
      ## if in signature mode
      max_height <- round(max_param,digits = 2)

      P <- P+
        ggplot2::scale_y_continuous(breaks = seq(0,max_height,by=0.05),
                                    labels = scales::percent,
                                    expand=c(0,0,0,0))+
        ggplot2::coord_cartesian(ylim=c(0,unique(blocks$ymax)))

    } else{

      ## adjust to limit ymaxheight
      P <- P + ggplot2::scale_y_continuous(expand=c(0,0,0,0))+
        ggplot2::coord_cartesian(ylim=c(0,unique(blocks$ymax)))
    }

  }

  P<- P+signature_barplot_formatting
  return(P)
}

signature_barplots <- function(mat,
                               err=NULL,
                               channel_set=NULL,
                               text_size=NULL,
                               order_by_num=FALSE,
                               gridlines=TRUE,
                               x_axis=TRUE,
                               y_axis=TRUE,
                               ncol=0,nrow=0,
                               return_list=FALSE,return_grob=FALSE,ymax=NULL){
  ##  ## no need Infer the column(s) to be plotted
  samples <- colnames(mat)
  n_samples <- ncol(mat)

  ## identify channel set if not provided by comparing to known sets
  if(is.null(channel_set)){
    ## guess
    potential_channel_set <- names(which(sapply(ch,function(x){length(intersect(rownames(mat),x$channels))==nrow(mat) && length(intersect(rownames(mat),x$channels))==length(x$channels)}))[1])
    if(!is.na(potential_channel_set)){
      channel_set <- potential_channel_set
    }

  }

  ## Get the font size
  if(is.null(text_size)){
    size_cor <- 9*exp(-(n_samples/8))
    size_cor <- ifelse(size_cor < 2,2,size_cor)
  }else{
    size_cor= text_size
  }

  ## generate the plots
  plts <- lapply(samples,function(i){
    print(i)
    sdf <- mat[,i,drop=F]

    if(!all(sdf[,i]<1)){
      title <- paste0(i,' (',as.character(round(sum(sdf),1)),' Indels)')
    }else{
      title <- i
    }
    ret <- signature_barplot(mat = sdf,channel_set = channel_set,text_size = size_cor/2,ymax=ymax)+
      ggtitle(title)+
      ggplot2::labs(x='',y='')+
      ## plus size modifications
      ggplot2::theme(legend.position = 'none',
                     plot.title = element_text(size = size_cor+10,color = 'black',face = 'bold'),
                     axis.title = ggplot2::element_text(size=size_cor+1,color='black'),
                     strip.text = ggplot2::element_text(size=size_cor+1,color='black'),
                     axis.text.x = ggplot2::element_text(size=size_cor-4,color = 'black',face = 'plain',family = 'sans'),
                     axis.text.y = ggplot2::element_text(size=size_cor,color='black',face='plain')
                     )

    if(gridlines==FALSE){
      ret <- ret+theme(panel.grid.major.y = ggplot2::element_blank())
    }

    if(x_axis==FALSE){
      ret <- ret+theme(axis.text.x.bottom = ggplot2::element_blank())
    }

    if(y_axis==FALSE){
      ret <- ret+theme(axis.text.y= ggplot2::element_blank())

    }


    ## add errbar
    if(!is.null(err)){

      errdf <- err[,i,drop=F] %>%
        as.data.frame %>%
        tibble::rownames_to_column()

      colnames(errdf) <- c('rowname','err')

      ## by default, mean and err have the same rownames
      errdf$upper <- errdf[['err']] + sdf[,i]
      errdf$lower <- -errdf[['err']] + sdf[,i]

      if(!is.null(ymax)){
        max_param <- ymax
      }else{
        max_param <- max(errdf$upper)
      }

      ## shift everything up
      ## modify the blocks
      ret$layers[[2]]$data$ymin <- max_param*1.08
      ret$layers[[2]]$data$ymax <- max_param*1.2

      ## modify the text layer
      ret$layers[[3]]$data$ymin <- max_param*1.08
      ret$layers[[3]]$data$ymax <- max_param*1.2

      ret <- ret+
        ggplot2::geom_errorbar(data=errdf,
                               mapping = ggplot2::aes(x=rowname,ymin=lower,ymax=upper),
                               inherit.aes = F,
                               width=0.7)+
        ggplot2::coord_cartesian(ylim=c(0,max_param*1.2))

    }
    return(ret)
  })

  ## modify the list of plots before export
  if(order_by_num==TRUE & n_samples >3){
    plts <- plts[order(colSums(mat))]
  }

  if(return_list==TRUE){
    p <- plts
  } else if(length(plts)==1){
    if(return_grob==TRUE){
      p <- suppressWarnings(ggplot2::ggplotGrob(plts[[1]]))
    }else{
      p <- plts[[1]]
    }

  }else{ ## returning more than one plot

    if(!missing(ncol)){
      plts <- c(plts,list(ncol=ncol))
    }

    if(!missing(nrow)){
      plts <- c(plts,list(nrow=nrow))
    }

    if(return_grob==TRUE){
      p <- suppressWarnings(do.call(gridExtra::arrangeGrob,plts))
    }else{
      suppressWarnings(do.call(gridExtra::grid.arrange,plts))
      p <- invisible()
    }
  }

  ## catchall return
  return(p)
}





bt <- function(x){paste0('`',x,'`')}

extract_legend = function(plot_in, theme_in = ggplot2::theme(legend.position = "top")){
  gtable::gtable_filter(ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot_in + theme_in)), "guide-box")
}






#' ### Glodzik style plots
#'
#' glodzik_barplot_formatting <- ggplot2::theme(axis.text.x= ggplot2::element_text(angle=90, vjust=0.5, size=10,colour = "black"),
#'                                              axis.text.y= ggplot2::element_text(size=10,colour = "black"),
#'                                              axis.title =  ggplot2::element_text(size=15),
#'                                              axis.ticks.x.bottom = ggplot2::element_blank(),
#'                                              axis.ticks.y.left= ggplot2::element_line(colour = 'black'),
#'                                              axis.line.y.left = ggplot2::element_line(colour = 'black'),
#'                                              legend.position='none',
#'                                              plot.title =  ggplot2::element_text(size=10),
#'                                              panel.grid = ggplot2::element_blank(),
#'                                              panel.grid.major.y = ggplot2::element_line(color = 'grey'),
#'                                              panel.background = ggplot2::element_rect(fill = "white"),
#'                                              panel.border =  ggplot2::element_rect(colour = "white", fill=NA),
#'                                              strip.background = ggplot2::element_rect(colour = 'white',fill='white'))
#'
#' glodzik_barplot <- function(df,x='Subtype',fill='Mut_Type',y='Summary',xlab='Channels',
#'                             ylab='Count',title='',text_size=2,ymax=NA){
#'
#'   ## Add a type for splitting the channels into two subplots
#'   df$Type <- 'Deletion'
#'   df$Type[stringi::stri_detect(df[[x]],regex = '\\+|Ins')] <- 'Insertion'
#'   df$Type <- factor(df$Type,levels=c('Insertion','Deletion'))
#'
#' #stringi::stri_detect(df[[x]],regex = '\\+')
#'   ## Determine the group coloring scheme
#'
#'   fill_group <- unique(as.character(df[[fill]]),na.rm=T)
#'   has_fill_group <- TRUE
#'   ## if no fill group present, default to using Type as fill group
#'   if(length(fill_group)==0){
#'     message(paste0('Fill group "',fill,'" not found in data. Defaulting to Type.'))
#'     fill_group <- c('Insertion','Deletion')
#'     fill <- 'Type'
#'     has_fill_group <- FALSE
#'   }
#'
#'   ## check fill_group against available palettes for a match
#'   which_palette <- which(sapply(palettes,
#'                                  function(x){
#'                                    return(all(names(x) %in% fill_group) &
#'                                             all(fill_group %in% names(x)) &
#'                                             length(x)==length(fill_group))
#'                                  }))
#'
#'   ## if no match for palette and no fill group, default fill group and default palette are used
#'   if( (length(which_palette)==0) & (has_fill_group==FALSE)){
#'     message('No matching palette found. Defaulting to default.')
#'     which_palette <- which(names(palettes)=="glodzik_default")
#'     group_color <- palettes[[which_palette]]
#'     ## else if there's just no match, but a valid fill_group
#'   } else if( (length(which_palette)==0) & (has_fill_group==TRUE) ){
#'     ##  Numeric coloring based on size
#'     n_fills <-length(fill_group)
#'     group_color <- palettes[['range']][1:n_fills]
#'     names(group_color) <-fill_group
#'
#'     ## Else, if there is a match for the palette
#'   } else if(length(which_palette) > 0){
#'     group_color <- palettes[[which_palette]][match(names(palettes[[which_palette]]),fill_group)]
#'   }else{
#'     stop("Error, this part should not be evaluated.")
#'   }
#'
#'   ## Generate lightened versions of the colors so that text can show up on them.
#'   blocks_color <- sapply(group_color,function(x){
#'     colorRampPalette(c(x,'white'))(4)[2]
#'   })
#'   names(blocks_color) <- paste0(names(blocks_color),'_1')
#'
#'
#'   # ## remove the complex indels
#'   # df <- df[df[[x]]!='[Com]',] ## remove complex events
#'
#'   ## Set order of X axis bars using the order they are given in the data frame
#'   df[[x]] <- factor(df[[x]],levels = unique(df[[x]]))
#'   df[[fill]] <- factor(df[[fill]],levels = unique(df[[fill]]))
#'
#'
#'   ## Generate the overhead blocks with group names
#'   split_by_type <- split(df[[fill]],df[['Type']])
#'
#'   blocks <- tibble::as_tibble(do.call(rbind,mapply(split_by_type,names(split_by_type),FUN = function(x,y){
#'     entry <- (table(x)[table(x)!=0])
#'     data.frame(Type=rep(y,length(entry)),
#'                fill=paste0(names(entry),'_1'),
#'                xmin=c(0,cumsum(entry)[-length(entry)])+0.5,
#'                xmax=cumsum(entry)+0.5)
#'   },SIMPLIFY = F)))
#'
#'   if(!is.na(ymax)){
#'     max_param <- ymax
#'   }else{
#'     max_param <- max(df[[y]])
#'   }
#'
#'
#'   blocks$ymin <- max_param*1.08
#'   blocks$ymax <- max_param*1.14
#'
#'   ## Modify text in the blocks as needed to save space ex.(Non-repeating --> NR)
#'   blocks$labels <- stringi::stri_replace_all(blocks[['fill']],replacement = '',regex = ' insertions| deletions|_1')
#'   blocks$labels <- stringi::stri_replace_all(blocks[['labels']],replacement = 'NR',regex = 'Non-repeating')
#'   blocks$labels <- stringi::stri_replace_all(blocks[['labels']],replacement = 'Spaced',regex = 'Longer A-periodic')
#'   blocks$labels <- stringi::stri_replace_all(blocks[['labels']],replacement = 'N-Mers',regex = 'N-Mer')
#'
#'
#'   ## Generate the initial plot
#'   p <- ggplot2::ggplot(df,ggplot2::aes_string(x=bt(x),y=bt(y),fill=bt(fill)))+
#'     ggplot2::geom_bar(stat='identity',position='dodge',size=0.3) +
#'     ggplot2::labs(ylab=ylab)+
#'     ggplot2::ggtitle(title)+
#'     ggplot2::facet_wrap(~Type,scales='free_x')
#'
#'
#'
#'   ## Alter the scale if neccesary
#'   if(all(df[[y]] < 1)){
#'     max_height <- round(max_param,digits = 2)
#'
#'     p <- p+
#'       ggplot2::scale_y_continuous(breaks = seq(0,max_height,by=0.05),
#'                                   labels = scales::percent,
#'                                   expand=c(0,0,0,0)) +  ggplot2::ylab('Percentage')+
#'       ggplot2::coord_cartesian(ylim=c(min(df[[y]]),unique(blocks$ymax)))
#'
#'   } else{
#'
#'     p <- p + ggplot2::scale_y_continuous(expand=c(0,0,0,0))+
#'       ggplot2::coord_cartesian(ylim=c(min(df[[y]]),unique(blocks$ymax)))
#'
#'   }
#'
#'
#'   ## Add the overhead blocks
#'   p <- p+
#'     ggplot2::geom_rect(data = blocks, ggplot2::aes(xmin=xmin,
#'                                          ymin=ymin,
#'                                          xmax=xmax,
#'                                          ymax=ymax,
#'                                          fill=fill),
#'                        inherit.aes = F)+
#'     ggplot2::geom_text(data=blocks,
#'                        ggplot2::aes(x=(xmax+xmin)/2,
#'                            y=(ymax+ymin)/2,
#'                            label=labels),
#'                        size=text_size,inherit.aes = F)
#'
#'   # ## Apply the group coloring
#'   # if(length(which_palette)>0){ ## if there's a match, then add in the group colorations
#'   p <- p +  ggplot2::scale_fill_manual(values = c(group_color,unlist(blocks_color)))
#'   # }
#'
#'   ## Add glodzik barplot formatting
#'   p <- p + glodzik_barplot_formatting
#'   return(p)
#' }
#'
#'
#' #' A multiple extension of glodzik_barplot with grid.arrange to provide layout support
#' #'
#' #' @param df A dataframe with more than 1 quantity to plot as columns, including a Subtype and Mut_Type column.
#' #' @param ...
#' #'
#' #' @return A grid.arrange object of plots.
#' #' @export
#' #'
#' #' @examples
#' #' ''
#' #'
#' glodzik_barplots<- function(df,text_size,err=F,order_by_num=F,ncol=0,nrow=0,
#'                             x='Subtype',fill='Mut_Type',y,xlab='Channels',
#'                             ylab='Count',title='',return_list=F,return_grob=F,ymax=NA){
#'   ## Infer the column(s) to be plotted
#'
#'   base_cols <- c(x,fill)
#'   if(!(fill %in% colnames(df))){
#'     fill <- ""
#'     base_cols <- c(x)
#'   }
#'
#'   if(missing(y)){
#'     df <- df[,c(base_cols, colnames(df)[!(colnames(df) %in% base_cols)])]
#'
#'   }else{
#'     df <- df[,c(base_cols,y)]
#'   }
#'
#'   conditions <- colnames(df)[!(colnames(df) %in% base_cols)]
#'   ## Get the font size
#'   if(missing(text_size)){
#'     size_cor <- 9*exp(-(length(conditions)/8))
#'     size_cor <- ifelse(size_cor < 2,2,size_cor)
#'   }else{
#'     size_cor= text_size
#'   }
#'   print(size_cor)
#'   plts <- parallel::mclapply(conditions,function(i){
#'     sdf <- df[,c(base_cols,i)]
#'
#'     if(!all(sdf[,i]<1)){
#'       title <- paste0(i,' (',as.character(round(sum(df[,i]),1)),' Indels)')
#'     }else{
#'       title <- i
#'     }
#'     ret <- glodzik_barplot(df = sdf,x = x,fill = fill,y = i,xlab,ylab,title,text_size = size_cor/5,ymax=ymax)+
#'       ggplot2::theme(legend.position = 'none',
#'                      axis.title = ggplot2::element_text(size=size_cor+1,color='black'),
#'                      strip.text = ggplot2::element_text(size=size_cor+1,color='black'),
#'                      axis.text.x = ggplot2::element_text(size=size_cor,color = 'black'),
#'                      axis.text.y = ggplot2::element_blank()) + ggplot2::labs(x='',y='')
#'
#'     if(!is.logical(err)){
#'
#'       errdf <- err[,c(base_cols,i)]
#'       errdf$upper <- errdf[[i]] + sdf[[i]]
#'       errdf$lower <- -errdf[[i]] + sdf[[i]]
#'
#'       errdf$Type <- 'Deletion'
#'       errdf$Type[stringi::stri_detect(errdf[[x]],regex = '\\+|Ins')] <- 'Insertion'
#'       errdf$Type <- factor(errdf$Type,levels=c('Insertion','Deletion'))
#'       # errdf <- errdf[errdf[[x]]!='[Com]',]
#'
#'       errdf[[x]] <- factor(errdf[[x]],levels = unique(errdf[[x]]))
#'
#'
#'       if(!is.na(ymax)){
#'         max_param <- ymax
#'       }else{
#'         max_param <- max(errdf$upper)
#'       }
#'
#'       ret$layers[[2]]$data$ymin <- max_param*1.08
#'       ret$layers[[2]]$data$ymax <- max_param*1.14
#'
#'       ret$layers[[3]]$data$ymin <- max_param*1.08
#'       ret$layers[[3]]$data$ymax <- max_param*1.14
#'
#'       ret <- ret+ggplot2::geom_errorbar(data=errdf,ggplot2::aes_string(x=x,ymin='lower',ymax='upper'))+
#'         ggplot2::coord_cartesian(ylim=c(min(sdf[[i]],errdf$lower),max_param*1.14))
#'
#'     }
#'     return(ret)
#'   },mc.cores = 2)
#'
#'   if(length(plts)==1){
#'
#'     #plts[[length(plts)]] <- plts[[length(plts)]]
#'
#'     if(return_grob==T){
#'       return(ggplot2::ggplotGrob(plts[[1]]))
#'     }
#'
#'
#'   }else{
#'
#'   }
#'
#'   if(order_by_num==T & ncol(df) >3){
#'     plts <- plts[order(colSums(df[,!(colnames(df) %in% c(x,fill))]))]
#'   }
#'
#'   if(return_list==T){
#'     return(plts)
#'   }else{
#'     if(!missing(ncol)){
#'       plts <- c(plts,list(ncol=ncol))
#'     }
#'
#'     if(!missing(nrow)){
#'       plts <- c(plts,list(nrow=nrow))
#'     }
#'
#'     if(return_grob==T){
#'       p <- do.call(gridExtra::arrangeGrob,plts)
#'     }else{
#'       p <- do.call(gridExtra::grid.arrange,plts)
#'     }
#'
#'     return(p)
#'   }
#' }
#'
#'
#'
#' # #### Name editing functions
#' # replace_all <- function(str,regexes,replacements){
#' #   for(i in 1:length(regexes)){
#' #     str <- gsub(pattern = regexes[i],replacement = replacements[i],x = str)
#' #   }
#' #   return(str)
#' # }
#' #
#' #



## Miscellaneous plotting generics

f8_col <- c("#2d5b35","#efb9b5","#9cc98e","#264481",
            "#f65e56","#8a86b8","#d1d0e2","#f5bf52", "#477fb4","#b97cf6","#559d54","#5e74f6","#b37e5a")
names(f8_col) <- paste0('S',1:13)

usethis::use_data(f8_col,overwrite = T)

f9_cols <- c("#2d5b35","#efb9b5","#9cc98e","#264481",
             "#f65e56","#b97cf6", "#d1d0e2","#f5bf52",
             "#477fb4","#8a86b8", "#559d54","#5e74f6",
             "#b37e5a","#000080","#D57A66","#008080",
             "#811eb4","#f58231","#d2f53c","#808000")
usethis::use_data(f9_cols,overwrite = T)


div_cols <- c('#e6194b','#3cb44b','#000000','#ffe119','#0082c8','#f58231','#911eb4',
              '#46f0f0','#f032e6','#d2f53c','#fabebe','#008080','#e6beff','#aa6e28',
              '#fffac8','#800000','#aaffc3','#808000','#ffd8b1','#000080','#808080','#FFFFFF')

usethis::use_data(div_cols,overwrite = T)

#' Colors for indel signatures.
#' @docType data
#' @usage f8_col
#' @format  An object of the type list
#' @examples
#' f8_col
"f8_col"


#' Divergent colors for indel profiles
#' @docType data
#' @usage div_col
#' @format  An object of the type list
#' @examples
#' div_col
"div_cols"


