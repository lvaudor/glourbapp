gmm_density=function(tib_gmm_results){
  x=seq(-100,100,by=1)
  data_density=tibble::tibble(.rows=NULL)
  for(i in 1:nrow(tib_gmm_results)){
    y=tib_gmm_results$prop[i]*dnorm(x=x,
                                    mean=tib_gmm_results$mean[i],
                                    sd=sqrt(tib_gmm_results$var[i]))
    data_density=dplyr::bind_rows(data_density,
                                  data.frame(cat=i,x=x,y=y))
  }
  data_density=data_density %>%
    dplyr::mutate(cat=paste0("G",cat))
  return(data_density)
}

plot_density_facet=function(GSWdensity,selected_reach,selected_zone){
  dat=GSWdensity %>%
    dplyr::filter(reach==selected_reach,
                  zone==selected_zone)
  n=dat$max_extent
  dat_frequency=glourbapp:::gmm_density(dat$gmm_results[[1]]) %>%
    dplyr::mutate(y=y*n)
  dat_means=dat$gmm_results[[1]]
  dat_decile_density=dat$decile_density[[1]]
  ggplot2::ggplot(dat_frequency)+
    ggplot2::geom_rect(data=dat_decile_density, fill="dark grey",
                       ggplot2::aes(xmin=x1,xmax=x2,ymin=0,ymax=y))+
    ggplot2::geom_line(data=dat_frequency,
                       ggplot2::aes(x=x,y=y,col=cat))+
    ggplot2::labs(x = "Value", y = "Proportion") +
    ggplot2::theme_minimal()+
    ggplot2::geom_vline(xintercept=0,linetype=3)+
    ggplot2::geom_vline(data=dat_means,
                        ggplot2::aes(xintercept=mean, col=cat),
                        linetype=3)+
    ggplot2::ggtitle(paste(selected_reach,selected_zone,sep="-"))+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

plot_density=function(city_GSWdensity){
  result=city_GSWdensity %>%
    dplyr::mutate(plots=purrr::map2(.x=reach,.y=zone,
                                    ~glourbapp:::plot_density_facet(city_GSWdensity,
                                                 selected_reach=.x,
                                                 selected_zone=.y)))
  get_result=function(selected_reach,selected_zone){
    sel=result %>% dplyr::filter(reach==selected_reach,zone==selected_zone)
    if(nrow(sel)==0){return(NULL)}else{
      plot=sel %>% dplyr::pull(plots) %>% .[[1]]
      return(plot)
    }
  }
  figure=ggpubr::ggarrange(
    get_result("upstream","corridor"),
    get_result("upstream","plain"),
    get_result("urban","corridor"),
    get_result("urban","plain"),
    get_result("downstream","corridor"),
    get_result("downstream","plain"),
    ncol = 2, nrow = 3)
  return(figure)
}
