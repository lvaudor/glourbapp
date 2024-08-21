
calculate_gmm_density=function(gmm){
  variance=gmm$parameters$variance$sigmasq
  if(length(variance)<gmm$G){variance=rep(variance,gmm$G)}
  data_gmm=data.frame(mean=gmm$parameters$mean,
                      variance=variance,
                      proportion=gmm$parameters$pro)
  x=seq(-100,100,by=1)
  data_density=tibble::tibble(.rows=NULL)
  for(i in 1:nrow(data_gmm)){
    y=data_gmm$proportion[i]*dnorm(x=x,
                                   mean=data_gmm$mean[i],
                                   sd=sqrt(data_gmm$variance[i]))
    data_density=dplyr::bind_rows(data_density,
                                  data.frame(x=x,y=y,cat=i))
  }
  data_density=data_density %>%
    dplyr::mutate(cat=paste0("G",cat))
  return(data_density)
}

calc_density_from_deciles = function(ddeciles,n){
  n=round(n)
  if (nrow(ddeciles) != 11) {
    stop("Le vecteur des déciles doit avoir une longueur de 11.")
  }
  if (n < 10) {
    stop("Les observations sont trop peu nombreuses pour étudier les distributions")
  }
  # Générer une distribution aléatoire basée sur les déciles
  generated_data <- numeric()
  for (i in 2:11) {
    set.seed(33)
    generated_data <- c(generated_data,
                        runif(round(n/10),
                              min = ddeciles$value[i-1],
                              max = ddeciles$value[i]))
  }
  dat_obs <- data.frame(values = generated_data)
  gmm = Mclust(dat_obs$values, G = 1:3, verbose=FALSE)
  dat_density=calculate_gmm_density(gmm) %>%
    dplyr::mutate(y=y*n)
  dat_means=data.frame(means=gmm$parameters$mean,
                       cat=paste0("G",1:gmm$G))
  dat_obs=dat_obs %>%
    dplyr::mutate(classification=as.factor(gmm$classification))
  dat_metrics=data.frame(G=gmm$G)
  return(list(dat_metrics=dat_metrics,
              dat_obs=dat_obs,
              dat_density=dat_density,
              dat_means=dat_means))
}
plot_density_from_deciles <- function(dat,title="") {
  ggplot2::ggplot(dat$dat_obs, ggplot2::aes(x = values) )+
    ggplot2::geom_histogram(fill="light grey",alpha=0.5,binwidth=1)+
    ggplot2::geom_line(data=dat$dat_density,
                       ggplot2::aes(x=x,y=y,col=cat))+
    ggplot2::labs(title = title,
         x = "Value", y = "Proportion") +
    ggplot2::theme_minimal()+
    ggplot2::geom_vline(xintercept=0,linetype=3)+
    ggplot2::geom_vline(data=dat$dat_means,
                        ggplot2::aes(xintercept=means, col=cat),
               linetype=3)
}
plot_density=function(city_GSWdensity){
  city=unique(city_GSWdensity$UrbanAggl)
  dat=city_GSWdensity %>%
    dplyr::mutate(title=paste(reach,zone,sep="-")) %>%
    dplyr::select(-data,-UrbanAggl)
  if(nrow(dat)==0){return(NULL)}
  dat_obs=dat %>%
    dplyr::mutate(obs=purrr::map(density,"dat_obs")) %>%
    tidyr::unnest(cols=c(obs))
  dat_density=dat %>%
    dplyr::mutate(density=purrr::map(density,"dat_density")) %>%
    tidyr::unnest(cols=c(density))
  dat_means=dat %>%
    dplyr::mutate(means=purrr::map(density,"dat_means")) %>%
    tidyr::unnest(cols=c(means))
  print(head(dat_obs))
  ggplot2::ggplot(dat_obs, ggplot2::aes(x = values) )+
    ggplot2::facet_wrap(facets=dplyr::vars(title),#rows=dplyr::vars(zone),cols=dplyr::vars(reach),
      scales="free")+
    ggplot2::geom_histogram(fill="light grey",alpha=0.5,binwidth=1)+
    ggplot2::geom_line(data=dat_density,
                       ggplot2::aes(x=x,y=y,col=cat))+
    ggplot2::labs(title = city,
         x = "Value", y = "Proportion") +
    ggplot2::theme_minimal()+
    ggplot2::geom_vline(xintercept=0,linetype=3)+
    ggplot2::geom_vline(data=dat_means,
                        ggplot2::aes(xintercept=means, col=cat),
               linetype=3)
}
