plot_preference <- function(group= "control", OG_scores=T){
      
      # alt method is to center  by response.score per bat to remove effect of some bats having higher scores in general than others. this doesn't necessarily make sense...
      # other method is to use proportions to remove effect of total number of choices
      if(OG_scores){
            points <- 
                  d %>% 
                  # get group
                  filter(Group==group) %>% 
                  group_by(Bat.ID) %>% 
                  mutate(pref = Response.score) %>% 
                  ungroup() %>% 
                  # identify trial for bootstrapping
                  mutate(effect= Playback)  
            
      }else{
            points <- 
                  d %>% 
                  # get group
                  filter(Group==group) %>% 
                  # center counts by bat
                  group_by(Bat.ID) %>% 
                  mutate(pref = Response.score - mean(Response.score)) %>% 
                  ungroup() %>% 
                  # identify trial for bootstrapping
                  mutate(effect= Playback)  
            
            
      }
      
      
      
      #effect = Playback      
      # get means and 95% CI
      set.seed(121)
      means <- 
            points %>% 
            boot_ci2(y=.$pref, x=.$effect)
      
      
      write_csv(means, paste0("confint_", ifelse(group=="control","naive","experienced")
                              , format(Sys.time(), "%Y-%m-%d_%H-%M")
                              , ".csv") )
      
      # plot means, 95% CI and raw data
      plot <- 
            means %>% 
            ggplot(aes(x=effect, y=mean, color=effect))+
            #facet_wrap(~Group, scales="free")+
            geom_point(size=3)+
            geom_jitter(data= points, aes(y= pref), size=1, alpha=0.5, width=0.1, height=0)+
            geom_errorbar(aes(ymin=low, ymax=high, width=.1), size=1)+
            #geom_hline(yintercept = ifelse(OG_scores, NA, 0), color= "dark grey")+
            ylab(ifelse(OG_scores,
                        "Response scores",
                        "relative response scores (observed - expected)"))+
            xlab("")+
            ggtitle(ifelse(group=="control", expression(paste("Naive ", italic("T. cirhossus"))), 
                           expression(paste("Experienced ", italic("T. cirrhosus")))))+
            theme_cowplot()+
            scale_color_manual(values=c('#1b9e77','dark grey','#d95f02','#7570b3'))
      
      ggsave(paste0("plot_", ifelse(group=="control","naive","experienced")
                    , format(Sys.time(), "%Y-%m-%d_%H-%M")
                    , ".pdf"))
      
      
      list(means,plot)
}

# get results----
# use that function to plot preferences for AJ
# table shows summary data that is plotted
(naive.results <- plot_preference(group= "control", OG_scores = T))

# use that function plot preferences for L
(experienced.results <- plot_preference(group="treatment", OG_scores = T))