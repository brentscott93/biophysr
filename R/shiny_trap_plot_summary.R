trap_selected_project <- "/Users/brentscott/Box Sync/Muscle Biophysics Lab/Data/biophysr/bscott/trap/project_skeletalAzo"
shiny_trap_plot_summary <- function(trap_selected_project){

  sum_sheet <- list.files(path = trap_selected_project,
                          pattern = "trap_summary.csv",
                          recursive = TRUE,
                          full.names = TRUE)

project_summary <- read_csv(sum_sheet)
all_events <- list.files(paste0(trap_selected_project, "/summary"),
                         pattern = "events.csv",
                         full.names = TRUE)

event_files_filtered <- read_csv(all_events)

#step size dotplot
ggdot <- ggplot(event_files_filtered, aes(x = condition,
                                 y = displacement_nm))+
  geom_dotplot(aes(fill = condition), binaxis = "y",
               binwidth = 0.5, alpha = 0.8,
               stackdir = "center")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
               geom="pointrange", color="black", size = 1, shape = 18)+
  #scale_fill_manual(values = colors)+
  theme_linedraw(base_size = 18)+
  #geom_hline(aes(yintercept = 0))+
  ggtitle("Step Size (mean \u00B1 sd)")+
  scale_fill_brewer(palette = "Dark2")+
  ylab("Displacement (nm)")+
  xlab("")+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

dir.create(paste0(trap_selected_project, "/summary/figures"))

ggsave(plot = ggdot, filename = paste0(trap_selected_project, "/summary/figures/step_size_dotplot.png"))


#step size histogram

conditions <- unique(event_files_filtered$conditions)


gghisto_step <- function(conditions, fill, data, bw){
  title <- conditions
  conditions <- enquo(conditions)
  df <- dplyr::filter(data, conditions == !!conditions)

  ggplot(data = df )+
    geom_histogram(aes(x = displacement_nm, y = stat(density)),
                   binwidth = bw, color = "black", size = 2)+
    stat_function(aes(x = displacement_nm),
                  fun = dnorm,
                  args = list(mean = mean(df$displacement_nm, na.rm = TRUE),
                              sd = sd(df$displacement_nm, na.rm = TRUE)),
                  color = "black",
                  size = 2,
                  linetype = "dashed")+
    annotate('text',x = Inf, y = Inf, hjust = 1, vjust =1,
             label = paste0("x̄ = ",
                            round(mean(df$displacement_nm, na.rm = TRUE), 2),
                            " \u00B1 ",
                            round(std.error(df$displacement_nm, na.rm = TRUE), 1),
                            " nm"),
             size = 8)+

    annotate('text',x = -Inf, y = Inf, hjust = 0, vjust =1,
             label = paste0(" # Events = ",
                            length(!is.na(df$displacement_nm))),
             size = 8)+
    scale_fill_brewer(palette = "Dark2")+
    theme_classic(base_size = 20)+
    ggtitle(title)+
    xlab("Displacement (nm)")+
    ylab("Density")+
    scale_y_continuous(expand= expand_scale(mult = c(0,0.05 )))+
    #scale_x_continuous(breaks = seq(floor(min(df$displacement_nm)), ceiling(max(df$displacement_nm)), by = by))+
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5))

}

colors <- brewer.pal(8, "Dark2")
colors <- colors[1:length(conditions)]
step_histograms <- map2(conditions, colors,  gghisto_step, data = event_files_filtered, bw = 2)

step_histograms[[2]]

##########



event_files_filtered$myo <- factor(event_files_filtered$myo, levels = c("myoV-WT", "myoV-S217A"))
event_files_filtered$phosphate <- factor(event_files_filtered$phosphate, levels = c("noPi", "Pi"))

facets <- tibble(myo = c("myoV-WT", "myoV-WT", "myoV-S217A", "myoV-S217A"),
                 phosphate = c("noPi", "Pi", "noPi", "Pi"),
                 text = c("WT", "WT w/Pi", "S217A", "S217A w/Pi")) %>%
  right_join(project_summary)

#facets$myo <- factor(facets$myo, levels = c("myoV-WT", "myoV-S217A"))
#facets$phosphate <- factor(facets$phosphate, levels = c("noPi", "Pi"))


#colors <- c("blue", "lightblue", "red", "pink")

#histo_step <- function(data, fill, title, summary){
 # ggplot(data = data)+
 #   geom_histogram(aes(x = displacement_nm, y = stat(density)),
       #            fill = conditions, binwidth = 2, color = "black", size = 2)+
   # annotate('text',x = Inf, y = Inf, label = "bar(x)", hjust = 1, vjust =1, parse=T, size = 12)+
   # annotate('text',x = Inf, y = Inf, hjust = 1, vjust =1, label = paste0(" mean = ",
     #                                                  round(summary$displacement_avg, 2),
     #                                                  " \u00B1 ",
       #                                              round(summary$displacement_se, 1),
        #                                              " nm"), size = 12)+
    # annotate('text',x = -27.5, y = 0.09, label = paste0("# Events = ",
    #                                                    summary$num_events),
     #        size = 14)+
   # scale_x_continuous(breaks = seq(-30, 30, by = 5))+
  #  facet_wrap(~conditions)+
  #  theme_classic(base_size = 20)+
 #   ggtitle(title)+
  #  xlab("Displacement (nm)")+
  #  ylab("Density")+
   # coord_cartesian(ylim = c(0, 0.127))+
    #scale_y_continuous(expand= expand_scale(mult = c(0,0.05 )))+
  #  theme(legend.position = "none",
  #        plot.title = element_text(face = "bold", hjust = 0.5))

#}

#toy <- filter(event_files_filtered, conditions == "10uM-ATP")

gghisto_step <- function(data, conditions, fill, bw){
  title <- conditions
   conditions <- enquo(conditions)
  df <- dplyr::filter(data, conditions == !!conditions)

ggplot(data = df )+
  geom_histogram(aes(x = displacement_nm, y = stat(density)),
                 fill = fill,
                binwidth = bw, color = "black", size = 2)+
  stat_function(aes(x = displacement_nm),
                fun = dnorm,
                args = list(mean = mean(df$displacement_nm, na.rm = TRUE),
                            sd = sd(df$displacement_nm, na.rm = TRUE)),
                color = "black",
                size = 2,
                linetype = "dashed")+
   annotate('text',x = Inf, y = Inf, hjust = 1, vjust =1,
            label = paste0("x̄ = ",
                           round(mean(df$displacement_nm, na.rm = TRUE), 2),
                           " \u00B1 ",
                           round(std.error(df$displacement_nm, na.rm = TRUE), 1),
                          " nm"),
            size = 8)+

   annotate('text',x = -Inf, y = Inf, hjust = 0, vjust =1,
            label = paste0(" # Events = ",
                           length(!is.na(df$displacement_nm))),
           size = 8)+
  theme_classic(base_size = 20)+
  ggtitle(title)+
  xlab("Displacement (nm)")+
  ylab("Density")+
  scale_y_continuous(expand= expand_scale(mult = c(0,0.05 )))+
  #scale_x_continuous(breaks = seq(floor(min(df$displacement_nm)), ceiling(max(df$displacement_nm)), by = by))+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

}

atp <- gghisto_step(data = event_files_filtered, conditions = "10uM-ATP", fill = "grey20", bw = 2)
azo <- gghisto_step(data = event_files_filtered, conditions = "50uM-Azo", fill = "#00FF00", bw = 2)
#atp
grid.arrange(atp, azo)#ncol =2)

#only pos
pos_events <- dplyr::filter(event_files_filtered, displacement_nm > 0)

atp2 <- gghisto_step(data = pos_events, conditions = "10uM-ATP", fill = "grey20", bw = 2)
azo2 <- gghisto_step(data = pos_events, conditions = "50uM-Azo", fill = "#00FF00", bw = 2)

grid.arrange(atp2, azo2)#ncol =2)


#wt_pi <- filter(event_files_filtered, myo == "myoV-WT" & phosphate == "Pi")
#wt_no <- filter(event_files_filtered, myo == "myoV-WT" & phosphate == "noPi")
#mut_no <- filter(event_files_filtered, myo == "myoV-S217A" & phosphate == "noPi")
#mut_no <- filter(event_files_filtered, myo == "myoV-S217A" & phosphate == "Pi")
#qqqq <- histo_step(data = event_files_filtered,
#           "data",
#           summary =project_summary)




ggsave("displacement_histograms.png")





######ON TIMES


event_files_filtered$condition <- factor(event_files_filtered$condition,
                                         levels = c("myoV-WT_pH7.0_noPi",
                                                    "myoV-WT_pH7.0_Pi",
                                                    "myoV-S217A_pH7.0_noPi",
                                                    "myoV-S217A_pH7.0_Pi"))

dat <- split(event_files_filtered, event_files_filtered$condition)

time_ons <-  map2(dat, c("blue",
                         "lightblue",
                         "red",
                         "pink"), fit_exp, bw = 15, zoom = 2000)


exp_dat <- tibble(time_on_ms = rexp(10000, rate = 0.015))

exp_fit <- fit_exp(exp_dat, bw = 10, color = "yellow", zoom = 2000)


f <- fitdistr(exp_dat$time_on_ms, "exponential")

x <- exp_dat$time_on_ms

x_val <- seq(min(x), max(x), along.with = x)
exp_curve <- dexp(x_val, rate = f$estimate)

c <- 1/f$estimate
names(c) <- "Expected Value or Mean (1/rate)"


p <- ggplot(data = exp_dat, aes(x = time_on_ms))+
  geom_histogram(aes(y = stat(density)), breaks = seq(0, max(exp_dat$time_on_ms)+1, 0.25), color = "black", fill = "blue")+
  theme_classic()+
  geom_line(aes(x = x_val, y = exp_curve), color = "black", size = 0.5)+
  annotate("text", label = paste0("Mean = ",
                                  round(1/f$estimate, digits = 2)), x = Inf, y = Inf,
           hjust = 1, vjust = 1, size = 5)+
  theme_classic()

hist(exp_dat$time_on_ms)

r <- list(fit = f,
          plot = p,
          mean = c)



grid.arrange(time_ons$`myoV-WT_pH7.0_noPi`$plot,
             time_ons$`myoV-WT_pH7.0_Pi`$plot,
             time_ons$`myoV-S217A_pH7.0_noPi`$plot,
             time_ons$`myoV-S217A_pH7.0_Pi`$plot,  ncol = 2)
}
