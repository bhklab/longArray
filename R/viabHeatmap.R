#' depict data from a 2-drug screen
#' @param dat data.frame with specific fields available
#' @param drugA character(1) drug name
#' @param drugB character(1) drug name
#' @param incell_line character(1) cell_line name
#' @return a ggplot object
#' @examples
#' data(dc)
#' viabDots(dc)
#' @export
viabDots = function (dat, drugA = "5-FU", drugB = "ABT-888", incell_line = "A2058") 
{
    stopifnot(all(c("drugA_name", "drugB_name", "cell_line", 
        "drugA.Conc..µM.", "drugB.Conc..µM.") %in% names(dat)))
    lit = dat %>% filter(drugA_name == drugA & drugB_name == 
        drugB & cell_line == incell_line)
    mmm = melt(lit, measure.vars=c("viability1", "viability2", "viability3", "viability4"))
    mmm$value = as.numeric(mmm$value) # ???
    ggplot(mmm, aes(x = factor(drugA.Conc..µM.), y=value, group = drugB.Conc..µM.)) + 
        geom_point() + facet_grid(cols=vars(drugB.Conc..µM.)) + xlab(paste("within column:", drugA, "conc.")) +
          ggtitle(paste("between column: ", drugB, "conc.")) + ylab(paste("viability of", incell_line))
}



old.viabHeatmap = function(dat, drugA="5-FU", drugB="ABT-888", cell_line = "A2058") {
  stopifnot(all(c("drugA_name", "drugB_name", "cell_line", 
       "drugA.Conc..µM.", "drugB.Conc..µM.") %in% names(dat)))
  lit = dat %>% filter(drugA_name == drugA & drugB_name == drugB
   & cell_line == cell_line)
  ggplot(lit, aes(x=drugA.Conc..µM., y=drugB.Conc..µM.)) + 
    geom_tile(aes(fill=viability1)) + ggtitle(paste0(drugA, " x ",  
        drugB, " in ", cell_line)) + xlab(paste(drugA, "µM")) + 
          ylab(paste(drugB, "µM"))
}
