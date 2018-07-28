#' model viability results from a 2-drug screen
#' @param dat data.frame with specific fields available
#' @param drugA character(1) drug name
#' @param drugB character(1) drug name
#' @param incell_line character(1) cell_line name
#' @return lm of the model for viability with concA x concB interaction and all main effects
#' @note Assumes independence of all replicates.
#' @examples
#' data(dc)
#' viabReg(dc)
#' @export
viabReg = function (dat, drugA = "5-FU", drugB = "ABT-888", incell_line = "A2058") 
{
    stopifnot(all(c("drugA_name", "drugB_name", "cell_line", 
        "drugA.Conc..µM.", "drugB.Conc..µM.") %in% names(dat)))
    lit = dat %>% filter(drugA_name == drugA & drugB_name == 
        drugB & cell_line == incell_line)
    mmm = melt(lit, measure.vars=c("viability1", "viability2", "viability3", "viability4"))
    lm(value~drugA.Conc..µM.*drugB.Conc..µM., data=mmm)
}

