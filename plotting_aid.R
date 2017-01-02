plotting_aid <- function(rdir, pdata_plot, pdata2_plot){

# ------------------------------------------------------------------------------- 
# pdata_plot
pdf (file = paste0(rdir,"pdata_plot_",Sys.Date(),".pdf"))
print(pdata_plot[[1]])
print(pdata_plot[[2]]) 
print(pdata_plot[[3]]) 
print(pdata_plot[[4]]) 
print(pdata_plot[[5]]) 
print(pdata_plot[[6]])
print(pdata_plot[[7]])
print(pdata_plot[[8]])


# pdata_plot_adv_baseline[[1]]
# pdata_plot_adv_baseline[[2]]
# pdata_plot_adv_baseline[[3]]
# pdata_plot_adv_baseline[[4]]
# pdata_plot_adv_baseline[[5]]
# pdata_plot_adv_baseline[[6]]
# print(pdata_plot_adv_baseline [[1]])
# print(pdata_plot_adv_baseline [[2]])
# print(pdata_plot_adv_baseline [[3]])
# print(pdata_plot_adv_baseline [[4]])
# print(pdata_plot_adv_baseline [[5]])
# print(pdata_plot_adv_baseline [[6]])
dev.off()

# ------------------------------------------------------------------------------- 
# pdata2_plot

pdf (file = paste0(rdir,"pdata_plot2_",Sys.Date(),".pdf"))
# pdata2_plot[[1]]
# pdata2_plot[[2]]
# pdata2_plot[[3]]
# pdata2_plot[[4]]
print(pdata2_plot[[1]])
print(pdata2_plot[[2]]) 
print(pdata2_plot[[3]]) 
print(pdata2_plot[[4]]) 
print(pdata2_plot[[5]]) 
dev.off()

} # end of function

