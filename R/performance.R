# LorenzCurve <- function(data, model, add = F, Legend = T, ColLine = NULL, xlab = "Observed",
#                         ylab = "Predicted", main = "Lorenz curve", ...)
# {
#   x = (0:100) / 100
#   if(!add)
#     plot(x, x, type = "l", main = main, xlab = xlab, ylab = ylab)
#   l = NULL
#   area = NULL
#   model = as.data.table(model)
#   for(i in 1:ncol(model))
#   {
#     ordercost = data[order(model[[i]], decreasing = T)]
#     cumcost   = cumsum(ordercost) / sum(ordercost)
#     lines(x, c(0, quantile(cumcost, 1:100 / 100)), col = if(is.null(ColLine)) i + 1 else ColLine, ...)
#     area = c(area, mean(cumcost))
#     # grid(lwd = 2)
#   }
#   if(Legend)
#     legend(x = 0.6,y = 0.55,legend = paste0('Gini: ', round(area, 3)), col = 2:(ncol(model) + 1), lty = c(1, 1), lwd = 2)
#   return(c("Gini" = area))
# }
