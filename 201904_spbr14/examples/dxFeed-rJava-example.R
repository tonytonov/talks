library(rJava)
library(ggplot2)
library(extrafont)
rjava_obj <- R6::R6Class('rJavaObject',
    public = list(
        symbol = NA,
        jobject = NULL,
        get_orders = function() {
            .jcall(self$jobject, '[[D', method = 'getOrders', simplify = TRUE)
        },
        initialize = function(symbol) {
            .jinit(classpath = '/path/to/dxfeed-samples-rjava.jar')
            self$symbol <- symbol
            self$jobject <- .jnew('com.dxfeed.sample._simple_/OrderBookR', symbol)
        }
    )
)
ibm <- rjava_obj$new("IBM")
Sys.sleep(2)
order_book <- as.data.frame(ibm$get_orders())
colnames(order_book) <- c("Price", "Size", "Side")
loadfonts(device = "win")
ggplot(order_book, aes(Price, Size, fill = factor(Side))) +
    geom_col() +
    xlim(c(267, 269)) +
    theme_minimal(base_size = 16, base_family = "Ubuntu") +
    scale_fill_discrete(guide = F) +
    labs(title = "Aggregated outstanding orders for IBM, 13.04.2017")
