library(ggplot2)

x <- 150
df <- read.table("input.dat", col.names=c("chunk", "mem", "user"))

nrow(df)
df1 <- cbind(df[, c("chunk", "mem")], "mem")
colnames(df1)[2] <- "value"
colnames(df1)[3] <- "var"
df2 <- cbind(df[, c("chunk", "user")], "user")
colnames(df2)[2] <- "value"
colnames(df2)[3] <- "var"
df <- rbind(df1, df2)

ggplot(df, aes(x=chunk, y=value)) + geom_line() +
    facet_grid(var ~ .,scales="free") +
    scale_x_log10() +
    geom_vline(xintercept=c(1, 2, 4, 8, 16, 32, 64, 128,
                            256, 512, 1024, 2048, 4096),
               color="blue", linetype="dotted") +
    geom_vline(xintercept=x, color="green") +
    geom_point() + geom_line() +
    ggsave("out.svg", height=4.5, width=9)
