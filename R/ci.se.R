ci.se <-
function(data.in, conf.level, verbose=0) {

    if (is.list(data.in)) {
        k = length(data.in)
    }
    if (is.data.frame(data.in) | is.matrix(data.in)) {
        k = ncol(data.in)
    }
    if (is.vector(data.in) & !is.list(data.in)) {
        k = 1
    }

    data.out <- as.data.frame(matrix(NA, nrow=k, ncol=8))
    colnames(data.out) <- c("n", "mean", "std.dev", "std.error", "conf.level",
                            "error.margin", "lower.ci", "upper.ci")

    for ( i in 1:k) {
        if (is.list(data.in)) {
            data.temp <- data.in[[i]]
        }
        if (is.data.frame(data.in) | is.matrix(data.in)) {
            data.temp <- data.in[ ,i]
        }
        if (is.vector(data.in) & !is.list(data.in)) {
            data.temp <- data.in
        }

        n = length(data.temp)
        sample.mean <- mean(data.temp)
        std.dev <- sd(data.temp)
        std.error <- std.dev / sqrt(n)
        error.margin <- std.error * qt(conf.level, df=n-1)
        data.out[i, ] <- c(n,
                          sample.mean,
                          std.dev,
                          std.error,
                          conf.level,
                          error.margin,
                          sample.mean - error.margin,
                          sample.mean + error.margin);

        if (verbose) {
            print(sprintf("k=%0.0f, i=%0.0f, n=%0.0f, mean=%0.3f, sd=%0.3f, se=%0.3f, error=%0.03f",
                          k, i, n, sample.mean, std.dev, std.error, error.margin))
        }
    }
    return(data.out)
}
