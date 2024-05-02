# internal function to create a series of windows along a vector,
# with a bit of padding on each end
#
# length = length of target vector
# window = length of window size
# pad = amount to pad on each end
#
# output = matrix with columns pre, start, end, post
# (start,end) are the non-overlapping windows
# pre and post give the wider intervals to use, with the padding

create_windows <-
    function(length, window, pad)
{
    if(length <= window) {
        return(data.frame(pre=1, start=1, end=length, post=length))
    }

    start <- c(1, seq(round(window/2), to=length, by=window))
    pre <- c(1, start[-1]-pad)
    end <- start+window-1
    post <- start+window-1+pad
    end[1] <- start[2]-1
    post[1] <- end[1]+pad

    result <- data.frame(pre=pre, start=start, end=end, post=post)

    result[result <= 1] <- 1
    result[result >= length] <- length

    result
}
