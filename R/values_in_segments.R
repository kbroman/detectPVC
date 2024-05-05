# interval function
#
# v = vector of indexes
#
# segments = matrix with rows corresponding to intervals and two columns with
# the start and end numeric index of each interval
#
# returns a logical vector with same length as v, indicating whether that
# element of v is contained within one of the segments

values_in_segments <-
    function(v, segments)
{

    m <- lapply(seq_len(nrow(segments)), function(r)
        v >= segments[r,1] & v <= segments[r,2])

    m <- matrix(unlist(m), byrow=TRUE, nrow=length(m))

    apply(m, 2, any)
}


# this is the converse: which segments contain at least one of the values
segments_contain_values <-
    function(v, segments)
{

    m <- lapply(seq_len(nrow(segments)), function(r)
        v >= segments[r,1] & v <= segments[r,2])

    m <- matrix(unlist(m), byrow=TRUE, nrow=length(m))

    apply(m, 1, any)
}
