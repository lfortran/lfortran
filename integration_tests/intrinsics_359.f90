program intrinsics_359
    implicit none
    integer :: k
    print *, pack( [(k, k = 1, 2)] , [.true., .true.] )
    if (any(pack( [(k, k = 1, 2)] , [.true., .true.] ) /= [1, 2])) error stop
end program