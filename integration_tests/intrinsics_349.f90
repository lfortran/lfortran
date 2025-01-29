program intrinsics_349
    integer :: A(2,2) = reshape([1,2,3,4],[2,2])
    call temp(A)
contains
    subroutine temp(A)
        integer, intent(inout) :: A(:,:)
        integer :: X(size(A,1)*size(A,2))
        X = [1,2,3,4]
        print *, pack(X, [.true., .true., .true., .true.])
        if (any(pack(X, [.true., .true., .true., .true.]) /= [1,2,3,4])) error stop
    end subroutine
end program