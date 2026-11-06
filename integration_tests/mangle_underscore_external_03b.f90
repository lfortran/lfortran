! Global procedures whose names differ only by a trailing underscore
subroutine mue03_f(k)
    integer, intent(inout) :: k
    k = k + 1
end subroutine

subroutine mue03_f_(k)
    integer, intent(inout) :: k
    k = k + 100
end subroutine

integer function mue03_h_(n)
    integer, intent(in) :: n
    mue03_h_ = 3*n
end function
