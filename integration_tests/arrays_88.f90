program arrays_88
    implicit none
    integer, parameter :: n = 5
    integer :: offset(n)
    real :: offsetr(n)
    integer :: pos
    integer :: offset_date

    offset = (/1, 2, 3, 4, 5/)
    offsetr = (/1.0, 2.0, 3.0, 4.0, 5.0/)
    pos = 10
    offset_date = 3

    print *, peek(pos + offset(:offset_date))
    if( any(peek(pos + offset(:offset_date)) /= [110, 120, 130]) ) error stop

    print *, peekr(pos + offsetr(:offset_date))
    if( any(peekr(pos + offsetr(:offset_date)) /= [110.0, 120.0, 130.0]) ) error stop

contains

    function peek(x) result(y)
        integer, intent(in) :: x(:)
        integer :: y(size(x))
        y = x * 10
    end function peek

    function peekr(x) result(y)
        real, intent(in) :: x(:)
        real :: y(size(x))
        y = x * 10
    end function peekr

end program arrays_88
