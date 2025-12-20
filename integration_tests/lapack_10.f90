program lapack_10
    implicit none

    ! Declare LSAMEN for the caller
    logical :: lsamen

    character(len=3) :: a, b
    integer :: n

    a = 'ABC'
    b = 'Abc'
    n = 3

    print *, lsamen(n, a, b)
end program lapack_10

logical function lsamen(n, ca, cb)
    implicit none
    integer, intent(in) :: n
    character(len=*), intent(in) :: ca, cb
    integer :: i

    logical :: lsame
    external :: lsame

    lsamen = .false.

    if (len(ca) < n .or. len(cb) < n) return

    do i = 1, n
        if (.not. lsame(ca(i:i), cb(i:i))) return
    end do

    lsamen = .true.
end function lsamen

logical function lsame(a, b)
    implicit none
    character(len=*), intent(in) :: a, b

    ! Case-insensitive comparison of single characters
    lsame = (iachar(a) == iachar(b) .or. &
             iachar(a) == iachar(b) + 32 .or. &
             iachar(a) + 32 == iachar(b))
end function lsame
