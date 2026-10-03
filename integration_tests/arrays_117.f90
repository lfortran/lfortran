program arrays_117
    use, intrinsic :: iso_fortran_env, only : int8, real64
    implicit none

    integer(int8), parameter :: n = 25_int8
    real(real64) :: random_vals(n)
    integer(int8) :: indx_r(n)
    integer :: i
    logical :: result

    ! initialize values
    random_vals = [(real(i, real64), i = 1, n)]

    ! vector subscript indices
    do i = 1, n
        indx_r(i) = int(i, int8)
    end do

    ! perform test
    result = all(random_vals(indx_r) <= random_vals(indx_r(20)))

    if (result .neqv. .false.) error stop "Vector subscript test failed"

    print *, "Test passed"

end program arrays_117