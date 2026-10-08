module traits_procedure_value_01_oracle_m
    use iso_fortran_env, only: real32, real64
    implicit none
    integer :: evaluations = 0
contains
    function sum_integer(x) result(s)
        integer, intent(in) :: x(:)
        integer :: s, i
        evaluations = evaluations + 1
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function
    function sum_real64(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: i
        evaluations = evaluations + 1
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function
    function sum_real32(x) result(s)
        real(real32), intent(in) :: x(:)
        real(real32) :: s
        integer :: i
        evaluations = evaluations + 1
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function
    subroutine copy_integer(x, y)
        integer, intent(in) :: x
        integer, intent(out) :: y
        y = x
    end subroutine
end module

program traits_procedure_value_01_oracle
    use traits_procedure_value_01_oracle_m
    implicit none
    procedure(sum_integer), pointer :: isum
    procedure(sum_real64), pointer :: dsum
    procedure(copy_integer), pointer :: copy
    ! GFortran can crash on a procedure pointer declared inside BLOCK.
    procedure(sum_real32), pointer :: ssum
    integer :: copied
    real(real32) :: stot(2)
    real(real64) :: dtot(2)
    isum => sum_integer
    dsum => sum_real64
    copy => copy_integer
    if (evaluations /= 0) error stop 1
    if (isum([1,2,3,4,5]) /= 15) error stop 2
    dtot(1) = dsum([1.d0,2.d0,3.d0,4.d0,5.d0])
    dtot(2) = dsum([2.d0,4.d0,6.d0,8.d0])
    if (any(abs(dtot - [15.d0,20.d0]) > 1.d-12)) error stop 3
    block
        ssum => sum_real32
        if (evaluations /= 3) error stop 4
        stot(1) = ssum([1.,2.,3.,4.,5.])
        stot(2) = ssum([2.,4.,6.,8.])
    end block
    if (any(abs(stot - [15.,20.]) > 1.e-6)) error stop 5
    call copy(19, copied)
    if (copied /= 19) error stop 6
    if (apply(sum_real64, [2.d0,3.d0]) /= 5.d0) error stop 7
    if (evaluations /= 6) error stop 8
contains
    function apply(f, x) result(s)
        procedure(sum_real64) :: f
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        s = f(x)
    end function
end program
