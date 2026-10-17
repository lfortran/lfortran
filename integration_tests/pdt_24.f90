module pdt_24_mod
    ! Assumed LEN type parameters of dummy arguments, including
    ! polymorphic passed objects of type-bound procedures.
    implicit none

    type :: matrix_t(nrow, ncol)
        integer, len :: nrow, ncol
        real :: x(nrow, ncol)
    contains
        procedure :: fill_seq
        procedure :: total
    end type

    type :: box_t(n)
        integer, len :: n
        character(len=n) :: text
    end type

contains

    subroutine fill_seq(this, start, step)
        class(matrix_t(*, *)), intent(inout) :: this
        real, intent(in) :: start, step
        integer :: i, j
        real :: val
        val = start
        do i = 1, this%nrow
            do j = 1, this%ncol
                this%x(i, j) = val
                val = val + step
            end do
        end do
    end subroutine

    real function total(this)
        class(matrix_t(*, *)), intent(in) :: this
        total = sum(this%x)
    end function

    subroutine upper(b)
        type(box_t(*)), intent(inout) :: b
        integer :: i
        do i = 1, b%n
            if (b%text(i:i) >= "a" .and. b%text(i:i) <= "z") then
                b%text(i:i) = achar(iachar(b%text(i:i)) - 32)
            end if
        end do
    end subroutine

end module

program pdt_24
    use pdt_24_mod
    implicit none
    type(matrix_t(2, 3)) :: a
    type(matrix_t(nrow=3, ncol=1)) :: b
    type(box_t(7)) :: s

    call a%fill_seq(1.0, 1.0)
    if (abs(a%x(2, 1) - 4.0) > 1e-6) error stop
    if (abs(a%total() - 21.0) > 1e-6) error stop
    call b%fill_seq(10.0, 0.5)
    if (b%nrow /= 3 .or. b%ncol /= 1) error stop
    if (abs(b%total() - 31.5) > 1e-6) error stop

    s%text = "pdt box"
    call upper(s)
    if (s%text /= "PDT BOX") error stop
    print *, a%total(), b%total(), s%text
end program
