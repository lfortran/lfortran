! Deferred constants declared with the DEFERRED declaration-type-spec statement
! of the Fortran 2028 working draft (J3/26-007r1, 16.4.1.3):
!
!     R1618 deferred-const-decl-stmt  is  DEFERRED declaration-type-spec,
!               deferred-const-attr-spec-list :: deferred-const-entity-decl-list
!
! A deferred constant is a deferred argument that has the PARAMETER attribute
! and can appear in a constant expression; its value is determined by
! instantiation. C1619 restricts the type to integer, logical or character, and
! a character deferred constant has assumed length, so all three are exercised
! here: the constant is used in an expression, as the bound of a local array,
! and as the condition of an IF, and each instantiation is checked to compute
! with the value of its instantiation argument.
!
! The constraints on the statement are pinned by tests/errors/deferred_const_decl_1.f90.

module template_deferred_const_01_m
    implicit none
    private
    public :: test_integer, test_logical, test_character

    template scale_tmpl(n)
        deferred integer, parameter :: n
        private
        public :: scale_by_n, sum_to_n
    contains
        ! `n` in a constant expression.
        function scale_by_n(x) result(z)
            integer, intent(in) :: x
            integer :: z
            z = x * n
        end function

        ! `n` as an array bound.
        function sum_to_n() result(z)
            integer :: z
            integer :: buf(n)
            integer :: i
            do i = 1, n
                buf(i) = i
            end do
            z = sum(buf)
        end function
    end template

    template flag_tmpl(b)
        deferred logical, parameter :: b
        private
        public :: pick
    contains
        function pick(x, y) result(z)
            integer, intent(in) :: x, y
            integer :: z
            if (b) then
                z = x
            else
                z = y
            end if
        end function
    end template

    ! C1619: a character deferred constant has assumed length, the length comes
    ! from the instantiation argument.
    template text_tmpl(s)
        deferred character(*), parameter :: s
        private
        public :: text_len, first_char
    contains
        function text_len() result(z)
            integer :: z
            z = len(s)
        end function
        function first_char() result(c)
            character(1) :: c
            c = s(1:1)
        end function
    end template

contains

    subroutine test_integer()
        integer, parameter :: four = 4
        integer, parameter :: seven = 7
        instantiate scale_tmpl {four}, only: scale4 => scale_by_n, sum4 => sum_to_n
        instantiate scale_tmpl {seven}, only: scale7 => scale_by_n, sum7 => sum_to_n
        if (scale4(5) /= 20) error stop
        if (sum4() /= 10) error stop
        if (scale7(5) /= 35) error stop
        if (sum7() /= 28) error stop
    end subroutine

    subroutine test_logical()
        logical, parameter :: yes = .true.
        logical, parameter :: no = .false.
        instantiate flag_tmpl {yes}, only: pick_first => pick
        instantiate flag_tmpl {no}, only: pick_second => pick
        if (pick_first(7, 9) /= 7) error stop
        if (pick_second(7, 9) /= 9) error stop
    end subroutine

    subroutine test_character()
        character(*), parameter :: hello = "hello"
        instantiate text_tmpl {hello}, only: hello_len => text_len, &
            hello_first => first_char
        if (hello_len() /= 5) error stop
        if (hello_first() /= "h") error stop
    end subroutine

end module

program template_deferred_const_01
use template_deferred_const_01_m
implicit none

call test_integer()
call test_logical()
call test_character()

print *, "ok"

end program
