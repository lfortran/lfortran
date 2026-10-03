module spec_expr_02_mod
    ! A specification expression of a local array calls a function whose
    ! argument is a derived type returned by another function.
    implicit none
    integer :: nfin_3 = 0

    type :: t
        integer :: c = 0
    contains
        final :: fin
    end type

    ! The component is deallocated with the function result, so nothing
    ! leaks.
    type :: p_t
        integer, allocatable :: c
    end type

contains

    pure function construct(c) result(r)
        integer, intent(in) :: c
        type(t) :: r
        r%c = c
    end function

    pure integer function comp(self)
        type(t), intent(in) :: self
        if (self%c == 0) error stop "comp: default-initialized argument"
        comp = self%c
    end function

    subroutine fin(self)
        type(t), intent(inout) :: self
        if (self%c == 3) nfin_3 = nfin_3 + 1
    end subroutine

    pure function construct_p(c) result(r)
        integer, intent(in) :: c
        type(p_t) :: r
        allocate(r%c, source = c)
    end function

    pure integer function comp_p(self)
        type(p_t), intent(in) :: self
        if (.not. allocated(self%c)) error stop "comp_p: unallocated component"
        comp_p = self%c
    end function

end module

program spec_expr_02
    use spec_expr_02_mod, only: construct, comp, construct_p, comp_p, nfin_3
    implicit none

    call s(2)
    call s_component()

contains

    subroutine s(n)
        integer, intent(in) :: n
        real :: tmp(comp(construct(3)))
        integer :: a(2, comp(construct(n)) + 1)
        integer :: b(comp(construct(n)):comp(construct(n + 3)))
        character(len=comp(construct(3))) :: str
        ! The result of `construct(3)` is finalized before the body executes
        if (nfin_3 < 1) error stop 1
        tmp = 1
        a = 5
        b = 7
        str = "abcdef"
        print *, size(tmp), size(a, 2), lbound(b, 1), ubound(b, 1), len(str), str
        if (size(tmp) /= 3) error stop 2
        if (abs(sum(tmp) - 3) > 1e-6) error stop 3
        if (size(a, 2) /= 3) error stop 4
        if (sum(a) /= 30) error stop 5
        if (lbound(b, 1) /= 2 .or. ubound(b, 1) /= 5) error stop 6
        if (b(2) /= 7 .or. b(5) /= 7) error stop 7
        if (len(str) /= 3 .or. str /= "abc") error stop 8
    end subroutine

    subroutine s_component()
        real :: tmp(comp_p(construct_p(4)))
        tmp = 2
        print *, size(tmp)
        if (size(tmp) /= 4) error stop 9
        if (abs(sum(tmp) - 8) > 1e-6) error stop 10
    end subroutine

end program
