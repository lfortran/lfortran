! A local declaration of a module procedure calls a module procedure
! defined later, and locals refer back to the procedures they belong to.
module spec_expr_14_mod
implicit none
contains
    integer function f(n) result(res)
        integer, intent(in) :: n
        integer :: work(g(n))
        procedure(f), pointer :: self
        self => f
        if (.not. associated(self, f)) error stop
        work = 1
        res = sum(work)
    end function f

    pure integer function g(n)
        integer, intent(in) :: n
        g = 2*n
    end function g

    ! The signature of h calls k, and a local of k refers to h.
    function h(n) result(r)
        integer, intent(in) :: n
        integer :: r(k(n))
        r = 5
    end function h

    pure integer function k(n)
        integer, intent(in) :: n
        procedure(h), pointer :: p
        p => null()
        k = n + 1
        if (associated(p)) k = 0
    end function k
end module spec_expr_14_mod

program spec_expr_14
use spec_expr_14_mod
implicit none
if (f(3) /= 6) error stop
if (size(h(4)) /= 5) error stop
if (any(h(4) /= 5)) error stop
print *, f(3), size(h(4))
end program spec_expr_14
