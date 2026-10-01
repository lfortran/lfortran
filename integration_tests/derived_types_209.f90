module derived_types_209_mod
    ! Subscripts of a component that is itself subscripted, with more
    ! than one subscript on the parent: r(i,j)%c(k,l)
    implicit none
    type :: cell
        real :: v(2,3)
    contains
        procedure :: total
        procedure :: bump
    end type cell
    type :: reach
        type(cell) :: q(2,3)
    end type reach
contains
    real function total(self, i, j)
        class(cell), intent(in) :: self
        integer, intent(in) :: i, j
        total = sum(self%v(1:i,1:j))
    end function total

    subroutine bump(self, x)
        class(cell), intent(inout) :: self
        real, intent(in) :: x
        self%v = self%v + x
    end subroutine bump
end module derived_types_209_mod

program derived_types_209
    use derived_types_209_mod, only: cell, reach
    implicit none
    type(reach) :: r(2)
    type(cell) :: c(2,2)
    integer :: n, k
    r(2)%q(2,3)%v = 0
    r(2)%q(2,3)%v(2,3) = 5
    n = 2
    k = 3
    if (r(2)%q(n,k)%v(n,k) /= 5) error stop
    c(1,2)%v = 1
    if (c(1,2)%total(2, 3) /= 6) error stop
    if (c(1,2)%v(n,k) /= 1) error stop
    call c(1,2)%bump(1.0)
    if (c(1,2)%v(n,k) /= 2) error stop
end program derived_types_209
