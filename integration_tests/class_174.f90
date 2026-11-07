! A deferred binding inherited through an abstract intermediate type keeps
! its vtable slot, so a call through the base type reaches the override.
module class_174_m1
    implicit none
    type, abstract :: base
    contains
        procedure(f_i), deferred :: f
        generic :: g => f
    end type
    abstract interface
        subroutine f_i(self)
            import :: base
            class(base), intent(inout) :: self
        end subroutine
    end interface
    type, extends(base), abstract :: mid
    end type
    type, extends(mid) :: leaf
        integer :: n = 0
    contains
        procedure :: f => leaf_f
    end type
contains
    subroutine leaf_f(self)
        class(leaf), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
end module

module class_174_m2
    implicit none
    type, abstract :: base2
    contains
        procedure(f2_i), deferred :: f
    end type
    type, extends(base2), abstract :: mid2
    contains
        procedure(f2_mid_i), deferred :: f
    end type
    abstract interface
        subroutine f2_i(self)
            import :: base2
            class(base2), intent(inout) :: self
        end subroutine
        subroutine f2_mid_i(self)
            import :: mid2
            class(mid2), intent(inout) :: self
        end subroutine
    end interface
    type, extends(mid2) :: leaf2
        integer :: n = 0
    contains
        procedure :: f => leaf2_f
    end type
contains
    subroutine leaf2_f(self)
        class(leaf2), intent(inout) :: self
        self%n = self%n + 10
    end subroutine
end module

module class_174_m3
    implicit none
    type :: circle
        real :: r = 1
    end type
    type :: triangle
        real :: b = 1, h = 1
    end type
    type, abstract :: visitor
    contains
        procedure(visit_c), deferred :: visit_circle
        generic :: visit => visit_circle
    end type
    type, extends(visitor), abstract :: visitor2
    contains
        procedure(visit_t), deferred :: visit_triangle
        generic :: visit => visit_triangle
    end type
    type, extends(visitor2) :: area
        real :: total = 0
    contains
        procedure :: visit_circle => area_circle
        procedure :: visit_triangle => area_triangle
    end type
    abstract interface
        subroutine visit_c(v, c)
            import :: visitor, circle
            class(visitor), intent(inout) :: v
            type(circle), intent(in) :: c
        end subroutine
        subroutine visit_t(v, t)
            import :: visitor2, triangle
            class(visitor2), intent(inout) :: v
            type(triangle), intent(in) :: t
        end subroutine
    end interface
contains
    subroutine area_circle(v, c)
        class(area), intent(inout) :: v
        type(circle), intent(in) :: c
        v%total = v%total + 3.0 * c%r**2
    end subroutine
    subroutine area_triangle(v, t)
        class(area), intent(inout) :: v
        type(triangle), intent(in) :: t
        v%total = v%total + 0.5 * t%b * t%h
    end subroutine
    subroutine accept_circle(c, v)
        type(circle), intent(in) :: c
        class(visitor), intent(inout) :: v
        call v%visit(c)
    end subroutine
    subroutine accept_triangle(t, v)
        type(triangle), intent(in) :: t
        class(visitor2), intent(inout) :: v
        call v%visit(t)
    end subroutine
end module

program class_174
    use class_174_m1
    use class_174_m2
    use class_174_m3
    implicit none
    class(base), allocatable :: x
    class(base2), allocatable :: y
    type(area) :: a

    allocate(leaf :: x)
    call x%g()
    call x%f()
    select type (x)
    type is (leaf)
        if (x%n /= 2) error stop
    class default
        error stop
    end select

    allocate(leaf2 :: y)
    call y%f()
    select type (y)
    type is (leaf2)
        if (y%n /= 10) error stop
    class default
        error stop
    end select

    call accept_circle(circle(r=1.0), a)
    call accept_triangle(triangle(b=2.0, h=3.0), a)
    if (abs(a%total - 6.0) > 1e-6) error stop
    print *, "ok"
end program
