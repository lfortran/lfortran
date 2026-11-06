! An abstract interface named `func` has no link name, so it must not take
! the link name "func" from a bind(c) procedure with another signature.
module class_173_a
    implicit none
    type, abstract :: pa
    contains
        procedure(func), deferred :: f
    end type
    abstract interface
        integer function func(this)
            import :: pa
            class(pa), intent(in) :: this
        end function
    end interface
end module

module class_173_b
    use class_173_a, only: pa
    implicit none
    type, extends(pa) :: pb
        integer :: n = 5
    contains
        procedure :: f => pb_f
    end type
contains
    integer function pb_f(this)
        class(pb), intent(in) :: this
        pb_f = 10 * this%n
    end function
end module

module class_173_c
    implicit none
    type, abstract :: qa
    contains
        procedure(func), deferred, nopass :: g
    end type
    abstract interface
        integer function func(x)
            real, intent(in) :: x
        end function
    end interface
end module

module class_173_d
    use iso_c_binding, only: c_int
    implicit none
contains
    integer(c_int) function mul(i, j) bind(c, name="func")
        integer(c_int), value :: i, j
        mul = i * j
    end function
end module

program class_173
    use class_173_a, only: pa
    use class_173_b, only: pb
    use class_173_d, only: mul
    implicit none
    class(pa), allocatable :: p
    allocate(pb :: p)
    if (p%f() /= 50) error stop
    if (mul(6, 7) /= 42) error stop
    print *, "ok"
end program
