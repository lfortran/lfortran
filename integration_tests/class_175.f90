! An abstract interface named `func` names no procedure. It must not take
! over the declaration of the external `func` (defined in class_175_ext.f90)
! that a module procedure calls through its interface body, whichever of the
! two is declared first.
module class_175_a
    implicit none
    type :: holder
        procedure(func), pointer, nopass :: pp => null()
    end type
    abstract interface
        integer function func(x)
            real, intent(in) :: x
        end function
    end interface
contains
    ! `aaa` sorts before `func`: its interface body is declared first.
    integer function aaa()
        interface
            integer function func(i, j)
                integer, intent(in) :: i, j
            end function
        end interface
        aaa = func(6, 7)
    end function
    ! `zzz` sorts after `func`: the abstract interface is declared first.
    integer function zzz()
        interface
            integer function func(i, j)
                integer, intent(in) :: i, j
            end function
        end interface
        zzz = func(2, 3)
    end function
    integer function twice(x)
        real, intent(in) :: x
        twice = int(2*x)
    end function
end module

module class_175_b
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
    type, extends(pa) :: pb
        integer :: n = 5
    contains
        procedure :: f => pb_f
    end type
contains
    integer function bbb()
        interface
            integer function func(i, j)
                integer, intent(in) :: i, j
            end function
        end interface
        bbb = func(4, 5)
    end function
    integer function pb_f(this)
        class(pb), intent(in) :: this
        pb_f = 10 * this%n
    end function
end module

program class_175
    use class_175_a, only: holder, aaa, zzz, twice
    use class_175_b, only: pa, pb, bbb
    implicit none
    type(holder) :: h
    class(pa), allocatable :: p
    h%pp => twice
    if (h%pp(3.0) /= 6) error stop 1
    if (aaa() /= 42) error stop 2
    if (zzz() /= 6) error stop 3
    allocate(pb :: p)
    if (p%f() /= 50) error stop 4
    if (bbb() /= 20) error stop 5
    print *, "ok"
end program
