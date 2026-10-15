! Procedure pointers whose interface has explicit-shape dummy bounds that
! reference host variables through an intrinsic call, a component, an array
! element and a lower bound (#14866)
program proc_ptr_43
    implicit none
    type :: t
        integer :: n
    end type
    integer :: n
    integer :: k(2)
    type(t) :: v
    abstract interface
        subroutine iface_max(a)
            import :: n
            integer, intent(inout) :: a(max(n, 1))
        end subroutine
        subroutine iface_comp(a)
            import :: t, v
            integer, intent(inout) :: a(v%n)
        end subroutine
        subroutine iface_elem(a)
            import :: k
            integer, intent(inout) :: a(k(1))
        end subroutine
        subroutine iface_lower(a)
            import :: n
            integer, intent(inout) :: a(n:n+2)
        end subroutine
    end interface
    procedure(iface_max), pointer :: p1
    procedure(iface_comp), pointer :: p2
    procedure(iface_elem), pointer :: p3
    procedure(iface_lower), pointer :: p4
    integer :: x(3), y(2)
    n = 3
    k = [2, 4]
    v%n = 2
    p1 => s_max
    p2 => s_comp
    p3 => s_elem
    p4 => s_lower
    x = 0
    call p1(x)
    if (any(x /= 3)) error stop
    y = 0
    call p2(y)
    call p3(y)
    if (any(y /= 4)) error stop
    call p4(x)
    if (any(x /= [10, 3, 3])) error stop
    print *, "ok"
contains
    subroutine s_max(a)
        integer, intent(inout) :: a(max(n, 1))
        a = a + size(a)
    end subroutine
    subroutine s_comp(a)
        integer, intent(inout) :: a(v%n)
        a = a + size(a)
    end subroutine
    subroutine s_elem(a)
        integer, intent(inout) :: a(k(1))
        a = a + size(a)
    end subroutine
    subroutine s_lower(a)
        integer, intent(inout) :: a(n:n+2)
        if (lbound(a, 1) /= 3) error stop
        a(n) = 10
    end subroutine
end program
