! A derived type defined in the specification part of a BLOCK construct is
! local to the BLOCK and shadows a type of the same name of the host.
module block_20_mod
    implicit none
    type :: t
        integer :: a
    end type
contains
    integer function f(n) result(r)
        integer, intent(in) :: n
        type(t) :: v
        v%a = n
        block
            type :: t
                real(8) :: x
                integer :: c(3)
            end type
            type :: pt
                integer :: k = 7
            end type
            type(t) :: w
            type(pt) :: q
            w%x = 2.5d0
            w%c = [1, 2, 3]
            r = v%a + int(w%x * 2) + sum(w%c) + q%k
        end block
    end function
end module

program block_20
    use block_20_mod, only: f
    implicit none
    type :: t
        integer :: a
    end type
    type(t) :: v
    integer :: r, i, s

    block
        type :: pt
            integer :: k
        end type
        type(pt) :: q
        q%k = 5
        r = q%k
    end block
    print *, r
    if (r /= 5) error stop

    v%a = 1
    block
        type :: t
            integer :: c
        end type
        type(t) :: w
        w%c = 20
        r = v%a + w%c
    end block
    print *, r
    if (r /= 21) error stop

    r = f(10)
    print *, r
    if (r /= 28) error stop

    block
        type :: u
            integer :: a
        end type
        type(u) :: x
        x%a = 1
        block
            type :: u
                real :: b
                integer :: c
            end type
            type(u) :: y
            y%b = 2.0
            y%c = 3
            r = x%a + int(y%b) + y%c
        end block
    end block
    print *, r
    if (r /= 6) error stop

    s = 0
    do i = 1, 3
        block
            type :: node
                integer :: val = 0
                type(node), pointer :: next => null()
            end type
            type, extends(node) :: node2
                integer :: extra
            end type
            type :: holder
                type(node) :: n
                integer, allocatable :: arr(:)
                character(len=5) :: name = "hello"
            end type
            type(holder) :: h
            type(node2) :: n2
            type(node), target :: tail
            type(node) :: ns(3)
            h%n%val = i
            allocate(h%arr(i))
            h%arr = i
            tail%val = 100
            h%n%next => tail
            n2%val = 1
            n2%extra = 2
            ns = [node(1), node(2), node(3)]
            if (h%name /= "hello") error stop
            if (.not. associated(h%n%next)) error stop
            s = s + h%n%val + sum(h%arr) + h%n%next%val + n2%val + n2%extra &
                + sum(ns%val)
        end block
    end do
    print *, s
    if (s /= 347) error stop

    block
        type :: pk(k)
            integer, kind :: k = 4
            integer(k) :: v
        end type
        type :: sq
            sequence
            integer :: a
            real :: b
        end type
        type(pk(8)) :: p
        type(sq) :: z
        p%v = 5_8
        z%a = 2
        z%b = 3.0
        r = int(p%v) + z%a + int(z%b)
    end block
    print *, r
    if (r /= 10) error stop
end program
