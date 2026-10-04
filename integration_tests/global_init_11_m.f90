! Module storage of every kind the startup hook of the defining object file
! sets up, none of it with a declaration initializer: each variable gets its
! initial value from its type's default initialization. Static data holds
! every default a constant describes, however deeply the types held by value
! nest, and the defaults it cannot hold -- those in a string's buffer, a
! procedure pointer's initial target, the elements of an array -- are
! statements of the module's initializer. See global_init_11.f90.
module global_init_11_m
    use iso_c_binding, only: c_int
    implicit none
    abstract interface
        integer(c_int) function iface(x)
            import :: c_int
            integer(c_int), intent(in) :: x
        end function iface
    end interface
    integer(c_int), target :: tgt(2) = [7, 8]
    integer(c_int), target :: tscal = 99
    type :: base
        integer(c_int) :: k = 5
        character(len=3) :: bname = "bse"
    contains
        procedure :: get_k
    end type
    type :: leaf
        integer(c_int) :: h = 3
        character(len=2) :: tag = "lf"
        integer(c_int), allocatable :: la(:)
    end type
    type :: inner
        integer(c_int) :: m = 11
        integer(c_int), pointer :: q(:) => null()
        character(len=2) :: nm = "in"
    end type
    type :: seq
        sequence
        integer(c_int) :: j = 8
        character(len=2) :: sq = "sq"
    end type
    type, extends(base) :: ext
        integer(c_int) :: v(3) = [1, 2, 3]
        real :: r = 2.5
        logical :: flag = .true.
        integer(c_int), pointer :: p => null()
        procedure(iface), pointer, nopass :: fp => twice
        integer(c_int), allocatable :: a(:)
        character(len=4) :: s = "abcd"
        character(len=2) :: cs(3) = ["c1", "c2", "c3"]
        type(inner) :: nested
        type(inner) :: nested_def = inner(12, null(), "nd")
        type(leaf) :: leaves(2)
        type(seq) :: sq
        class(base), allocatable :: poly
        class(base), allocatable :: polys(:)
        integer(c_int) :: plain
    end type
    type :: small
        integer(c_int) :: n = 7
        real :: w = 1.5
    end type
    type :: bag
        type(leaf) :: leaves_def(2) = [leaf(21, "l1"), leaf(22, "l2")]
    end type
    type(ext) :: e
    type(bag) :: b
    type(leaf) :: leaf_arr(3)
    type(small) :: small_arr(4)
    type(small) :: grid(2, 3)
contains
    integer(c_int) function twice(x)
        integer(c_int), intent(in) :: x
        twice = 2*x
    end function twice
    integer(c_int) function thrice(x)
        integer(c_int), intent(in) :: x
        thrice = 3*x
    end function thrice
    integer(c_int) function get_k(this)
        class(base), intent(in) :: this
        get_k = this%k
    end function get_k
end module global_init_11_m
