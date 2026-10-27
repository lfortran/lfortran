! Variables whose members or descriptors need setting up at run time.
submodule (submodule_60_m) submodule_60_s
    implicit none
    type :: tp
        integer, pointer :: q(:) => null()
    end type
    type :: tc
        character(len=4) :: nm = "abcd"
        integer :: k = 3
    end type
    type :: ta
        integer, allocatable :: v(:)
        character(len=:), allocatable :: s
    end type
    type(tp) :: obj
    type(tp) :: objs(2)
    type(tc) :: objc(3)
    type(tc) :: objb(2) = tc("wxyz", 5)
    type(ta) :: oas(2)
    character(len=5), allocatable :: ca(:)
    integer, target :: sx = 42
    integer, pointer :: ip => sx
    integer, pointer :: mp => marr(2)
contains
    module procedure s_check
        code = 0
        if (objc(3)%nm /= "abcd" .or. objc(3)%k /= 3) code = code + 1
        if (objb(2)%nm /= "wxyz" .or. objb(1)%k /= 5) code = code + 2
        if (ip /= 42 .or. mp /= 8) code = code + 4
        if (associated(objs(2)%q)) code = code + 8
        if (filled) then
            if (.not. associated(obj%q)) then
                code = code + 16
            else if (sum(obj%q) /= 10) then
                code = code + 16
            end if
            if (.not. allocated(ca)) then
                code = code + 32
            else if (ca(2) /= "hello" .or. len(ca) /= 5) then
                code = code + 32
            end if
            if (sum(oas(2)%v) /= 3 .or. oas(1)%s /= "zz") code = code + 64
        else
            if (associated(obj%q)) code = code + 16
            if (allocated(ca)) code = code + 32
            if (allocated(oas(2)%v)) code = code + 64
        end if
    end procedure

    module procedure s_fill
        allocate(obj%q(2))
        obj%q = 5
        allocate(ca(2))
        ca = "hello"
        allocate(oas(2)%v(3))
        oas(2)%v = 1
        oas(1)%s = "zz"
    end procedure
end submodule
