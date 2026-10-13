! Module variables with SAVE, given explicitly or implied by an initializer,
! live until the program ends like the module's other variables. With
! --detect-leaks the storage they own is released at program exit, except
! the class wrapper of a scalar class pointer with SAVE, which is static.
module save_25_m
    implicit none
    type :: empty_t
    end type empty_t
    type :: base_t
        integer :: k = 1
    end type base_t
    type, extends(base_t) :: ext_t
        integer :: j = 2
    end type ext_t
    type :: holder_t
        type(empty_t), allocatable :: e
        type(base_t), allocatable :: b
        integer, allocatable :: arr(:)
        character(len=:), allocatable :: text
    end type holder_t
    type(holder_t), save :: h
    integer, allocatable, save :: ia(:)
    character(len=:), allocatable, save :: cs
    class(base_t), allocatable, save :: ca
    type(base_t), target, save :: targets(2)
    class(base_t), pointer, save :: cp
    class(base_t), pointer :: cp_init => null()
    class(base_t), pointer, save :: parr(:) => null()
contains
    subroutine setup()
        h%e = empty_t()
        h%b = base_t(3)
        allocate(h%arr(3))
        h%arr = [1, 2, 3]
        h%text = "text"
        allocate(ia(4))
        ia = 7
        cs = "saved"
        allocate(ext_t :: ca)
        cp => targets(1)
        cp_init => targets(2)
        parr => targets
    end subroutine setup
    integer function ca_j() result(j)
        j = -1
        select type (ca)
        type is (ext_t)
            j = ca%j
        end select
    end function ca_j
end module save_25_m

program save_25
    use save_25_m
    implicit none
    call setup()
    if (.not. allocated(h%e)) error stop
    if (h%b%k /= 3) error stop
    if (sum(h%arr) /= 6) error stop
    if (h%text /= "text") error stop
    if (sum(ia) /= 28) error stop
    if (cs /= "saved") error stop
    if (ca_j() /= 2) error stop
    if (.not. associated(cp, targets(1))) error stop
    if (.not. associated(cp_init, targets(2))) error stop
    if (size(parr) /= 2) error stop
    print *, "save_25 ok"
end program save_25
