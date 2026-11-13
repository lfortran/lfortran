program transfer_pointer_01
    use iso_c_binding
    implicit none

    ! f90wrap recovers a derived-type pointer by transferring it
    ! through an integer(c_int) pair. Both words of the pointer have
    ! to be copied, and the recovered pointer has to be usable.
    type :: payload
        real(8), allocatable :: y(:)
    end type payload
    type :: wrap
        type(payload), pointer :: p
    end type wrap
    type :: pair
        integer(c_int) :: a, b
    end type pair
    type :: base_box
        integer(c_int) :: a
    end type base_box
    type, extends(base_box) :: ext_box
        integer(c_int) :: b
    end type ext_box
    type :: box8
        integer(8) :: v
    end type box8
    type :: small2
        integer(2) :: v
    end type small2

    type(wrap) :: w, w2
    type(wrap), allocatable :: w_alloc
    type(wrap), target :: w_backing
    type(wrap), pointer :: w_ptr
    type(pair) :: pr
    type(ext_box) :: eb, eb2
    type(box8) :: b8
    type(small2) :: sm
    type(c_ptr) :: cp
    integer(c_int) :: bits(2)
    integer(c_int), allocatable :: abits(:)
    integer(8) :: iv

    allocate(w%p)
    allocate(w%p%y(2))
    w%p%y(1) = 1.5d0
    w%p%y(2) = 2.5d0
    bits = transfer(w, bits)
    w2 = transfer(bits, w2)
    if (.not. associated(w2%p)) error stop "pointer lost"
    if (.not. allocated(w2%p%y)) error stop "allocatable lost"
    if (size(w2%p%y) /= 2) error stop "allocatable size"
    if (w2%p%y(1) /= 1.5d0) error stop "allocatable value"
    if (w2%p%y(2) /= 2.5d0) error stop "allocatable value 2"

    ! Allocatable and pointer derived-type targets
    w_alloc = transfer(bits, w)
    if (.not. allocated(w_alloc)) error stop "alloc target"
    if (.not. associated(w_alloc%p, w%p)) error stop "alloc target ptr"
    w_ptr => w_backing
    w_ptr%p => null()
    w_ptr = transfer(bits, w_ptr)
    if (.not. associated(w_backing%p, w%p)) error stop "ptr target"

    ! `w2%p` is the same target as `w%p`. Free that one target.
    deallocate(w%p%y)
    deallocate(w%p)

    ! Non-pointer scalar expression, c_ptr, partial bytes, short slice, extends
    iv = 1234567890123_8
    b8 = transfer(iv + 1_8, b8)
    if (b8%v /= 1234567890124_8) error stop "scalar expr"
    cp = c_null_ptr
    bits = transfer(cp, bits)
    if (bits(1) /= 0 .or. bits(2) /= 0) error stop "c_ptr null"
    sm%v = 258_2
    bits = -1
    bits = transfer(sm, bits, 2)
    if (iand(bits(1), int(z'FFFF', c_int)) /= 258) error stop "partial bytes"
    allocate(abits(3))
    abits = [11, 77, 88]
    pr = transfer(abits(2:3), pr)
    if (pr%a /= 77 .or. pr%b /= 88) error stop "alloc slice"
    deallocate(abits)
    allocate(abits(1))
    abits(1) = 55
    pr%b = 99
    pr = transfer(abits, pr)
    if (pr%a /= 55) error stop "short alloc"
    eb%a = 10
    eb%b = 20
    bits = transfer(eb, bits)
    eb2 = transfer(bits, eb2)
    if (eb2%a /= 10 .or. eb2%b /= 20) error stop "extends"

    print *, "ok"
end program transfer_pointer_01
