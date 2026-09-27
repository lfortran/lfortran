! Pointers initially associated with parts of global_init_20_a's storage, so
! that global_init_20_a has to be initialized first wherever it is linked or
! loaded, and the C-callable procedures every native test drives both
! modules with. Each check returns 0, or the number of the first thing that
! is wrong.
module global_init_20_b
    use iso_c_binding, only: c_int
    use global_init_20_a, only: h
    implicit none
    character(len=3), pointer :: ps => h%s
    integer(c_int), pointer :: pv => h%v(2)
    integer(c_int), allocatable :: barr(:)
end module global_init_20_b

integer(c_int) function global_init_20_check_initial() bind(c)
    use iso_c_binding, only: c_int
    use global_init_20_a, only: h, arr, parr
    use global_init_20_b, only: ps, pv, barr
    implicit none
    global_init_20_check_initial = 1
    if (.not. associated(ps)) return
    global_init_20_check_initial = 2
    if (len(ps) /= 3 .or. ps /= "abc" .or. h%s /= "abc") return
    global_init_20_check_initial = 3
    if (.not. associated(pv, h%v(2))) return
    global_init_20_check_initial = 4
    if (pv /= 2 .or. any(h%v /= [1, 2, 3])) return
    global_init_20_check_initial = 5
    if (allocated(h%extra) .or. allocated(arr) .or. allocated(barr)) return
    global_init_20_check_initial = 6
    if (associated(parr)) return
    global_init_20_check_initial = 0
end function global_init_20_check_initial

subroutine global_init_20_mutate() bind(c)
    use global_init_20_a, only: h, arr, parr
    use global_init_20_b, only: ps, pv, barr
    implicit none
    ps = "xyz"
    pv = 20
    allocate(h%extra(2), arr(3), barr(1))
    h%extra = 7
    arr = 8
    barr = 9
    parr => h%v
end subroutine global_init_20_mutate

! Whether what global_init_20_mutate did is all still there, which also
! checks that `ps` and `pv` alias global_init_20_a's storage.
integer(c_int) function global_init_20_check_mutated() bind(c)
    use iso_c_binding, only: c_int
    use global_init_20_a, only: h, arr, parr
    use global_init_20_b, only: ps, pv, barr
    implicit none
    global_init_20_check_mutated = 1
    if (h%s /= "xyz" .or. ps /= "xyz") return
    global_init_20_check_mutated = 2
    if (h%v(2) /= 20 .or. pv /= 20) return
    global_init_20_check_mutated = 3
    if (.not. allocated(h%extra) .or. .not. allocated(arr) .or. .not. allocated(barr)) return
    global_init_20_check_mutated = 4
    if (sum(h%extra) /= 14 .or. sum(arr) /= 24 .or. barr(1) /= 9) return
    global_init_20_check_mutated = 5
    if (.not. associated(parr, h%v)) return
    global_init_20_check_mutated = 6
    if (parr(2) /= 20) return
    global_init_20_check_mutated = 0
end function global_init_20_check_mutated
