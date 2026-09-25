! A C constructor that runs before the startup hook of global_init_11_m's
! object file (see global_init_11c.c) changes every part of the module's
! storage whose initial state is static data. The hook creates the storage
! the layout does not hold in place and runs the module's initializer, which
! holds the defaults static data cannot; neither may undo those changes, and
! between them every variable ends up with its default initialization. What
! the hook and the initializer do put there is not changed beforehand: that
! storage does not hold its initial state until they have run.
! These are external procedures of their own file, so that the module's
! object file can go last on the link line.
subroutine global_init_11_change() bind(c)
    use global_init_11_m
    implicit none
    e%k = 50
    e%v(2) = 20
    e%r = 25.0
    e%flag = .false.
    e%p => tscal
    e%nested%m = 110
    e%nested_def%m = 120
    e%leaves(2)%h = 30
    b%leaves_def(1)%h = 210
    e%sq%j = 80
    e%sq%sq = "SQ"
    e%plain = 4
end subroutine global_init_11_change

integer(c_int) function global_init_11_check(changed) bind(c)
    use iso_c_binding, only: c_int
    use global_init_11_m
    implicit none
    integer(c_int), value :: changed
    logical :: c
    integer :: nfail, i, j
    nfail = 0
    c = changed /= 0
    ! Static data, which the constructor may have changed.
    call expect(e%k == merge(50, 5, c), "e%k")
    call expect(all(e%v == merge([1, 20, 3], [1, 2, 3], c)), "e%v")
    call expect(e%r == merge(25.0, 2.5, c), "e%r")
    call expect(e%flag .neqv. c, "e%flag")
    call expect(e%nested%m == merge(110, 11, c), "e%nested%m")
    call expect(e%nested_def%m == merge(120, 12, c), "e%nested_def%m")
    call expect(e%leaves(1)%h == 3 .and. e%leaves(2)%h == merge(30, 3, c), "e%leaves%h")
    call expect(b%leaves_def(1)%h == merge(210, 21, c) .and. b%leaves_def(2)%h == 22, &
        "b%leaves_def%h")
    call expect(e%sq%j == merge(80, 8, c) .and. e%sq%sq == merge("SQ", "sq", c), "e%sq")
    if (c) then
        call expect(associated(e%p, tscal), "e%p")
        call expect(e%plain == 4, "e%plain")
    else
        call expect(.not. associated(e%p), "e%p")
    end if
    ! Defaults the module's initializer gives, and the storage the hook sets up.
    call expect(associated(e%fp), "e%fp")
    call expect(e%fp(21) == 42, "e%fp call")
    call expect(e%bname == "bse", "e%bname")
    call expect(e%s == "abcd", "e%s")
    call expect(all(e%cs == ["c1", "c2", "c3"]), "e%cs")
    call expect(e%nested%nm == "in" .and. e%nested_def%nm == "nd", "e%nested%nm")
    call expect(e%leaves(1)%tag == "lf" .and. e%leaves(2)%tag == "lf", "e%leaves%tag")
    call expect(b%leaves_def(1)%tag == "l1" .and. b%leaves_def(2)%tag == "l2", &
        "b%leaves_def%tag")
    call expect(.not. allocated(e%a), "e%a")
    call expect(.not. associated(e%nested%q) .and. .not. associated(e%nested_def%q), &
        "e%nested%q")
    call expect(.not. allocated(e%leaves(1)%la), "e%leaves%la")
    call expect(.not. allocated(e%poly) .and. .not. allocated(e%polys), "e%poly")
    call expect(e%get_k() == e%k, "e%get_k")
    do i = 1, 3
        call expect(leaf_arr(i)%h == 3 .and. leaf_arr(i)%tag == "lf", "leaf_arr")
        call expect(.not. allocated(leaf_arr(i)%la), "leaf_arr%la")
    end do
    do i = 1, 4
        call expect(small_arr(i)%n == 7 .and. small_arr(i)%w == 1.5, "small_arr")
    end do
    do j = 1, 3
        do i = 1, 2
            call expect(grid(i, j)%n == 7 .and. grid(i, j)%w == 1.5, "grid")
        end do
    end do
    ! The storage set up at startup works, and belongs to one object each.
    allocate(e%a(2)); e%a = 4
    call expect(sum(e%a) == 8, "e%a use")
    deallocate(e%a)
    e%s = "wxyz"; e%bname = "BSE"; e%cs(2) = "CC"
    call expect(e%s == "wxyz" .and. e%bname == "BSE", "e%s use")
    call expect(all(e%cs == ["c1", "CC", "c3"]), "e%cs use")
    e%nested%q => tgt
    call expect(sum(e%nested%q) == 15 .and. .not. associated(e%nested_def%q), "e%nested%q use")
    e%nested_def%nm = "zz"
    call expect(e%nested_def%nm == "zz" .and. e%nested%nm == "in", "e%nested%nm use")
    e%leaves(1)%tag = "L1"
    call expect(e%leaves(1)%tag == "L1" .and. e%leaves(2)%tag == "lf", "e%leaves%tag use")
    allocate(e%leaves(2)%la(3)); e%leaves(2)%la = 2
    call expect(sum(e%leaves(2)%la) == 6 .and. .not. allocated(e%leaves(1)%la), &
        "e%leaves%la use")
    deallocate(e%leaves(2)%la)
    allocate(e%poly); e%poly%k = 9
    call expect(e%poly%k == 9 .and. e%poly%bname == "bse", "e%poly use")
    deallocate(e%poly)
    allocate(e%polys(2))
    call expect(size(e%polys) == 2 .and. e%polys(2)%k == 5, "e%polys use")
    deallocate(e%polys)
    e%fp => thrice
    call expect(e%fp(2) == 6, "e%fp use")
    allocate(leaf_arr(2)%la(2)); leaf_arr(2)%la = 5
    call expect(sum(leaf_arr(2)%la) == 10, "leaf_arr%la use")
    deallocate(leaf_arr(2)%la)
    leaf_arr(3)%tag = "zz"
    call expect(leaf_arr(3)%tag == "zz" .and. leaf_arr(1)%tag == "lf", "leaf_arr%tag use")
    global_init_11_check = nfail
    if (nfail == 0) print *, "ok"
contains
    subroutine expect(cond, what)
        logical, intent(in) :: cond
        character(*), intent(in) :: what
        if (.not. cond) then
            print *, "FAIL: ", what
            nfail = nfail + 1
        end if
    end subroutine expect
end function global_init_11_check
