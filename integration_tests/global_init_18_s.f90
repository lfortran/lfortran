subroutine global_init_18_set()
    use global_init_18_m
    implicit none
    if (st%n /= 7 .or. st%tag /= "ab") error stop 1
    if (allocated(st%a) .or. allocated(arr)) error stop 2
    if (counter /= 0) error stop 3
    st%n = 9
    st%tag = "zz"
    allocate(st%a(2), arr(3))
    st%a = 5
    arr = 6
    counter = counter + 1
end subroutine global_init_18_set

subroutine global_init_18_check()
    use global_init_18_m
    implicit none
    if (st%n /= 9 .or. st%tag /= "zz") error stop 4
    if (.not. allocated(st%a) .or. .not. allocated(arr)) error stop 5
    if (sum(st%a) /= 10 .or. sum(arr) /= 18) error stop 6
    if (counter /= 1) error stop 7
    deallocate(st%a, arr)
    print *, "ok"
end subroutine global_init_18_check
