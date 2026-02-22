program modules_66
    use modules_66_base, only: container_t
    use modules_66_worker, only: extract_int
    implicit none

    type(container_t) :: c
    integer :: r

    allocate(c%item, source=42)
    r = extract_int(c)
    if (r /= 42) error stop

    print *, "PASSED: modules_66"
end program modules_66
