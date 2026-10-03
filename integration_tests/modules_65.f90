program modules_65
    use modules_65_base, only: base_t, holder_t
    use modules_65_child, only: child_t
    use modules_65_worker, only: classify
    implicit none

    type(holder_t) :: h1, h2
    integer :: r

    allocate(child_t :: h1%item)
    h1%item%width = 10
    select type (x => h1%item)
    class is (child_t)
        x%extra = 99
    end select

    allocate(base_t :: h2%item)
    h2%item%width = 7

    r = classify(h1)
    if (r /= 99) error stop
    r = classify(h2)
    if (r /= 7) error stop

    print *, "PASSED: modules_65"
end program modules_65
