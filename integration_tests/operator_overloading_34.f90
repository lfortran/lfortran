program operator_overloading_34
    use operator_overloading_34_mod
    implicit none
    type(array_type) :: a
    logical :: res(2, 3)
    allocate(a%val(2, 3))
    a%val = 5.0
    res = a .gt. 3.0
    if (.not. all(res)) error stop
    a%val = 1.0
    res = a .gt. 3.0
    if (any(res)) error stop
    print *, "ok"
end program
