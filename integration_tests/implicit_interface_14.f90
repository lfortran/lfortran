program main
    implicit type(foo)(a-z)
    type foo
        integer x, y
    end type

    z = foo(42, 1)
    print *, z%x, z%y
end
