! --mangle-underscore-external does not rename a bind(c) name.
program mangle_underscore_external_04
    use iso_c_binding, only: c_int
    implicit none
    interface
        integer(c_int) function c_add_one(a) bind(c)
            import :: c_int
            integer(c_int), value :: a
        end function
    end interface
    if (c_add_one(3) /= 4) error stop
end program
