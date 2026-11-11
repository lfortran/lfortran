! A class(*) variable holds a value of its dynamic type, so the data
! buffer that holds it must be as large as that type. A complex(16) is
! 32 bytes, which does not fit in the 16-byte class wrapper.
! lfortran/lfortran#17472
program class_star_complex128_01
    implicit none
    class(*), allocatable :: u
    complex(16) :: z

    z = (1.5_16, -2.5_16)
    u = z
    select type (u)
    type is (complex(16))
        if (u /= (1.5_16, -2.5_16)) error stop 1
    class default
        error stop 2
    end select

    ! The same value assigned again, to a variable that is already
    ! allocated with the dynamic type.
    u = z
    select type (u)
    type is (complex(16))
        if (u /= (1.5_16, -2.5_16)) error stop 3
    class default
        error stop 4
    end select

    ! A class(*) holding a complex(16) component of a derived type.
    call check_component()

    print *, "ok"
contains

    subroutine check_component()
        type :: holder_t
            class(*), allocatable :: c
        end type
        type(holder_t) :: h
        h%c = z
        associate (c => h%c)
            select type (c)
            type is (complex(16))
                if (c /= (1.5_16, -2.5_16)) error stop 5
            class default
                error stop 6
            end select
        end associate
    end subroutine check_component

end program class_star_complex128_01
