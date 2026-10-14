! An operator interface block is built in the scope that declares it: a main
! program (#14025), a module procedure, an internal procedure or an external
! procedure (#14643)
module operator_interface_scope_01_m
    implicit none
contains
    integer function viaproc()
        interface operator(.plus.)
            integer function iplus(i, j)
                integer, intent(in) :: i, j
            end function iplus
        end interface
        viaproc = 2 .plus. 3
    end function viaproc
end module operator_interface_scope_01_m

program operator_interface_scope_01
    use operator_interface_scope_01_m
    implicit none
    interface operator(.pl.)
        procedure lpl
    end interface
    interface
        integer function viaext()
        end function
    end interface
    if (.not. (.true. .pl. .false.)) error stop 1
    if (.false. .pl. .false.) error stop 2
    if (viaproc() /= 50) error stop 3
    if (viaint() /= 70) error stop 4
    if (viaext() /= 90) error stop 5
    print *, "OK"
contains
    logical function lpl(x, y)
        logical, intent(in) :: x, y
        lpl = x .or. y
    end function

    integer function viaint()
        interface operator(.plus.)
            integer function iplus(i, j)
                integer, intent(in) :: i, j
            end function iplus
        end interface
        viaint = 3 .plus. 4
    end function viaint
end program

integer function viaext()
    interface operator(.plus.)
        integer function iplus(i, j)
            integer, intent(in) :: i, j
        end function iplus
    end interface
    viaext = 4 .plus. 5
end function viaext

integer function iplus(i, j)
    integer, intent(in) :: i, j
    iplus = 10*(i + j)
end function iplus
