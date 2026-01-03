MODULE intrinsics_337_mod
implicit none

! interface with same name as intrinsic 'dble'
interface dble
    module procedure interface_dble_fun
end interface

! interface with same name as intrinsic 'float'
interface float
    module procedure interface_float_fun
end interface

! interface with same name as intrinsic 'dfloat'
interface dfloat
    module procedure interface_dfloat_fun
end interface

! interface with same name as intrinsic 'shifta'
interface shifta
    module procedure interface_shifta_fun
end interface

CONTAINS

    subroutine sub1()
        integer :: intg
        intg = 1
        print *, dble(intg)
        if (abs(dble(intg) - 1.D0) > epsilon(1.D0)) error stop
    end subroutine sub1

    subroutine sub2()
        integer :: intg
        intg = 1
        print *, float(intg)
        if (abs(float(intg) - 1.0) > epsilon(1.0)) error stop
    end subroutine sub2

    subroutine sub3()
        integer :: intg
        intg = 1
        print *, dfloat(intg)
        if (abs(dfloat(intg) - 1.D0) > epsilon(1.D0)) error stop
    end subroutine sub3

    subroutine sub4()
        integer :: intg
        intg = 1
        print *, shifta(intg, intg)
        if (shifta(intg, intg) /= 0) error stop
    end subroutine sub4

    doubleprecision function interface_dble_fun()
        interface_dble_fun = 5.0D0
    end function interface_dble_fun

    double precision function interface_float_fun()
        interface_float_fun = 5.0D0
    end function interface_float_fun

    double precision function interface_dfloat_fun()
        interface_dfloat_fun = 5.0D0
    end function interface_dfloat_fun

    double precision function interface_shifta_fun()
        interface_shifta_fun = 5.0D0
    end function interface_shifta_fun
end module intrinsics_337_mod

program intrinsics_337
    use intrinsics_337_mod
    implicit none
    ! this call matches with intrinsic 'dble'
    print *, dble(1)
    if (abs(dble(1) - 1.D0) > epsilon(1.D0)) error stop
    ! this call matches with intrinsic 'float'
    print *, float(1)
    if (abs(float(1) - 1.0) > epsilon(1.0)) error stop
    ! this call matches with intrinsic 'dfloat'
    print *, dfloat(1)
    if (abs(dfloat(1) - 1.D0) > epsilon(1.D0)) error stop
    ! this call matches with intrinsic 'shifta'
    print *, shifta(1, 1)
    if (shifta(1, 1) /= 0) error stop

    ! this call matches with interface defined in module
    print *, dble()
    if (abs(dble() - 5.D0) > epsilon(1.D0)) error stop
    ! this call matches with interface defined in module
    print *, float()
    if (abs(float() - 5.0) > epsilon(1.0)) error stop
    ! this call matches with interface defined in module
    print *, dfloat()
    if (abs(dfloat() - 5.D0) > epsilon(1.D0)) error stop
    ! this call matches with interface defined in module
    print *, shifta()
    if (abs(shifta() - 5.D0) > epsilon(1.D0)) error stop

    call sub1()
    call sub2()
    call sub3()
    call sub4()
end program intrinsics_337
