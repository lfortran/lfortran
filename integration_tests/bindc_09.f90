program bindc_09
    ! Test: function declared EXTERNAL then defined locally
    ! Reproduces type mismatch when function is declared EXTERNAL (BindC ABI
    ! expected by caller) but defined locally (Source ABI, returns struct).
    ! The caller tries to convert struct to struct which fails without fix.
    implicit none

    ! Declare complex function as EXTERNAL - this makes caller expect BindC ABI
    complex :: get_complex_local
    external :: get_complex_local

    complex :: result

    ! Call the function - caller expects vector type return from EXTERNAL
    ! but function actually returns struct type since it's locally defined
    result = get_complex_local(1.0, 2.0)

    ! Verify result
    if (abs(real(result) - 1.0) > 1e-5) error stop
    if (abs(aimag(result) - 2.0) > 1e-5) error stop

    print *, "PASS"
end program

! Local function definition - uses Source ABI, returns struct
complex function get_complex_local(re, im)
    implicit none
    real, intent(in) :: re, im
    get_complex_local = cmplx(re, im)
end function
