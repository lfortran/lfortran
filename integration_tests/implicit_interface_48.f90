! Non-conforming extension (gfortran needs -fallow-argument-mismatch):
! two references to the same external under an implicit interface with
! different actual kinds. No single set of dummy characteristics can
! satisfy both (F2018 15.5.2.5). LFortran keeps the first-inferred
! signature as the canonical procedure and calls the second through an
! explicit FunctionPointerCast, which is the separate-compilation model
! when the definition is not in this translation unit.
program implicit_interface_48
    implicit none
    integer(1) :: bytval(3)
    integer(2) :: shval(3)

    bytval = 0_1
    shval = 0_2

    call store_first(bytval)
    call store_first(shval)

    if (bytval(1) /= 7) error stop
    print *, "ok"
end program
