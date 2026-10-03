! COMMON in a module procedure, initialized from BLOCK DATA.
! The using module must import the synthetic file_common_block_* symbols
! into the procedure scope (like USE) so save_mod_files / asr_verify succeed
! and LLVM emits the COMMON module before the user module.
module common_43_mod
contains
    subroutine show_value()
        integer :: value_var
        common /saved_value/ value_var
        if (value_var /= 1) error stop
    end subroutine show_value
end module common_43_mod

block data init_value
    integer :: value_var
    common /saved_value/ value_var
    data value_var /1/
end block data init_value

program common_43
    use common_43_mod, only: show_value
    call show_value()
end program common_43
