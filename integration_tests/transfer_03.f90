module stdlib_hash_32bit_water_transfer_03
    use iso_fortran_env, only: int16, int8, int32, int64

contains

    ! pure module function int8_water_hash( key ) result(hash_code)
    !     integer(int32)             :: hash_code
    !     integer(int8), intent(in)  :: key(:)
    ! end function int8_water_hash

    subroutine int16_water_hash( key )
        integer(int16), intent(in) :: key(:)
        print *, transfer( key, 0_int8, size(key, kind=int64) )
        ! TODO: values diverge with GFortran, PR intends to fix code generation error

        ! hash_code = int8_water_hash( transfer( key, 0_int8, size(key, kind=int64) ) )

    end subroutine int16_water_hash

end module stdlib_hash_32bit_water_transfer_03

program transfer_03
    use stdlib_hash_32bit_water_transfer_03
    integer(int16) :: key(10)
    key = 9
    call int16_water_hash(key)
end program
