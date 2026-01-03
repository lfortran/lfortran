module stdlib_hash_32bit_water_transfer_05
    use iso_fortran_env, only: int8, int32, int64
    implicit none
contains

    subroutine int8_water_hash( key )
        integer(int8), intent(in)  :: key(:)
        print *, sum(key)
        if (sum(key) /= -9) error stop

    end subroutine int8_water_hash

    subroutine character_water_hash( key )
        character(*), intent(in)   :: key

        call int8_water_hash( transfer( key, 0_int8, len(key, kind=int64) ) )

    end subroutine character_water_hash

end module stdlib_hash_32bit_water_transfer_05

program transfer_05
    use stdlib_hash_32bit_water_transfer_05
    character(len=10) :: key
    key = 'abcdefghij'
    call character_water_hash(key)
end program
