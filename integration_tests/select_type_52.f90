module select_type_52_mod
    implicit none
    type :: array_type
        integer :: id = 0
    end type array_type

    type :: array_ptr_type
        type(array_type), pointer :: array(:,:) => null()
    end type array_ptr_type
contains
    subroutine dispatch(input, expected_kind, expected_id)
        class(*), dimension(..), intent(in) :: input
        integer, intent(in) :: expected_kind
        integer, intent(in) :: expected_id
        select rank(input)
        rank(0)
            select type(input)
            class is(array_type)
                if (expected_kind /= 1) error stop "rank0: wrong kind"
                if (input%id /= expected_id) error stop "rank0: wrong id"
            class is(array_ptr_type)
                if (expected_kind /= 2) error stop "rank0: wrong kind"
            class default
                error stop "rank0: unexpected default"
            end select
        rank(1)
            select type(input)
            class is(array_type)
                if (expected_kind /= 3) error stop "rank1: wrong kind"
                if (input(1)%id /= expected_id) error stop "rank1: wrong id"
            type is(real)
                if (expected_kind /= 4) error stop "rank1: wrong kind"
            class default
                error stop "rank1: unexpected default"
            end select
        end select
    end subroutine dispatch
end module select_type_52_mod

program select_type_52
    use select_type_52_mod
    implicit none
    type(array_type) :: scalar_val
    type(array_type) :: rank1_val(1)
    type(array_ptr_type) :: ptr_val
    real :: r(3) = [1.0, 2.0, 3.0]

    scalar_val%id = 42
    call dispatch(scalar_val, 1, 42)

    call dispatch(ptr_val, 2, 0)

    rank1_val(1)%id = 7
    call dispatch(rank1_val, 3, 7)

    call dispatch(r, 4, 0)
end program select_type_52
