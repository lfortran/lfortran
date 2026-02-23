! If you need a function, put it into the module below and remove the same
! number of lines below the module to keep the rest of the lines in this file
! intact.
module continue_compilation_3_mod







contains

    subroutine check_incompatible_type(i)
      real :: i
    end subroutine check_incompatible_type

    subroutine intent_out_test(x)
        integer, intent(out) :: x
        x = 42
    end subroutine intent_out_test

    subroutine intent_inout_test(y)
        integer, intent(inout) :: y
        y = y + 1
    end subroutine intent_inout_test

    subroutine assumed_rank(x)
        integer, intent(in) :: x(..)
        print *, x
        print *, reshape(x, [size(x)])
        print *, x(1)
	    x = [1, 2]
        print *, (x /= [1, 2])
    end subroutine assumed_rank

    subroutine ss(x)
    character(:), allocatable :: x
    x = "AB"          ! allocates x with length 2
    end subroutine ss



end module continue_compilation_3_mod

! Only put declarations and statements here, no subroutines (those go above).
program continue_compilation_3
    use continue_compilation_3_fake_module
    use continue_compilation_3_mod

    ! Put declarations below without empty lines
    implicit none
    integer :: a(5), b(10), x1(3,2), c, a1(3), i, k = 3, x
    character(4) :: str(3)
    integer, parameter :: x3 = 2
    character(len=8) :: s, s1 = "lfortran"
    rea :: test_real(12)
    real :: test_re()
    int ::

    type :: t
        integer :: xx
    end type
    type(t) :: y
    integer :: merge_i = 4, merge_j = 5
    integer(8) :: merge_k = 8

    call intent_out_test(1)  ! Error: literal constant with intent(out)
    call intent_out_test(x + 1)  ! Error: expression with intent(out)
    call intent_inout_test(2)  ! Error: literal constant with intent(inout)
    call intent_inout_test(x * 2)  ! Error: expression with intent(inout)
    call check_incompatible_type(i)  ! Error: incompatible type passed



    













    ! array_01_cc
    a = []
    ! array_02_cc
    print *, [[[], [[]]], [[]], []]
    print *, [[[], [[]]], []]
    ! array_03_cc
    b(:,:) = 1
    b(:,:) = 2
    ! array_04_cc
    y%xx(:) = 1
    ! array_06_cc
    str(1, 2)(:) = '1234'
    str(1,2,3)(:) = '1234'
    ! array_constructor_with_asterisk_in_type_spec_cc
    print *, [character(*) :: "a", "b", "ball", "cat"]
    print *, [character(*) :: "a2", "b2", "ball2", "cat2"]
    !array_shape_01_cc
    x1 = reshape([1,2,3,4],[2,2])
    x1 = reshape([1,2,3,4],[1,2])
    !arithmetic_if1_cc
    i = -3
    c = 0
    if ("yy") 1, 2, 3
    1 c = c + 1
    2 c = c + 2
    3 c = c + 4
    print *, c
    if (c /= 7) error stop
    !array_size_01_cc
    i = size(a1, 1, 4, kind=4)
    i = size()
    !assign_01_cc
    x3 = 1
    print *, x3
    ! data_implied_do1
    data(a1(i), i=1, k) / 1, 2, 3 /
    ! data_implied_do2
    data(a1(i), i=1, 3, k) / 1, 2, 3 /
    ! data_implied_do3
    data(a1(i), i=k, 3) / 1, 2, 3 /

    !program_variable
    i = foo
    !rewind_invalid_kwarg1
    rewind(end="world")


    !sign_01
    print *, sign(1, 1_8)
    !specific_type_intrinsic
    print*, dabs(1)
    !sqrt_neg
    print *, sqrt(-1.0)
    !string_binop
    print *, "a" + "b"
    !string_negative_start_index
    s = "lfortran"
    print*, "s:", s(-1:4)
    !string_slice
    print*, s1(-2:6)
    !string_slice2
    print*, "Length of s:", len(s)
    print*, s1(1: 9)
    !substring_noninteger_endidx
    print*, s1(1:5.2)
    !substring_noninteger_startidx
    print*, s1(1.1:5)
    !substring_noninteger_stride
    print*, s(1:5:2.2)
    !type_mismatch1
    x = "x"
    !type_mismatch2
    x = 5 + "x"
    !subroutine1, subroutine2, subroutine3, subroutine5
    call bpe()
    i = bpe()
    print *, xx
    test_re = 1245.13
    c(1) = 1

    integer :: i_incorrect_pragma
        !LF$unroll 4  ! Error: Missing space after `!LF$`
    do i_incorrect_pragma = 1, 10
        print *, i_incorrect_pragma
    end do
    !merge_bits
    print *, merge_bits(1, 2, 3_8)
    print *, merge_bits(merge_i,merge_j,merge_k)

    ! adding the new test for performing logical .and. between int-logical type.
    !and_mismatch_int_logical
    print *, 5 .and. .true.

    !and_mismatch_real_logical
    print *, 3.14 .and. .false.

    !and_okay_int_int
    print *, 5 .and. 6

    !adding a few more cases
    print *, 5 .or. 6
    print *, 5 .eqv. 6
    print *, .true. .neqv. 6
    print *, 3.14 .and. "abcd"
    print *, "abcd" .neqv. "cdef"
    print *, 1 .neqv. 2
    print *, [1,2,3] .and. .true.
    print *, [1.0, 2.0] .or. [3.0, 4.0]
    print *, "str1" .or. "str2"
    print *, "x" .and. .false.
    print *, .NOT. "lf"
    print *, "8356" .or. 8356
    print *, "8356" .eqv. 8356.00
    print *, ['c', 'o', 'd', 'e'] .or. ['m', 'a', 's']
    print *, ["welcome", "to", "lf"] .and. "contributors"  !even size diff of array element must be caught
    call ss("hello")
    contains 
    subroutine bpe()
        print *, size(bpe)
        bpe = d
    end subroutine
        
end program
