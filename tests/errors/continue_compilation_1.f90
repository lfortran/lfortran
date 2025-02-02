program continue_compilation_1
    implicit integer(a-f), real(e-z)

    integer :: a(3), b(3), b1(3, 3), a3(3, 3, 3), b4(3, 3, 3, 3), a5, c5, i, arr1(3), arr2(2, 3), arr3(2, 1, 3)
    character :: a1(3, 3)
    logical :: a2(3, 3), mask1(3), mask2(2, 3), mask3(2, 1, 3), mask4(3, 2), mask5(2, 3, 1)
    integer(kind=8) :: b5
    real(8) :: y1
    real :: z1
    integer, parameter :: i1 = 2
    character(len=5) :: string = "hello"
	character(len=1) :: set(2) = ["l", "h"]

    a5 = 8
    b5 = 12_8
    c5 = 2

    !loop_test
    do i=1,3
       i = i + 1
       print*,i
    end do
    !maskl_incorrect_bit_size
    print*, maskl(63)
    !maskr_incorrect_bit_size
    print*, maskr(63)
    !maskl_negative
    print*, maskl(-24)
    !maskr_negative
    print*, maskr(-24)
    !matrix_matmul_01
    print *, matmul(a1, b1)
    !matrix_matmul_02
    print *, matmul(b1, a1)
    !matrix_matmul_03
    print *, matmul(a2, b1)
    !matrix_matmul_04
    print *, matmul(a3, b1)
    !matrix_matmul_05
    print *, matmul(b1, b4)
    !matrix_matmul_06
    print *, matmul(a, b)
    !matrix_transpose_01
    print *, transpose(a)
    !merge_bits_comp
    print *, merge_bits(8, 12_8, 2)
    !merge_bits_run
    print *, merge_bits(a5, b5, c5)

    !Does not work correctly : Issue: #5469 -------------
    ! !max_01
    ! y1 = 5.2d0
    ! z1 = 9.0
    ! print *, max(y1, z1)
    ! !max_02
    ! print *, max(b5, a5)
    ! !min_01
    ! print *, min(y1, z1)
    ! !min_02
    ! print *, min(b5, a5)
    !------------------------------

    !modulo_01
    print *, modulo(1, 0)
    !more_kwargs_than_acceptable_to_subroutine
    call my_func(y=1, x=2, z=1)

    contains

    subroutine my_func(x, y)
        integer, intent(in) :: x, y
        print *, "hi"
    end subroutine
    !nint_overflow
    print*, nint(1e12_8)
    print*, nint(1000000000000.0000000000000000d0)
    ! open_invalid_kwarg1
    OPEN(file="numbers", hello="world")
    !parameter_01
    i1 = 3
    print*,i1
    call FLUSH(1, 2)

    print*, verify(string, set, kind= [4, 4] )
    print *, and([1, 2, 3], [1, 2, 3])
    character(3), parameter :: ar1 = repeat(["abc", "#^1", "123"], [1, 2, 3])

    print *, dshiftl(1, 2, 34)
    print *, dshiftl(1, 2, -2)

    print *, dshiftr(1, 2, 34)
    print *, dshiftr(1, 2, -2)

    print *, selected_int_kind([1,2,3])
    print *, selected_real_kind([1,2,3])
    print *, selected_char_kind(['c', 'a', 'b'])

    arr1 = reshape([1, 2, 3], [3])
    arr2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    arr3 = reshape([1, 2, 3, 4, 5, 6], [2, 1, 3])
    mask1 = reshape([.true., .false., .true.], [3])
    mask2 = reshape([.true., .false., .true., .true., .false., .true.], [2, 3])
    mask3 = reshape([.true., .false., .true., .true., .false., .true.], [2, 1, 3])

    print *, sum(arr1, dim = 2)
    print *, sum(arr1, dim = -1)
    print *, sum(arr1, mask = mask1, dim = 2)
    print *, sum(arr1, mask = mask1, dim = -1)

    print *, product(arr2, dim = 3)
    print *, product(arr2, dim = -1)
    print *, product(arr2, mask = mask2, dim = 3)
    print *, product(arr2, mask = mask2, dim = -1)

    print *, iparity(arr3, dim = 4)
    print *, iparity(arr3, dim = -1)
    print *, iparity(arr3, mask = mask3, dim = 4)
    print *, iparity(arr3, mask = mask3, dim = -1)

    integer :: q1
    real :: r1
    character :: c1

    if (q1) q1 = 1
    if (r1) r1 = 1.0
    if (c1) c1 = 'a'

    mask4 = reshape([.true., .false., .true., .true., .false., .true.], [3, 2])
    mask5 = reshape([.true., .false., .true., .true., .false., .true.], [2, 3, 1])

    print *, sum(arr1, mask2)
    print *, sum(arr2, mask3, 2)
    print *, iparity(arr2, mask4)
    print *, iparity(arr3, mask5, 3)

    ! argument_not_a_variable
    print *, present(a + 1)

    ! argument_not_optional
    print *, present(a)

    print *, pack([1, 2, 3], [.true., .true., .true., .true.])

    print *, reshape("hello", [2, 3])
    print *, reshape(.true., [2, 3])
    print *, reshape([1, 2, 3, 4], "hello")
    print *, reshape([1, 2, 3, 4], .false.)
   print *, "hello"
   exit
   print *, "world"
end program
