program continue_compilation_1
    implicit integer(a-f), real(e-z)

    integer :: a(3), b(3), b1(3, 3), a3(3, 3, 3), b4(3, 3, 3, 3), a5, c5, i
    character :: a1(3, 3)
    logical :: a2(3, 3)
    integer(kind=8) :: b5
    real(8) :: y1
    real :: z1
    integer, parameter :: i1 = 2

    real :: x
    logical :: y
     

    real :: adwf = .true.
    x = y 
    
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

end program
