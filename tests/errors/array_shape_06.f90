module m_array_shape_06
    use, intrinsic :: iso_fortran_env, only: real32
    implicit none
contains
    subroutine write_partial(upstream_grad, output)
        real(real32), dimension(:,:), intent(in) :: upstream_grad
        real(real32), dimension(:,:), intent(out) :: output
        output = upstream_grad * 2.0_real32
    end subroutine write_partial
end module m_array_shape_06

program array_shape_06
    use, intrinsic :: iso_fortran_env, only: real32
    use m_array_shape_06
    implicit none

    real(real32) :: ug(8, 1)
    real(real32), allocatable :: out(:,:)

    allocate(out(1, 1))
    ug = 1.0_real32
    out = 0.0_real32

    call write_partial(ug, out)
end program array_shape_06
