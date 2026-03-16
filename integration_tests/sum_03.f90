program sum_03
    use iso_fortran_env, only: sp => real32
    complex(sp), parameter :: x_sum_mask(3) = [(1.0_sp, 0.0_sp), (-2.0_sp, 0.0_sp), (3.0_sp, 0.0_sp)]
    logical :: sum_mask(3)
    logical, parameter :: sum_mask_2(3) = [.true., .false., .true.]
    complex(sp) :: sum_masked_res, sum_masked_res_2

    sum_mask = [.true., .false., .true.]
    sum_masked_res = sum(x_sum_mask, sum_mask)
    sum_masked_res_2 = sum(x_sum_mask, sum_mask_2)
    if (abs(sum_masked_res - cmplx(4.0_sp, 0.0_sp, kind=sp)) > 1.0e-5_sp) error stop
    if (abs(sum_masked_res_2 - cmplx(4.0_sp, 0.0_sp, kind=sp)) > 1.0e-5_sp) error stop
end program sum_03