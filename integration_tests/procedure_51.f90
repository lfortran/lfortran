module solver_min
    use types, only: dp
    use mixings, only: mixing_linear
    implicit none
contains

    subroutine solve_one()
        integer :: nq, ne, nband, scf_max_iter
        real(dp), allocatable :: vin(:, :), vout(:, :), tmp(:), energies(:)
        real(dp) :: scf_alpha, scf_l2_eps, scf_eig_eps

        nq = 3
        ne = 2
        nband = 2
        scf_max_iter = 0
        scf_alpha = 0.4_dp
        scf_l2_eps = 1e-4_dp
        scf_eig_eps = 1e-5_dp

        allocate(vin(nq, ne), vout(nq, ne), tmp(nq*ne), energies(nband))
        vin = 1.0_dp
        vout = 0.0_dp

        call mixing_linear(Ffunc, integral, reshape(vin, [nq*ne]), &
            nband, scf_max_iter, scf_alpha, scf_l2_eps, scf_eig_eps, tmp)

        if (any(abs(tmp - 1.0_dp) > 1e-12_dp)) error stop

    contains

        subroutine Ffunc(x, y, eng)
            real(dp), intent(in) :: x(:)
            real(dp), intent(out) :: y(:), eng(:)
            vin = reshape(x, shape(vin))
            vout = vin
            y = reshape(vout, shape(y))
            eng = 0.0_dp
            energies = eng
        end subroutine

        real(dp) function integral(x)
            real(dp), intent(in) :: x(:)
            integral = sum(x)
        end function
    end subroutine

end module

program procedure_51
    use solver_min, only: solve_one
    implicit none
    call solve_one()
end program
