module recursive_mod_subroutines_16
contains
subroutine recursive_fun1(x, f)
    implicit none
    real(8), intent(in) :: x(:)
    real(8), intent(out) :: f
end subroutine recursive_fun1
end module recursive_mod_subroutines_16

module test_solver_mod_subroutines_16

contains

    subroutine test_solver()

    contains

        subroutine temp(rec)
            use recursive_mod_subroutines_16
            procedure(recursive_fun1) :: rec
        end subroutine

        subroutine recursive_fun2()
            use, non_intrinsic :: recursive_mod_subroutines_16, only : recursive_fun1
            call temp(recursive_fun1)
        end subroutine recursive_fun2

    end subroutine test_solver

end module test_solver_mod_subroutines_16


program subroutines_16
  use test_solver_mod_subroutines_16
end program
