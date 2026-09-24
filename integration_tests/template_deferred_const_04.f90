! Local named constants of a templated procedure whose initializers are
! expressions of the template's deferred constant, used as array bounds. Each
! instantiation must give the arrays the extents of its own constant, and the
! constants (including negated and real ones, and numeric intrinsics of the
! constant, negative powers, comparisons and logical operations of the
! constant) the values of its own constant. real(16) constants are in
! template_deferred_const_05.

module template_deferred_const_04_m
    implicit none
    private
    public :: test_three, test_five

    integer, parameter :: three = 3, five = 5

    template tmpl {n}
        deferred integer, parameter :: n
        private
        public :: size_mul, size_add, size_chain, fill_sum, neg, &
            size_neg, neg_real, intr, size_intr, intr_real, pow_neg, logic
    contains
        function size_mul() result(r)
            integer :: r
            integer, parameter :: m = n*2
            integer :: y(m)
            r = size(y)
        end function

        function size_add() result(r)
            integer :: r
            integer, parameter :: m = n+1
            integer :: y(m)
            r = size(y)
        end function

        function size_chain() result(r)
            integer :: r
            integer, parameter :: a = n+1, b = a*2
            integer :: y(b)
            r = size(y)
        end function

        function fill_sum() result(r)
            integer :: r
            integer, parameter :: m = n*2
            integer :: y(m)
            integer :: i
            do i = 1, m
                y(i) = i
            end do
            r = sum(y)
        end function

        function neg() result(r)
            integer :: r
            integer, parameter :: d = -n
            r = d
        end function

        function size_neg() result(r)
            integer :: r
            integer, parameter :: m = -(-n), k = -n
            integer :: y(m), z(k + 3*n)
            r = size(y) + 100*size(z)
        end function

        function neg_real() result(r)
            real :: r
            real, parameter :: x = -(n*2.5), y = -x/2
            r = x + 100*y
        end function

        function intr() result(r)
            integer :: r
            integer, parameter :: a = abs(-n), b = max(n, 4) + min(n, 4), &
                c = mod(n, 2) + modulo(-n, 4), d = sign(2, -n) + dim(n, 1)
            r = a + 10*b + 100*c + 1000*d
        end function

        function size_intr() result(r)
            integer :: r
            integer, parameter :: m = int(n*2.5), k = abs(n - 10)
            integer :: y(m), z(k), w(nint(n*0.9) + floor(-n*0.5) + ceiling(n*0.5))
            r = size(y) + 100*size(z) + 10000*size(w)
        end function

        function intr_real() result(r)
            real :: r
            real, parameter :: x = real(n), y = -real(n)/2, z = sqrt(real(n*n))
            r = x + 10*y + 100*z
        end function

        function pow_neg() result(r)
            integer :: r
            integer, parameter :: a = 2**(-n), b = (-1)**(-n), c = 1**(-n), &
                d = 12/(n-1)
            integer :: y(c + 1)
            r = a + 10*b + 100*size(y) + 1000*d
        end function

        function logic() result(r)
            integer :: r
            logical, parameter :: a = n > 3, b = n > 2 .and. n < 10, &
                c = .not. a .or. real(n) > 4.5, d = a .eqv. c
            r = 0
            if (a) r = r + 1
            if (b) r = r + 10
            if (c) r = r + 100
            if (d) r = r + 1000
        end function
    end template

contains

    subroutine test_three()
        instantiate tmpl {three}, only: size_mul_3 => size_mul, &
            size_add_3 => size_add, size_chain_3 => size_chain, &
            fill_sum_3 => fill_sum, neg_3 => neg, size_neg_3 => size_neg, &
            neg_real_3 => neg_real, intr_3 => intr, &
            size_intr_3 => size_intr, intr_real_3 => intr_real, &
            pow_neg_3 => pow_neg, logic_3 => logic
        if (size_mul_3() /= 6) error stop
        if (size_add_3() /= 4) error stop
        if (size_chain_3() /= 8) error stop
        if (fill_sum_3() /= 21) error stop
        if (neg_3() /= -3) error stop
        if (size_neg_3() /= 603) error stop
        if (abs(neg_real_3() - 367.5) > 1e-4) error stop
        if (intr_3() /= 273) error stop
        if (size_intr_3() /= 30707) error stop
        if (abs(intr_real_3() - 288.0) > 1e-4) error stop
        print *, size_mul_3(), size_add_3(), size_chain_3(), fill_sum_3()
        print *, neg_3(), size_neg_3(), neg_real_3()
        if (pow_neg_3() /= 6190) error stop
        if (logic_3() /= 110) error stop
        print *, intr_3(), size_intr_3(), intr_real_3()
        print *, pow_neg_3(), logic_3()
    end subroutine

    subroutine test_five()
        instantiate tmpl {five}, only: size_mul_5 => size_mul, &
            size_add_5 => size_add, size_chain_5 => size_chain, &
            fill_sum_5 => fill_sum, neg_5 => neg, size_neg_5 => size_neg, &
            neg_real_5 => neg_real, intr_5 => intr, &
            size_intr_5 => size_intr, intr_real_5 => intr_real, &
            pow_neg_5 => pow_neg, logic_5 => logic
        if (size_mul_5() /= 10) error stop
        if (size_add_5() /= 6) error stop
        if (size_chain_5() /= 12) error stop
        if (fill_sum_5() /= 55) error stop
        if (neg_5() /= -5) error stop
        if (size_neg_5() /= 1005) error stop
        if (abs(neg_real_5() - 612.5) > 1e-4) error stop
        if (intr_5() /= 2495) error stop
        if (size_intr_5() /= 50512) error stop
        if (abs(intr_real_5() - 480.0) > 1e-4) error stop
        print *, size_mul_5(), size_add_5(), size_chain_5(), fill_sum_5()
        print *, neg_5(), size_neg_5(), neg_real_5()
        if (pow_neg_5() /= 3190) error stop
        if (logic_5() /= 1111) error stop
        print *, intr_5(), size_intr_5(), intr_real_5()
        print *, pow_neg_5(), logic_5()
    end subroutine

end module

program template_deferred_const_04
    use template_deferred_const_04_m, only: test_three, test_five
    implicit none
    call test_three()
    call test_five()
end program
