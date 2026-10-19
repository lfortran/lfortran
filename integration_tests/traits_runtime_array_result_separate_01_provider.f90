! The provider: Ramp holds n and a scale, and its samples are 1, ..., n
! times the scale.
module traits_runtime_array_result_separate_01_provider_m
    use traits_runtime_array_result_separate_01_contracts_m, only: ISamples
    implicit none
    type :: Ramp
        integer :: n = 0
        real(8) :: scale = 1
    end type Ramp
    implements ISamples :: Ramp
        procedure, pass :: samples => ramp_samples
        procedure, pass :: window => ramp_window
        procedure, pass :: series => ramp_series
        procedure, pass :: scaled => ramp_scaled
    end implements
contains
    function ramp_samples(self) result(x)
        class(Ramp), intent(in) :: self
        real(8), allocatable :: x(:)
        integer :: i
        allocate(x(self%n))
        do i = 1, self%n
            x(i) = i * self%scale
        end do
    end function ramp_samples

    function ramp_window(self, n) result(x)
        class(Ramp), intent(in) :: self
        integer, intent(in) :: n
        integer :: x(n)
        integer :: i
        do i = 1, n
            x(i) = self%n + i
        end do
    end function ramp_window

    function ramp_series(self, n) result(a)
        class(Ramp), intent(in) :: self
        integer, intent(in) :: n
        real(8), allocatable :: a(:)
        integer :: i
        allocate(a(n))
        do i = 1, n
            a(i) = self%n + i * self%scale
        end do
    end function ramp_series

    function ramp_scaled(self, v) result(x)
        class(Ramp), intent(in) :: self
        real(8), intent(in) :: v(:)
        real(8) :: x(size(v))
        x = v * self%scale
    end function ramp_scaled
end module traits_runtime_array_result_separate_01_provider_m
