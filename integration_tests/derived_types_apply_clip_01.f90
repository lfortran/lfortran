module derived_types_apply_clip_01_mod
    implicit none

    type :: clip_type
        logical :: l_min_max = .false.
        logical :: l_norm    = .false.
        real :: min_val = -huge(1.0)
        real :: max_val =  huge(1.0)
        real :: norm_val =  huge(1.0)
    contains
        procedure, pass(this) :: apply => apply_clip
    end type clip_type

contains

    pure subroutine apply_clip(this, length, gradient, bias)
        class(clip_type), intent(in) :: this
        integer, intent(in) :: length
        real, dimension(length), intent(inout) :: gradient
        real, dimension(:), optional, intent(inout) :: bias

        real :: scale
        real, dimension(:), allocatable :: bias_

        if (present(bias)) then
            bias_ = bias
        else
            allocate(bias_(1), source=0.0)
        end if

        if (this%l_min_max) then
            gradient = max(this%min_val, min(this%max_val, gradient))
            bias_ = max(this%min_val, min(this%max_val, bias_))
        end if

        if (this%l_norm) then
            scale = min(1.0, &
                this%norm_val / sqrt(sum(gradient**2.0) + &
                sum(bias_)**2.0))
            if (scale < 1.0) then
                gradient = gradient * scale
                bias_ = bias_ * scale
            end if
        end if

        if (present(bias)) bias = bias_

    end subroutine apply_clip

end module derived_types_apply_clip_01_mod

program derived_types_apply_clip_01
    use derived_types_apply_clip_01_mod
    implicit none

    type(clip_type) :: clipper
    real, dimension(5) :: grad
    integer :: i

    clipper%l_min_max = .true.
    clipper%min_val = -1.0
    clipper%max_val =  1.0

    grad = [2.0, -3.0, 0.5, -0.5, 4.0]
    call clipper%apply(5, grad)

    if (abs(grad(1) - 1.0) > 1.0e-6) error stop
    if (abs(grad(2) - (-1.0)) > 1.0e-6) error stop
    if (abs(grad(3) - 0.5) > 1.0e-6) error stop
    if (abs(grad(4) - (-0.5)) > 1.0e-6) error stop
    if (abs(grad(5) - 1.0) > 1.0e-6) error stop

    print *, "PASS"
end program derived_types_apply_clip_01
