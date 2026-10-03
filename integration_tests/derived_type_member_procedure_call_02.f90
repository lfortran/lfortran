module derived_type_member_procedure_call_02_mod
    implicit none
contains

    subroutine set_raster_line_style(style, line_style, line_pattern, pattern_size, pattern_length, pattern_distance)
        character(len=*), intent(in) :: style
        character(len=10), intent(out) :: line_style
        real, intent(out) :: line_pattern(:)
        integer, intent(out) :: pattern_size
        real, intent(out) :: pattern_length
        real, intent(out) :: pattern_distance

        line_style = style
        line_pattern = 0.0
        pattern_size = size(line_pattern)
        pattern_length = 0.0
        pattern_distance = 0.0
    end subroutine set_raster_line_style

end module derived_type_member_procedure_call_02_mod

module derived_type_member_procedure_call_02_type
    use derived_type_member_procedure_call_02_mod, only: set_raster_line_style
    implicit none

    type :: raster_image_t
        character(len=10) :: line_style = '-'
        real :: line_pattern(20)
        integer :: pattern_size = 1
        real :: pattern_length = 1.0
        real :: pattern_distance = 0.0
    contains
        procedure :: set_line_style => raster_set_line_style
    end type raster_image_t

contains

    subroutine raster_set_line_style(this, style)
        class(raster_image_t), intent(inout) :: this
        character(len=*), intent(in) :: style

        call set_raster_line_style(style, this%line_style, this%line_pattern, &
                                   this%pattern_size, this%pattern_length, this%pattern_distance)
    end subroutine raster_set_line_style

end module derived_type_member_procedure_call_02_type

program derived_type_member_procedure_call_02
    use derived_type_member_procedure_call_02_type, only: raster_image_t
    implicit none

    type(raster_image_t) :: img
    call img%set_line_style('--')
    write(*, '(I0)') img%pattern_size
end program derived_type_member_procedure_call_02
