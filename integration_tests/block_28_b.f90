module block_28_b
    implicit none
    type :: box
        integer :: v
    end type box
end module block_28_b

integer function unbox(x)
    use block_28_b, only: box
    implicit none
    type(box), intent(in) :: x
    unbox = x%v
end function unbox

function make_box(k) result(r)
    use block_28_b, only: box
    implicit none
    integer, intent(in) :: k
    type(box) :: r
    r%v = 2*k
end function make_box
