module block_28_a
    implicit none
contains
    integer function twice(k)
        integer, intent(in) :: k
        block
            interface
                integer function unbox(x)
                    use block_28_b, only: box
                    type(box), intent(in) :: x
                end function unbox

                function make_box(k) result(r)
                    use block_28_b, only: box
                    integer, intent(in) :: k
                    type(box) :: r
                end function make_box
            end interface
            twice = unbox(make_box(k))
        end block
    end function twice
end module block_28_a
