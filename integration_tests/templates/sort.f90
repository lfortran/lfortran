module sort_m
    implicit none
    private
    public :: sort_tmpl

    requirement comparable(T, lt, gt)
        type, deferred :: T
        elemental function lt(lhs, rhs)
            type(T), intent(in) :: lhs, rhs
            logical :: lt
        end function
        elemental function gt(lhs, rhs)
            type(T), intent(in) :: lhs, rhs
            logical :: gt
        end function
    end requirement

    template sort_tmpl(T, lt, gt)
        private
        public :: sorted_order, sorted, sort

        requires comparable(T, lt, gt)

        generic :: operator(<) => lt
        generic :: operator(>) => gt

        generic :: sorted_order => sorted_order_
        generic :: sorted => sorted_
        generic :: sort => sort_
    contains
        pure recursive function sorted_order_(array) result(sorted_indices)
            type(T), intent(in) :: array(:)
            integer, allocatable :: sorted_indices(:)

            integer :: i

            associate(n => size(array))
                select case (n)
                case (0)
                    allocate(sorted_indices(0))
                case (1)
                    sorted_indices = [1]
                case (2)
                    if (array(1) > array(2)) then
                        sorted_indices = [2, 1]
                    else
                        sorted_indices = [1, 2]
                    end if
                case default
                    associate(pivot => (n/2 + 1), indices => [(i, i = 1, n)])
                        associate( &
                                less_than_pivot => array < array(pivot), &
                                greater_than_pivot => array > array(pivot))
                            associate( &
                                    indices_less_than_pivot => pack(indices, less_than_pivot), &
                                    indices_greater_than_pivot => pack(indices, greater_than_pivot), &
                                    indices_equal_pivot => pack(indices, .not.(less_than_pivot.or.greater_than_pivot)))
                                associate( &
                                        sorted_less_than => sorted_order_(array(indices_less_than_pivot)), &
                                        sorted_greater_than => sorted_order_(array(indices_greater_than_pivot)))
                                    sorted_indices = &
                                            [ indices_less_than_pivot(sorted_less_than) &
                                            , indices_equal_pivot &
                                            , indices_greater_than_pivot(sorted_greater_than) &
                                            ]
                                end associate
                            end associate
                        end associate
                    end associate
                end select
            end associate
        end function

        pure function sorted_(array)
            type(T), intent(in) :: array(:)
            type(T), allocatable :: sorted_

            sorted_ = array(sorted_order(array))
        end function

        pure subroutine sort_(array)
            type(T), intent(inout) :: array(:)

            array = sorted(array)
        end subroutine
    end template
end module

program test_sort
    use sort_m, only: sort_tmpl

    implicit none

    instantiate sort_tmpl(real, operator(<), operator(>)), only: sorted_order
    instantiate sort_tmpl(integer, operator(<), operator(>)), only: sorted_order
    instantiate sort_tmpl(real, operator(>), operator(<)), only: reverse_sorted_order => sorted_order

    associate(real_order => sorted_order([3.0, 2.0, 2.0, 1.0]))
        if (.not.all(real_order == [4, 2, 3, 1])) then
            print *, "real_order was ", real_order, " but should have been ", [4, 2, 3, 1]
        end if
    end associate

    associate(int_order => sorted_order([3, 2, 2, 1]))
        if (.not.all(int_order == [4, 2, 3, 1])) then
            print *, "int_order was ", int_order, " but should have been ", [4, 2, 3, 1]
        end if
    end associate

    associate(reverse_order => reverse_sorted_order([1.0, 2.0, 2.0, 3.0]))
        if (.not.all(reverse_order == [4, 2, 3, 1])) then
            print *, "reverse_order was ", reverse_order, " but should have been ", [4, 2, 3, 1]
        end if
    end associate
end program