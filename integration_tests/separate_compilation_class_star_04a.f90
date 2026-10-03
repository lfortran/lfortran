module separate_compilation_class_star_04_base
    implicit none
    type :: graph_type
        integer :: id = 0
    end type graph_type
end module separate_compilation_class_star_04_base

module separate_compilation_class_star_04_user
    use separate_compilation_class_star_04_base, only: graph_type
    implicit none
contains
    subroutine work(out_id1, out_id2)
        integer, intent(out) :: out_id1, out_id2
        class(*), allocatable :: data_poly(:,:)
        data_poly = get_sample()
        select type(data_poly)
        type is (graph_type)
            out_id1 = data_poly(1,1)%id
            out_id2 = data_poly(1,2)%id
        class default
            out_id1 = -1
            out_id2 = -1
        end select
    end subroutine work

    function get_sample() result(sample)
        type(graph_type) :: sample(1, 2)
        sample(1,1)%id = 11
        sample(1,2)%id = 22
    end function get_sample
end module separate_compilation_class_star_04_user
