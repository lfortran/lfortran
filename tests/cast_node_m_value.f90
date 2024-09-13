! we do casting from source to destination below during
! initialization of variable's, we save it's ASR via tests.toml
! to ensure that the `m_value` of the cast node is set
! correctly
program cast_node_m_value
    implicit none
    real(4), parameter :: i4_to_r4(3) = [1, 2, 3]
    real(8), parameter :: i4_to_r8(3) = [1, 2, 3]
    complex(4), parameter :: i4_to_c4(3) = [1, 2, 3]
    complex(8), parameter :: i4_to_c8(3) = [1, 2, 3]
    complex(4), parameter :: r4_to_c4(3) = [1., 2., 3.]
    complex(8), parameter :: r4_to_c8(3) = [1., 2., 3.]
    integer(1), parameter :: i4_to_i1(3) = [1, 2, 3]
    complex(8), parameter :: c4_to_c8(3) = [(1.0, 0.0), (2.0, 0.0), (3.0, 0.0)]
end program cast_node_m_value
