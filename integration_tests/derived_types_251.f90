! Module `derived_types_251_a` with type `b_node` and module
! `derived_types_251_a_b` with type `node`: array components of these types
! are deep copied with each module's own copy function when the modules are
! compiled separately, although owner and type names joined with `_` agree.
program derived_types_251
    use derived_types_251_a, only: work1
    use derived_types_251_a_b, only: work2
    implicit none
    call work1()
    call work2()
end program
