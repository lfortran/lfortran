! Names the instance entities of the owner module of traits_generic_type_04
! the way an earlier version of the compiler named them. LFortran accepts
! identifiers that start with an underscore, so a source can spell these
! names, but no entity of the module has them, and a USE that lists or
! renames symbols never imports an instance entity. Each module is rejected.
module traits_generic_type_04_reserved_type
    use traits_generic_type_04_owner, only: __averager__simplesum, __averager__simplesum_init
end module traits_generic_type_04_reserved_type

module traits_generic_type_04_reserved_copy
    use traits_generic_type_04_owner, only: make_avg, __averager__simplesum_average
end module traits_generic_type_04_reserved_copy

module traits_generic_type_04_reserved_rename
    use traits_generic_type_04_owner, only: averager_t => __averager__simplesum
end module traits_generic_type_04_reserved_rename

module traits_generic_type_04_reserved_rename_all
    use traits_generic_type_04_owner, averager_init => __averager__simplesum_init
end module traits_generic_type_04_reserved_rename_all

module traits_generic_type_04_reserved_conformance
    use traits_generic_type_04_owner, only: __implements___averager__simplesum_iaverager
end module traits_generic_type_04_reserved_conformance
