module traits_numeric_05_facade_m
    use traits_numeric_05_ops_m, only: PublicNumeric => RenamedNumeric, &
        PublicInteger => IntegerOnly, bumped => bump, integer_bumped => integer_bump
    implicit none
    private
    public :: PublicNumeric, PublicInteger, bumped, integer_bumped
end module traits_numeric_05_facade_m
