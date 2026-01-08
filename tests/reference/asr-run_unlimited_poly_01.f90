([1m[35mTranslationUnit[39m[0m
    ([33mSymbolTable[39m
        1
        {
            unlimited_poly_test:
                ([1m[35mProgram[39m[0m
                    ([33mSymbolTable[39m
                        2
                        {
                            u:
                                ([1m[35mVariable[39m[0m
                                    2
                                    u
                                    []
                                    [1m[32mLocal[39m[0m
                                    ()
                                    ()
                                    [1m[32mDefault[39m[0m
                                    ([1m[35mAllocatable[39m[0m
                                        ([1m[35mStructType[39m[0m
                                            []
                                            []
                                            .false.
                                            .true.
                                        )
                                    )
                                    2 [33m~unlimited_polymorphic_type[39m
                                    [1m[32mSource[39m[0m
                                    [1m[32mPublic[39m[0m
                                    [1m[32mRequired[39m[0m
                                    .false.
                                    .false.
                                    .false.
                                    ()
                                    .false.
                                    .false.
                                ),
                            ~select_type_block_:
                                ([1m[35mBlock[39m[0m
                                    ([33mSymbolTable[39m
                                        4
                                        {
                                            
                                        })
                                    ~select_type_block_
                                    [([1m[35mAssignment[39m[0m
                                        ([1m[35mVar[39m[0m 2 [33mu[39m)
                                        ([1m[35mIntegerConstant[39m[0m [36m42[39m ([1m[35mInteger[39m[0m 4) [1m[32mDecimal[39m[0m)
                                        ()
                                        .false.
                                        .false.
                                    )
                                    ([1m[35mPrint[39m[0m
                                        ([1m[35mStringConstant[39m[0m
                                            "It is an integer!"
                                            ([1m[35mString[39m[0m 1 ([1m[35mIntegerConstant[39m[0m [36m17[39m ([1m[35mInteger[39m[0m 4) [1m[32mDecimal[39m[0m) [1m[32mExpressionLength[39m[0m [1m[32mDescriptorString[39m[0m)
                                        )
                                    )
                                    ([1m[35mPrint[39m[0m
                                        ([1m[35mStringFormat[39m[0m
                                            ()
                                            [([1m[35mStringConstant[39m[0m
                                                "Value is:"
                                                ([1m[35mString[39m[0m 1 ([1m[35mIntegerConstant[39m[0m [36m9[39m ([1m[35mInteger[39m[0m 4) [1m[32mDecimal[39m[0m) [1m[32mExpressionLength[39m[0m [1m[32mDescriptorString[39m[0m)
                                            )
                                            ([1m[35mVar[39m[0m 2 [33mu[39m)]
                                            [1m[32mFormatFortran[39m[0m
                                            ([1m[35mAllocatable[39m[0m
                                                ([1m[35mString[39m[0m 1 () [1m[32mDeferredLength[39m[0m [1m[32mDescriptorString[39m[0m)
                                            )
                                            ()
                                        )
                                    )
                                    ([1m[35mIf[39m[0m
                                        ()
                                        ([1m[35mIntegerCompare[39m[0m
                                            ([1m[35mVar[39m[0m 2 [33mu[39m)
                                            [1m[32mNotEq[39m[0m
                                            ([1m[35mIntegerConstant[39m[0m [36m42[39m ([1m[35mInteger[39m[0m 4) [1m[32mDecimal[39m[0m)
                                            ([1m[35mLogical[39m[0m 4)
                                            ()
                                        )
                                        [([1m[35mErrorStop[39m[0m
                                            ()
                                        )]
                                        []
                                    )]
                                ),
                            ~unlimited_polymorphic_type:
                                ([1m[35mStruct[39m[0m
                                    ([33mSymbolTable[39m
                                        3
                                        {
                                            
                                        })
                                    ~unlimited_polymorphic_type
                                    ([1m[35mStructType[39m[0m
                                        []
                                        []
                                        .false.
                                        .true.
                                    )
                                    []
                                    []
                                    []
                                    [1m[32mSource[39m[0m
                                    [1m[32mPublic[39m[0m
                                    .false.
                                    .true.
                                    []
                                    ()
                                    ()
                                )
                        })
                    unlimited_poly_test
                    []
                    [([1m[35mAllocate[39m[0m
                        [(([1m[35mVar[39m[0m 2 [33mu[39m)
                        []
                        ()
                        ()
                        ([1m[35mInteger[39m[0m 4))]
                        ()
                        ()
                        ()
                    )
                    ([1m[35mSelectType[39m[0m
                        ([1m[35mVar[39m[0m 2 [33mu[39m)
                        ()
                        [([1m[35mTypeStmtType[39m[0m
                            ([1m[35mInteger[39m[0m 4)
                            [([1m[35mBlockCall[39m[0m
                                -1
                                2 [33m~select_type_block_[39m
                            )]
                        )]
                        [([1m[35mPrint[39m[0m
                            ([1m[35mStringConstant[39m[0m
                                "Unknown type"
                                ([1m[35mString[39m[0m 1 ([1m[35mIntegerConstant[39m[0m [36m12[39m ([1m[35mInteger[39m[0m 4) [1m[32mDecimal[39m[0m) [1m[32mExpressionLength[39m[0m [1m[32mDescriptorString[39m[0m)
                            )
                        )
                        ([1m[35mErrorStop[39m[0m
                            ()
                        )]
                    )]
                )
        })
    []
)
