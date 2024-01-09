# Abstract Syntax Representation (ASR)

The aim of ASR is to represent all semantics in a non-redundant way, and that
has all the semantic information available locally, so that the backend can
do a single pass over ASR and have all the information at hand to generate
code.

ASR is always semantically valid Fortran code. It is as far from the original
Fortran language code as possible (i.e. everything is explicitly figured out,
all semantic information gathered and readily available locally from each ASR
node), while ensuring no semantic information was lost (no lowering was
done), so one can still generate Fortran code from ASR that will be logically
equivalent to the original code.

ASR can be used to do Fortran level transformations (such as optimizations).
