#!/usr/bin/env bash

# # Check for Essential Conflicts in Bison Grammar
#
# We parse Fortran using a GLR parser that has non-essential conflicts caused
# by the keyword/id ambiguity, but no essential conflicts (any other conflict).
#
# Non-essential conflict: We define a non-essential conflict as one that is
# caused by the keywords (`KW_*`) in the `id` rule conflicting with other
# rules. A non-essential conflict disappears when we remove those keywords from
# the `id` rule. These conflicts cannot actually cause an ambiguity in the
# Fortran grammar due to the way it is carefully constructed/defined: one can
# always determine from the context if a given name is a user-defined name or a
# keyword in some Fortran construct. However to make this determination one has
# to understand the context (the grammar is not context free).
#
# Essential conflict: We define an essential conflict as such conflict that
# does not disappear when when we remove the keywords from the `id` rule. The
# full Fortran grammar does not have any such conflicts. We thus enforce in our
# Bison implementation that we do not have any essential conflicts either.
#
# Motivation: the keyword/id ambiguity cannot be avoided in Fortran and must be
# either handled by a strong tokenizer that parses the source code using
# a recursive descent parser to fully disambiguate a keyword and an id and then
# the Bison parser can be LALR(1). Or by having a simple tokenizer that
# tokenizes names as `KW_*` tokens if they match any kwyword and an `TK_NAME`
# otherwise, and then in the Bison grammar we have a rule that `id` is either
# `TK_NAME` or any `KW_*`. This broad rule however causes up to 500 conflicts
# (total of shift/reduce and reduce/reduce) in the full grammar (we call these
# conflicts non-essential), and the grammar is thus not LALR(1) anymore, and we
# have to use the GLR parser. However, these non-essential conflicts cannot
# make the GLR parser to return "ambiguous syntax" error, since Fortran code
# can always be disambiguated from the full context, so the GLR parser always
# recovers. On the other hand, any essential conflict will cause true ambiguity
# in the grammar that is there even if keywords are disambiguated, and thus can
# make the GLR parser to fail on valid code. The other motivation is that
# essential conflicts can easily make the GLR grammar to have thousands of
# conflicts, making the GLR parser very slow. By ensuring that all our
# conflicts are non-essential, the GLR grammar executes quickly, because all
# concurrent parsers are short lived due to the nature of these non-essential
# conflicts and the Fortran grammar guarantees that all these parser will
# merge. An essential conflict could make a concurrent parser to run long, and
# it is not guaranteed by the Fortran grammar that they will all merge. The
# third motivation is to keep the door open to use LALR(1) grammar and a strong
# tokenizer in the future: this script is ensuring that we can always convert
# our GLR grammar to an LALR(1) grammar with no conflicts, that would require a
# strong tokenizer to disambiguate the `TK_NAME` and `KW_*` tokens. That way we
# are maintaining a full conflict-free LALR(1) Fortran grammar as a foundation,
# and then we have options how to implement the id/keyword ambiguity resolution
# on top, currently we do it using a GLR grammar.
#
# How we test: To test for essential conflicts, we remove all non-essential
# conflicts as follows: this test patches the parser.yy to remove all `KW_*`
# keywords from the `id` rule. Then we remove the `%glr-parser` line (which
# makes Bison treat the parser as LALR(1)) and change the `%expect` lines to
# expect 0 conflicts. We run Bison and ensure it gives no errors. This ensures
# there are no essential conflicts in the GLR grammar.

set -ex

patch -p1 < ci/parser.yy.patch
sed -i '/^%expect-rr/d' src/lfortran/parser/parser.yy
sed -i 's/^%expect .*/%expect 0/' src/lfortran/parser/parser.yy
sed -i '/^%glr-parser/d' src/lfortran/parser/parser.yy
(cd src/lfortran/parser && bison -Wall -d parser.yy)

echo "Patched grammar is LALR(1), no essential conflicts in GLR."
