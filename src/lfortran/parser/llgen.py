#!/usr/bin/env python3
"""
Generates a recursive descent parser in C++ from an LL grammar with
predicates and actions.

    llgen.py grammar.ll output.cpp

The grammar file consists of:

    %code {{            C++ copied to the output as it is: the beginning and
    ...                 the end of the file, and member functions written by
    }}                  hand (predicates and helpers).

    %first NAME: T...   The tokens that a hand-written function can start
                        with (NAME is any name), for use in a condition.

    %operators NAME OPERAND     A function NAME(min_prec) that parses binary
        left 8 TK_PLUS { ... }  operators by precedence climbing; operands are
        right 11 TK_POW { ... } parsed by OPERAND(). An action combines `x`
    %end                        and `y` into `x`; `op` is the operator token.

    rule NAME(PARAMS) -> TYPE [try]
        : ALTERNATIVE
        | ALTERNATIVE
        ;

Each rule becomes one member function. An alternative is a sequence of:

    TK_X  KW_X  "("     a token (a string is an alias, see ALIASES)
    name(args)          a call of a rule or of a hand-written function
    x=ITEM              declares `x`: the value of a call, or the index of a
                        token (its value is V[x], its location Lc[x])
    x<-ITEM             assigns to an `x` declared before
    ( A | B )           a choice; with `?` optional, with `*` repeated
    TK_X?               an optional token; `x=TK_X?` declares a bool
    { ... }             C++ code (an action); `@$` is the location from the
                        first token of the rule to the last token consumed
    &{ ... }            a predicate: a C++ condition that must hold

The generator decides which alternative to take from the tokens that each
one can start with (its FIRST set) and from the predicates at its beginning,
trying the alternatives in the order they are written. Large rules become a
`switch` on the current token. If no alternative applies, a `try` rule
returns a null value without consuming anything; any other rule gives up
(`unsup()`), as it does when an expected token is missing.
"""

import re
import sys

ALIASES = {
    "(": "TK_LPAREN", ")": "TK_RPAREN", ",": "TK_COMMA", "=": "TK_EQUAL",
    "::": "TK_DBL_COLON", ":": "TK_COLON", "=>": "TK_ARROW", "*": "TK_STAR",
    "/": "TK_SLASH", "[": "TK_LBRACKET", "]": "TK_RBRACKET",
    "{": "TK_LBRACE", "}": "TK_RBRACE", "%": "TK_PERCENT", "+": "TK_PLUS",
    "-": "TK_MINUS", "//": "TK_CONCAT", "?": "TK_QUESTION",
    "..": "TK_DBL_DOT", "/)": "TK_RBRACKET_OLD",
}

TOKEN_RE = re.compile(r"^(TK_|KW_)[A-Z0-9_]+$|^END_OF_FILE$")


def error(line, msg):
    sys.exit("llgen: line %d: %s" % (line, msg))


def is_name_token(t):
    return t == "TK_NAME" or (t.startswith("KW_") and t != "KW_IF_STMT")


# ----------------------------------------------------------------------------
# Grammar items

class Tok:
    def __init__(self, name, line):
        self.name, self.line = name, line
        self.cap = self.assign = None
        self.optional = False


class Call:
    def __init__(self, name, args, line):
        self.name, self.args, self.line = name, args, line
        self.cap = self.assign = None


class Code:
    def __init__(self, text, line):
        self.text, self.line = text, line


class Pred:
    def __init__(self, text, line):
        self.text, self.line = text, line


class Group:
    def __init__(self, alts, suffix, line):
        self.alts, self.suffix, self.line = alts, suffix, line


class Rule:
    def __init__(self, name, params, type, is_try, alts, line):
        self.name, self.params, self.type = name, params, type
        self.is_try, self.alts, self.line = is_try, alts, line


class First:
    """The tokens a sequence can start with, and the predicates before them"""
    def __init__(self):
        self.tokens = set()
        self.name = False       # any name
        self.any = False        # unknown: every token
        self.preds = []

    def merge(self, other):
        self.tokens |= other.tokens
        self.name |= other.name
        self.any |= other.any


# ----------------------------------------------------------------------------
# Reading the grammar

def skip_code(s, i, line):
    """`i` is after an opening brace; returns the index after its match"""
    depth = 1
    n = len(s)
    while i < n:
        c = s[i]
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return i + 1
        elif c == '"' or c == "'":
            i += 1
            while i < n and s[i] != c:
                if s[i] == "\\":
                    i += 1
                i += 1
        elif s.startswith("//", i):
            while i < n and s[i] != "\n":
                i += 1
            continue
        i += 1
    error(line, "unterminated { }")


def lex(s, line):
    """Tokenizes the body of a rule into (kind, value, line)"""
    out = []
    i = 0
    n = len(s)
    while i < n:
        c = s[i]
        if c == "\n":
            line += 1
            i += 1
        elif c.isspace():
            i += 1
        elif s.startswith("//", i):
            while i < n and s[i] != "\n":
                i += 1
        elif c == "{" or (c == "&" and s.startswith("&{", i)):
            start = i + (1 if c == "{" else 2)
            end = skip_code(s, start, line)
            out.append(("code" if c == "{" else "pred", s[start:end-1], line))
            line += s.count("\n", i, end)
            i = end
        elif c == '"':
            j = s.index('"', i + 1)
            lit = s[i+1:j]
            if lit not in ALIASES:
                error(line, 'unknown token "%s"' % lit)
            out.append(("tok", ALIASES[lit], line))
            i = j + 1
        elif c.isalpha() or c == "_":
            j = i
            while j < n and (s[j].isalnum() or s[j] == "_"):
                j += 1
            word = s[i:j]
            if TOKEN_RE.match(word):
                out.append(("tok", word, line))
                i = j
            elif j < n and s[j] == "(":
                depth = 0
                k = j
                while True:
                    if s[k] == "(":
                        depth += 1
                    elif s[k] == ")":
                        depth -= 1
                        if depth == 0:
                            break
                    k += 1
                out.append(("call", (word, s[j+1:k]), line))
                line += s.count("\n", i, k)
                i = k + 1
            else:
                out.append(("ident", word, line))
                i = j
        elif s.startswith("<-", i):
            out.append(("sym", "<-", line))
            i += 2
        elif c in "()?*|=":
            out.append(("sym", c, line))
            i += 1
        else:
            error(line, "unexpected character %r" % c)
    return out


class BodyParser:
    def __init__(self, toks):
        self.t = toks
        self.i = 0

    def peek(self, kind=None, value=None):
        if self.i >= len(self.t):
            return False
        k, v, _ = self.t[self.i]
        return (kind is None or k == kind) and (value is None or v == value)

    def line(self):
        if self.i < len(self.t):
            return self.t[self.i][2]
        return self.t[-1][2]

    def alts(self):
        alts = [self.seq()]
        while self.peek("sym", "|"):
            self.i += 1
            alts.append(self.seq())
        return alts

    def seq(self):
        items = []
        while self.i < len(self.t) and not self.peek("sym", "|") \
                and not self.peek("sym", ")"):
            items.append(self.item())
        return items

    def item(self):
        kind, value, line = self.t[self.i]
        cap = assign = None
        if kind == "ident":
            self.i += 1
            if self.peek("sym", "="):
                cap = value
            elif self.peek("sym", "<-"):
                assign = value
            else:
                error(line, "unexpected name `%s`" % value)
            self.i += 1
            kind, value, line = self.t[self.i]
        self.i += 1
        if kind == "tok":
            it = Tok(value, line)
            if self.peek("sym", "?"):
                self.i += 1
                it.optional = True
        elif kind == "call":
            it = Call(value[0], value[1], line)
        elif kind == "code":
            return Code(value, line)
        elif kind == "pred":
            return Pred(value, line)
        elif kind == "sym" and value == "(":
            alts = self.alts()
            if not self.peek("sym", ")"):
                error(self.line(), "expected )")
            self.i += 1
            suffix = ""
            if self.peek("sym", "?") or self.peek("sym", "*"):
                suffix = self.t[self.i][1]
                self.i += 1
            return Group(alts, suffix, line)
        else:
            error(line, "unexpected `%s`" % (value,))
        it.cap, it.assign = cap, assign
        return it


def read_grammar(filename):
    lines = open(filename, encoding="utf-8").read().split("\n")
    parts = []      # ("code", text) and ("rule", Rule) in order
    firsts = {}
    operators = None
    i = 0
    while i < len(lines):
        l = lines[i]
        s = l.strip()
        if s == "" or s.startswith("//"):
            i += 1
        elif s == "%code {{":
            j = i + 1
            while lines[j].strip() != "}}":
                j += 1
            parts.append(("code", "\n".join(lines[i+1:j]) + "\n"))
            i = j + 1
        elif s.startswith("%first "):
            name, toks = s[7:].split(":")
            f = First()
            for t in toks.split():
                if t == "NAME":
                    f.name = True
                else:
                    f.tokens.add(t)
            firsts[name.strip()] = f
            i += 1
        elif s.startswith("%operators "):
            _, name, operand = s.split()
            ops = []
            j = i + 1
            while lines[j].strip() != "%end":
                m = re.match(r"\s*(left|right)\s+(\d+)\s+(\w+)\s+\{(.*)\}\s*$",
                    lines[j])
                if not m:
                    error(j + 1, "bad operator line")
                ops.append((m.group(1), int(m.group(2)), m.group(3),
                    m.group(4).strip()))
                j += 1
            operators = (name, operand, ops)
            parts.append(("operators", operators))
            i = j + 1
        elif s.startswith("rule "):
            m = re.match(r"rule\s+(\w+)\s*\((.*)\)\s*->\s*(.+?)\s*(\btry)?$", s)
            if not m:
                error(i + 1, "bad rule header")
            j = i + 1
            while lines[j].strip() != ";":
                j += 1
                if j >= len(lines):
                    error(i + 1, "rule without the final `;` line")
            body = "\n".join(lines[i+1:j])
            body = body.lstrip()
            lead = "\n".join(lines[i+1:j])
            first_line = i + 2 + (len(lead) - len(lead.lstrip("\n")))
            if not body.startswith(":"):
                error(i + 2, "the alternatives of a rule start with `:`")
            toks = lex(body[1:], first_line)
            p = BodyParser(toks)
            alts = p.alts()
            if p.i != len(toks):
                error(p.line(), "unexpected `)`")
            parts.append(("rule", Rule(m.group(1), m.group(2), m.group(3),
                bool(m.group(4)), alts, i + 1)))
            i = j + 1
        else:
            error(i + 1, "unexpected line: " + s)
    return parts, firsts


# ----------------------------------------------------------------------------
# FIRST sets

class Generator:
    def __init__(self, parts, firsts):
        self.parts = parts
        self.rules = {r.name: r for k, r in parts if k == "rule"}
        self.firsts = dict(firsts)
        for k, v in parts:
            if k == "operators":
                # An expression can start with almost anything
                f = First()
                f.any = True
                self.firsts[v[0]] = f
        self.rule_first = {}
        changed = True
        for name in self.rules:
            self.rule_first[name] = First()
        while changed:
            changed = False
            for name, r in self.rules.items():
                f = First()
                for alt in r.alts:
                    f.merge(self.first_of(alt))
                old = self.rule_first[name]
                if (f.tokens, f.name, f.any) != (old.tokens, old.name, old.any):
                    self.rule_first[name] = f
                    changed = True
        self.out = []

    def first_of(self, items):
        """FIRST of a sequence, with the predicates that precede its first
        token"""
        f = First()
        for it in items:
            if isinstance(it, Code):
                continue
            if isinstance(it, Pred):
                f.preds.append(it.text.strip())
                continue
            if isinstance(it, Tok):
                f.tokens.add(it.name)
                if it.optional:
                    continue
                return f
            if isinstance(it, Call):
                if it.name in self.rule_first:
                    f.merge(self.rule_first[it.name])
                elif it.name in self.firsts:
                    f.merge(self.firsts[it.name])
                else:
                    f.any = True
                return f
            if isinstance(it, Group):
                empty = it.suffix != ""
                for alt in it.alts:
                    g = self.first_of(alt)
                    if g.preds:
                        # A predicate inside a group cannot be hoisted
                        f.any = True
                    f.merge(g)
                    if not g.tokens and not g.name and not g.any:
                        empty = True
                if not empty:
                    return f
        # The sequence can be empty: anything can follow
        f.any = True
        return f

    def condition(self, f):
        """The C++ condition for a FIRST set; None if it is always true"""
        parts = []
        if not f.any:
            toks = sorted(t for t in f.tokens
                if not (f.name and is_name_token(t)))
            alts = ["K[i] == %s" % t for t in toks]
            if f.name:
                alts.insert(0, "is_name(K[i])")
            if len(alts) == 1:
                parts.append(alts[0])
            else:
                parts.append("(" + " || ".join(alts) + ")")
        for p in f.preds:
            parts.append("(" + p + ")")
        if not parts:
            return None
        return " && ".join(parts)

    # ------------------------------------------------------------------
    # Code generation

    def emit(self, indent, text):
        self.out.append("    " * indent + text)

    def emit_code(self, indent, it):
        text = it.text
        if "@$" in text:
            self.emit(indent, "l = span(_s);")
            text = text.replace("@$", "l")
        lines = text.strip("\n").split("\n")
        # Remove the common indentation
        pad = min((len(x) - len(x.lstrip()) for x in lines if x.strip()),
            default=0)
        for x in lines:
            x = x[pad:] if x.strip() else ""
            self.emit(indent if x else 0, x.rstrip())

    def gen_seq(self, items, indent, known_first):
        """`known_first`: the caller has checked that the current token is
        the first token of the sequence"""
        first = True
        for it in items:
            if isinstance(it, Code):
                self.emit_code(indent, it)
                continue
            if isinstance(it, Pred):
                if not first:
                    self.emit(indent, "if (!(%s)) unsup(%d);"
                        % (it.text.strip(), it.line))
                continue
            if isinstance(it, Tok):
                if it.optional:
                    if it.cap:
                        self.emit(indent, "bool %s = accept(%s);"
                            % (it.cap, it.name))
                    elif it.assign:
                        self.emit(indent, "%s = accept(%s);"
                            % (it.assign, it.name))
                    else:
                        self.emit(indent, "accept(%s);" % it.name)
                else:
                    if it.cap:
                        self.emit(indent, "size_t %s = i;" % it.cap)
                    elif it.assign:
                        self.emit(indent, "%s = i;" % it.assign)
                    if first and known_first:
                        self.emit(indent, "i++;")
                    else:
                        self.emit(indent, "expect_(%s, %d);"
                            % (it.name, it.line))
            elif isinstance(it, Call):
                call = "%s(%s)" % (it.name, it.args)
                if it.cap:
                    self.emit(indent, "auto %s = %s;" % (it.cap, call))
                elif it.assign:
                    self.emit(indent, "%s = %s;" % (it.assign, call))
                else:
                    self.emit(indent, call + ";")
            elif isinstance(it, Group):
                self.gen_group(it, indent)
            first = False

    def single_token(self, items):
        """True if the sequence starts with one given (not optional) token"""
        for it in items:
            if isinstance(it, (Code, Pred)):
                continue
            return isinstance(it, Tok) and not it.optional
        return False

    def gen_group(self, g, indent):
        conds = []
        for alt in g.alts:
            conds.append(self.condition(self.first_of(alt)))
        if g.suffix == "*":
            if len(g.alts) == 1:
                if conds[0] is None:
                    error(g.line, "a repeated group needs a condition")
                self.emit(indent, "while (%s) {" % conds[0])
                self.gen_seq(g.alts[0], indent + 1,
                    self.single_token(g.alts[0]))
                self.emit(indent, "}")
                return
            self.emit(indent, "for (;;) {")
            self.gen_chain(g.alts, conds, indent + 1, "break;", g.line)
            self.emit(indent, "}")
            return
        if g.suffix == "?":
            self.gen_chain(g.alts, conds, indent, None, g.line)
            return
        self.gen_chain(g.alts, conds, indent, "unsup(%d);" % g.line, g.line)

    def gen_chain(self, alts, conds, indent, otherwise, line):
        """if / else if chain over the alternatives"""
        closed = False
        for n, (alt, cond) in enumerate(zip(alts, conds)):
            if cond is None:
                if n == 0:
                    self.emit(indent, "{")
                else:
                    self.emit(indent, "} else {")
                self.gen_seq(alt, indent + 1, False)
                closed = True
                break
            kw = "if" if n == 0 else "} else if"
            self.emit(indent, "%s (%s) {" % (kw, cond))
            self.gen_seq(alt, indent + 1, self.single_token(alt))
        if not closed and otherwise:
            self.emit(indent, "} else {")
            self.emit(indent + 1, otherwise)
        self.emit(indent, "}")

    def gen_rule(self, r):
        params = r.params.strip()
        self.emit(1, "%s %s(%s) {" % (r.type, r.name, params))
        self.emit(2, "size_t _s = i;")
        self.emit(2, "(void)_s;")
        self.emit(2, "Location l;")
        self.emit(2, "(void)l;")
        if r.is_try:
            none = "return {};"
        else:
            none = "unsup(%d);" % r.line
        firsts = [self.first_of(alt) for alt in r.alts]
        conds = [self.condition(f) for f in firsts]
        if len(r.alts) == 1 and not r.is_try:
            self.gen_seq(r.alts[0], 2, False)
        elif len(r.alts) <= 4:
            self.gen_chain(r.alts, conds, 2, none, r.line)
        else:
            self.gen_switch(r, firsts, conds, none)
        if r.type != "void":
            # Reached only if an alternative does not return
            self.emit(2, "unsup(%d);" % r.line)
        self.emit(1, "}")
        self.emit(0, "")

    def gen_switch(self, r, firsts, conds, none):
        """A switch on the current token that jumps to the first alternative
        (in the order of the grammar) which can start with it and whose
        predicates hold"""
        tokens = sorted(set(t for f in firsts for t in f.tokens))
        used = set()

        def jumps(tok):
            """The alternatives to try for `tok` (None: any other token)"""
            lines = []
            for n, f in enumerate(firsts):
                if tok is None:
                    if f.any:
                        test = ""
                    elif f.name:
                        test = "is_name(K[i])"
                    else:
                        continue
                elif f.any or tok in f.tokens \
                        or (f.name and is_name_token(tok)):
                    test = ""
                else:
                    continue
                c = " && ".join([test] * bool(test)
                    + ["(%s)" % p for p in f.preds])
                used.add(n)
                if c:
                    lines.append("if (%s) goto _alt%d;" % (c, n))
                else:
                    lines.append("goto _alt%d;" % n)
                    return lines, True
            return lines, False

        cases = {}
        for t in tokens:
            lines, final = jumps(t)
            cases.setdefault((tuple(lines), final), []).append(t)
        self.emit(2, "switch (K[i]) {")
        for (lines, final), toks in cases.items():
            for k in range(0, len(toks), 4):
                self.emit(3, " ".join("case %s:" % t for t in toks[k:k+4]))
            for x in lines:
                self.emit(4, x)
            if not final:
                self.emit(4, "break;")
        lines, final = jumps(None)
        self.emit(3, "default:")
        for x in lines:
            self.emit(4, x)
        if not final:
            self.emit(4, "break;")
        self.emit(2, "}")
        self.emit(2, none)
        for n, alt in enumerate(r.alts):
            if n not in used:
                error(r.line, "alternative %d of `%s` can never be taken"
                    % (n + 1, r.name))
            self.emit(2, "_alt%d: {" % n)
            f = firsts[n]
            known = self.single_token(alt) and not f.any and not f.name \
                and len(f.tokens) == 1
            self.gen_seq(alt, 3, known)
            if r.type == "void":
                self.emit(3, "return;")
            else:
                self.emit(3, "unsup(%d);" % r.line)
            self.emit(2, "}")

    def gen_operators(self, name, operand, ops):
        self.emit(1, "static int %s_precedence(int t) {" % name)
        self.emit(2, "switch (t) {")
        by_prec = {}
        for assoc, prec, tok, action in ops:
            by_prec.setdefault(prec, []).append(tok)
        for prec, toks in sorted(by_prec.items()):
            self.emit(3, " ".join("case %s:" % t for t in toks)
                + " return %d;" % prec)
        self.emit(3, "default: return 0;")
        self.emit(2, "}")
        self.emit(1, "}")
        self.emit(0, "")
        right = sorted(set(p for a, p, t, c in ops if a == "right"))
        self.emit(1, "ast_t* %s(int min_prec) {" % name)
        self.emit(2, "size_t _s = i;")
        self.emit(2, "ast_t *x = %s();" % operand)
        self.emit(2, "for (;;) {")
        self.emit(3, "int t = K[i];")
        self.emit(3, "int prec = %s_precedence(t);" % name)
        self.emit(3, "if (prec == 0 || prec < min_prec) return x;")
        self.emit(3, "size_t op = i;")
        self.emit(3, "(void)op;")
        self.emit(3, "i++;")
        if right:
            test = " || ".join("prec == %d" % p for p in right)
            self.emit(3, "ast_t *y = %s((%s) ? prec : prec + 1);"
                % (name, test))
        else:
            self.emit(3, "ast_t *y = %s(prec + 1);" % name)
        self.emit(3, "Location l = span(_s);")
        self.emit(3, "switch (t) {")
        for assoc, prec, tok, action in ops:
            self.emit(4, "case %s: %s break;" % (tok, action))
        self.emit(4, "default: break;")
        self.emit(3, "}")
        self.emit(2, "}")
        self.emit(1, "}")
        self.emit(0, "")

    def generate(self, source):
        self.emit(0, "// Generated by llgen.py from %s; do not edit." % source)
        self.emit(0, "")
        for kind, v in self.parts:
            if kind == "code":
                self.out.append(v.rstrip("\n"))
                self.emit(0, "")
            elif kind == "operators":
                self.gen_operators(*v)
            else:
                self.gen_rule(v)
        return "\n".join(self.out) + "\n"


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    parts, firsts = read_grammar(sys.argv[1])
    g = Generator(parts, firsts)
    text = g.generate(sys.argv[1].replace("\\", "/").split("/")[-1])
    with open(sys.argv[2], "w", encoding="utf-8") as f:
        f.write(text)


if __name__ == "__main__":
    main()
