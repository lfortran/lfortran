#!/usr/bin/env python3
"""
Generates a recursive descent parser in C++ from an LL grammar with
predicates.

    llgen.py grammar.ll rules.h tokens.h [--report]

The output is a list of member functions, one per rule, to be included in the
class of the parser, which is written by hand (ll_parser.cpp) and provides the
tokens, the predicates and the functions that the grammar declares as
`%extern`. The second output is the header with the enumeration of the
tokens.

Declarations
------------

    %token [<MEMBER>] NAME ["ALIAS"] [NUMBER]
    %keyword [<MEMBER>] NAME
        A token; MEMBER is the member of YYSTYPE with its value and ALIAS
        another name for it in the rules ("(" for TK_LPAREN). A %keyword is
        a token that is also a name.

    %extern NAME -> TYPE [first: T...] [token: T] [resume: FUNCTION]
        A function written by hand that the rules can call. `first` lists
        the tokens it can start with (NAME is any name, SEP any separator);
        `token` says that it takes exactly one token of that kind. Without
        them, it can start with anything. `resume` names a function for
        error recovery: if what follows the call in a rule does not parse,
        FUNCTION(value) is called, and if it returns true (it has reported
        the error and extended the value), what follows is parsed again.

    %predicate NAME
        A condition written by hand, to be used as `&NAME` or `!NAME`.

    %nested OPEN... : CLOSE... until STOP...
        The tokens that open and close a bracketed group, for NESTED below.
        A group that reaches one of the STOP tokens is not closed.

    %lookahead NAME
        : PATTERN
        ;
        A condition on the tokens that follow, to be used as `&NAME` or
        `!NAME`, written as a regular expression over tokens:

            T           a token, or one of the classes NAME, SEP, `_` (any)
            ~[ T | T ]  any token that is not one of these
            NESTED      a bracketed group with all that is in it
            A B         A and then B
            A | B       A or B
            ( A )       grouping; ( A )* any number of times, ( A )? optional

        It is matched from left to right without going back: the next token
        alone decides whether a repetition goes on and which alternative is
        taken, so the cost is one pass over the tokens it matches.

    %operators NAME OPERAND
        left 8 "+" => ADD(x, y, @$)
        right 11 "**" => POW(x, y, @$)
        prefix 11 "-" => UNARY_MINUS(x, @$)
    %end
        A function NAME(min_prec = 1) that parses expressions by precedence
        climbing; the operands are parsed by OPERAND. An action combines the
        operands `x` and `y`; `$op` is the value of the operator token. The
        operand `x` of a prefix operator is NAME(its precedence): all that
        binds at least that tightly.

Rules
-----

    rule NAME -> TYPE
        : ALTERNATIVE
        | ALTERNATIVE
        ;

    rule NAME?(PARAMETERS) -> TYPE      `?`: the value is null if the rule
                                        does not start at the current token

An alternative is a sequence of:

    KW_X  TK_X  "("     a token
    name  name(args)    a call of a rule or of an %extern function; the
                        argument `@` passes the start of the calling rule,
                        to a parameter `size_t s` of the called rule, which
                        then continues the construct of the caller
    x=ITEM              keeps the value of a call in `x`, or the token:
                        `$x` is then its value and `@x` its location
    ITEM?               optional: `x` is null (or false, for a token) if it
                        is absent
    ITEM*               repeated, a rule or function with `?` semantics: `x`
                        is the list of the values
    ITEM % ","          a list of one or more ITEMs separated by a token
    ( A | B )           a choice; with `?` optional, with `*` repeated
    &[ T1 T2 | T3 ]     a predicate on the next tokens: T1 then T2, or T3;
                        `_` is any token, NAME any name, SEP any separator
    ![ ... ]            the negation
    &name  !name        a %predicate, or its negation
    ^                   marks the end of the location `@^`
    => EXPRESSION       the value of the rule: a C++ expression, usually a
                        call of a macro of semantics.h. `@$` is the location
                        from the first token of the rule to the last token
                        consumed, `@^` the same up to the mark `^`.

An alternative that ends with a call (and has no `=>`) has the value of
that call.

Choosing an alternative
-----------------------

The alternatives are tried in the order they are written. The generator
computes the sequences of tokens that each alternative can start with, as
far as needed (at most MAX_LOOKAHEAD tokens) to tell it apart from the
alternatives after it, and tests those tokens, together with the predicates
at the beginning of the alternative. An alternative that can never be taken
is an error. With `--report`, the generator lists the alternatives that start
with a keyword which a later alternative would take as a name, and that have
no predicate: there the order of the alternatives decides.
"""

import re
import sys

MAX_LOOKAHEAD = 5
MAX_SEQUENCES = 64    # of tokens, in the test of one alternative
END = "..."         # in a token sequence: anything can follow
SEP_TOKENS = {"TK_NEWLINE", "TK_COMMENT", "TK_EOLCOMMENT", "TK_SEMICOLON"}


def error(line, msg):
    sys.exit("llgen: line %d: %s" % (line, msg))


# ----------------------------------------------------------------------------
# Tokens

class Tokens:
    def __init__(self, filename):
        self.order = []     # (token, value or None), as declared
        self.type = {}      # token -> the member of YYSTYPE with its value
        self.alias = {}     # "(" -> TK_LPAREN
        self.names = {"TK_NAME"}    # the tokens that are names
        for l in open(filename, encoding="utf-8"):
            m = re.match(r'%(token|keyword)\s+(?:<(\w+)>\s+)?(\w+)'
                r'(?:\s+"([^"]+)")?(?:\s+(\d+))?\s*$', l)
            if m:
                self.order.append((m.group(3), m.group(5)))
                self.type[m.group(3)] = m.group(2)
                if m.group(4):
                    self.alias[m.group(4)] = m.group(3)
                if m.group(1) == "keyword":
                    self.names.add(m.group(3))
            elif l.startswith(("%token", "%keyword")):
                sys.exit("%s: bad token declaration: %s" % (filename, l))

    # The header with `enum yytokentype`. The numbers start at 258, as they
    # did when Bison generated the header.
    def header(self, source):
        out = ["// Generated by llgen.py from %s, do not edit" % source, "",
            "#ifndef LFORTRAN_PARSER_TOKENS_H",
            "#define LFORTRAN_PARSER_TOKENS_H", "",
            "enum yytokentype {"]
        n = 258
        for name, value in self.order:
            if value is None:
                value = n
                n += 1
            out.append("    %s = %s," % (name, value))
        out.append("    // One more than the last token")
        out.append("    TOKEN_END = %d," % n)
        out += ["};", "", "#endif", ""]
        return "\n".join(out)

    def is_name(self, t):
        return t in self.names


# ----------------------------------------------------------------------------
# Grammar items

class Tok:
    def __init__(self, name, line):
        self.name, self.line = name, line
        self.cap = None
        self.optional = False


class Call:
    def __init__(self, name, args, line):
        self.name, self.args, self.line = name, args, line
        self.cap = None
        self.suffix = ""        # "", "?", "*" or "%"
        self.sep = None


class Group:
    def __init__(self, alts, suffix, line):
        self.alts, self.suffix, self.line = alts, suffix, line


class Pred:
    def __init__(self, neg, name, pattern, line):
        self.neg, self.name, self.pattern, self.line = neg, name, pattern, line


class Mark:
    def __init__(self, line):
        self.line = line


class Result:
    def __init__(self, expr, line):
        self.expr, self.line = expr, line


class Rule:
    def __init__(self, name, params, type, nullable, alts, line):
        self.name, self.params, self.type = name, params, type
        self.nullable, self.alts, self.line = nullable, alts, line


class Lookahead:
    def __init__(self, name, alts, line):
        self.name, self.alts, self.line = name, alts, line


class Extern:
    def __init__(self, type, first, single, resume=None):
        self.type, self.first, self.single = type, first, single
        self.resume = resume


# ----------------------------------------------------------------------------
# Reading the grammar

class BodyParser:
    def __init__(self, text, line, tokens):
        self.s, self.line, self.tokens = text, line, tokens
        self.i = 0

    def skip(self):
        s = self.s
        while self.i < len(s):
            c = s[self.i]
            if c == "\n":
                self.line += 1
                self.i += 1
            elif c.isspace():
                self.i += 1
            elif s.startswith("//", self.i):
                while self.i < len(s) and s[self.i] != "\n":
                    self.i += 1
            else:
                break

    def peek(self, text):
        self.skip()
        return self.s.startswith(text, self.i)

    def take(self, text):
        if self.peek(text):
            self.i += len(text)
            return True
        return False

    def token(self):
        """A token name or an alias in quotes, at the current position"""
        self.skip()
        s = self.s
        if s[self.i] == '"':
            j = s.index('"', self.i + 1)
            lit = s[self.i+1:j]
            if lit not in self.tokens.alias:
                error(self.line, 'unknown token "%s"' % lit)
            self.i = j + 1
            return self.tokens.alias[lit]
        m = re.compile(r"\w+").match(s, self.i)
        if not m:
            error(self.line, "expected a token")
        self.i = m.end()
        return m.group(0)

    def look_alts(self):
        alts = [self.look_seq()]
        while self.take("|"):
            alts.append(self.look_seq())
        return alts

    def look_seq(self):
        """A list of (kind, value, suffix), where kind is one of: tok, not,
        nested, group"""
        items = []
        while True:
            self.skip()
            if self.i >= len(self.s) or self.s[self.i] in "|)":
                return items
            if self.take("("):
                item = ["group", self.look_alts(), ""]
                if not self.take(")"):
                    error(self.line, "expected )")
            elif self.take("~["):
                toks = [self.token()]
                while self.take("|"):
                    toks.append(self.token())
                if not self.take("]"):
                    error(self.line, "expected ]")
                item = ["not", toks, ""]
            else:
                t = self.token()
                if t == "NESTED":
                    item = ["nested", None, ""]
                else:
                    if t not in ("_", "NAME", "SEP") \
                            and t not in self.tokens.type:
                        error(self.line, "unknown token %s" % t)
                    item = ["tok", t, ""]
            if self.i < len(self.s) and self.s[self.i] in "*?":
                item[2] = self.s[self.i]
                self.i += 1
            items.append(tuple(item))

    def alts(self):
        alts = [self.seq()]
        while self.take("|"):
            alts.append(self.seq())
        return alts

    def seq(self):
        items = []
        while True:
            self.skip()
            if self.i >= len(self.s) or self.s[self.i] in "|)":
                return items
            items.append(self.item())
            if isinstance(items[-1], Result):
                self.skip()
                if self.i < len(self.s) and self.s[self.i] not in "|)":
                    error(self.line, "nothing can follow `=> ...`")
                return items

    def balanced(self, open, close):
        """The text up to the matching `close`; the position is after the
        `open`"""
        s = self.s
        depth = 1
        j = self.i
        while depth > 0:
            if j >= len(s):
                error(self.line, "unbalanced " + open)
            if s[j] == open:
                depth += 1
            elif s[j] == close:
                depth -= 1
            elif s[j] == '"':
                j = s.index('"', j + 1)
            j += 1
        text = s[self.i:j-1]
        self.line += text.count("\n")
        self.i = j
        return text

    def pattern(self):
        text = self.balanced("[", "]")
        p = BodyParser(text, self.line, self.tokens)
        alts = []
        cur = []
        while True:
            p.skip()
            if p.i >= len(text):
                break
            if p.take("|"):
                alts.append(cur)
                cur = []
            else:
                t = p.token()
                if t not in ("_", "NAME", "SEP") and t not in self.tokens.type:
                    error(self.line, "unknown token %s" % t)
                cur.append(t)
        alts.append(cur)
        return alts

    def result(self):
        """The expression after `=>`: to the end of the line on which its
        parentheses are balanced"""
        s = self.s
        self.skip()
        j = self.i
        depth = 0
        while j < len(s):
            c = s[j]
            if c in "([{":
                depth += 1
            elif c in ")]}":
                if depth == 0:
                    break
                depth -= 1
            elif c == '"':
                j = s.index('"', j + 1)
            elif c == "\n" and depth == 0:
                break
            elif c == "|" and depth == 0 and s[j-1] != "|" \
                    and s[j+1:j+2] != "|":
                break
            j += 1
        text = s[self.i:j].strip()
        self.line += s.count("\n", self.i, j)
        self.i = j
        return " ".join(text.split())

    def item(self):
        self.skip()
        s = self.s
        line = self.line
        if self.take("=>"):
            return Result(self.result(), line)
        if self.take("^"):
            return Mark(line)
        if s.startswith("&[", self.i) or s.startswith("![", self.i):
            neg = s[self.i] == "!"
            self.i += 2
            return Pred(neg, None, self.pattern(), line)
        if s[self.i] in "&!":
            neg = s[self.i] == "!"
            self.i += 1
            m = re.compile(r"\w+").match(s, self.i)
            self.i = m.end()
            return Pred(neg, m.group(0), None, line)
        if self.take("("):
            alts = self.alts()
            if not self.take(")"):
                error(self.line, "expected )")
            suffix = ""
            if s.startswith("?", self.i) or s.startswith("*", self.i):
                suffix = s[self.i]
                self.i += 1
            return Group(alts, suffix, line)
        cap = None
        m = re.compile(r"(\w+)=(?![>=])").match(s, self.i)
        if m:
            cap = m.group(1)
            self.i = m.end()
        if s[self.i] == '"' or re.compile(r"(TK_|KW_)\w+|END_OF_FILE\b").match(
                s, self.i):
            t = self.token()
            if t not in self.tokens.type:
                error(line, "unknown token %s" % t)
            it = Tok(t, line)
            it.cap = cap
            if s.startswith("?", self.i):
                self.i += 1
                it.optional = True
            return it
        m = re.compile(r"\w+").match(s, self.i)
        if not m:
            error(line, "unexpected `%s`" % s[self.i:self.i+10])
        self.i = m.end()
        args = ""
        if s.startswith("(", self.i):
            self.i += 1
            args = self.balanced("(", ")")
        it = Call(m.group(0), args, line)
        it.cap = cap
        if s.startswith("?", self.i) or s.startswith("*", self.i):
            it.suffix = s[self.i]
            self.i += 1
        elif self.peek("%"):
            self.i += 1
            it.suffix = "%"
            it.sep = self.token()
        return it


def read_grammar(filename, tokens):
    lines = open(filename, encoding="utf-8").read().split("\n")
    rules = []
    externs = {}
    predicates = set()
    lookaheads = []
    nested = None
    operators = []
    i = 0
    while i < len(lines):
        s = lines[i].strip()
        if s == "" or s.startswith("//"):
            i += 1
        elif s.startswith("%extern "):
            resume = None
            m = re.match(r"(.*?)\s+resume:\s*(\w+)$", s)
            if m:
                s, resume = m.group(1), m.group(2)
            m = re.match(r"%extern\s+(\w+)\s*->\s*(.+?)"
                r"(?:\s+(first|token):\s*(.*))?$", s)
            if not m:
                error(i + 1, "bad %extern")
            first = None
            if m.group(4) is not None:
                p = BodyParser(m.group(4), i + 1, tokens)
                first = []
                while True:
                    p.skip()
                    if p.i >= len(p.s):
                        break
                    first.append(p.token())
            externs[m.group(1)] = Extern(m.group(2).strip(), first,
                m.group(3) == "token", resume)
            i += 1
        elif s.startswith(("%token ", "%keyword ")):
            i += 1
        elif s.startswith("%predicate "):
            predicates.add(s.split()[1])
            i += 1
        elif s.startswith("%nested "):
            m = re.match(r"%nested\s+(.+?)\s*:\s*(.+?)\s+until\s+(.+)$", s)
            if not m:
                error(i + 1, "bad %nested")
            nested = []
            for part in m.groups():
                p = BodyParser(part, i + 1, tokens)
                toks = []
                while True:
                    p.skip()
                    if p.i >= len(p.s):
                        break
                    toks.append(p.token())
                nested.append(toks)
            i += 1
        elif s.startswith("%lookahead "):
            name = s.split()[1]
            j = i + 1
            while lines[j].strip() != ";":
                j += 1
                if j >= len(lines):
                    error(i + 1, "lookahead without the final `;` line")
            body = "\n".join(lines[i+1:j])
            if not body.lstrip().startswith(":"):
                error(i + 2, "a lookahead starts with `:`")
            p = BodyParser(body.replace(":", " ", 1), i + 2, tokens)
            alts = p.look_alts()
            p.skip()
            if p.i < len(p.s):
                error(p.line, "unexpected `%s`" % p.s[p.i])
            lookaheads.append(Lookahead(name, alts, i + 1))
            predicates.add(name)
            i = j + 1
        elif s.startswith("%operators "):
            _, name, operand = s.split()
            ops = []
            j = i + 1
            while lines[j].strip() != "%end":
                m = re.match(r'\s*(left|right|prefix)\s+(\d+)\s+(\S+)\s+=>\s*(.*)$',
                    lines[j])
                if not m:
                    error(j + 1, "bad operator line")
                t = BodyParser(m.group(3), j + 1, tokens).token()
                ops.append((m.group(1), int(m.group(2)), t, m.group(4).strip()))
                j += 1
            operators.append((name, operand, ops))
            i = j + 1
        elif s.startswith("rule "):
            m = re.match(r"rule\s+(\w+)(\?)?\s*(?:\((.*)\))?\s*->\s*(.+)$", s)
            if not m:
                error(i + 1, "bad rule header")
            j = i + 1
            while lines[j].strip() != ";":
                j += 1
                if j >= len(lines):
                    error(i + 1, "rule without the final `;` line")
            body = "\n".join(lines[i+1:j])
            if not body.lstrip().startswith(":"):
                error(i + 2, "the alternatives of a rule start with `:`")
            k = body.index(":")
            p = BodyParser(body[k+1:], i + 2 + body.count("\n", 0, k), tokens)
            alts = p.alts()
            p.skip()
            if p.i != len(p.s):
                error(p.line, "unexpected `%s`" % p.s[p.i])
            rules.append(Rule(m.group(1), m.group(3) or "", m.group(4).strip(),
                bool(m.group(2)), alts, i + 1))
            i = j + 1
        else:
            error(i + 1, "unexpected line: " + s)
    return rules, externs, predicates, operators, lookaheads, nested


# ----------------------------------------------------------------------------
# The generator

class Generator:
    def __init__(self, rules, externs, predicates, operators, tokens,
            lookaheads=(), nested=None):
        self.lookaheads, self.nested = lookaheads, nested
        self.rule_list = rules
        self.rules = {r.name: r for r in rules}
        self.externs = externs
        self.predicates = predicates
        self.operators = operators
        self.tokens = tokens
        for name, operand, ops in operators:
            self.externs[name] = Extern("ast_t*", None, False)
        self.memo = {}
        self.busy = set()
        self.out = []
        self.report = []
        self.check()

    def check(self):
        for r in self.rule_list:
            for alt in r.alts:
                self.check_seq(alt, r)

    def check_seq(self, items, r):
        for it in items:
            if isinstance(it, Call):
                if it.name not in self.rules and it.name not in self.externs:
                    error(it.line, "unknown rule `%s`" % it.name)
            elif isinstance(it, Pred) and it.name:
                if it.name not in self.predicates \
                        and it.name not in self.rules:
                    error(it.line, "unknown predicate `%s`" % it.name)
            elif isinstance(it, Group):
                for alt in it.alts:
                    self.check_seq(alt, r)

    def call_type(self, name):
        if name in self.rules:
            return self.rules[name].type
        return self.externs[name].type

    # ------------------------------------------------------------------
    # Token sequences that an item, a sequence or a rule can start with.
    # A sequence shorter than `k` that does not end with END was cut by the
    # end of the rule: what follows is not known either.

    def first_rule(self, name, k):
        key = (name, k)
        if key in self.memo:
            return self.memo[key]
        if key in self.busy:
            return {(END,)}
        self.busy.add(key)
        if name in self.rules:
            f = set()
            for alt in self.rules[name].alts:
                f |= self.first_seq(alt, k)
        else:
            ext = self.externs[name]
            if ext.first is None:
                f = {(END,)}
            elif ext.single:
                f = {(t,) for t in ext.first}
            else:
                f = {(t, END)[:k] for t in ext.first}
        self.busy.discard(key)
        self.memo[key] = f
        return f

    def first_item(self, it, k):
        """The set of sequences; an empty sequence means that the item can
        be absent"""
        if isinstance(it, Tok):
            f = {(it.name,)}
            if it.optional:
                f.add(())
            return f
        if isinstance(it, Call):
            f = set(self.first_rule(it.name, k))
            if it.suffix == "%":
                f = {t if (len(t) >= k or t[-1:] == (END,)) else t + (END,)
                    for t in f}
            elif it.suffix:
                f.add(())
            return f
        if isinstance(it, Group):
            f = set()
            for alt in it.alts:
                f |= self.first_seq(alt, k)
            if it.suffix:
                f.add(())
            return f
        if isinstance(it, Pred) and it.pattern and not it.neg:
            # What follows has to match the pattern
            return {tuple(alt + [END])[:k] for alt in it.pattern}
        return {()}

    def first_seq(self, items, k):
        partial = {()}
        done = set()
        for it in items:
            if isinstance(it, Result):
                break
            if isinstance(it, Mark) or (isinstance(it, Pred)
                    and not (it.pattern and not it.neg)):
                continue
            f = self.first_item(it, k)
            new = set()
            for t in partial:
                for u in f:
                    if u == ():
                        new.add(t)
                        continue
                    v = (t + u)[:k]
                    if len(v) >= k or v[-1] == END:
                        done.add(v)
                    else:
                        new.add(v)
            partial = new
            if isinstance(it, Pred) or not partial:
                # A pattern says what follows; nothing more to add
                done |= partial
                partial = set()
                break
        return done | partial

    def compatible(self, a, b):
        """Can a token be both `a` and `b`?"""
        if a == b or a in ("_", END) or b in ("_", END):
            return True
        for x, y in ((a, b), (b, a)):
            if x == "NAME" and self.tokens.is_name(y):
                return True
            if x == "SEP" and y in SEP_TOKENS:
                return True
        return False

    def overlap(self, s1, s2):
        for a in s1:
            for b in s2:
                ok = True
                for x, y in zip(a, b):
                    if x == END or y == END:
                        break
                    if not self.compatible(x, y):
                        ok = False
                        break
                if ok:
                    return True
        return False

    def keyword_as_name(self, s1, s2):
        """Do two sets of sequences overlap because a keyword in one of them
        is taken as a name in the other?"""
        for a in s1:
            for b in s2:
                ok = True
                found = False
                for x, y in zip(a, b):
                    if x == END or y == END:
                        break
                    if not self.compatible(x, y):
                        ok = False
                        break
                    if x != y and "NAME" in (x, y) and "_" not in (x, y):
                        found = True
                if ok and found:
                    return True
        return False

    def literal_prefix(self, items):
        n = 0
        for it in items:
            if isinstance(it, (Mark, Pred)):
                continue
            if isinstance(it, Tok) and not it.optional:
                n += 1
            else:
                break
        return n

    def pattern_depth(self, items):
        """How far the token patterns at the beginning of an alternative
        look"""
        pos = 0
        depth = 0
        for it in items:
            if isinstance(it, Tok) and not it.optional:
                pos += 1
            elif isinstance(it, Pred) and it.pattern and not it.neg:
                depth = max(depth, pos + max(len(a) for a in it.pattern))
                break
            elif isinstance(it, (Mark, Pred)):
                continue
            else:
                break
        return depth

    def leading_preds(self, items):
        preds = []
        for it in items:
            if isinstance(it, Pred):
                if not (it.pattern and not it.neg):
                    preds.append(it)
            elif not isinstance(it, Mark):
                break
        return preds

    def decide(self, alts, where, line):
        """For each alternative: the token sequences to test and its leading
        predicates"""
        res = []
        for n, alt in enumerate(alts):
            preds = self.leading_preds(alt)
            later = alts[n+1:]
            depth = None
            for k in range(1, MAX_LOOKAHEAD + 1):
                f = self.first_seq(alt, k)
                if not any(self.overlap(f, self.first_seq(a, k)) for a in later):
                    depth = k
                    break
            if depth is None:
                # Not told apart from a later alternative: test all the
                # tokens that this alternative is known to start with
                best = self.first_seq(alt, 1)
                depth = 1
                for k in range(2, MAX_LOOKAHEAD + 1):
                    f = self.first_seq(alt, k)
                    if len(f) > MAX_SEQUENCES:
                        break
                    if f != best:
                        best = f
                        depth = k
                if not preds and any(self.keyword_as_name(best,
                        self.first_seq(a, depth)) for a in later):
                    self.report.append("%s (line %d): in alternative %d a "
                        "keyword wins over a name of a later alternative"
                        % (where, line, n + 1))
            depth = max(depth, min(2, self.literal_prefix(alt)),
                min(MAX_LOOKAHEAD, self.pattern_depth(alt)))
            seqs = self.first_seq(alt, depth)
            if not preds:
                for m, (prev, prev_preds) in enumerate(res):
                    if not prev_preds and prev == seqs:
                        error(line, "%s: alternatives %d and %d start with "
                            "the same tokens, so the second one can never "
                            "be taken" % (where, m + 1, n + 1))
            res.append((seqs, preds))
        return res

    # ------------------------------------------------------------------
    # Conditions

    def trie(self, seqs):
        root = {}
        for t in seqs:
            cur = root
            for e in t:
                if e == END:
                    break
                cur = cur.setdefault(e, {})
            # Anything can follow the tokens of the sequence
            cur[END] = {}
        return root

    # ------------------------------------------------------------------
    # Lookaheads

    def look_test(self, e):
        return self.test(e, 0).replace("K[i]", "K[j]")

    def look_first(self, item, line):
        """The condition on K[j] with which the item starts"""
        kind, value, _ = item
        if kind == "tok":
            return "K[j] != END_OF_FILE" if value == "_" \
                else self.look_test(value)
        if kind == "not":
            return "!(%s)" % " || ".join(
                [self.look_test(t) for t in value] + ["K[j] == END_OF_FILE"])
        if kind == "nested":
            if self.nested is None:
                error(line, "NESTED needs a %nested declaration")
            return self.any_of([self.look_test(t) for t in self.nested[0]])
        return self.any_of([self.look_first_seq(alt, line) for alt in value])

    def any_of(self, conds):
        # No parentheses around a single comparison: Clang warns about them
        return conds[0] if len(conds) == 1 else "(%s)" % " || ".join(conds)

    def look_first_seq(self, seq, line):
        if not seq or seq[0][2]:
            error(line, "a repetition, an option or an alternative of a "
                "lookahead has to start with a token that is always there")
        return self.look_first(seq[0], line)

    def gen_look_seq(self, seq, ind, line):
        for item in seq:
            kind, value, suffix = item
            if suffix:
                self.emit(ind, "%s (%s) {" % ("while" if suffix == "*"
                    else "if", self.look_first(item, line)))
                self.gen_look_item(item, ind + 1, line, True)
                self.emit(ind, "}")
            else:
                self.gen_look_item(item, ind, line, False)

    def gen_look_item(self, item, ind, line, checked):
        kind, value, _ = item
        if kind in ("tok", "not"):
            if not checked:
                self.emit(ind, "if (!(%s)) return false;"
                    % self.look_first(item, line))
            self.emit(ind, "j++;")
        elif kind == "nested":
            if not checked:
                self.emit(ind, "if (!(%s)) return false;"
                    % self.look_first(item, line))
            opens, closes, stops = self.nested
            self.emit(ind, "for (int depth = 0;;) {")
            self.emit(ind + 1, "int t = K[j];")
            self.emit(ind + 1, "if (%s) {" % " || ".join(
                "t == %s" % t for t in opens))
            self.emit(ind + 2, "depth++;")
            self.emit(ind + 1, "} else if (%s) {" % " || ".join(
                "t == %s" % t for t in closes))
            self.emit(ind + 2, "depth--;")
            self.emit(ind + 1, "} else if (%s) {" % " || ".join(
                "t == %s" % t for t in stops))
            self.emit(ind + 2, "return false;")
            self.emit(ind + 1, "}")
            self.emit(ind + 1, "j++;")
            self.emit(ind + 1, "if (depth == 0) break;")
            self.emit(ind, "}")
        elif len(value) == 1:
            self.gen_look_seq(value[0], ind, line)
        else:
            for n, alt in enumerate(value):
                self.emit(ind, "%sif (%s) {" % ("} else " if n else "",
                    self.look_first_seq(alt, line)))
                self.emit(ind + 1, "j++;") if alt[0][0] in ("tok", "not") \
                    else self.gen_look_item(alt[0], ind + 1, line, True)
                self.gen_look_seq(alt[1:], ind + 1, line)
            self.emit(ind, "} else {")
            self.emit(ind + 1, "return false;")
            self.emit(ind, "}")

    def gen_lookahead(self, la):
        self.emit(1, "bool %s() const {" % la.name)
        self.emit(2, "size_t j = i;")
        self.gen_look_item(("group", la.alts, ""), 2, la.line, False)
        self.emit(2, "(void)j;")
        self.emit(2, "return true;")
        self.emit(1, "}")
        self.emit(0, "")

    def test(self, e, pos):
        k = "K[i]" if pos == 0 else "K[i+%d]" % pos
        if e == "NAME":
            return "is_name(%s)" % k
        if e == "SEP":
            return "is_sep(%s)" % k
        return "%s == %s" % (k, e)

    def trie_cond(self, node, pos):
        """The C++ condition for a trie of token sequences; None if it is
        always true"""
        if END in node or not node:
            return None
        groups = {}
        for e, sub in node.items():
            c = self.trie_cond(sub, pos + 1)
            groups.setdefault(c, []).append(e)
        parts = []
        for c, elems in groups.items():
            if "_" in elems:
                if c is None:
                    return None
                parts.append(c)
                continue
            elems = [e for e in elems if not ("NAME" in elems
                and e != "NAME" and self.tokens.is_name(e))]
            tests = [self.test(e, pos) for e in sorted(elems)]
            t = tests[0] if len(tests) == 1 else "(" + " || ".join(tests) + ")"
            parts.append(t if c is None else "(%s && %s)" % (t, c))
        if len(parts) == 1:
            return parts[0]
        return "(" + " || ".join(parts) + ")"

    def pattern_cond(self, pattern):
        alts = []
        for alt in pattern:
            tests = [self.test(e, n) for n, e in enumerate(alt) if e != "_"]
            alts.append(" && ".join(tests) if tests else "true")
        if len(alts) == 1:
            return alts[0]
        return " || ".join("(%s)" % a for a in alts)

    def pred_cond(self, p):
        if p.name in self.rules:
            # The tokens that the rule can start with
            c = self.trie_cond(self.trie(self.first_rule(p.name, 2)), 0)
            if c is None:
                error(p.line, "`%s` can start with any token" % p.name)
            return "!(%s)" % c if p.neg else "(%s)" % c
        if p.name:
            c = "%s()" % p.name
            return "!" + c if p.neg else c
        c = self.pattern_cond(p.pattern)
        if p.neg:
            return "!(%s)" % c
        # No parentheses around a single comparison: Clang warns about them
        return "(%s)" % c if ("&&" in c or "||" in c) else c

    def condition(self, seqs, preds):
        parts = []
        c = self.trie_cond(self.trie(seqs), 0)
        if c is not None:
            parts.append(c)
        parts += [self.pred_cond(p) for p in preds]
        if not parts:
            return None
        return " && ".join(parts)

    # ------------------------------------------------------------------
    # Captures

    def list_type(self, t):
        if t == "ast_t*":
            return "Vec<ast_t*>", False
        if t.endswith("*"):
            return "Vec<%s>" % t[:-1].strip(), True
        return "Vec<%s>" % t, False

    def captures(self, items, caps):
        for it in items:
            if isinstance(it, Tok) and it.cap:
                self.add_capture(caps, it.cap,
                    "bool" if it.optional else "size_t", it)
            elif isinstance(it, Call) and it.cap:
                t = self.call_type(it.name)
                if it.suffix in ("*", "%"):
                    t = self.list_type(t)[0]
                self.add_capture(caps, it.cap, t, it)
            elif isinstance(it, Group):
                for alt in it.alts:
                    self.captures(alt, caps)

    def add_capture(self, caps, name, type, it):
        if name in caps and caps[name][0] != type:
            error(it.line, "`%s` has two types: %s and %s"
                % (name, caps[name][0], type))
        if name in caps and isinstance(it, Tok) \
                and caps[name][1] != it.name:
            caps[name] = (type, None)
        else:
            caps[name] = (type, it.name if isinstance(it, Tok) else None)

    # ------------------------------------------------------------------
    # Code generation

    def emit(self, indent, text):
        self.out.append("    " * indent + text)

    def expand(self, expr, caps, line, indent):
        """Replaces `$x`, `@x`, `@$` and `@^` in an expression"""
        def value(m):
            name = m.group(1)
            if name not in caps or caps[name][1] is None:
                error(line, "`$%s` is not a token" % name)
            field = self.tokens.type[caps[name][1]]
            if field is None:
                error(line, "the token of `$%s` has no value" % name)
            return "V[%s].%s" % (name, field)
        expr = re.sub(r"\$(\w+)", value, expr)
        if "@$" in expr:
            self.emit(indent, "l = span(_s);")
            expr = expr.replace("@$", "l")
        expr = expr.replace("@^", "lm")
        for name in set(re.findall(r"@(\w+)", expr)):
            if name not in caps or caps[name][0] != "size_t":
                error(line, "`@%s` is not a token" % name)
            self.emit(indent, "Location l_%s = Lc[%s];" % (name, name))
        expr = re.sub(r"@(\w+)", r"l_\1", expr)
        return expr

    def call_expr(self, it):
        args = re.sub(r"(^|,\s*)@(?=\s*(,|$))", r"\1_s", it.args)
        return "%s(%s)" % (it.name, args)

    def gen_seq(self, items, indent, known_first, r, caps):
        first = True
        returned = False
        for n, it in enumerate(items):
            if isinstance(it, Result):
                expr = self.expand(it.expr, caps, it.line, indent)
                if r.type == "void":
                    self.emit(indent, "%s;" % expr)
                    self.emit(indent, "return;")
                else:
                    self.emit(indent, "return %s;" % expr)
                returned = True
                break
            if isinstance(it, Mark):
                self.emit(indent, "lm = span(_s);")
                continue
            if isinstance(it, Pred):
                if not first:
                    self.emit(indent, "if (!(%s)) unsup(%d);"
                        % (self.pred_cond(it), it.line))
                continue
            if isinstance(it, Tok):
                if it.optional:
                    if it.cap:
                        self.emit(indent, "%s = accept(%s);" % (it.cap, it.name))
                    else:
                        self.emit(indent, "accept(%s);" % it.name)
                else:
                    if it.cap:
                        self.emit(indent, "%s = i;" % it.cap)
                    if first and known_first:
                        self.emit(indent, "i++;")
                    else:
                        self.emit(indent, "expect_(%s, %d);" % (it.name, it.line))
            elif isinstance(it, Call):
                if n == len(items) - 1 and not it.cap and not it.suffix \
                        and r.type != "void":
                    self.emit(indent, "return %s;" % self.call_expr(it))
                    returned = True
                    break
                self.gen_call(it, indent)
                ext = self.externs.get(it.name)
                if ext is not None and ext.resume and it.cap:
                    # Error recovery: parse the rest again after `resume`
                    self.emit(indent, "for (;;) {")
                    self.emit(indent + 1, "try {")
                    returned = self.gen_seq(items[n+1:], indent + 2, False,
                        r, caps)
                    self.emit(indent + 1, "} catch (const LLUnsupported &) {")
                    self.emit(indent + 2, "if (!%s(%s)) throw;"
                        % (ext.resume, it.cap))
                    self.emit(indent + 2, "continue;")
                    self.emit(indent + 1, "}")
                    self.emit(indent + 1, "break;")
                    self.emit(indent, "}")
                    break
            elif isinstance(it, Group):
                self.gen_group(it, indent, r, caps)
            first = False
        return returned

    def gen_call(self, it, indent):
        call = self.call_expr(it)
        if it.suffix == "?":
            ext = self.externs.get(it.name)
            if it.name in self.rules:
                if not self.rules[it.name].nullable:
                    error(it.line, "`%s?` needs `rule %s?`" % (it.name, it.name))
                cond = None
            elif ext.first is not None:
                cond = self.condition({(t, END) for t in ext.first}, [])
            else:
                cond = None
            pre = "%s = " % it.cap if it.cap else ""
            if cond:
                self.emit(indent, "if (%s) %s%s;" % (cond, pre, call))
            else:
                self.emit(indent, "%s%s;" % (pre, call))
        elif it.suffix in ("*", "%"):
            deref = self.list_type(self.call_type(it.name))[1]
            v = it.cap or "_v"
            if not it.cap:
                error(it.line, "a list needs a name: `x=%s%s`"
                    % (it.name, it.suffix))
            self.emit(indent, "list_new(m_a, %s);" % v)
            elem = "*_e" if deref else "_e"
            if it.suffix == "%":
                self.emit(indent, "do {")
                self.emit(indent + 1, "auto _e = %s;" % call)
                self.emit(indent + 1, "%s.push_back(m_a, %s);" % (v, elem))
                self.emit(indent, "} while (accept(%s));" % it.sep)
            else:
                if it.name in self.rules and not self.rules[it.name].nullable:
                    error(it.line, "`%s*` needs `rule %s?`" % (it.name, it.name))
                self.emit(indent, "for (;;) {")
                self.emit(indent + 1, "auto _e = %s;" % call)
                self.emit(indent + 1, "if (!_e) break;")
                self.emit(indent + 1, "%s.push_back(m_a, %s);" % (v, elem))
                self.emit(indent, "}")
        elif it.cap:
            self.emit(indent, "%s = %s;" % (it.cap, call))
        else:
            self.emit(indent, call + ";")

    def single_token(self, seqs, items):
        """True if the alternative starts with one given token, which the
        condition has checked"""
        for it in items:
            if isinstance(it, (Pred, Mark)):
                continue
            return isinstance(it, Tok) and not it.optional
        return False

    def gen_group(self, g, indent, r, caps):
        where = "rule %s, group" % r.name
        decisions = self.decide(g.alts, where, g.line)
        conds = [self.condition(s, p) for s, p in decisions]
        if g.suffix == "*":
            if len(g.alts) == 1:
                if conds[0] is None:
                    error(g.line, "a repeated group needs a condition")
                self.emit(indent, "while (%s) {" % conds[0])
                self.gen_seq(g.alts[0], indent + 1,
                    self.single_token(decisions[0][0], g.alts[0]), r, caps)
                self.emit(indent, "}")
                return
            self.emit(indent, "for (;;) {")
            self.gen_chain(g.alts, decisions, conds, indent + 1, "break;", r,
                caps, g.line)
            self.emit(indent, "}")
        elif g.suffix == "?":
            self.gen_chain(g.alts, decisions, conds, indent, None, r, caps,
                g.line)
        else:
            self.gen_chain(g.alts, decisions, conds, indent,
                "unsup(%d);" % g.line, r, caps, g.line)

    def gen_chain(self, alts, decisions, conds, indent, otherwise, r, caps,
            line):
        closed = False
        for n, (alt, cond) in enumerate(zip(alts, conds)):
            if cond is None:
                if n != len(alts) - 1:
                    error(line, "alternative %d can never be taken (rule %s)"
                        % (n + 2, r.name))
                if n == 0:
                    self.emit(indent, "{")
                else:
                    self.emit(indent, "} else {")
                self.gen_seq(alt, indent + 1, False, r, caps)
                closed = True
                break
            self.emit(indent, "%s (%s) {" % ("if" if n == 0 else "} else if",
                cond))
            self.gen_seq(alt, indent + 1,
                self.single_token(decisions[n][0], alt), r, caps)
        if not closed and otherwise:
            self.emit(indent, "} else {")
            self.emit(indent + 1, otherwise)
        self.emit(indent, "}")

    def declare(self, caps, indent):
        for name, (type, _) in caps.items():
            if type.startswith("Vec<"):
                self.emit(indent, "%s %s;" % (type, name))
                self.emit(indent, "%s.from_pointer_n(nullptr, 0);" % name)
            else:
                self.emit(indent, "%s %s{};" % (type, name))
            self.emit(indent, "(void)%s;" % name)

    def gen_rule(self, r):
        params = r.params.strip()
        self.emit(1, "%s %s(%s) {" % (r.type, r.name, params))
        if re.search(r"\bsize_t s\b", params):
            self.emit(2, "size_t _s = s;")
        else:
            self.emit(2, "size_t _s = i;")
        self.emit(2, "(void)_s;")
        self.emit(2, "Location l, lm;")
        self.emit(2, "(void)l; (void)lm;")
        none = "return {};" if r.nullable else "unsup(%d);" % r.line
        decisions = self.decide(r.alts, "rule %s" % r.name, r.line)
        conds = [self.condition(s, p) for s, p in decisions]
        all_caps = []
        for alt in r.alts:
            caps = {}
            self.captures(alt, caps)
            all_caps.append(caps)
        if len(r.alts) == 1 and not r.nullable:
            self.declare(all_caps[0], 2)
            returned = self.gen_seq(r.alts[0], 2, False, r, all_caps[0])
            if not returned and r.type != "void":
                self.emit(2, "unsup(%d);" % r.line)
        elif len(r.alts) <= 4:
            # if / else if chain; each alternative in its own block
            closed = False
            for n, alt in enumerate(r.alts):
                cond = conds[n]
                if cond is None:
                    if n != len(r.alts) - 1:
                        error(r.line, "alternative %d of rule %s can never "
                            "be taken" % (n + 2, r.name))
                    self.emit(2, "{" if n == 0 else "} else {")
                    closed = True
                else:
                    self.emit(2, "%s (%s) {" % ("if" if n == 0
                        else "} else if", cond))
                self.declare(all_caps[n], 3)
                returned = self.gen_seq(alt, 3, cond is not None
                    and self.single_token(decisions[n][0], alt), r,
                    all_caps[n])
                if not returned:
                    if r.type != "void":
                        self.emit(3, "unsup(%d);" % r.line)
                    else:
                        self.emit(3, "return;")
            if not closed:
                self.emit(2, "} else {")
                self.emit(3, none)
            self.emit(2, "}")
        else:
            self.gen_switch(r, decisions, all_caps, none)
        self.emit(1, "}")
        self.emit(0, "")

    def gen_switch(self, r, decisions, all_caps, none):
        """A switch on the current token that jumps to the first alternative
        (in the order of the grammar) which can start with the next tokens
        and whose predicates hold"""
        tries = [self.trie(s) for s, p in decisions]
        tokens = sorted(set(e for t in tries for e in t
            if e not in (END, "_", "NAME", "SEP")) | (SEP_TOKENS
            if any("SEP" in t for t in tries) else set()))
        used = set()

        def jumps(tok):
            lines = []
            for n, (t, (seqs, preds)) in enumerate(zip(tries, decisions)):
                subs = []
                always = False
                extra = None
                for e, sub in t.items():
                    if e in (END, "_"):
                        if e == END:
                            always = True
                        subs.append(sub)
                    elif tok is None:
                        if e == "NAME":
                            extra = "is_name(K[i])"
                            subs.append(sub)
                    elif e == tok or (e == "NAME" and self.tokens.is_name(tok)) \
                            or (e == "SEP" and tok in SEP_TOKENS):
                        subs.append(sub)
                if not subs:
                    continue
                parts = []
                if extra:
                    parts.append(extra)
                if not always:
                    cs = [self.trie_cond(s, 1) for s in subs]
                    if None not in cs:
                        cs = list(dict.fromkeys(cs))
                        parts.append(cs[0] if len(cs) == 1
                            else "(" + " || ".join(cs) + ")")
                parts += [self.pred_cond(p) for p in preds]
                used.add(n)
                if parts:
                    lines.append("if (%s) goto _alt%d;"
                        % (" && ".join(parts), n))
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
                error(r.line, "alternative %d of rule %s can never be taken"
                    % (n + 1, r.name))
            self.emit(2, "_alt%d: {" % n)
            self.declare(all_caps[n], 3)
            t = tries[n]
            known = self.single_token(decisions[n][0], alt) \
                and not (set(t) & {END, "_", "NAME", "SEP"})
            returned = self.gen_seq(alt, 3, known, r, all_caps[n])
            if not returned:
                if r.type != "void":
                    self.emit(3, "unsup(%d);" % r.line)
                else:
                    self.emit(3, "return;")
            self.emit(2, "}")

    def gen_operators(self, name, operand, all_ops):
        ops = [op for op in all_ops if op[0] != "prefix"]
        prefix = [op for op in all_ops if op[0] == "prefix"]
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
        self.emit(1, "ast_t* %s(int min_prec = 1) {" % name)
        self.emit(2, "size_t _s = i;")
        self.emit(2, "ast_t *x;")
        self.emit(2, "switch (K[i]) {")
        for assoc, prec, tok, action in prefix:
            action = action.replace("@$", "l").replace("$op",
                "V[_s].%s" % self.tokens.type[tok])
            self.emit(3, "case %s: {" % tok)
            self.emit(4, "i++;")
            self.emit(4, "x = %s(%d);" % (name, prec))
            self.emit(4, "Location l = span(_s);")
            self.emit(4, "x = %s;" % action)
            self.emit(4, "break;")
            self.emit(3, "}")
        self.emit(3, "default: x = %s();" % operand)
        self.emit(2, "}")
        self.emit(2, "for (;;) {")
        self.emit(3, "int t = K[i];")
        self.emit(3, "int prec = %s_precedence(t);" % name)
        self.emit(3, "if (prec == 0 || prec < min_prec) return x;")
        self.emit(3, "size_t op = i;")
        self.emit(3, "(void)op;")
        self.emit(3, "i++;")
        if right:
            test = " || ".join("prec == %d" % p for p in right)
            self.emit(3, "ast_t *y = %s((%s) ? prec : prec + 1);" % (name, test))
        else:
            self.emit(3, "ast_t *y = %s(prec + 1);" % name)
        self.emit(3, "Location l = span(_s);")
        self.emit(3, "switch (t) {")
        for assoc, prec, tok, action in ops:
            field = self.tokens.type[tok]
            action = action.replace("@$", "l")
            if "$op" in action:
                action = action.replace("$op", "V[op].%s" % field)
            self.emit(4, "case %s: x = %s; break;" % (tok, action))
        self.emit(4, "default: break;")
        self.emit(3, "}")
        self.emit(2, "}")
        self.emit(1, "}")
        self.emit(0, "")

    def generate(self, source):
        self.emit(0, "// Generated by llgen.py from %s; do not edit." % source)
        self.emit(0, "// Member functions of the parser, one per rule.")
        self.emit(0, "")
        self.emit(1, "// A name: TK_NAME or a keyword that can be a name (%keyword)")
        self.emit(1, "static bool is_name(int t) {")
        self.emit(2, "switch (t) {")
        names = sorted(self.tokens.names)
        for k in range(0, len(names), 4):
            self.emit(3, " ".join("case %s:" % t for t in names[k:k+4]))
        self.emit(4, "return true;")
        self.emit(3, "default: return false;")
        self.emit(2, "}")
        self.emit(1, "}")
        self.emit(0, "")
        for la in self.lookaheads:
            self.gen_lookahead(la)
        for op in self.operators:
            self.gen_operators(*op)
        for r in self.rule_list:
            self.gen_rule(r)
        return "\n".join(self.out) + "\n"


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    if len(args) != 3:
        sys.exit(__doc__)
    tokens = Tokens(args[0])
    with open(args[2], "w", encoding="utf-8") as f:
        f.write(tokens.header(args[0].replace("\\", "/").split("/")[-1]))
    rules, externs, predicates, operators, lookaheads, nested \
        = read_grammar(args[0], tokens)
    g = Generator(rules, externs, predicates, operators, tokens, lookaheads,
        nested)
    text = g.generate(args[0].replace("\\", "/").split("/")[-1])
    with open(args[1], "w", encoding="utf-8") as f:
        f.write(text)
    if "--report" in sys.argv:
        for x in dict.fromkeys(g.report):
            print(x)
        print("%d rules; in %d alternatives a keyword wins over a name by the "
            "order of the alternatives" % (len(rules),
            len(dict.fromkeys(g.report))))


if __name__ == "__main__":
    main()
