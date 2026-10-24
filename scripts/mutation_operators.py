from __future__ import annotations

from decimal import Decimal, InvalidOperation, localcontext
from fractions import Fraction
import re

from mutation_lisp import Form, Mutation, MutationRunnerError, parse_forms


OPERATOR_PAIRS = (
    ("<", "<="),
    ("=", "/="),
    ("+", "-"),
    ("*", "/"),
    ("and", "or"),
)
FUNCTION_OPERATORS = {"<", "<=", "=", "/=", "+", "-", "*", "/"}
DECIMAL_FLOAT_RE = re.compile(
    r"^[+-]?(?:(?:[0-9]+\.[0-9]*|\.[0-9]+)(?:[eEsSfFdDlL][+-]?[0-9]+)?|"
    r"[0-9]+[eEsSfFdDlL][+-]?[0-9]+)$"
)
FLOAT_EXPONENT_RE = re.compile(r"([eEsSfFdDlL])([+-]?[0-9]+)$")
RADIX_NUMBER_RE = re.compile(
    r"^#([bBoOxX]|[0-9]+[rR])([+-]?[0-9A-Za-z]+)(?:/([+-]?[0-9A-Za-z]+))?$"
)
RATIO_RE = re.compile(r"^([+-]?[0-9]+)/([+-]?[0-9]+)$")
INTEGER_RE = re.compile(r"^[+-]?[0-9]+$")

def _symbol_info(token_text: str) -> tuple[str, str] | None:
    if "|" in token_text or "\\" in token_text:
        return None
    if ":" not in token_text:
        return "", token_text.lower()
    prefix, name = token_text.rsplit(":", 1)
    package = prefix.replace(":", "").lower()
    if package not in {"cl", "common-lisp"}:
        return None
    return prefix + ":", name.lower()


def _operator_replacement(token_text: str) -> tuple[str, str] | None:
    info = _symbol_info(token_text)
    if info is None:
        return None
    prefix, name = info
    for left, right in OPERATOR_PAIRS:
        if name == left:
            return prefix + right, f"{left} -> {right}"
        if name == right:
            return prefix + left, f"{right} -> {left}"
    return None


def _function_operator(token_text: str) -> bool:
    info = _symbol_info(token_text)
    return info is not None and info[1] in FUNCTION_OPERATORS


def _parse_integer(text: str) -> int | None:
    match = RADIX_NUMBER_RE.fullmatch(text)
    if match:
        radix_text, numerator_text, denominator_text = match.groups()
        if radix_text.lower() == "b":
            base = 2
        elif radix_text.lower() == "o":
            base = 8
        elif radix_text.lower() == "x":
            base = 16
        else:
            base = int(radix_text[:-1])
            if not 2 <= base <= 36:
                return None
        try:
            numerator = int(numerator_text, base)
            if denominator_text is None:
                return numerator
            denominator = int(denominator_text, base)
            if denominator == 0:
                return None
            value = Fraction(numerator, denominator)
            return value.numerator if value.denominator == 1 else None
        except ValueError:
            return None
    if INTEGER_RE.fullmatch(text):
        try:
            return int(text, 10)
        except ValueError:
            return None
    return None


def _parse_rational(text: str) -> Fraction | None:
    radix_match = RADIX_NUMBER_RE.fullmatch(text)
    if radix_match:
        radix_text, numerator_text, denominator_text = radix_match.groups()
        if denominator_text is None:
            return None
        if radix_text.lower() == "b":
            base = 2
        elif radix_text.lower() == "o":
            base = 8
        elif radix_text.lower() == "x":
            base = 16
        else:
            base = int(radix_text[:-1])
            if not 2 <= base <= 36:
                return None
        try:
            numerator = int(numerator_text, base)
            denominator = int(denominator_text, base)
            if denominator == 0:
                return None
            return Fraction(numerator, denominator)
        except ValueError:
            return None
    match = RATIO_RE.fullmatch(text)
    if not match:
        return None
    try:
        numerator, denominator = map(int, match.groups())
        if denominator == 0:
            return None
        return Fraction(numerator, denominator)
    except ValueError:
        return None


def _float_literal(text: str) -> tuple[Decimal, str] | None:
    if not DECIMAL_FLOAT_RE.fullmatch(text):
        return None
    marker_match = FLOAT_EXPONENT_RE.search(text)
    marker = marker_match.group(1).lower() if marker_match else "e"
    decimal_text = text
    if marker_match:
        marker_start = marker_match.start(1)
        decimal_text = text[:marker_start] + "e" + text[marker_match.end(1) :]
    try:
        value = Decimal(decimal_text)
    except InvalidOperation:
        return None
    if not value.is_finite():
        return None
    return value, marker


def _format_float(value: Decimal, marker: str) -> str:
    scientific = format(value, "E")
    mantissa, exponent = scientific.split("E", 1)
    if "." not in mantissa:
        mantissa += ".0"
    return f"{mantissa}{marker}{exponent}"


def _mutated_number(text: str, delta: int) -> str | None:
    if text.startswith("#\\") or "|" in text or "\\" in text:
        return None
    rational = _parse_rational(text)
    if rational is not None:
        changed = rational + delta
        if changed.denominator == 1:
            return str(changed.numerator)
        return f"{changed.numerator}/{changed.denominator}"
    integer = _parse_integer(text)
    if integer is not None:
        return str(integer + delta)
    float_value = _float_literal(text)
    if float_value is not None:
        value, marker = float_value
        digits = len(value.as_tuple().digits)
        precision = max(50, digits + max(0, value.adjusted()) + 10)
        with localcontext() as context:
            context.prec = precision
            changed = value + Decimal(delta)
        return _format_float(changed, marker)
    return None


def _walk(forms: list[Form]):
    stack = list(reversed(forms))
    while stack:
        form = stack.pop()
        yield form
        stack.extend(reversed(form.children))


def _context_form(form: Form, source: str) -> str:
    current = form
    while current.parent is not None and current.parent.kind != "list":
        current = current.parent
    if current.parent is not None and current.parent.kind == "list":
        return source[current.parent.start : current.parent.end]
    return source[form.start : form.end]


def _line_column(source: str, offset: int) -> tuple[int, int]:
    line = source.count("\n", 0, offset) + 1
    previous_newline = source.rfind("\n", 0, offset)
    return line, offset - previous_newline


def _mutation(
    relative_source: str,
    source: str,
    start: int,
    end: int,
    replacement: str,
    operator: str,
    form: str,
) -> Mutation | None:
    original = source[start:end]
    if original == replacement:
        return None
    line, column = _line_column(source, start)
    return Mutation(
        source=relative_source,
        start=start,
        end=end,
        replacement=replacement,
        original=original,
        operator=operator,
        form=form,
        line=line,
        column=column,
    )


def discover_mutations(relative_source: str, source: str) -> list[Mutation]:
    forms = parse_forms(source)
    mutations: list[Mutation] = []

    for form in _walk(forms):
        if form.kind == "atom" and form.token is not None:
            if not form.quoted and "#'" in form.prefixes and _function_operator(form.token.text):
                replacement = _operator_replacement(form.token.text)
                if replacement is not None:
                    replacement_text, operator = replacement
                    mutation = _mutation(
                        relative_source,
                        source,
                        form.token.start,
                        form.token.end,
                        replacement_text,
                        f"function designator {operator}",
                        _context_form(form, source),
                    )
                    if mutation is not None:
                        mutations.append(mutation)

            for delta, label in ((-1, "constant - 1"), (1, "constant + 1")):
                replacement_text = _mutated_number(form.token.text, delta)
                if replacement_text is not None:
                    mutation = _mutation(
                        relative_source,
                        source,
                        form.token.start,
                        form.token.end,
                        replacement_text,
                        label,
                        _context_form(form, source),
                    )
                    if mutation is not None:
                        mutations.append(mutation)

        if form.kind != "list" or form.quoted or not form.children:
            continue
        head = form.children[0]
        if head.kind != "atom" or head.token is None:
            continue

        replacement = _operator_replacement(head.token.text)
        if replacement is not None:
            replacement_text, operator = replacement
            mutation = _mutation(
                relative_source,
                source,
                head.token.start,
                head.token.end,
                replacement_text,
                operator,
                source[form.start : form.end],
            )
            if mutation is not None:
                mutations.append(mutation)

        info = _symbol_info(head.token.text)
        head_name = info[1] if info is not None else ""
        if head_name in {"if", "when"} and len(form.children) >= 2:
            condition = form.children[1]
            condition_source = source[condition.start : condition.end]
            mutation = _mutation(
                relative_source,
                source,
                condition.start,
                condition.end,
                f"(not {condition_source})",
                f"negate {head_name} condition",
                source[form.start : form.end],
            )
            if mutation is not None:
                mutations.append(mutation)

        if head_name == "function" and len(form.children) >= 2:
            designator = form.children[1]
            if designator.kind == "atom" and designator.token is not None:
                if _function_operator(designator.token.text):
                    function_replacement = _operator_replacement(designator.token.text)
                    if function_replacement is not None:
                        replacement_text, operator = function_replacement
                        mutation = _mutation(
                            relative_source,
                            source,
                            designator.token.start,
                            designator.token.end,
                            replacement_text,
                            f"function designator {operator}",
                            source[form.start : form.end],
                        )
                        if mutation is not None:
                            mutations.append(mutation)

    return sorted(mutations, key=lambda item: (item.start, item.operator, item.end))
