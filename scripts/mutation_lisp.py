from __future__ import annotations

from dataclasses import dataclass, field
import re


class MutationRunnerError(RuntimeError):
    pass


@dataclass(frozen=True)
class Token:
    kind: str
    text: str
    start: int
    end: int


@dataclass
class Form:
    kind: str
    start: int
    end: int
    token: Token | None = None
    prefixes: tuple[str, ...] = ()
    children: list[Form] = field(default_factory=list)
    parent: Form | None = None
    quoted: bool = False


@dataclass(frozen=True)
class Mutation:
    source: str
    start: int
    end: int
    replacement: str
    original: str
    operator: str
    form: str
    line: int
    column: int

def _is_delimiter(character: str) -> bool:
    return character.isspace() or character in '();"\'`,'


def tokenize(source: str) -> list[Token]:
    tokens: list[Token] = []
    length = len(source)
    index = 0

    while index < length:
        character = source[index]
        if character.isspace() or character == "\ufeff":
            index += 1
            continue
        if character == ";":
            newline = source.find("\n", index)
            index = length if newline < 0 else newline + 1
            continue
        if source.startswith("#|", index):
            comment_start = index
            index += 2
            depth = 1
            while index < length and depth:
                if source.startswith("#|", index):
                    depth += 1
                    index += 2
                elif source.startswith("|#", index):
                    depth -= 1
                    index += 2
                else:
                    index += 1
            if depth:
                raise MutationRunnerError(
                    f"Unterminated block comment at character {comment_start}."
                )
            continue
        if source.startswith("#;", index):
            tokens.append(Token("sexp-comment", "#;", index, index + 2))
            index += 2
            continue
        if source.startswith("#'", index):
            tokens.append(Token("prefix", "#'", index, index + 2))
            index += 2
            continue
        if source.startswith("#(", index):
            tokens.append(Token("prefix", "#(", index, index + 2))
            index += 1
            continue
        if re.match(r"#[sSaAcCpP]\(", source[index:]):
            tokens.append(Token("prefix", source[index : index + 2], index, index + 2))
            index += 1
            continue
        if source.startswith("#\\", index):
            start = index
            index += 2
            if index >= length:
                raise MutationRunnerError(
                    f"Incomplete character literal at character {start}."
                )
            if _is_delimiter(source[index]):
                index += 1
            else:
                while index < length and not _is_delimiter(source[index]):
                    if source[index] == "\\" and index + 1 < length:
                        index += 2
                    else:
                        index += 1
            tokens.append(Token("character", source[start:index], start, index))
            continue
        if character == "(":
            tokens.append(Token("left-paren", character, index, index + 1))
            index += 1
            continue
        if character == ")":
            tokens.append(Token("right-paren", character, index, index + 1))
            index += 1
            continue
        if character in "'`":
            tokens.append(Token("prefix", character, index, index + 1))
            index += 1
            continue
        if character == ",":
            end = index + (2 if index + 1 < length and source[index + 1] == "@" else 1)
            tokens.append(Token("prefix", source[index:end], index, end))
            index = end
            continue
        if character == '"':
            start = index
            index += 1
            while index < length:
                if source[index] == "\\":
                    index += 2
                elif source[index] == '"':
                    index += 1
                    break
                else:
                    index += 1
            else:
                raise MutationRunnerError(
                    f"Unterminated string literal at character {start}."
                )
            tokens.append(Token("string", source[start:index], start, index))
            continue

        start = index
        in_bar_symbol = False
        while index < length:
            current = source[index]
            if current == "\\" and index + 1 < length:
                index += 2
            elif current == "|":
                in_bar_symbol = not in_bar_symbol
                index += 1
            elif not in_bar_symbol and _is_delimiter(current):
                break
            else:
                index += 1
        if start == index:
            raise MutationRunnerError(f"Unable to tokenize character {index}.")
        tokens.append(Token("atom", source[start:index], start, index))

    return tokens


def _quoted_by_prefix(parent_quoted: bool, prefixes: tuple[str, ...]) -> bool:
    quoted = parent_quoted
    for prefix in prefixes:
        if prefix in {"'", "`", "#(", "#S", "#s", "#A", "#a", "#C", "#c", "#P", "#p"}:
            quoted = True
        elif prefix in {",", ",@"}:
            quoted = False
    return quoted


def parse_forms(source: str) -> list[Form]:
    tokens = tokenize(source)

    def parse_one(
        position: int, parent: Form | None, parent_quoted: bool
    ) -> tuple[Form, int]:
        prefixes: list[str] = []
        prefix_start: int | None = None
        while position < len(tokens):
            token = tokens[position]
            if token.kind == "sexp-comment":
                position += 1
                if position < len(tokens):
                    _, position = parse_one(position, None, False)
                continue
            if token.kind != "prefix":
                break
            if prefix_start is None:
                prefix_start = token.start
            prefixes.append(token.text)
            position += 1

        if position >= len(tokens):
            raise MutationRunnerError("A reader prefix has no following form.")

        token = tokens[position]
        if token.kind == "left-paren":
            node = Form(
                kind="list",
                start=prefix_start if prefix_start is not None else token.start,
                end=token.end,
                prefixes=tuple(prefixes),
                parent=parent,
                quoted=_quoted_by_prefix(parent_quoted, tuple(prefixes)),
            )
            position += 1
            while position < len(tokens) and tokens[position].kind != "right-paren":
                if tokens[position].kind == "sexp-comment":
                    position += 1
                    if position < len(tokens):
                        _, position = parse_one(position, None, False)
                    continue
                child, position = parse_one(position, node, node.quoted)
                node.children.append(child)
            if position >= len(tokens):
                raise MutationRunnerError(
                    f"Unterminated list form at character {node.start}."
                )
            node.end = tokens[position].end
            return node, position + 1

        if token.kind == "right-paren":
            raise MutationRunnerError(f"Unexpected close parenthesis at {token.start}.")
        node = Form(
            kind=token.kind,
            start=prefix_start if prefix_start is not None else token.start,
            end=token.end,
            token=token,
            prefixes=tuple(prefixes),
            parent=parent,
            quoted=_quoted_by_prefix(parent_quoted, tuple(prefixes)),
        )
        return node, position + 1

    forms: list[Form] = []
    position = 0
    while position < len(tokens):
        if tokens[position].kind == "right-paren":
            raise MutationRunnerError(
                f"Unexpected close parenthesis at {tokens[position].start}."
            )
        form, position = parse_one(position, None, False)
        forms.append(form)
    return forms
