import os
import re
import json
from collections import namedtuple


regex = re.compile(r"""(?x)
    (?P<lc>;[^\n]*$)|  # line comment
    (?P<dc>\#;)|       # datum comment
    (?P<bc>            # block comment
        \#\|(?:
                [^\#|]|
                \#(?!\|)|
                \|(?!\#)|
                \#\|(?:
                        [^\#|]|
                        \#(?!\|)|
                        \|(?!\#)
                    )*
                \|\#
            )*
        \|\#)|
    (?P<ch>\#\\(?:[x][0-9a-fA-F]+\b|[0-9a-zA-Z]+\b|[ \S]))|   # character
    (?P<st>"(?:[^"\\]|\\.)*")|                                # string
    (?P<br>[(\[\])])|                                         # bracket
    (?P<at>'?\#\{[^(\[{}\])#`'",\n]*\}|                       # gensym
           '[{}]|
           \#\d+=\#\{[^(\[{}\])#`'",\n]*\}|
           [`'](?=(?:[;"]|\#\\))|
           [^(\[{}\])#`',\s]+[^(\[{}\])`',\s]*|
           [^(\[{}\])\s]+)|                                   # atomic
    (?P<nl>^\n+)
""", flags=re.MULTILINE|re.DOTALL)


OPENING = 0
CLAUSES = 1
CLOSING = 2
TEXTLEN = 3

brackets = {
    '(': ')',
    '[': ']'
}


Token = namedtuple('Token', ['type', 'text'])

def token_type(clause):
    if isinstance(clause, Token):
        return clause.type
    return None

def is_comment(clause):
    return token_type(clause) in {'bc', 'lc'}

def is_line_comment(clause):
    return token_type(clause) == 'lc'

def is_datum_comment(clause):
    return token_type(clause) == 'dc'

packet_regex = re.compile(r"(?:['`](?:#\d*(?:vu8|vfx)?)|(?:#\d*(?:vu8|vfx)))")
syntax_regex = re.compile(r"['`#]+(?:['`#,@]+|\d*(?:vu8|vfx)?)|['`#,]['`#,@]*")

# ...
def is_datum_packet(packet):
    if packet_regex.match(packet[OPENING]):
        for clause in packet[CLAUSES]:
            if isinstance(clause, list):
                return False
        return True
    return False

def is_syntax_aux(clause):
    if token_type(clause) == 'at':
        match = syntax_regex.match(clause.text)
        if match and match.group() == clause.text:
            return True
    return False

def is_newline(clause):
    return token_type(clause) == 'nl'

def parse(code):
    packet_stack = [[None, [], None, 0]]
    packet_stack_append = packet_stack.append
    packet_stack_pop = packet_stack.pop
    for m in regex.finditer(code):
        for name, group in m.groupdict().items():
            if group:
                break
        if group in brackets:
            packet_stack_append([group, [], brackets[group], 2])
        elif packet_stack:
            if group == packet_stack[-1][CLOSING]:
                packet = packet_stack_pop()
                newtop = packet_stack[-1]
                clauses = newtop[CLAUSES]
                if clauses and is_syntax_aux(clauses[-1]):
                    prefix = clauses.pop().text
                    packet[OPENING] = prefix + packet[OPENING]
                    packet[TEXTLEN] += len(prefix)
                    newtop[TEXTLEN] -= len(prefix)
                packet[TEXTLEN] += len(packet[CLAUSES]) - 1

                clauses.append(packet)
                newtop[TEXTLEN] += packet[TEXTLEN]
            else:
                packet = packet_stack[-1]
                if name == 'nl' and not packet[CLAUSES]:
                    continue
                token = Token(name, group)
                packet[CLAUSES].append(token)
                packet[TEXTLEN] += len(group)
        else:
            raise Exception("found unmatched bracket at " + str(m.span()))

    packet = packet_stack.pop()
    if packet_stack:
        raise Exception("parse code error")
    return packet


def write_tokens(packet, file):
    if packet[OPENING]:
        file.write(str(Token("ob", packet[OPENING])) + '\n')
    for clause in packet[CLAUSES]:
        if isinstance(clause, Token):
            file.write(str(clause) + '\n')
        else:
            write_tokens(clause, file)
    if packet[CLOSING]:
        file.write(str(Token("cb", packet[CLOSING])) + '\n')


def run_tokenizer_test():
    for filename in os.listdir("test"):
        if (not filename.endswith(".ss") or
            filename.count('.') > 1):
            continue

        base, ext = os.path.splitext(filename)
        infile_path = os.path.join("test", filename)
        outfile_name = "{}.tokens.py".format(base)
        outfile_path = os.path.join("test", outfile_name)
        with open(infile_path, 'r', encoding='utf-8') as infile:
            code = infile.read()
            tree = parse(code)
            print("parsed file: " + infile_path)
            if not tree:
                raise "parse error at file: " + filename
            with open(outfile_path, "w+", encoding='utf-8') as outfile:
                write_tokens(tree, outfile)


if __name__ == "__main__":
    run_tokenizer_test()
