import os
import re
import sys
import json
try:
    from clean import *
    from decode import *
except:
    from .clean import *
    from .decode import *
    import sublime
    import sublime_plugin

    def get_region_indent_level(view, region):
        start_pt = region.begin()
        row, col = view.rowcol(start_pt)
        return col

    class SchemeCodeFormatterCommand(sublime_plugin.TextCommand):
        def run(self, edit):
            try:
                self.format_regions(edit)
                sublime.status_message("formated")
            except Exception as e:
                sublime.status_message(str(e))

        def format_regions(self, edit):
            regions = [s for s in self.view.sel()]
            view_encoding = self.view.encoding()
            if view_encoding == 'Undefined':
                view_encoding = 'utf-8'

            for region in reversed(regions):
                if region.size() < 4: continue

                indent_level = get_region_indent_level(self.view, region)
                original_code = self.view.substr(region)
                prettied_code = pretty(original_code, indent_level)
                if prettied_code != original_code:
                    self.view.replace(edit, region, prettied_code)


# indent: 1(bracket), 2(symbol) or user defined
# keep: how many items keep in current line
formats = {
    "and": {"keep": 2},
    "begin": {"keep": 1, "indent": 2},
    "call/cc": {"keep": 1, "indent": 2},
    "call-with-values": {"keep": 1, "indent": 2},
    "case": {"keep": 2, "indent": 2},
    "case-lambda": {"keep": 1, "indent": 2},
    "cond": {"keep": 1, "indent": 2},
    "cons": {"keep": 2},
    "define": {"keep": 2, "indent": 2},
    "define-record-type": {"keep": 2, "indent": 2},
    "define-syntax": {"keep": 2, "indent": 2},
    "else": {"keep": 1, "indent": 1},
    "eval-when": {"keep": 2, "indent": 2},
    "fields": {"keep": 2, "indent": 8},
    "filter": {"keep": 2},
    "fluid-let": {"keep": 2, "indent": 2},
    "fold-left": {"keep": 1, "indent": 2},
    "fold-right": {"keep": 1, "indent": 2},
    "for-each": {"keep": 1, "indent": 2},
    "if": {"keep": 2},
    "lambda": {"keep": 2, "indent": 2},
    "let": {"keep": "first_list", "indent": 2},
    "let*": {"keep": 2, "indent": 2},
    "letrec": {"keep": 1, "indent": 2},
    "let-syntax": {"keep": 2, "indent": 2},
    "let-values": {"keep": 2, "indent": 2},
    "local": {"keep": 2, "indent": 2},
    "map": {"keep": 2},
    "meta-cond": {"keep": 1, "indent": 2},
    "not": {"keep": 2},
    "or": {"keep": 2},
    "set!": {"keep": 2, "indent": 2},
    "syntax": {"keep": 2, "indent": 2},
    "syntax-case": {"keep": 3, "indent": 2},
    "syntax-rules": {"keep": 2, "indent": 2},
    "unless": {"keep": 2, "indent": 8},
    "when": {"keep": 2},
    "with-syntax": {"keep": 2, "indent": 2}
}


def clause_length(clause):
    if isinstance(clause, list):
        return clause[TEXTLEN]
    return len(clause.text)

def find_first_list(clauses):
    for i in range(1, len(clauses)):
        if isinstance(clauses[i], list):
            return i + 1
    return 1

def has_line_comment_flatly(packet):
    for clause in packet[CLAUSES]:
        if is_line_comment(clause):
            return True
    return False

def has_line_comment_deeply(packet):
    for clause in packet[CLAUSES]:
        if isinstance(clause, list) and has_line_comment_deeply(clause):
            return True
        elif is_line_comment(clause):
            return True
    return False

# if first is constant or parent is lambda...
# fill

def is_similar_clauses(clauses1, clauses2):
    for i in range(len(clauses1)):
        cl1 = clauses1[i]
        cl2 = clauses2[i]
        if type(cl1) != type(cl2):
            return False
        if isinstance(cl1, list):
            _clauses1 = cl1[CLAUSES]
            _clauses2 = cl2[CLAUSES]
            if len(_clauses1) != len(_clauses2):
                return False
            if not is_similar_clauses(_clauses1, _clauses2):
                return False
        else:
            if (cl1.type in {'bc', 'lc', 'dc'} or
                cl2.type in {'bc', 'lc', 'dc'}):
                return False
    return True

def is_similar_packet(packet1, packet2):
    clauses1 = packet1[CLAUSES]
    clauses2 = packet2[CLAUSES]
    if len(clauses1) != len(clauses2):
            return False
    if (len(clauses1) == 0 or
        clauses1[0] == clauses2[0] and
        is_similar_clauses(clauses1[1:], clauses2[1:])):
        return True
    return False

def is_regular_clauses(clauses):
    if len (clauses) < 2:
        return False

    first_clause = None
    has_similar = False
    for clause in clauses:
        if (is_comment(clause) or
            is_datum_comment(clause) or
            is_newline(clause)):
            continue

        if isinstance(clause, Token) or clause[TEXTLEN] > 100:
            return False

        if not first_clause:
            first_clause = clause
            continue

        if not is_similar_packet(clause, first_clause):
            return False

        has_similar = True

    return has_similar


def pretty_datum_packet(packet, indent_level):
    yield packet[OPENING]

    clauses = packet[CLAUSES]
    if clauses:
        indent_level += len(packet[OPENING])
        newline_indent_space = '\n' + indent_level * ' '
        current_line_length = indent_level
        current_line_holdon = False
        end_index = len(clauses) - 1
        for index in range(end_index + 1):
            if current_line_holdon:
                yield ' '
                current_line_length += 1
            else:
                current_line_holdon = True
            clause = clauses[index]
            if not is_comment(clause):
                yield clause.text
                if index == end_index:
                    break
                current_line_length += len(clause.text)
                if current_line_length + len(clause.text) > 75:
                    yield newline_indent_space
                    current_line_length = indent_level
                    current_line_holdon = False
            else:
                if current_line_length + len(clause.text) > 100:
                    yield newline_indent_space
                yield clause.text
                yield newline_indent_space
                current_line_length = indent_level
                current_line_holdon = False

        if is_line_comment(clauses[end_index]):
            yield newline_indent_space

    yield packet[CLOSING]


# XXX: Bad code, full of empirical parameters.
def pretty_packet(packet, indent_level):
    yield packet[OPENING]

    clauses = packet[CLAUSES]
    if clauses:
        nkeep = 1
        nclauses = len(clauses)
        first_clause = clauses[0]
        clause_lengths = [clause_length(c) for c in clauses[:2]]
        open_indent_level = indent_level + len(packet[OPENING])
        next_indent_level = open_indent_level
        is_keyword_clause = False
        keep2_line_length = open_indent_level + clause_lengths[0] + \
                            (1 + clause_lengths[1]) if nclauses > 1 else 0
        keepx_line_length = indent_level + packet[TEXTLEN]
        keep_all_in_aline = keepx_line_length <= 75 and \
                            not has_line_comment_deeply(packet)

        if isinstance(first_clause, Token) and not is_comment(first_clause):
            tokstr = first_clause.text
            toklen = len(tokstr)
            if tokstr in formats:
                is_keyword_clause = True
                fmt = formats[tokstr]
                keep = fmt.get("keep", 2)
                if keep == "first_list":
                    keep = find_first_list(clauses)
                nkeep = keep
                if "indent" in fmt:
                    next_indent_level += fmt["indent"] - 1
                else:
                    next_indent_level += toklen + 1

                if (nkeep == 2 and nclauses > 1 and
                    clause_lengths[0] > 8 and
                    keep2_line_length > 75 and
                    clause_lengths[1] / packet[TEXTLEN] > 0.5):
                    nkeep = 1

                if keepx_line_length > 60:
                    keep_all_in_aline = False

            elif not keep_all_in_aline:
                next_indent_level += 1
                has_list = False
                for i in range(2, nclauses):
                    if isinstance(clauses[i], list):
                        has_list = True
                        break
                if (toklen <= 8 or
                    has_list and nclauses > 2 and
                    keep2_line_length <= 75 and
                    (isinstance(clauses[2], list) or is_comment(clauses[2]))):
                    nkeep = 2
                    # if a list's head weight exceeds a certain limit,
                    # we consider it as a computation but definition.
                    if (toklen <= 8 and
                        (not has_list or
                         toklen <= 4 or
                         (nclauses > 2 and
                          (clause_lengths[0] +
                           clause_lengths[1]) / packet[TEXTLEN] > 0.33 or
                          isinstance(clauses[2], Token) and
                          not is_comment(clauses[2]) or
                          is_regular_clauses(clauses[1:])))):
                        next_indent_level += toklen

                if has_list and nclauses == 4:
                    if (isinstance(clauses[1], Token) and
                        isinstance(clauses[2], Token) and
                        isinstance(clauses[3], list) and
                        keep2_line_length + len(clauses[2].text) <= 75):
                        nkeep = 3

        if not keep_all_in_aline:
            if not is_keyword_clause:
                if nkeep > 1:
                    if (nclauses > 1 and
                        clause_lengths[0] > 25 and
                        keep2_line_length > 75):
                        nkeep = 2 if nclauses > 2 else 1
                elif packet[TEXTLEN] - clause_lengths[0] <= 20:
                    nkeep = nclauses

                if ((keepx_line_length <= 75 or packet[TEXTLEN] <= 50 or
                     keepx_line_length <= 80 and nclauses > 3 and
                     keep2_line_length >= 60) and
                    not has_line_comment_flatly(packet)):
                    nkeep = nclauses

            _indent_level = open_indent_level
            for i in range(min(nkeep, nclauses)):
                if i > 0:
                    yield ' '
                    _indent_level += 1
                clause = clauses[i]
                if isinstance(clause, list):
                    if (_indent_level + clause[TEXTLEN] <= 75 and
                        not has_line_comment_flatly(clause)):
                        _pretty = pretty_clauses_in_aline
                    else:
                        _pretty = pretty_clauses
                    yield from _pretty([clause], _indent_level)
                else:
                    yield clause.text
                    if is_line_comment(clause):
                        nkeep = i + 1
                        if nkeep == nclauses:
                            yield '\n' + open_indent_level * ' '
                        break
                    _indent_level += len(clause.text)

            clauses = clauses[nkeep:]
            if is_regular_clauses(clauses):
                yield from pretty_regular_clauses(clauses, next_indent_level)
            else:
                yield from pretty_clauses(clauses, next_indent_level, True)
        else:
            yield from pretty_clauses_in_aline(clauses, next_indent_level)

    yield packet[CLOSING]


def pretty_clauses_in_aline(clauses, indent_level):
    indent_space = indent_level * ' '
    last_clause = None
    for clause in clauses:
        if isinstance(clause, list):
            if last_clause:
                if is_line_comment(last_clause):
                    yield '\n' + indent_space
                else:
                    yield ' '

            yield clause[OPENING]
            subclauses = clause[CLAUSES]
            if subclauses:
                new_indent_level = indent_level + len(clause[OPENING])
                yield from pretty_clauses_in_aline(subclauses, new_indent_level)
            yield clause[CLOSING]
        else:
            if last_clause and not is_newline(clause):
                if is_line_comment(last_clause):
                    yield '\n' + indent_space
                else:
                    yield ' '
            yield clause.text
        last_clause = clause

    if is_comment(last_clause):
        yield '\n' + indent_space
    elif is_newline(last_clause):
        yield indent_space


def pretty_clauses(clauses, indent_level, initial_indent=False):
    indent_space = indent_level * ' '
    last_clause = None
    for clause in clauses:
        if initial_indent:
            if not ((is_datum_comment(clause) and
                     is_datum_comment(last_clause)) or
                    is_newline(clause)):
                yield '\n' + indent_space
        else:
            initial_indent = True
        if isinstance(clause, list):
            if is_datum_packet(clause):
                yield from pretty_datum_packet(clause, indent_level)
            else:
                yield from pretty_packet(clause, indent_level)
        else:
            yield clause.text
        last_clause = clause

    if is_comment(last_clause):
        yield '\n' + indent_space
    elif is_newline(last_clause):
        yield indent_space


def pretty_packet_in_aline(packet):
    yield packet[OPENING]
    clauses = packet[CLAUSES]
    if clauses:
        last_clause = None
        for clause in clauses:
            if last_clause:
                yield ' '
            if isinstance(clause, list):
                yield from pretty_packet_in_aline(clause)
            else:
                yield clause.text
            last_clause = clause
    yield packet[CLOSING]


def pretty_regular_clauses(clauses, indent_level):
    newline_indent_space = '\n' + indent_level * ' '
    last_clause = None
    for clause in clauses:
        if not ((is_datum_comment(clause) and
                 is_datum_comment(last_clause)) or
                is_newline(clause)):
            yield newline_indent_space
        if isinstance(clause, list):
            yield from pretty_packet_in_aline(clause)
        else: # comment
            yield clause.text
        last_clause = clause

    if is_comment(last_clause):
        yield newline_indent_space
    elif is_newline(last_clause):
        yield indent_level * ' '


def pretty(code, initial_indent_level=0):
    packet = parse(code)
    return "".join(pretty_clauses(packet[CLAUSES], initial_indent_level))


def pprint(code):
    print(pretty(code))


def minimize(code, sep=' ', remove_datum_comment=False):
    def _iter_clauses(clauses):
        datum_comment_depth = 0
        for idx, clause in enumerate(clauses):
            if isinstance(clause, list):
                if datum_comment_depth > 0:
                    datum_comment_depth -= 1
                    continue
                yield clause[OPENING]
                yield from _iter_clauses(clause[CLAUSES])
                yield clause[CLOSING]
            else:
                if clause.type in {'bc', 'lc', 'nl'}:
                    continue
                elif remove_datum_comment and clause.type == 'dc':
                    datum_comment_depth += 1
                    continue
                if datum_comment_depth > 0:
                    datum_comment_depth -= 1
                    continue
                if (idx > 0 and
                    not (isinstance(clauses[idx-1], list) or
                         clause.type == 'dc' and
                         clauses[idx-1].type == 'dc')):
                    yield sep
                yield clause.text

    clauses = parse(code)[CLAUSES]
    return "".join(_iter_clauses(clauses))


def run_simple_test():
    pprint("""
(define-syntax xdefine
  (lambda (x)
    (syntax-case x ()
      ((key (name . args) b1 b2 ...)
       (with-implicit (key rcb fp bfp tb it)
         #'(define (name rcb fp bfp tb it . args)
           b1
           b2
           ...))))))
""")
    pprint("""
(define-syntax meta-assert
  (lambda (x)
    (syntax-case x ()
      [(_ e)
       #`(let-syntax ([t
                       (if e
                           (lambda () #'(void))
                           #,(#%$make-source-oops #f
                               "failed meta-assertion"
                               #'e))])
           (void))])))
""")
    pprint("""
(begin
  (error? ; unbound variable $l1-b
          $l1-b))

(begin
  (define ; comment
          variable
          10))

(begin (define variable 10))

(begin (let ; comment
            name
            ([x 0][y 1])))
(begin (let name ; comment
         ([x 0][y 1])))
(begin (void ; comment
         ))
""")


def run_validity_test(encoder, subfix, **kwargs):
    print("begin {} test...".format(encoder.__name__))

    import subprocess
    relpath = 'test'
    abspath = os.path.abspath('test')
    npass = 0
    nfile = 0
    for filename in os.listdir(relpath):
        if (not filename.endswith(".ss") or
            filename.count('.') > 1):
            continue
        nfile += 1

        print("file: {}".format(filename))
        base, ext = os.path.splitext(filename)
        primary_file_path = os.path.join(relpath, filename)
        encoded_file_name = "{}.{}.ss".format(base, subfix)
        encoded_file_path = os.path.join(relpath, encoded_file_name)
        with open(primary_file_path, 'r', encoding='utf-8') as infile:
            code = infile.read()
            code = encoder(code, **kwargs)
            print("encode done!, ", end='')
            with open(encoded_file_path, "w+", encoding='utf-8') as outfile:
                outfile.write(code)

        primary_pretty_file_path = os.path.join(abspath, "{}.1.ss".format(base))
        encoded_pretty_file_path = os.path.join(abspath, "{}.2.ss".format(base))

        def pretty_file(i, o):
            p = subprocess.Popen("scheme", shell=True, stdin=subprocess.PIPE,
                stderr=subprocess.PIPE, stdout=subprocess.PIPE)
            p.communicate('(pretty-file "{}" "{}")'.format(i, o).replace(
                '\\', '/').encode('utf-8'))

        pretty_file(primary_file_path, primary_pretty_file_path)
        pretty_file(encoded_file_path, encoded_pretty_file_path)

        succeed = False
        if os.path.exists(primary_pretty_file_path):
            if os.path.exists(encoded_pretty_file_path):
                size1 = os.path.getsize(primary_pretty_file_path)
                size2 = os.path.getsize(encoded_pretty_file_path)
                if size1 == size2:
                    succeed = True
                    npass += 1
        print("validate", ["fail!", "pass!"][succeed])

    print("finished {} test with {} pass, {} fail.\n".format(
        encoder.__name__, npass, nfile - npass))


if __name__ == '__main__':
    run_validity_test(pretty, "pretty")
    # run_validity_test(minimize, "min")
    # run_validity_test(minimize, "min", sep='\n', remove_datum_comment=True)
    run_simple_test()
    clean_test_generated_files(r"[^.]*(?:\.pretty)?\.ss")
    try:
        import shutil
        shutil.rmtree("__pycache__")
        print("removed __pycache__")
    except:
        pass
