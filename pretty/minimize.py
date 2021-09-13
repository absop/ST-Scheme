try:
    from .read import *
except:
    from read import *


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
