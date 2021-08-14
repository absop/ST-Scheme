import re
import subprocess

class RegexpTrie(dict):
    eow = 0

    def __init__(self, words=[]):
        for word in words:
            node = self
            for char in word:
                if char not in node:
                    node[char] = RegexpTrie()
                node = node[char]
            node.set_eow()

    def set_eow(self):
        self[self.eow] = True

    def get_eow(self):
        return self.eow in self

    def branches(self):
        return filter(lambda c: c != self.eow, self)

    def escape(self, symbol_chars):
        # escape regexp meta characters except '-', without optimizing
        _special_chars_map = {i: '\\' + chr(i) for i in b'?*+|^$\\.&'}
        return symbol_chars.translate(_special_chars_map)

    def groups(self):
        branches = sorted(self.branches())
        return [self.escape(c) + self[c].regexp() for c in branches]

    def regexp(self):
        if hasattr(self, '_regexp'):
            return self._regexp
        groups = self.groups()
        regexp = '|'.join(groups)
        # multiple groups or word end and len(suffix) > 1
        if len(groups) > 1 or self.get_eow() and len(regexp) > 1:
            regexp = '(?:%s)' % regexp
        # word end and there are suffixes
        if self.get_eow() and len(groups) > 0:
            regexp += '?'
        self._regexp = regexp
        return regexp

    def print(self):
        print(self.regexp())

    def pprint(self):
        print(self.pretty_regexp(8))

    def pretty_regexp(self, indent_level=0, indent_char=' '):
        def pretty(trie, indent_level):
            regexp = trie.regexp()
            if indent_level + len(regexp) < 79:
                yield regexp
                return
            groups = sorted(trie.branches())
            if len(groups) > 1 or trie.get_eow() and len(regexp) > 1:
                yield '(?:'
                indent_level += 3
                indent_chars = f'\n{indent_char * (indent_level - 1)}|'
                for i, c in enumerate(groups):
                    if i > 0:
                        yield indent_chars
                    yield from pretty_branch(trie, c, indent_level)
                yield ')'
            elif len(groups) == 1:
                yield from pretty_branch(trie, groups[0], indent_level)

            if trie.get_eow() and len(groups) > 0:
                yield '?'

        def pretty_branch(trie, c, indent_level):
            ec = trie.escape(c)
            yield ec
            yield from pretty(trie[c], indent_level + len(ec))

        self.compress()
        indent = indent_level * indent_char
        regexp = ''.join(pretty(self, indent_level))
        return f'{indent}(?x)\n{indent}{regexp}'

    def compress(self):
        def merge_single_nodes(node, trie, old_key, new_key):
            branches = list(trie.branches())
            if len(branches) > 1 or trie.get_eow():
                if old_key != new_key:
                    node.pop(old_key)
                    node[new_key] = trie
                trie.compress()
            elif branches:
                char = branches[0]
                trie = trie[char]
                merge_single_nodes(node, trie, old_key, new_key + char)
        for char in list(self.branches()):
            merge_single_nodes(self, self[char], char, char)


if __name__ == '__main__':
    def print_regexp(caption, words):
        print('{0:=^79}'.format(caption))
        trie = RegexpTrie(words)
        trie.pprint()

    def test(words):
        import time
        trie = RegexpTrie(words)
        regexs = { 'Directly Join Words': '|'.join(map(re.escape, words))
                 , 'Compact Optimized': trie.regexp()
                 , 'Pretty Optimized': trie.pretty_regexp()}

        for title, regex in regexs.items():
            regexs[title] = re.compile(regex)

        print(f'\n{f"Test with {len(words)} words":=^61}')
        for title, regex in regexs.items():
            # Correctness
            match = all(regex.fullmatch(word).group() == word
                        for word in words)
            # Performance
            start = time.time()
            for i in range(10):
                for word in words:
                    regex.fullmatch(word).group()
            cost = time.time() - start
            print(f'{title:>20}: match: {match}, cost: {cost}')

    cmd = 'scheme -q --script regexp.ss'
    out = subprocess.check_output(cmd, shell=True, text=True)
    funs, defs, syns = [line.split(' ') for line in out.split('\n')]

    print_regexp('functions', funs)
    print_regexp('definitions', defs)
    print_regexp('syntaxes', syns)

    test(funs)
