import os
import re
import subprocess
from contextlib import contextmanager

from pretty import pretty
from minimize import minimize

@contextmanager
def cd(path):
    cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(cwd)

unix = lambda path: path.replace('\\', '/')
testdir = unix(os.path.abspath('../.scheme'))

def scheme_pretty_file(i, o):
    cmd = f'echo (pretty-file "{i}" "{o}") | scheme -q'
    out = subprocess.call(cmd, shell=True,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)

def validate_pretty(source_file, pretty_file):
    pretty_source_file = "{}.1.ss".format(source_file[:-3])
    pretty_pretty_file = "{}.2.ss".format(source_file[:-3])
    scheme_pretty_file(source_file, pretty_source_file)
    scheme_pretty_file(pretty_file, pretty_pretty_file)

    if os.path.exists(pretty_source_file):
        size1 = os.path.getsize(pretty_source_file)
        if os.path.exists(pretty_pretty_file):
            size2 = os.path.getsize(pretty_pretty_file)
            return size1 == size2
    return False

def run_validity_test(encoder, subfix, **kwargs):
    print("begin {} test...".format(encoder.__name__))
    npass, nfile = 0, 0
    for file in os.listdir():
        if (not file.endswith(".ss") or
            file.count('.') > 1):
            continue
        nfile += 1

        base, ext = os.path.splitext(file)
        pretty_file = "{}.{}.ss".format(base, subfix)
        print('  pretty...', end='')
        with open(file, 'r', encoding='utf-8') as infile:
            code = infile.read()
            code = encoder(code, **kwargs)
            with open(pretty_file, "w+", encoding='utf-8') as outfile:
                outfile.write(code)
                print('done', end='! ')

        print('validate...', end='')
        succeed = validate_pretty(file, pretty_file)
        npass += succeed
        print(["fail!", "pass!"][succeed], end=', ')
        print("file: ", file)

    print("finished {} test with {} pass, {} fail.\n".format(
        encoder.__name__, npass, nfile - npass))


def remove_files_with_pattern(wd, regexp=r'.+\.(\d|pretty|min)\.ss'):
    print("begin remove files:")
    print('  workdir:', wd)
    with cd(wd):
        regex = re.compile(regexp)
        for filename in os.listdir():
            if regex.match(filename):
                os.remove(filename)
                print('  removed: ' + filename)
    print("remove files done!\n")


if __name__ == '__main__':
    # run_simple_test()
    with cd(testdir):
        run_validity_test(pretty, "pretty")
        # run_validity_test(minimize, "min")
        # run_validity_test(minimize, "min", sep='\n', remove_datum_comment=True)
    remove_files_with_pattern(testdir)
    try:
        import shutil
        shutil.rmtree("__pycache__")
        print("removed __pycache__")
    except:
        pass


def pprint(code):
    print(pretty(code))

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
