import os
import re


_test_path = 'test'

def clean_test_generated_files(regexp=r".+\.ss"):
    print("begin clean files")

    regex = re.compile(regexp)
    for filename in os.listdir(_test_path):
        if regex.match(filename):
            continue

        filepath = os.path.join(_test_path, filename)
        os.remove(filepath)
        print('removed file: ' + filepath)

    print("finished clean files\n")


if __name__ == "__main__":
    clean_test_generated_files()
