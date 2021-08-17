import os
import subprocess
import difflib
import sys


# ENVIRONMENT SETUP

if '__file__' not in globals() \
        or not __file__.endswith("testsuite/driver/runtests.py"):
    raise Exception("runtests.py meant to be run as script: python3 /path/to/testsuite/driver/runtests.py")

TESTSUITE_DIR = os.path.abspath(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
PARENT_PROJECT_DIR = os.path.abspath(os.path.dirname(TESTSUITE_DIR))

COMPILER_PROJECT_DIR = os.path.join(PARENT_PROJECT_DIR, "compiler")
INTERPRETER_PROJECT_DIR = os.path.join(PARENT_PROJECT_DIR, "interpreter")


# EXECUTE TESTS


class TestFailure (Exception):
    def __init__(self, msg):
        self.msg = msg


def collect_test_files(dir_path):
    test_files = {}
    for test_file in os.listdir(dir_path):
        (name, ext) = os.path.splitext(os.path.basename(test_file))

        if name in test_files:
            files_dict = test_files[name]
        else:
            files_dict = {}
            test_files[name] = files_dict

        if ext == ".m":
            files_dict["in"] = os.path.join(dir_path, test_file)
        elif ext == ".out":
            files_dict["out"] = os.path.join(dir_path, test_file)

    return test_files


def compile_file(m_file):
    eval_proc = subprocess.run(["stack", "run", "--", m_file],
                               cwd=COMPILER_PROJECT_DIR,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if eval_proc.returncode != 0:
        raise TestFailure("Miranda compilation failed: file='{}' exit-code='{}' output:\n{}".format(m_file, eval_proc.returncode, eval_proc.stdout.decode("UTF-8")))

    (directory, filename) = os.path.split(m_file)
    return os.path.join(directory, os.path.splitext(filename)[0] + ".s")


def eval_file(s_file):
    eval_proc = subprocess.run(["cargo", "run", "--", s_file],
                               cwd=INTERPRETER_PROJECT_DIR,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if eval_proc.returncode != 0:
        raise TestFailure("Stack code evaluation failed: file='{}' exit-code='{}' output:\n{}".format(s_file, eval_proc.returncode, eval_proc.stdout.decode("UTF-8")))

    return eval_proc.stdout.decode("utf-8")


class TestResult (object):
    PASSED = "PASSED"
    FAILED = "FAILED"
    EXCLUDED = "EXCLUDED"

    def __init__(self, result, msg=''):
        self.result = result
        self.msg = msg

    def print(self, test_name):
        if sys.stdout.isatty():
            print(self._get_ansi_color_code(), end='')

        print("[{}] {}".format(test_name, self.result))
        if self.msg:
            print(self.msg)

        if sys.stdout.isatty():
            print('\033[0m', end='')  # reset ANSI color

    def _get_ansi_color_code(self):
        if self.result == TestResult.PASSED:
            return '\033[0;32m'  # green
        elif self.result == TestResult.FAILED:
            return '\033[0;31m'  # red
        elif self.result == TestResult.EXCLUDED:
            return '\033[0;33m'  # yellow


def exec_test(file_dict):
    if "in" not in file_dict:
        return TestResult(TestResult.FAILED, "missing input file")
    elif "out" not in file_dict:
        return TestResult(TestResult.FAILED, "missing output file")

    m_file = file_dict["in"]
    out_file = file_dict["out"]

    with open(m_file, 'r') as f:
        if f.readline().startswith("-- EXCLUDE"):
            return TestResult(TestResult.EXCLUDED)

    try:
        s_file = compile_file(m_file)
        eval_res = eval_file(s_file).splitlines(keepends=True)


        with open(out_file, 'r') as f:
            expected_out = f.readlines()

        diff = list(difflib.unified_diff(expected_out, eval_res, fromfile=s_file, tofile=out_file, lineterm='\n'))

        if len(diff) != 0:
            return TestResult(TestResult.FAILED, "Output did not match expected\n{}".format('\n'.join(diff)))

        return TestResult(TestResult.PASSED)
    except TestFailure as tf:
        return TestResult(TestResult.FAILED, tf.msg)


if __name__ == '__main__':
    for n, fs in collect_test_files(os.path.join(TESTSUITE_DIR, "tests/should-succeed")).items():
        exec_test(fs).print(n)
