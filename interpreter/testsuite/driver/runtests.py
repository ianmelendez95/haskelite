import os
import subprocess
import difflib


# ENVIRONMENT SETUP

if '__file__' not in globals() \
        or not __file__.endswith("runtests.py"):
    raise Exception("runtests.py meant to be run as script: python3 /path/to/runtests.py")

TESTSUITE_DIR = os.path.abspath(os.path.dirname(os.path.dirname(__file__)))
PROJECT_DIR = os.path.abspath(os.path.dirname(TESTSUITE_DIR))

# build the binary

subprocess.run(["cargo", "build", "--manifest-path", os.path.join(PROJECT_DIR, "Cargo.toml")], check=True)
BIN = os.path.join(PROJECT_DIR, "target/debug/interpreter")

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

        if ext == ".s":
            files_dict["in"] = os.path.join(dir_path, test_file)
        elif ext == ".out":
            files_dict["out"] = os.path.join(dir_path, test_file)

    return test_files


def eval_file(s_file):
    eval_proc = subprocess.run([BIN, s_file], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if eval_proc.returncode != 0:
        raise TestFailure("Stack code evaluation failed: file='{}' exit-code='{}' output:\n{}".format(s_file, eval_proc.returncode, eval_proc.stdout.decode("UTF-8")))

    return eval_proc.stdout.decode("UTF-8")


def exec_test(s_file, out_file):
    eval_res = eval_file(s_file).splitlines(keepends=True)

    with open(out_file, 'r') as f:
        expected_out = f.readlines()

    diff = list(difflib.unified_diff(expected_out, eval_res))

    if len(diff) != 0:
        raise TestFailure("Output did not match expected\n{}".format(''.join(diff)))


for n, fs in collect_test_files(os.path.join(TESTSUITE_DIR, "tests/should-succeed")).items():
    print("[{}] ".format(n), end='')

    if "in" not in fs:
        print("FAILED - missing stack code file")
    elif "out" not in fs:
        print("FAILED - missing output file")

    try:
        res = exec_test(fs["in"], fs["out"])
        print("PASSED")
    except TestFailure as tf:
        print("FAILED\n{}".format(tf.msg))


