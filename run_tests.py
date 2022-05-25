import argparse
from distutils import core
import pathlib
import sys
import subprocess
from typing import Tuple

PROGRAM_EXTENSION = '.hwt'
OUT_EXTENSION = '.out'

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Runs tests for Hawat Programming Language. Folder \'good\' should contain programs that run without failure. Folder \'bad\' should contain programs with syntax, typecheck or runtime errors. Both should contain files int the form of \'name.hwt\' and corresponding output \'name.out\'. ')

    parser.add_argument("--interpreter", help="Interpreter executable", default='interpreter')
    parser.add_argument("--test_folder", help="Path to folder containing 'good' and 'bad' folders", default=".")
    return parser.parse_args()


def get_file_content(file_path: pathlib.Path) -> str:
    with open(file_path, 'r') as f:
        text = f.read()
    
    return text

def check_file(in_file: pathlib.Path, interpreter: pathlib.Path) -> bool:
    interpreter_process = subprocess.run([str(interpreter.resolve()), str(in_file.resolve())], capture_output=True)

    print(f'Running {str(in_file):<40}', end='')
    out = interpreter_process.stdout.decode()
    err = interpreter_process.stderr.decode()

    out_to_check = out if out else err # The interpreter should only output on stderr or stdout, we can check only one

    out_file = in_file.with_suffix(OUT_EXTENSION)
    out_content = get_file_content(out_file)
    if out_to_check == out_content:
        print('Success')
        return True
    else:
        print(f'Failure. Expected {repr(out_content)}, got {repr(out_to_check)}.')
        return False

def run_folder(path: pathlib.Path, interpreter: pathlib.Path) -> Tuple[int, int]:
    correct = 0
    total = 0

    for file in path.iterdir():
        if file.suffix == PROGRAM_EXTENSION:
            total += 1
            correct += check_file(file, interpreter)

    return correct, total

def check_exists(folder: pathlib.Path) -> None:
    if not folder.exists():
        
        exit(1)

def main() -> None:
    args = parse_args()

    test_folder = pathlib.Path(args.test_folder)
    interpreter = pathlib.Path(args.interpreter)

    correct_good, total_good = run_folder(test_folder / 'good', interpreter)
    correct_bad, total_bad = run_folder(test_folder / 'bad', interpreter)

    print(f'Correct {correct_good + correct_bad} / {total_good + total_bad}')


if __name__ == "__main__":
    try:
        main()
    except FileNotFoundError as not_found:
        print(f'Directory/file \'{not_found.filename}\' does not exist', file=sys.stderr)