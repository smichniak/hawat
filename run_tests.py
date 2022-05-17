import argparse
from http.client import PROCESSING
import pathlib
import sys
import subprocess

PROGRAM_EXTENSION = '.hwt'
OUT_EXTENSION = '.out'

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Runs tests for Hawat Programming Language. Folder \'good\' should contain programs that run without failure. Folder \'bad\' should contain programs with syntax, typecheck or runtime errors. Both should contain files int the form of \'name.hwt\' and corresponding output \'name.out\'. ')

    parser.add_argument("--interpreter", help="Interpreter executable", default='interpreter')
    parser.add_argument("--test_folder", help="Path to folder containing 'good' and 'bad' folders", default=".")
    return parser.parse_args()


def check_file(file_path: pathlib.Path, interpreter: pathlib.Path):
    out_file = file_path.with_suffix(OUT_EXTENSION)
    interpreter_process = subprocess.run([str(interpreter.resolve()), str(file_path.resolve())])

    print(file_path)
    print(interpreter_process.stderr)
    print(interpreter_process.stdout)


def run_folder(path: pathlib.Path, interpreter: pathlib.Path):
    check_exists(path)

    for file in path.iterdir():
        if file.suffix == PROGRAM_EXTENSION:
            check_file(file, interpreter)


def check_exists(folder: pathlib.Path):
    if not folder.exists():
        print(f'Directory/file \'{folder}\' does not exist', file=sys.stderr)
        exit(1)


def main() -> None:
    args = parse_args()

    test_folder = pathlib.Path(args.test_folder)
    check_exists(test_folder)

    interpreter = pathlib.Path(args.interpreter)
    check_exists(interpreter)

    run_folder(test_folder / 'good', interpreter)
    run_folder(test_folder / 'bad', interpreter)


if __name__ == "__main__":
    main()