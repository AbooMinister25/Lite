from pathlib import Path
from dataclasses import dataclass


@dataclass
class Test:
    name: str
    take: str
    then: str


def parse_tests(content: str) -> list[Test]:
    tests: list[Test] = []
    items = content.split(";")

    for item in items:
        if item in ("", "\n"):
            break

        name, _, content = item.partition(":")
        take, then = content.split(",")

        test = Test(name.strip(), take.strip(), then.strip())
        tests.append(test)

    return tests


def read_tests() -> list[Test]:
    cwd = Path.cwd()
    tests: list[Test] = []

    for test_file in cwd.glob("tests/test_*"):
        with open(test_file, "r") as f:
            content = f.read()
            tests.extend(parse_tests(content))

    return tests


def write_tests(tests: list[Test]):
    cwd = Path.cwd()
    test_files = [f"test_{i.name.split('_')[1]}" for i in cwd.glob("tests/test_*")]


if __name__ == "__main__":
    tests = read_tests()

    print(tests)
