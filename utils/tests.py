import subprocess
from pathlib import Path

from rich import print as rprint


class Test:
    def __init__(
        self,
        name: str,
        path: str,
        tests: list[tuple[str, list[str]]],
    ):
        self.name = name
        self.path = path
        self.tests = tests

    def check(self):
        for test in self.tests:
            flag_argument = "--tokenize" if test[0] == "tokens" else "--output-ast"
            p = subprocess.run(
                [
                    "cargo",
                    "run",
                    "-p",
                    "main",
                    "--",
                    flag_argument,
                    self.path,
                ],
                capture_output=True,
            )
            output = p.stdout.decode("utf-8")

            values = output.splitlines()
            passed = values == test[1]

            if passed:
                rprint(f" [green]passed[/green]")
            else:
                rprint(f" [red]failed[/red]")
                print(f"expected: {test[1]}\nfound: {values}")


def parse_test(content: str, path: Path) -> Test:
    comments = [
        line.replace("//", "") for line in content.splitlines() if line.startswith("//")
    ]
    expected: list[tuple[str, list[str]]] = []

    for comment in comments:
        values: list[str] = []
        pos = 0

        while pos < len(comment):
            c = comment[pos]
            if c.isidentifier():
                acc = c

                while pos + 1 < len(comment) and comment[pos + 1].isidentifier():
                    pos += 1
                    acc += comment[pos]

                if pos + 1 < len(comment) and comment[pos + 1] in ("(", "["):
                    pos += 1
                    value = comment[pos]

                    while comment[pos] not in (")", "]"):
                        pos += 1
                        value += comment[pos]

                        if comment[pos] in ("(", "["):
                            while comment[pos] not in (")", "]"):
                                pos += 1
                                value += comment[pos]

                            pos += 1
                            value += comment[pos]

                    acc += value

                values.append(acc)

            pos += 1

        expect_type = values[1]
        expected.append((expect_type, values[2:]))

    return Test(
        path.stem,
        f"{path.parent if path.parent != 'tests' else ''}/{path.name}",
        expected,
    )


def read_tests() -> list[Test]:
    path = Path.cwd() / "tests"
    tests: list[Test] = []

    for test_file in path.rglob("*.lite"):
        with open(test_file, "r") as f:
            content = f.read()
            tests.append(parse_test(content, test_file))

    return tests


def run_tests(tests: list[Test]):
    rprint("[bold green]Running[/bold green] tests")
    print("========================")
    for test in tests:
        rprint(
            f"[bold green]Running[/bold green] test [bold]{test.name}[/bold]", end=""
        )
        test.check()


if __name__ == "__main__":
    tests = read_tests()
    run_tests(tests)
