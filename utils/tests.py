import subprocess
from pathlib import Path

from rich import print as rprint


class Test:
    def __init__(self, name: str, path: str, expect_type: str, values: list[str]):
        self.name = name
        self.path = path
        self.expect_type = expect_type
        self.values = values
        self.flag_argument = (
            "--tokenize" if self.expect_type == "tokens" else "--output-ast"
        )

    def check(self):
        p = subprocess.run(
            [
                "cargo",
                "run",
                "-p",
                "main",
                "--",
                self.flag_argument,
                self.path,
            ],
            capture_output=True,
        )
        output = p.stdout.decode("utf-8")

        values = output.splitlines()
        passed = values == self.values

        if passed:
            rprint(f" [green]passed[/green]")
        else:
            rprint(f" [red]failed[/red]")
            print(f"expected: {self.values}\nfound: {values}")


def parse_test(content: str, path: Path) -> list[Test]:
    tests: list[Test] = []
    test_comments = [
        line.replace("//", "").split()
        for line in content.splitlines()
        if line.startswith("//")
    ]

    for comment in test_comments:
        expect_type = comment[1]

        # todo: Probably refactor this into a for loop or something later :p
        values = [
            f"{value} {comment[2:][idx + 1]}"
            if "(" in value and ")" not in value
            else value
            for idx, value in enumerate(comment[2:])
            if not (")" in value and not "(" in value)
        ]

        tests.append(
            Test(
                path.stem,
                f"{path.parent if path.parent != 'tests' else ''}/{path.name}",
                expect_type,
                values,
            )
        )

    return tests


def read_tests() -> list[Test]:
    path = Path.cwd() / "tests"
    tests: list[Test] = []

    for test_file in path.rglob("*.lite"):
        with open(test_file, "r") as f:
            content = f.read()
            tests.extend(parse_test(content, test_file))

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
