import subprocess
from pathlib import Path

from rich import print as rprint


class Test:
    def __init__(
        self,
        name: str,
        path: str,
        tests: list[tuple[str, str]],
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

            values = " ".join(
                [i for i in output.splitlines() if i != "Newline"]
            ).strip()

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
    expected: list[tuple[str, str]] = []

    for comment in comments:
        values = comment.split(":")[1].strip()

        if values.startswith("parse"):
            test_type = "parse"
            values = values.removeprefix("parse").strip()
        else:
            test_type = "tokens"
            values = values.removeprefix("tokens").strip()

        expected.append((test_type, values))

    return Test(
        path.stem,
        f"{path.parent if path.parent != 'tests' else ''}/{path.name}",
        expected,
    )


def read_tests() -> list[Test]:
    path = Path("../tests")
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
