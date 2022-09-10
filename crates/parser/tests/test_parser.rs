use parser::{ast::Statement, Parser};
use span::Spanned;

fn parse_string(string: &str) -> Vec<Spanned<Statement>> {
    let mut parser = Parser::new(string, "test.lt");
    let (ast, _) = parser.parse();

    ast
}

#[test]
fn parse_literals() {
    let test_str = r#"10 3.5 true false "hello world" 'c'"#;
    let ast = parse_string(test_str);

    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_idents() {
    let test_str = "foo _bar a123";
    let ast = parse_string(test_str);

    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_tuple() {
    let test_str = r"(1, 2, 3)";
    let ast = parse_string(test_str);

    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_array() {
    let test_str = "[1, 2, 3]";
    let ast = parse_string(test_str);

    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_unary() {
    let test_str = "!foo";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);

    let test_str = "-bar";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_binary() {
    let test_str = "5 + 5 - 3 / 2";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_call() {
    let test_str = "foo(a, b, c)";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_assignment() {
    let test_str = "a = 10";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_block() {
    let test_str = "
do
    println(10)
end
    ";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_if() {
    let test_str = r#"
if 1 == 1 do
    println("All is right with the world")
else if 1 == 2 do
    println("no its not??")
else do
    println("what the heck")
end
    "#;
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_for() {
    let test_str = "
for i in [1, 2, 3] do
    println(i)
end
    ";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_while() {
    let test_str = r#"
while 1 == 1 do
    println("All is right with the world")
end
    "#;
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_return() {
    let test_str = "return 10";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_import() {
    let test_str = "import foo";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_let() {
    let test_str = "
let foo = 10
let mut foo = 20
    ";
    let ast = parse_string(test_str);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_function() {
    let test_str = r#"
func foo(a: int, b: int) do
    return 10
end
    "#;
    let ast = parse_string(test_str);

    insta::assert_yaml_snapshot!(ast);
}
