use parser::{ast::Statement, Parser};
use span::Spanned;

fn parse_string(string: &str) -> Vec<Spanned<Statement>> {
    let mut parser = Parser::new(string, "test.lt");
    let (ast, _) = parser.parse();

    ast
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
