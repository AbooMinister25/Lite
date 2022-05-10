use parser::Parser;

fn main() {
    let source = r#"(1, 2, 3) ("foo", "bar", "baz") (hello, hey, bye) ('a', 'b', 'c')"#;
    let mut parser = Parser::new(source, "test.lt");

    let (node, _) = parser.parse_expression(1).unwrap();
    println!("{}", node);
}
