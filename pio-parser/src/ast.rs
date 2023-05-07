use std::collections::HashMap;

use crate::parser::Span;

pub struct Symbol<'i> {
    public: bool,
    span: Span<'i>
}

pub enum Value<'i> {
    // the datasheet does not define bounds for the integers. The reference impl uses `int`.
    Integer(isize, Span<'i>),
    Identifier(Span<'i>)
}

pub enum Expression<'i> {
    Value(Value<'i>),
    Plus(Box<Expression<'i>>, Box<Expression<'i>>),
    Minus(Box<Expression<'i>>, Box<Expression<'i>>),
    Multiply(Box<Expression<'i>>, Box<Expression<'i>>),
    Divide(Box<Expression<'i>>, Box<Expression<'i>>),
    Or(Box<Expression<'i>>, Box<Expression<'i>>),
    And(Box<Expression<'i>>, Box<Expression<'i>>),
    Xor(Box<Expression<'i>>, Box<Expression<'i>>),
    Negative(Box<Expression<'i>>),
    Reverse(Box<Expression<'i>>),
}

pub struct GlobalDirective<'i> {
    v: Span<'i>
}

pub struct File<'i> {
    directive: HashMap<Span<'i>, Span<'i>>,
    programs: HashMap<Span<'i>, Program<'i>>
}

struct Program<'i> {
    instructions: Vec<Instruction<'i>>,
    // language -> code block
    // % lang {
    // …
    // %}
code_block: HashMap<Span<'i>, Span<'i>>
}

struct Instruction<'i> {
    line: Span<'i>
}

