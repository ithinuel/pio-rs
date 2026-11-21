use std::num::ParseIntError;

use logos::Logos;

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Error {
    InvalidToken(logos::Span),
    IncompleteBlockComment,
    ParseInt(ParseIntError),
    #[default]
    Unknown,
}
impl From<ParseIntError> for Error {
    fn from(value: ParseIntError) -> Self {
        Error::ParseInt(value)
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidToken(span) => write!(f, "invalid token at {span:?}"),
            Error::IncompleteBlockComment => write!(f, "incomplete block comment"),
            Error::ParseInt(e) => write!(f, "{e}"),
            Error::Unknown => write!(f, "unknown token"),
        }
    }
}
impl std::error::Error for Error {}

fn parse_one<'i>(_: &mut logos::Lexer<'i, Token<'i>>) -> Result<i32, Error> {
    Ok(1)
}
fn parse_zero<'i>(_: &mut logos::Lexer<'i, Token<'i>>) -> Result<i32, Error> {
    Ok(0)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Logos)]
#[logos(error = Error)]
pub enum CodeBlock<'i> {
    #[regex("[ \t\r]+")]
    Blank,

    #[token("\n")]
    NewLine,

    #[regex(r"%\}[ \t\r]*")]
    EndOfBlock,

    #[regex("[^\n]+", priority = 1, allow_greedy = true)]
    ContentLine(&'i str),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Logos)]
#[logos(error = Error)]
pub enum CComment {
    #[regex("[ \t\r]+")]
    Blank,

    #[token("*/")]
    EndOfBlock,

    #[token("*")]
    Star,

    #[regex(r"[^\*\n]+", priority = 1)]
    NotStar,

    #[token("\n")]
    NewLine,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Logos)]
#[logos(error = Error)]
#[logos(skip "[ \t\r]")]
pub enum LangOpt<'i> {
    #[regex("\"[^\n\"]*\"")]
    String(&'i str),

    #[token("=")]
    Equal,

    #[regex("0[bB][01]+", |lex| lex.slice().get(2..).map(|s| i32::from_str_radix(s, 2)).map(|_| lex.slice()))]
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().map(|_| lex.slice()), priority = 2)]
    #[regex("0[xX][0-9a-fA-F]+", |lex| lex.slice().get(2..).map(|s| i32::from_str_radix(s, 16)).map(|_| lex.slice()))]
    Int(&'i str),

    #[regex("[^ \t\r\n\"=]+", priority = 1)]
    NonWS(&'i str),

    #[regex("\n+")]
    NewLine,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Logos)]
#[logos(error = Error)]
#[logos(subpattern blank = r"[ \t\r]")]
#[logos(subpattern id = r"[a-zA-Z_][a-zA-Z0-9_]*")]
#[logos(subpattern output_fmt = r"[^(?&blank)]+")]
#[logos(subpattern comment = r"(;|//)[^\n]*")]
#[logos(skip r"(?&blank)+")]
#[logos(skip(r"(?&comment)", allow_greedy = true))]
pub enum Token<'i> {
    #[token("\n")]
    NewLine,
    #[regex(r"%(?&blank)*(?&output_fmt)(?&blank)*\{")]
    CodeBlockStart(&'i str),
    #[token("/*")]
    CCommentStart,
    #[token(",")]
    Comma,
    #[token("::")]
    Reverse,
    #[token(":")]
    Colon,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("+")]
    Plus,
    #[token("--")]
    #[token("−−")]
    Decrement,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("|")]
    Or,
    #[token("&")]
    And,
    #[token("^")]
    Xor,
    #[token("!=")]
    NotEqual,
    #[token("!")]
    #[token("~")]
    Not,

    #[token(".program", ignore(case))]
    Program,
    #[token(".wrap_target", ignore(case))]
    WrapTarget,
    #[token(".wrap", ignore(case))]
    Wrap,
    #[token(".word", ignore(case))]
    Word,
    #[token(".define", ignore(case))]
    Define,
    #[token(".side_set", ignore(case))]
    SideSet,
    #[token(".origin", ignore(case))]
    Origin,
    #[token(".lang_opt", ignore(case))]
    LangOpt,
    #[regex(r"\.(?&id)")]
    UnknownDirective,

    #[token("JMP", ignore(case))]
    Jmp,
    #[token("WAIT", ignore(case))]
    Wait,
    #[token("IN", ignore(case))]
    In,
    #[token("OUT", ignore(case))]
    Out,
    #[token("PUSH", ignore(case))]
    Push,
    #[token("PULL", ignore(case))]
    Pull,
    #[token("MOV", ignore(case))]
    Mov,
    #[token("IRQ", ignore(case))]
    Irq,
    #[token("SET", ignore(case))]
    Set,
    #[token("NOP", ignore(case))]
    Nop,

    #[token("PUBLIC", ignore(case))]
    Public,

    #[token("OPTIONAL", ignore(case))]
    #[token("OPT", ignore(case))]
    Optional,
    #[token("SIDE", ignore(case))]
    #[token("SIDESET", ignore(case))]
    #[token("SIDE_SET", ignore(case))]
    Side,
    #[token("PIN", ignore(case))]
    Pin,
    #[token("GPIO", ignore(case))]
    Gpio,
    #[token("OSRE", ignore(case))]
    Osre,
    #[token("JMPPIN", ignore(case))]
    Jmppin,
    #[token("PREV", ignore(case))]
    Prev,
    #[token("NEXT", ignore(case))]
    Next,

    #[token("RXFIFO[Y]", ignore(case))]
    RxFifoY,
    #[token("RXFIFO[0]", ignore(case))]
    RxFifo0,
    #[token("RXFIFO[1]", ignore(case))]
    RxFifo1,
    #[token("RXFIFO[2]", ignore(case))]
    RxFifo2,
    #[token("RXFIFO[3]", ignore(case))]
    RxFifo3,

    #[token("PINS", ignore(case))]
    Pins,
    #[token("NULL", ignore(case))]
    Null,
    #[token("PINDIRS", ignore(case))]
    Pindirs,
    #[token("X", ignore(case))]
    X,
    #[token("Y", ignore(case))]
    Y,
    #[token("PC", ignore(case))]
    Pc,
    #[token("EXEC", ignore(case))]
    Exec,
    #[token("ISR", ignore(case))]
    Isr,
    #[token("OSR", ignore(case))]
    Osr,
    #[token("STATUS", ignore(case))]
    Status,

    #[token("BLOCK", ignore(case))]
    Block,
    #[token("NOBLOCK", ignore(case))]
    NoBlock,
    #[token("IFFULL", ignore(case))]
    IfFull,
    #[token("IFEMPTY", ignore(case))]
    IfEmpty,
    #[token("REL", ignore(case))]
    Rel,

    #[token("CLEAR", ignore(case))]
    Clear,
    #[token("NOWAIT", ignore(case))]
    NoWait,

    #[regex("0[bB][01]+", |lex| i32::from_str_radix(&lex.slice()[2..], 2).map_err(Error::ParseInt))]
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().map_err(Error::ParseInt))]
    #[regex("0[xX][0-9a-fA-F]+", |lex| i32::from_str_radix(&lex.slice()[2..], 16).map_err(Error::ParseInt))]
    #[token("ONE", parse_one, ignore(case))]
    #[token("ZERO", parse_zero, ignore(case))]
    Int(i32),

    #[regex("(?&id)", priority = 1)]
    Id(&'i str),

    // lang opt tokens
    String(&'i str),
    Equal,
    LangOptInt(&'i str),
    NonWS(&'i str),

    // other tokens
    CodeBlockContent(&'i str),
    EndOfFile,
}

enum Modes<'i> {
    Outer(logos::SpannedIter<'i, Token<'i>>),
    CodeBlock(logos::Lexer<'i, CodeBlock<'i>>, usize),
    LangOpt(logos::SpannedIter<'i, LangOpt<'i>>),
}

pub struct Lexer<'i> {
    mode: Modes<'i>,
    reached_end: bool,
}
impl<'i> Lexer<'i> {
    pub fn new(s: &'i str) -> Self {
        Self {
            mode: Modes::Outer(Token::lexer(s).spanned()),
            reached_end: false,
        }
    }
}

// Clones as we switch between modes
impl<'i> Iterator for Lexer<'i> {
    type Item = Result<(usize, Token<'i>, usize), Error>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            break match &mut self.mode {
                Modes::Outer(outer) => {
                    let (mut res, mut span) = match outer.next() {
                        Some(s) => s,
                        None if !self.reached_end => {
                            self.reached_end = true;
                            let end = outer.span().end;
                            return Some(Ok((end, Token::EndOfFile, end)));
                        }
                        None => return None,
                    };
                    match res {
                        Ok(Token::CodeBlockStart(token)) => {
                            self.mode =
                                Modes::CodeBlock(logos::Lexer::clone(&**outer).morph(), span.end);
                            let mut iterator = token
                                .chars()
                                .enumerate()
                                .skip(1)
                                .skip_while(|(_, c)| [' ', '\t'].contains(c));
                            let start = iterator.next().unwrap().0;
                            let end = iterator
                                .find(|(_, c)| [' ', '\t', '{'].contains(c))
                                .unwrap()
                                .0;

                            res = Ok(Token::CodeBlockStart(&token[start..end]));
                            span = start..end;
                        }
                        Ok(Token::LangOpt) => {
                            self.mode =
                                Modes::LangOpt(logos::Lexer::clone(&**outer).morph().spanned())
                        }
                        Ok(Token::CCommentStart) => {
                            let mut ccomment = logos::Lexer::clone(&**outer).morph();
                            let mut last;
                            loop {
                                last = ccomment.next()?;
                                if let Ok(CComment::EndOfBlock) = last {
                                    break;
                                }
                            }
                            self.mode = Modes::Outer(ccomment.morph().spanned());
                            if last != Ok(CComment::EndOfBlock) {
                                return Some(Err(Error::IncompleteBlockComment));
                            }
                            // go fetch another token
                            continue;
                        }
                        _ => {}
                    }
                    Some(res.map(|t| (span.start, t, span.end)))
                }
                Modes::CodeBlock(codeblock, start) => {
                    for res in &mut *codeblock {
                        match res {
                            Ok(CodeBlock::EndOfBlock) => break,
                            Err(e) => return Some(Err(e)),
                            _ => {}
                        };
                    }
                    // - consume %} but do not include it in the codeblock content
                    // - exclude the preceding \n from the codeblock too.
                    let start = *start;
                    let end = codeblock.span().start - 1;
                    let res = codeblock
                        .source()
                        .get(start..end)
                        .map(|s| Ok((start, Token::CodeBlockContent(s), end)));

                    self.mode = Modes::Outer(codeblock.to_owned().morph().spanned());
                    res
                }
                Modes::LangOpt(lang_opt) => Some(match lang_opt.next() {
                    None => {
                        self.reached_end = true;
                        let end = lang_opt.span().end;
                        self.mode =
                            Modes::Outer(logos::Lexer::clone(&**lang_opt).morph().spanned());
                        Ok((end, Token::EndOfFile, end))
                    }
                    Some((Ok(t), span)) => {
                        let t = match t {
                            LangOpt::String(s) => Token::String(s),
                            LangOpt::Equal => Token::Equal,
                            LangOpt::Int(s) => Token::LangOptInt(s),
                            LangOpt::NonWS(s) => Token::NonWS(s),
                            LangOpt::NewLine => {
                                self.mode = Modes::Outer(
                                    logos::Lexer::clone(&**lang_opt).morph().spanned(),
                                );
                                Token::NewLine
                            }
                        };
                        Ok((span.start, t, span.end))
                    }
                    Some((Err(e), _)) => Err(e),
                }),
            };
        }
    }
}

#[cfg(test)]
mod test {
    use super::Lexer;
    use super::Token::*;

    #[test]
    fn program_directive() {
        let l: Vec<_> = Lexer::new(".program hello_world").collect();
        assert_eq!(
            &l,
            &[
                Ok((0, Program, 8)),
                Ok((9, Id("hello_world"), 20)),
                Ok((20, EndOfFile, 20))
            ]
        );
    }
    #[test]
    fn multiple_lines() {
        let l: Vec<_> = Lexer::new(".program hello_world\npublic label: nop").collect();
        assert_eq!(
            &l,
            &[
                Ok((0, Program, 8)),
                Ok((9, Id("hello_world"), 20)),
                Ok((20, NewLine, 21)),
                Ok((21, Public, 27)),
                Ok((28, Id("label"), 33)),
                Ok((33, Colon, 34)),
                Ok((35, Nop, 38)),
                Ok((38, EndOfFile, 38))
            ]
        );
    }
}
