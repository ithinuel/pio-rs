// match {
//   r"[\t\f\v ]" => {}, // ignore whitespace
//   r"(;|//)[^\n\r]*" => {}, // ignore `;` and `//` comments
//   r"/\*(\*[^/]|[^\*])*\*?\*/" => {}, // ignore /* comments */
//   r"\r?\n" => NEWLINE,
//   r"\.lang_opt [^\n\r]*" => LANG_OPT, // lex `.lang_opt` directives
// } else {
//   _
// }

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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Logos)]
#[logos(error = Error)]
pub enum CodeBlock<'i> {
    #[regex("[ \t\r]+")]
    Blank,

    #[token("\n")]
    NewLine,

    #[regex(r"%\}[ \t\r]*")]
    EndOfBlock,

    #[regex(".*")]
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

    #[regex(r"[^\*\n]*")]
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

    #[regex("[^ \t\r\n\"=]+")]
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
#[logos(skip r"(?&comment)")]
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

    #[token(".program")]
    Program,
    #[token(".wrap_target")]
    WrapTarget,
    #[token(".wrap")]
    Wrap,
    #[token(".word")]
    Word,
    #[token(".define")]
    Define,
    #[token(".side_set")]
    SideSet,
    #[token(".origin")]
    Origin,
    #[token(".lang_opt")]
    LangOpt,
    #[regex(r"\.(?&id)")]
    UnknownDirective,

    #[token("JMP", ignore(ascii_case))]
    Jmp,
    #[token("WAIT", ignore(ascii_case))]
    Wait,
    #[token("IN", ignore(ascii_case))]
    In,
    #[token("OUT", ignore(ascii_case))]
    Out,
    #[token("PUSH", ignore(ascii_case))]
    Push,
    #[token("PULL", ignore(ascii_case))]
    Pull,
    #[token("MOV", ignore(ascii_case))]
    Mov,
    #[token("IRQ", ignore(ascii_case))]
    Irq,
    #[token("SET", ignore(ascii_case))]
    Set,
    #[token("NOP", ignore(ascii_case))]
    Nop,

    #[token("PUBLIC", ignore(ascii_case))]
    Public,

    #[token("OPTIONAL", ignore(ascii_case))]
    #[token("OPT", ignore(ascii_case))]
    Optional,
    #[token("SIDE", ignore(ascii_case))]
    #[token("SIDESET", ignore(ascii_case))]
    #[token("SIDE_SET", ignore(ascii_case))]
    Side,
    #[token("PIN", ignore(ascii_case))]
    Pin,
    #[token("GPIO", ignore(ascii_case))]
    Gpio,
    #[token("OSRE", ignore(ascii_case))]
    Osre,

    #[token("PINS", ignore(ascii_case))]
    Pins,
    #[token("NULL", ignore(ascii_case))]
    Null,
    #[token("PINDIRS", ignore(ascii_case))]
    Pindirs,
    #[token("X", ignore(ascii_case))]
    X,
    #[token("Y", ignore(ascii_case))]
    Y,
    #[token("PC", ignore(ascii_case))]
    Pc,
    #[token("EXEC", ignore(ascii_case))]
    Exec,
    #[token("ISR", ignore(ascii_case))]
    Isr,
    #[token("OSR", ignore(ascii_case))]
    Osr,
    #[token("STATUS", ignore(ascii_case))]
    Status,

    #[token("BLOCK", ignore(ascii_case))]
    Block,
    #[token("NOBLOCK", ignore(ascii_case))]
    NoBlock,
    #[token("IFFULL", ignore(ascii_case))]
    IfFull,
    #[token("IFEMPTY", ignore(ascii_case))]
    IfEmpty,
    #[token("REL", ignore(ascii_case))]
    Rel,

    #[token("CLEAR", ignore(ascii_case))]
    Clear,
    #[token("NOWAIT", ignore(ascii_case))]
    NoWait,

    #[regex("0[bB][01]+", |lex| lex.slice().get(2..).map(|s| i32::from_str_radix(s, 2)).unwrap())]
    #[regex("[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    #[regex("0[xX][0-9a-fA-F]+", |lex| lex.slice().get(2..).map(|s| i32::from_str_radix(s, 16)).unwrap())]
    #[token("ONE", |_| { Ok::<_,Error>(1) }, ignore(ascii_case))]
    #[token("ZERO", |_| { Ok::<_,Error>(0) }, ignore(ascii_case))]
    Int(i32),

    #[regex("(?&id)")]
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
                            self.mode = Modes::CodeBlock(outer.to_owned().morph(), span.end);
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
                            self.mode = Modes::LangOpt(outer.to_owned().morph().spanned())
                        }
                        Ok(Token::CCommentStart) => {
                            let mut ccomment = outer.to_owned().morph();
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
                    // - consune %} but do not include it in the codeblock content
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
                        self.mode = Modes::Outer(lang_opt.to_owned().morph().spanned());
                        Ok((end, Token::EndOfFile, end))
                    }
                    Some((Ok(t), span)) => {
                        let t = match t {
                            LangOpt::String(s) => Token::String(s),
                            LangOpt::Equal => Token::Equal,
                            LangOpt::Int(s) => Token::LangOptInt(s),
                            LangOpt::NonWS(s) => Token::NonWS(s),
                            LangOpt::NewLine => {
                                self.mode = Modes::Outer(lang_opt.to_owned().morph().spanned());
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
