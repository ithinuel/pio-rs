use nom;
use pio::ProgramWithDefines;
use std::collections::HashMap;


pub fn program_parser<const PROGRAM_SIZE: usize>(_input: &str) -> nom::IResult<&str, ProgramWithDefines<HashMap<String, i32>, PROGRAM_SIZE>> {
    todo!()
}

