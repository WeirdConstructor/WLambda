mod xml;
pub mod csv;
mod net;
mod process;
mod http;
mod mqtt;
use super::compiler::*;

pub fn add_to_symtable(st: &mut SymbolTable) {
    net::add_to_symtable(st);
    xml::add_to_symtable(st);
    process::add_to_symtable(st);
    http::add_to_symtable(st);
    mqtt::add_to_symtable(st);
}
