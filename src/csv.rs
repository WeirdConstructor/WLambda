use crate::vval::VVal;

struct CSVParser {
    delim:   char,
    row_sep: String,
    data:    std::vec::Vec<char>,
    pos:     usize,

    cur_table: VVal,
    cur_row:   VVal,
}

impl CSVParser {
    pub fn new(delim: char, row_sep: &str) -> Self {
        Self {
            data: vec![],
            row_sep: row_sep.to_string(),
            delim,
            pos: 0,
            cur_table: VVal::Nul,
            cur_row: VVal::Nul,
        }
    }

    fn rest_len(&self) -> usize {
        if self.pos > self.data.len() {
            0
        } else {
            self.data.len() - self.pos
        }
    }

    fn skip_char(&mut self, count: usize) { self.pos += count; }

    fn check_char(&mut self, c: char) -> bool {
        if self.rest_len() <= 0 { return false; }
        self.next_char() == c
    }

    fn next_char(&mut self) -> char {
        if self.rest_len() <= 0 { return '\0'; }
        self.data[self.pos]
    }

    fn next_char_skip(&mut self) -> char {
        let c = self.next_char();
        self.skip_char(1);
        c
    }

    fn check_row_sep(&mut self) -> bool {
        let sep = &self.row_sep;
        if self.rest_len() < sep.len() { return false; }

        let mut i = 0;
        for c in sep.chars() {
            if self.data[self.pos + i] != c { return false; }
            i += 1;
        }

        true
    }

    fn on_field(&mut self, data: String) {
        self.cur_row.push(VVal::new_str_mv(data));
    }

    fn on_row_end(&mut self) {
        self.cur_table.push(
            std::mem::replace(&mut self.cur_row, VVal::vec()));
    }

    fn parse_escaped_field(&mut self) -> Result<bool, String> {
        let mut field_data = String::new();

        let mut end_found = false;

        while !end_found && self.rest_len() > 0 {
            if self.check_char('"') {
                self.skip_char(1);

                if self.check_char('"') {
                    self.skip_char(1);
                    field_data.push('"');
                } else {
                    end_found = true;
                }

            } else if self.check_char('\x0d') {
                self.skip_char(1);
                if self.check_char('\x0a') { self.skip_char(1); }
                field_data.push('\x0a');

            } else if self.check_char('\x0a') {
                self.skip_char(1);
                field_data.push('\x0a');

            } else {
                field_data.push(self.next_char_skip());
            }
        }

        if self.rest_len() <= 0 { end_found = true; }

        let mut row_end = false;

        if self.rest_len() <= 0
           || self.check_row_sep()
        {
            self.skip_char(self.row_sep.len());
            row_end = true;

        } else if self.check_char(self.delim) {
            self.skip_char(1);

        } else {
            return Err(
                format!(
                    "Runaway escaped field '\"', field data: [{}]",
                    field_data));
        }

        if end_found { self.on_field(field_data); }
        if row_end { self.on_row_end(); }

        Ok(end_found)
    }

    fn parse_delimited_field(&mut self) -> Result<bool, String> {
        let mut field_data = String::new();
        let mut end_found = false;
        let mut row_end   = false;

        while !end_found && self.rest_len() > 0 {
            if self.check_row_sep() {
                self.skip_char(self.row_sep.len());
                end_found = true;
                row_end   = true;

            } else if self.check_char(self.delim) {
                self.skip_char(1);
                end_found = true;
            } else {
                field_data.push(self.next_char_skip());
            }
        }

        if self.rest_len() <= 0 {
            end_found = true;
            row_end   = true;
        }

        if end_found { self.on_field(field_data); }
        if row_end   { self.on_row_end(); }

        Ok(end_found)
    }

    fn parse_field(&mut self) -> Result<bool, String> {
        if self.rest_len() < 1 { return Ok(false); }
        if self.next_char() == '"' {
            self.skip_char(1);
            self.parse_escaped_field()
        } else {
            self.parse_delimited_field()
        }
    }

    pub fn parse(&mut self, data: String) -> Result<VVal, String> {
        self.data      = data.chars().collect();
        self.pos       = 0;
        self.cur_table = VVal::vec();
        self.cur_row   = VVal::vec();

        while self.rest_len() > 0 {
            if !self.parse_field()? {
                return Err(format!("Couldn't find end."));
            }
        }

        Ok(std::mem::replace(&mut self.cur_table, VVal::Nul))
    }
}

pub fn parse_csv(delim: char, row_sep: &str, data: &str) -> Result<VVal, String> {
    let mut csvp = CSVParser::new(delim, row_sep);
    csvp.parse(data.to_string())
}

pub fn to_csv(delim: char, row_sep: &str, escape_all: bool, table: VVal) -> String {
    let mut ret = String::new();
    let need_escape_chars = format!("\t\r\n \"{}{}", delim, row_sep);

    for row in table.iter() {
        let mut first_field = true;

        for cell in row.iter() {
            if !first_field { ret.push(delim); }
            else { first_field = false; }

            let field = cell.s_raw();

            if escape_all
               || field.find(|c| need_escape_chars.find(c).is_some()).is_some()
            {
                ret.push('"');
                for c in field.chars() {
                    match c {
                        '"' => { ret += "\"\""; }
                        _   => { ret.push(c); }
                    }
                }
                ret.push('"');
            } else {
                ret += &field;
            }
        }

        ret += row_sep;
    }

    ret
}

//string to_csv(const VVal::VV &table, char sep, const string &row_sep, bool wrapAll)
//{
//    regex re("[\t\r\n " + string(&sep, 1) + "]");
//
//    stringstream ss;
//
//    for (auto row : *table)
//    {
//        bool first_field = true;
//        for (auto cell : *row)
//        {
//            if (!first_field) ss << sep;
//            else first_field = false;
//
//            string field = cell->s();
//
//            smatch match;
//            if (regex_search(field, match, re) || wrapAll)
//            {
//                stringstream field_out;
//                field_out << "\"";
//                for (auto c : field)
//                {
//                    if (c == '"') field_out << "\"\"";
//                    else          field_out << c;
//                }
//                field_out << "\"";
//                field = field_out.str();
//
//            }
//
//            ss << field;
//        }
//        ss << row_sep;
//    }
//
//    return ss.str();
//}
