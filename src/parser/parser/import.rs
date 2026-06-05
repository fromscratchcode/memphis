use crate::{
    domain::{FromImportPath, Identifier, ModulePath},
    lexer::Token,
    parser::{Parser, ParserResult},
};

impl Parser<'_> {
    pub fn parse_module_path(&mut self) -> ParserResult<ModulePath> {
        if !matches!(self.current_token(), Token::Identifier(_)) {
            return Ok(ModulePath::default());
        }

        let mut path = vec![self.parse_identifier()?];
        while self.current_token() == &Token::Dot {
            self.consume(&Token::Dot)?;
            path.push(self.parse_identifier()?);
        }
        Ok(ModulePath::new(path))
    }

    pub fn parse_import_path(&mut self) -> ParserResult<FromImportPath> {
        match self.current_token() {
            Token::Dot => {
                let mut levels = 0;
                while self.current_token() == &Token::Dot {
                    self.consume(&Token::Dot)?;
                    levels += 1;
                }

                let path = self.parse_module_path()?;
                Ok(FromImportPath::Relative(levels, path))
            }
            _ => {
                let path = self.parse_module_path()?;
                assert!(!path.segments().is_empty());
                Ok(FromImportPath::Absolute(path))
            }
        }
    }

    pub fn parse_optional_alias(&mut self) -> ParserResult<Option<Identifier>> {
        if self.current_token() == &Token::As {
            self.consume(&Token::As)?;
            let alias = self.parse_identifier()?;
            Ok(Some(alias))
        } else {
            Ok(None)
        }
    }
}
