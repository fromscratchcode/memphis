use crate::{
    lexer::Token,
    parser::{
        types::{Param, Params},
        Parser, ParserError,
    },
};

impl Parser<'_> {
    pub fn parse_function_def_args(&mut self, end_token: Token) -> Result<Params, ParserError> {
        let mut args = Vec::new();
        let mut args_var = None;
        let mut kwargs_var = None;
        while self.current_token() != &end_token {
            // This is to support positional-only parameters.
            // Context: PEP 570 (https://peps.python.org/pep-0570/)
            // TODO test positional-only parameters now that we support args/kwargs
            if self.current_token() == &Token::Slash {
                self.consume(&Token::Slash)?;

                // We will only see a comma if the slash isn't the last "parameter".
                // We test this is the "slash_args" interpreter test. This is also found in
                // types.py in the standard lib.
                if self.current_token() == &Token::Comma {
                    self.consume(&Token::Comma)?;
                } else {
                    break;
                }
            }

            if self.current_token() == &Token::Asterisk {
                self.consume(&Token::Asterisk)?;

                // We will see an asterisk without a trailing identifier for keyword-only
                // parameters. TODO we do not yet enforce this.
                // Context: PEP 3102 (https://peps.python.org/pep-3102/)
                if matches!(self.current_token(), Token::Identifier(_)) {
                    args_var = Some(self.parse_identifier()?);
                }

                // If *args is not at the end of the args (only kwargs can come after), we must
                // allow for a comma. This is similar to how we optionally consume a comma as the
                // last step of each loop iteration.
                self.consume_optional(&Token::Comma);
                continue;
            }

            if self.current_token() == &Token::DoubleAsterisk {
                self.consume(&Token::DoubleAsterisk)?;
                kwargs_var = Some(self.parse_identifier()?);
                break;
            }

            let arg = self.parse_identifier()?;
            let default = if self.current_token() == &Token::Assign {
                self.consume(&Token::Assign)?;
                Some(self.parse_simple_expr()?)
            } else {
                None
            };

            args.push(Param { arg, default });

            // Support for type hints. Will there be reason to store these alongside the params
            // themselves? Perhaps for future toolings like memphis-lsp.
            //
            // Not sure if the check for end_token here is correct or not. This is for lambdas.
            if end_token != Token::Colon && self.current_token() == &Token::Colon {
                self.consume(&Token::Colon)?;
                let _type = self.parse_simple_expr()?;
            }

            self.consume_optional(&Token::Comma);
        }

        Ok(Params {
            args,
            args_var,
            kwargs_var,
        })
    }
}
