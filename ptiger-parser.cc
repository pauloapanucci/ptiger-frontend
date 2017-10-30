#include "ptiger-lexer.h"

namespace Ptiger {


struct Parser {
public:
    Parser(Lexer &lexer_) : lexer(lexer_) {}

    void parse_program();
    void parse_statements();
    bool done_end_of_file();
    void parse_statement_seq(bool (Parser::*done) ())

private:
    Lexer &lexer;
};

bool Parser::done_end_of_file () {
    const_TokenPtr t = lexer.peek_token();
    return (t->get_id() == Ptiger::END_OF_FILE);
}

void Parser::parse_program() {
    parse_statement_seq(&Parser::done_end_of_file);
}

void Parser::parse_statement_seq(bool (Parser::*done) ()) {
    while (this->*done) {
        parse_statement();
    }
}

void Parser::parse_statements() {
    const_TokenPtr  t = lexer.peek_token();
    switch (t->get_id ()){
        case Ptiger::TYPE:
            parse_type_declaration ();
            break;
        case Ptiger::VAR:
            parse_variable_declaration ();
            break;
        case Ptiger::FUNCTION:
            parse_function_declaration ();
            break;

    }
}

}