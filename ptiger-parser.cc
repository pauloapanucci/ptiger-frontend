#include <iostream>
#include <memory>

#include "ptiger/ptiger-parser.h"
#include "ptiger/ptiger-lexer.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "convert.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "fold-const.h"

namespace Ptiger {


struct Parser {
public:
    Parser(Lexer &lexer_) : lexer(lexer_) {}

    bool done_end_of_file();
    void unexpected_token(const_TokenPtr t);
    void skip_after_semicolon();
    bool skip_token(Ptiger::TokenId token_id);
    const_TokenPtr expect_token(Ptiger::TokenId token_id);

    void parse_program();
    bool parse_type();
    void parse_expressions();

    void parse_declarations();
    void parse_variable_declaration();
    void parse_type_declaration();

private:
    Lexer &lexer;
    void parse_expression_seq (bool (Parser::*done) ());
};




void Parser::unexpected_token(const_TokenPtr t) {
    error_at(t->get_locus(), "unexpected %s\n", t->get_token_description());
}

void Parser::skip_after_semicolon(){
    const_TokenPtr t = lexer.peek_token();

    while(t->get_id() != Ptiger::END_OF_FILE && t->get_id() != Ptiger::SEMICOLON) {
        lexer.skip_token();
        t = lexer.peek_token();
    }

    if(t->get_id() == Ptiger::SEMICOLON) lexer.skip_token();

}

bool Parser::skip_token(Ptiger::TokenId token_id) {
    return expect_token(token_id) != const_TokenPtr();
}

const_TokenPtr Parser::expect_token(Ptiger::TokenId token_id) {
    const_TokenPtr t = lexer.peek_token();
    if(t->get_id() == token_id){
        lexer.skip_token();
        return t;
    }
    else {
        error_at(t->get_locus(), "expecting %s but %s found\n", get_token_description(token_id), t->get_token_description());
        return const_TokenPtr();
    }
}

bool Parser::done_end_of_file () {
    const_TokenPtr t = lexer.peek_token();
    return (t->get_id() == Ptiger::END_OF_FILE);
}

void Parser::parse_program() {
    parse_expression_seq(&Parser::done_end_of_file);
}

void Parser::parse_expression_seq(bool (Parser::*done) ()) {
    while (!(this->*done) ()) {
        parse_expressions();
    }
}

bool Parser::parse_type () {
    const_TokenPtr t = lexer.peek_token ();
    switch (t->get_id ()){
        case Ptiger::INT:
            lexer.skip_token ();
            return true;
        case Ptiger::REAL:
            lexer.skip_token ();
            return true;
        default:
            unexpected_token (t);
            return false;
    }
}

void Parser::parse_type_declaration() {
    if(!skip_token(Ptiger::TYPE)){
        skip_after_semicolon();
        return;
    }
    const_TokenPtr identifier = expect_token (Ptiger::IDENTIFIER);
    if(identifier == NULL){
        skip_after_semicolon();
        return;
    }
    if(!skip_token(Ptiger::EQ)){
        skip_after_semicolon();
        return;
    }
    if(!parse_type()) return;

}

void Parser::parse_variable_declaration() {
    if(!skip_token(Ptiger::VAR)){
        skip_after_semicolon();
        return;
    }
    const_TokenPtr identifier = expect_token (Ptiger::IDENTIFIER);
    if(identifier == NULL){
        skip_after_semicolon();
        return;
    }
    if(!skip_token(Ptiger::ASSIGN)){
        skip_after_semicolon();
        return;
    }
    if(!parse_type()) return;

}

void Parser::parse_declarations() {
    const_TokenPtr  t = lexer.peek_token();
    switch (t->get_id ()){
        case Ptiger::TYPE:
            parse_type_declaration();
            break;
        case Ptiger::VAR:
            parse_variable_declaration();
            break;
    }
}

void Parser::parse_expressions() {
    const_TokenPtr  t = lexer.peek_token();
    switch (t->get_id ()){
        case Ptiger::LET:
            parse_declarations();
            t = lexer.peek_token();
            if(t->get_id() == Ptiger::IN) parse_expressions();
            else error_at(t->get_locus(), "missing IN statement\n", t->get_token_description());
            t = lexer.peek_token();
            if(t->get_id() != Ptiger::END) error_at(t->get_locus(), "missing END statement\n", t->get_token_description());
            break;
        default:
            unexpected_token (t);
            skip_after_semicolon ();
            break;
    }
}

}

static void ptiger_parse_file (const char *filename);

void ptiger_parse_files (int num_files, const char **files) {
    for (int i = 0; i < num_files; i++) {
        ptiger_parse_file (files[i]);
    }
}

static void ptiger_parse_file (const char *filename) {
    FILE *file = fopen (filename, "r");
    if (file == NULL) {
        fatal_error (UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
    }

    Ptiger::Lexer lexer (filename, file);
    Ptiger::Parser parser (lexer);

    parser.parse_program ();

    fclose (file);
}
