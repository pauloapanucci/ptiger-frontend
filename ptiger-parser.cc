#include <iostream>
#include <memory>

#include "ptiger/ptiger-parser.h"
#include "ptiger/ptiger-lexer.h"
#include "ptiger/ptiger-tree.h"
#include "ptiger/ptiger-symbol.h"
#include "ptiger/ptiger-symbol-mapping.h"
#include "ptiger/ptiger-scope.h"

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

        void skip_after_end();

        bool skip_token(Ptiger::TokenId token_id);

        const_TokenPtr expect_token(Ptiger::TokenId token_id);

        void parse_program();

        Tree parse_type();

        Tree parse_assignment(const_TokenPtr identifier);

        void parse_expressions();

        void parse_let_expression();

        void parse_declarations();

        Tree parse_variable_declaration();

        void parse_type_declaration();

        SymbolPtr query_variable(const std::string &name, location_t loc);

    private:
        Lexer &lexer;
        Scope scope;

        void parse_expression_seq(bool (Parser::*done)());
    };


    void Parser::unexpected_token(const_TokenPtr t) {
        error_at(t->get_locus(), "unexpected %s\n", t->get_token_description());
    }

    void Parser::skip_after_semicolon() {
        const_TokenPtr t = lexer.peek_token();

        while (t->get_id() != Ptiger::END_OF_FILE && t->get_id() != Ptiger::SEMICOLON) {
            lexer.skip_token();
            t = lexer.peek_token();
        }

        if (t->get_id() == Ptiger::SEMICOLON) lexer.skip_token();

    }

    bool Parser::skip_token(Ptiger::TokenId token_id) {
        return expect_token(token_id) != const_TokenPtr();
    }

    const_TokenPtr Parser::expect_token(Ptiger::TokenId token_id) {
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() == token_id) {
            lexer.skip_token();
            return t;
        } else {
            error_at(t->get_locus(), "expecting %s but %s found\n", get_token_description(token_id),
                     t->get_token_description());
            return const_TokenPtr();
        }
    }

    bool Parser::done_end_of_file() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == Ptiger::END_OF_FILE);
    }

    void Parser::parse_program() {
        parse_expression_seq(&Parser::done_end_of_file);
    }

    void Parser::parse_expression_seq(bool (Parser::*done)()) {
        while (!(this->*done)()) {
            parse_expressions();
        }
    }

    Tree Parser::parse_type() {
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case Ptiger::INT:
                lexer.skip_token();
                return integer_type_node;
                break;
            case Ptiger::REAL:
                lexer.skip_token();
                return float_type_node;
                break;
            default:
                unexpected_token(t);
                return Tree::error();
                break;
        }
    }

    void Parser::parse_type_declaration() {
        if (!skip_token(Ptiger::TYPE)) {
            skip_after_semicolon();
            return;
        }
        const_TokenPtr identifier = expect_token(Ptiger::IDENTIFIER);
        if (identifier == NULL) {
            skip_after_semicolon();
            return;
        }
        if (!skip_token(Ptiger::EQ)) {
            skip_after_semicolon();
            return;
        }
//        if (!parse_type()) return;

    }

    SymbolPtr Parser::query_variable(const std::string &name, location_t loc) {
        SymbolPtr sym = scope.lookup(name);
        if (sym == NULL) {
            error_at(loc, "variable '%s' not declared in the current scope",
                     name.c_str());
        }
//        else if (sym->get_kind() != Ptiger::VAR) {
//            error_at(loc, "name '%s' is not a variable", name.c_str());
//            sym = SymbolPtr();
//        }
        return sym;
    }

    Tree Parser::parse_assignment(const_TokenPtr identifier) {
//        const_TokenPtr identifier = expect_token(Ptiger::IDENTIFIER);
        if (identifier == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }

        SymbolPtr sym = query_variable(identifier->get_str(), identifier->get_locus());
        if (sym == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }

        gcc_assert(!sym->get_tree_decl().is_null());
        Tree var_decl = sym->get_tree_decl();

        const_TokenPtr assig_tok = expect_token(Ptiger::ASSIGN);
        if (assig_tok == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }

        const_TokenPtr first_of_expr = lexer.peek_token();

        Tree expr = parse_expressions();
        if (expr.is_error()) return Tree::error();

//        skip_token(Ptiger::SEMICOLON);

        if (var_decl.get_type() != expr.get_type()) {
            error_at(first_of_expr->get_locus(), "cannot assign value of type %s to variable '%s' of type %s",
                     print_type(expr.get_type()), sym->get_name().c_str(), print_type(var_decl.get_type()));
            return Tree::error();
        }

        Tree assig_expr = build_tree(MODIFY_EXPR, assig_tok->get_locus(), void_type_node, var_decl, expr);

        return assig_expr;
    }

    Tree Parser::parse_variable_declaration() {
        if (!skip_token(Ptiger::VAR)) {
            skip_after_semicolon();
            return Tree::error();
        }
        const_TokenPtr identifier = expect_token(Ptiger::IDENTIFIER);
        if (identifier == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() == Ptiger::COLON) {
            skip_token(Ptiger::COLON);

            Tree type_tree = parse_type();
            if (type_tree.is_error()) {
                skip_after_semicolon();
                return Tree::error();
            }

            if (scope.get_current_mapping().get(identifier->get_str())) {
                error_at(identifier->get_locus(), "variable '%s' already declared in this scope",
                         identifier->get_str().c_str());
                return Tree::error();
            }

            SymbolPtr sym( new Symbol(identifier->get_str()) );
            scope.get_current_mapping().insert(sym);

            Tree decl = build_decl(identifier->get_locus(), VAR_DECL, get_identifier(sym->get_name().c_str()),
                                   type_tree.get_tree());

            gcc_assert(!stack_var_decl_chain.empty());
            stack_var_decl_chain.back().append(decl);

            sym->set_tree_decl(decl);

            Tree stmt = build_tree(DECL_EXPR, identifier->get_locus(), void_type_node, decl);

            return stmt;


        }
//        if (!skip_token(Ptiger::ASSIGN)) {
//        skip_after_semicolon();
//        return;
//            skip_token(Ptiger::ASSIGN);
//        }
//        t = lexer.peek_token();
//        if (t->get_id() == Ptiger::INT) {
//            skip_token(Ptiger::INT);
//        } else if (t->get_id() == Ptiger::REAL) {
//            skip_token(Ptiger::REAL);
//        } else unexpected_token(t);
        parse_assignment(identifier);
    }

    void Parser::parse_declarations() {
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case Ptiger::TYPE:
                parse_type_declaration();
                break;
            case Ptiger::VAR:
                parse_variable_declaration();
                break;
        }
    }

    void Parser::skip_after_end() {
        const_TokenPtr t = lexer.peek_token();

        while (t->get_id() != Ptiger::END_OF_FILE && t->get_id() != Ptiger::END) {
            lexer.skip_token();
            t = lexer.peek_token();
        }

        if (t->get_id() == Ptiger::END) lexer.skip_token();
    }

    void Parser::parse_let_expression() {
        if (!skip_token(Ptiger::LET)) {
            skip_after_end();
            return;
        }

        parse_declarations();
        skip_token(Ptiger::IN);

//    parse_expression_seq(&Parser::done_end_of_file);

        const_TokenPtr t = lexer.peek_token();

        if (t->get_id() == Ptiger::END) skip_token(Ptiger::END);
        else {
            unexpected_token(t);
            skip_after_end();
        }


    }

    void Parser::parse_expressions() {
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case Ptiger::LET:
                parse_let_expression();
                break;
            default:
                unexpected_token(t);
                skip_after_semicolon();
                break;
        }
    }

}

static void ptiger_parse_file(const char *filename);

void ptiger_parse_files(int num_files, const char **files) {
    for (int i = 0; i < num_files; i++) {
        ptiger_parse_file(files[i]);
    }
}

static void ptiger_parse_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        fatal_error(UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
    }

    Ptiger::Lexer lexer(filename, file);
    Ptiger::Parser parser(lexer);

    parser.parse_program();

    fclose(file);
}
