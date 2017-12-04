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

#include "ptiger/ptiger-func.h"

namespace Ptiger {

struct Parser {
private:
        void skip_after_semicolon();

        void skip_after_end();

        bool skip_token(TokenId);

        const_TokenPtr expect_token(TokenId);

        void unexpected_token(const_TokenPtr);

        // Expression parsing
        int left_binding_power(const_TokenPtr tok);

        Tree null_denotation(const_TokenPtr tok);

        Tree left_denotation(const_TokenPtr tok, Tree left);

        Tree parse_exp(int right_binding_power);

        Tree coerce_binary_arithmetic(const_TokenPtr tok, Tree *left, Tree *right);

        bool check_logical_operands(const_TokenPtr tok, Tree left, Tree right);

        Tree get_printf_addr();

        Tree get_puts_addr();

        Tree get_scanf_addr();

        Tree build_let_expression(Tree decl, Tree in_part);

        Tree build_label_decl(const char *name, location_t loc);

        Tree build_if_expression(Tree bool_expr, Tree then_part, Tree else_part);

        Tree build_function_declaration(const_TokenPtr funcname, Tree paramlist, int nargs, Tree return_type, Tree function_body_expr);

        Tree build_while_expression(Tree bool_expr, Tree while_body);

        Tree build_for_expression(SymbolPtr ind_var, Tree lower_bound, Tree upper_bound,
                                  Tree for_body_expr_list);

        const char *print_type(Tree type);

        TreeExprList &get_current_expr_list();

        void enter_scope();

        struct TreeSymbolMapping {
                Tree bind_expr;
                Tree block;
        };

        struct FuncArgsList {
                TreeExprList args_type;
                int n_args;
        };

        TreeSymbolMapping leave_scope();

        SymbolPtr query_type(const std::string &name, location_t loc);

        SymbolPtr query_variable(const std::string &name, location_t loc);

        SymbolPtr query_integer_variable(const std::string &name, location_t loc);

        void parse_expression_seq(bool (Parser::*done)());

        std::list<Ptiger::Func::arg> parse_param_list(bool (Parser::*done)());

        std::list<Ptiger::Func::arg> parse_param_list_call(bool (Parser::*done)());

        void parse_expression_parenthesis_seq(bool (Parser::*done)());

        Tree parse_func_expression_seq(bool (Parser::*done)());

        void parse_declaration_seq(bool (Parser::*done)());

        bool done_end();

        bool done_let();

        bool done_in();

        bool done_parenthesis();

        bool done_end_or_else();

        bool done_end_of_file();

        void insert_external_library_functions();

        typedef Tree (Parser::*BinaryHandler)(const_TokenPtr, Tree);

        BinaryHandler get_binary_handler(TokenId id);

#define BINARY_HANDLER_LIST \
        BINARY_HANDLER (plus, PLUS) \
        BINARY_HANDLER (minus, MINUS) \
        BINARY_HANDLER (mult, TIMES) \
        BINARY_HANDLER (div, DIVIDE) \
      \
        BINARY_HANDLER (equal, EQ) \
        BINARY_HANDLER (different, NEQ) \
        BINARY_HANDLER (lower_than, LT) \
        BINARY_HANDLER (lower_equal, LE) \
        BINARY_HANDLER (greater_than, GT) \
        BINARY_HANDLER (greater_equal, GE) \
      \
        BINARY_HANDLER (logical_and, AND) \
        BINARY_HANDLER (logical_or, OR) \
      \
        BINARY_HANDLER (array_ref, LBRACK) \
      \
        BINARY_HANDLER (field_ref, DOT)

#define BINARY_HANDLER(name, _) \
        Tree binary_ ## name (const_TokenPtr tok, Tree left);

        BINARY_HANDLER_LIST

#undef BINARY_HANDLER

public:
        Parser(Lexer &lexer_) : lexer(lexer_), puts_fn(), printf_fn(), scanf_fn() {
        }

        Tree parse_expression();

        Tree parse_exp();

        Tree parse_let_expression();

        void parse_program();

        // Tree parse_statement();

        Tree parse_declaration();

        Tree parse_variable_declaration();

        Tree parse_function_declaration();

        Tree build_function_declaration();

        Tree parse_function_call(FuncPtr func);

        Tree parse_identifier();

        Tree parse_type_declaration();

        Tree parse_type();

        Tree parse_record();

        Tree parse_field_declaration(std::vector <std::string> &field_names);

        Tree parse_assignment_expression_declaration(Tree var, const_TokenPtr assig_tok, bool isImplicit, Tree implicitExp);

        Tree parse_assignment_expression();

        Tree parse_if_expression();

        Tree parse_while_expression();

        Tree parse_for_expression();

        Tree parse_for_declaration_expression(const_TokenPtr identifier);

        Tree parse_read_statement();

        Tree parse_write_statement();

        // Tree parse_expression();

        Tree parse_expression_naming_variable();

        Tree parse_expression_naming_variable_declaration(Tree expr);

        Tree parse_lhs_assignment_expression();

        Tree parse_boolean_expression();

        Tree parse_integer_expression();

private:
        Lexer &lexer;
        Scope scope;

        tree main_fndecl;

        Tree puts_fn;
        Tree printf_fn;
        Tree scanf_fn;

        std::vector <TreeExprList> stack_expr_list;
        std::vector <TreeChain> stack_var_decl_chain;

        std::vector <BlockChain> stack_block_chain;
};

void Parser::skip_after_semicolon() {
        const_TokenPtr t = lexer.peek_token();

        while (t->get_id() != Ptiger::END_OF_FILE && t->get_id() != Ptiger::SEMICOLON) {
                lexer.skip_token();
                t = lexer.peek_token();
        }

        if (t->get_id() == Ptiger::SEMICOLON)
                lexer.skip_token();
}

void Parser::skip_after_end() {
        const_TokenPtr t = lexer.peek_token();

        while (t->get_id() != Ptiger::END_OF_FILE && t->get_id() != Ptiger::END) {
                lexer.skip_token();
                t = lexer.peek_token();
        }

        if (t->get_id() == Ptiger::END)
                lexer.skip_token();
}

const_TokenPtr Parser::expect_token(Ptiger::TokenId token_id) {
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() == token_id) {
                lexer.skip_token();
                return t;
        } else {
                error_at(t->get_locus(), "expecting %s but %s found\n",
                         get_token_description(token_id), t->get_token_description());
                return const_TokenPtr();
        }
}

bool Parser::skip_token(Ptiger::TokenId token_id) {
        return expect_token(token_id) != const_TokenPtr();
}

void Parser::unexpected_token(const_TokenPtr t) {
        ::error_at(t->get_locus(), "unexpected %s\n", t->get_token_description());
}

void Parser::parse_program() {
        // Built type of main "int (int, char**)"
        tree main_fndecl_type_param[] = {
                integer_type_node,                         /* int */
                build_pointer_type(build_pointer_type(char_type_node)) /* char** */
        };
        tree main_fndecl_type
                = build_function_type_array(integer_type_node, 2, main_fndecl_type_param);
        // Create function declaration "int main(int, char**)"
        main_fndecl = build_fn_decl("main", main_fndecl_type);

        // Enter top level scope
        enter_scope();

        insert_external_library_functions();
        // program -> statement*
        parse_expression_seq(&Parser::done_end_of_file);
        // Append "return 0;"
        tree resdecl
                = build_decl(UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
        DECL_CONTEXT(resdecl) = main_fndecl;
        DECL_RESULT(main_fndecl) = resdecl;
        tree set_result
                = build2(INIT_EXPR, void_type_node, DECL_RESULT(main_fndecl),
                         build_int_cst_type(integer_type_node, 0));
        tree return_expr = build1(RETURN_EXPR, void_type_node, set_result);

        get_current_expr_list().append(return_expr);

        // Leave top level scope, get its binding expression and its main block
        TreeSymbolMapping main_tree_scope = leave_scope();
        Tree main_block = main_tree_scope.block;

        // Finish main function
        BLOCK_SUPERCONTEXT(main_block.get_tree()) = main_fndecl;
        DECL_INITIAL(main_fndecl) = main_block.get_tree();
        DECL_SAVED_TREE(main_fndecl) = main_tree_scope.bind_expr.get_tree();

        DECL_EXTERNAL(main_fndecl) = 0;
        DECL_PRESERVE_P(main_fndecl) = 1;

        // Convert from GENERIC to GIMPLE
        gimplify_function_tree(main_fndecl);

        // Insert it into the graph
        cgraph_node::finalize_function(main_fndecl, true);

        main_fndecl = NULL_TREE;
}

bool Parser::done_end_of_file() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == Ptiger::END_OF_FILE);
}

bool Parser::done_in() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == Ptiger::IN);
}

bool Parser::done_end() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == Ptiger::END || t->get_id() == Ptiger::END_OF_FILE);
}

bool Parser::done_let() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == Ptiger::END /*|| t->get_id() == Ptiger::END_OF_FILE*/);
}

bool Parser::done_end_or_else() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == Ptiger::END || t->get_id() == Ptiger::ELSE
                || t->get_id() == Ptiger::END_OF_FILE);
}

bool Parser::done_parenthesis() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == Ptiger::RPAREN || t->get_id() == Ptiger::END);
}

// void Parser::parse_expression_seq(bool (Parser::*done)()) {
//     // Parse statements until done and append to the current stmt list;
//     while (!(this->*done)()) {
//         Tree expr = parse_expression();
//         get_current_expr_list().append(expr);
//     }
// }

void Parser::parse_expression_seq(bool (Parser::*done)()) {
        while (!(this->*done)()) {
                Tree expr = parse_expression();
                get_current_expr_list().append(expr);
                if(!(this->*done)())
                        skip_token(Ptiger::SEMICOLON);
        }
}

void Parser::parse_expression_parenthesis_seq(bool (Parser::*done)()) {
        bool isSequence = false;
        const_TokenPtr t = lexer.peek_token();
        if(t->get_id() == Ptiger::LPAREN) {
                isSequence = true;
                lexer.skip_token();
        }
        if(isSequence) {
                while (!(this->*done)()) {
                        Tree expr = parse_expression();
                        get_current_expr_list().append(expr);
                        if(isSequence && !(this->*done)())
                                skip_token(Ptiger::SEMICOLON);
                }
                skip_token(Ptiger::RPAREN);
                isSequence = false;
        }else{
                Tree expr = parse_expression();
                get_current_expr_list().append(expr);
        }
}

Tree Parser::parse_func_expression_seq(bool (Parser::*done)()){
        bool isSequence = false;
        const_TokenPtr t = lexer.peek_token();
        Tree expr;
        if(t->get_id() == Ptiger::LPAREN) {
                isSequence = true;
                lexer.skip_token();
        }
        if(isSequence) {
                while (!(this->*done)()) {
                        expr = parse_expression();
                        get_current_expr_list().append(expr);
                        if(isSequence && !(this->*done)())
                                skip_token(Ptiger::SEMICOLON);
                }
                skip_token(Ptiger::RPAREN);
                isSequence = false;
        }else{
                expr = parse_expression();
                get_current_expr_list().append(expr);
        }
        return expr;
}

void Parser::parse_declaration_seq(bool (Parser::*done)()) {
        // Parse statements until done and append to the current stmt list;
        while (!(this->*done)()) {
                Tree decl = parse_declaration();
                get_current_expr_list().append(decl);
        }
}

void Parser::enter_scope() {
        scope.push_scope();
        scope.push_scope_fn();

        TreeExprList expr_list;
        stack_expr_list.push_back(expr_list);

        stack_var_decl_chain.push_back(TreeChain());
        stack_block_chain.push_back(BlockChain());
}

Parser::TreeSymbolMapping Parser::leave_scope() {
        TreeExprList current_expr_list = get_current_expr_list();
        stack_expr_list.pop_back();

        TreeChain var_decl_chain = stack_var_decl_chain.back();
        stack_var_decl_chain.pop_back();

        BlockChain subblocks = stack_block_chain.back();
        stack_block_chain.pop_back();

        tree new_block
                = build_block(var_decl_chain.first.get_tree(),
                              subblocks.first.get_tree(),
                              /* supercontext */ NULL_TREE, /* chain */ NULL_TREE);

        // Add the new block to the current chain of blocks (if any)
        if (!stack_block_chain.empty()) {
                stack_block_chain.back().append(new_block);
        }

        // Set the subblocks to have the new block as their parent
        for (tree it = subblocks.first.get_tree(); it != NULL_TREE;
             it = BLOCK_CHAIN(it))
                BLOCK_SUPERCONTEXT(it) = new_block;

        tree bind_expr
                = build3(BIND_EXPR, void_type_node, var_decl_chain.first.get_tree(),
                         current_expr_list.get_tree(), new_block);

        TreeSymbolMapping tree_scope;
        tree_scope.bind_expr = bind_expr;
        tree_scope.block = new_block;

        scope.pop_scope();
        scope.pop_scope_fn();

        return tree_scope;
}

TreeExprList & Parser::get_current_expr_list() {
        return stack_expr_list.back();
}

Tree Parser::parse_identifier(){
        const_TokenPtr t = lexer.peek_token();
        // SymbolPtr s = query_variable(t->get_str(), t->get_locus());
        SymbolPtr s = scope.lookup(t->get_str());
        FuncPtr f = scope.lookup_fn(t->get_str());
        // if (s == NULL)
        //     return Tree::error();
        if (s != NULL)
                return parse_assignment_expression();
        else if (t != NULL) {
                return parse_function_call(f);
        }

}

Tree Parser::parse_expression() {
        /*
           expression ->  let
         |  let_expression
         |  if_expression
         |  for_expression
         |  while_expression
         |  assignment_expression
         |  write_expression_quebrando_um_galho
         */
        const_TokenPtr t = lexer.peek_token();
        // printf("\tTOKEN NO PARSER : %s\n", t->get_token_description());
        switch (t->get_id()) {
        case Ptiger::WRITE:
                return parse_write_statement();
                break;
        // case Ptiger::VAR:
        //     return parse_variable_declaration();
        //     break;
        case Ptiger::LET:
                return parse_let_expression();
                break;
        case Ptiger::IF:
                return parse_if_expression();
        case Ptiger::FOR:
                return parse_for_expression();
        case Ptiger::WHILE:
                return parse_while_expression();
        case Ptiger::IDENTIFIER:
                return parse_identifier();
        // return parse_assignment_expression();
        default:
                unexpected_token(t);
                skip_after_semicolon();
                return Tree::error();
                break;
        }

        gcc_unreachable();
}


Tree Parser::parse_assignment_expression(){
        // assignment_statement -> expression ":=" expression ";"

        Tree variable = parse_lhs_assignment_expression ();

        if (variable.is_error ())
                return Tree::error ();

        const_TokenPtr assig_tok = expect_token (Ptiger::ASSIGN);
        if (assig_tok == NULL) {
                skip_after_semicolon ();
                return Tree::error ();
        }

        const_TokenPtr first_of_expr = lexer.peek_token ();

        Tree expr = parse_exp ();
        if (expr.is_error ())
                return Tree::error ();

        if (variable.get_type () != expr.get_type ()) {
                error_at (first_of_expr->get_locus (),
                          "cannot assign value of type %s to a variable of type %s",
                          print_type (expr.get_type ()),
                          print_type (variable.get_type ()));
                return Tree::error ();
        }

        Tree assig_expr = build_tree (MODIFY_EXPR, assig_tok->get_locus (),
                                      void_type_node, variable, expr);

        return assig_expr;
}

Tree Parser::parse_assignment_expression_declaration(Tree var, const_TokenPtr assig_tok, bool isImplicit, Tree implicitExp) {
        // assignment_statement -> expression ":=" expression ";"
        Tree variable = parse_expression_naming_variable_declaration(var);

        if (variable.is_error()) return Tree::error();

        // const_TokenPtr assig_tok = expect_token(Ptiger::ASSIG);
        // if (assig_tok == NULL) {
        //     skip_after_semicolon();
        //     return Tree::error();
        // }

        const_TokenPtr first_of_expr = lexer.peek_token();
        Tree expr;
        if(!isImplicit)
                expr = parse_exp();
        else
                expr = implicitExp;
        if (expr.is_error())
                return Tree::error();
        // skip_token(Ptiger::SEMICOLON);

        if (variable.get_type() != expr.get_type()) {
                error_at(first_of_expr->get_locus(),
                         "cannot assign value of type %s to a variable of type %s",
                         print_type(expr.get_type()),
                         print_type(variable.get_type()));
                return Tree::error();
        }
        Tree assig_expr = build_tree(MODIFY_EXPR, assig_tok->get_locus(),
                                     void_type_node, variable, expr);

        // lexer.skip_token();

        return assig_expr;
}

Tree Parser::parse_variable_declaration() {
        // variable_declaration -> "var" identifier ":" type ";"

        if (!skip_token(Ptiger::VAR)) {
                skip_after_end();
                return Tree::error();
        }

        const_TokenPtr identifier = expect_token(Ptiger::IDENTIFIER);
        if (identifier == NULL) {
                skip_after_end();
                return Tree::error();
        }

        Tree type_tree;
        Tree implicitExp;
        bool isImplicit = false;
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() == Ptiger::COLON) {
                lexer.skip_token();
                implicitExp = NULL_TREE;
                type_tree = parse_type();
                if (type_tree.is_error()) {
                        skip_after_semicolon();
                        return Tree::error();
                }
        }
        else if (t->get_id() == Ptiger::ASSIGN) {
            isImplicit = true;
            lexer.skip_token();
            implicitExp = parse_exp();
            type_tree = implicitExp.get_type();
            if (type_tree.is_error()) {
                    skip_after_semicolon();
                    return Tree::error();
            }
        }
        // if (!skip_token(Ptiger::COLON)) {
        //         skip_after_end();
        //         return Tree::error();
        // }


        // Tree type_tree = parse_type();

        if (type_tree.is_error()) {
                skip_after_end();
                return Tree::error();
        }

        if (scope.get_current_mapping().get(identifier->get_str())) {
                error_at(identifier->get_locus(),
                         "name '%s' already declared in this scope",
                         identifier->get_str().c_str());
        }

        SymbolPtr sym(new Symbol(Ptiger::VARIABLE, identifier->get_str()));
        scope.get_current_mapping().insert(sym);

        Tree decl = build_decl(identifier->get_locus(), VAR_DECL,
                               get_identifier(sym->get_name().c_str()),
                               type_tree.get_tree());
        DECL_CONTEXT(decl.get_tree()) = main_fndecl;

        gcc_assert(!stack_var_decl_chain.empty());
        stack_var_decl_chain.back().append(decl);

        sym->set_tree_decl(decl);


        // if (!skip_token(Ptiger::ASSIGN)) {
        //     skip_after_end();
        //     return Tree::error();
        // }


        Tree expr = build_tree(DECL_EXPR, identifier->get_locus(), void_type_node, decl);
        get_current_expr_list().append(expr);


        const_TokenPtr assig_tok = lexer.peek_token();
        if(!isImplicit) skip_token(Ptiger::ASSIGN);

        Tree assign_tree;
        if(isImplicit)
            assign_tree = parse_assignment_expression_declaration(decl, assig_tok, true, implicitExp);
        else
            assign_tree = parse_assignment_expression_declaration(decl, assig_tok, false, NULL_TREE);

        if (assign_tree.is_error()) {
                skip_after_semicolon();
                return Tree::error();
        }

        return assign_tree;
}

void Parser::insert_external_library_functions(){

        tree return_type;
        std::list<Ptiger::Func::arg> argslist;
        Ptiger::Func::arg a;
        const std::string str;

        //int *initArray(int size, int init)
        // tree return_type = build_pointer_type(integer_type_node);
        // std::list<tree> argslist (integer_type_node, integer_type_node);
        //
        // FuncPtr func(new Func(Ptiger::EXTERNAL, funcname->get_str(), return_type, argslist));
        // scope.get_current_mapping_fn().insert(func);

        //void print_hello_ptiger()
        tree return_type1 = void_type_node;
        std::list<Ptiger::Func::arg> argslist1;
        const std::string str1 = "print_hello_ptiger";

        FuncPtr func1(new Func(Ptiger::EXTERNAL, str1, return_type1, argslist1));
        scope.get_current_mapping_fn().insert(func1);

        //void print_integer(int n)
        tree return_type2 = void_type_node;

        a.arg_type = integer_type_node;
        a.expr = NULL_TREE;
        std::list<Ptiger::Func::arg> argslist2;
        argslist2.push_back(a);

        const std::string str2 = "print_integer";

        FuncPtr func2(new Func(Ptiger::EXTERNAL, str2, return_type2, argslist2));
        scope.get_current_mapping_fn().insert(func2);

        // printf("ALL EXTERNAL FUNCTIONS LOADED\n");

}

//     std::list<Ptiger::Func::arg> Parser::parse_param_list(bool (Parser::*done)()){
//         Ptiger::Func::arg a;
//         std::list<Ptiger::Func::arg> list;
//         // std::list<Ptiger::Func::arg>iterator it;
//         int i = 0;
//         while (!(this->*done)()) {
//             Tree type = parse_type();
//             a.arg_type = type.get_tree();
//             a.expr = NULL_TREE;
//             // it = list.end();
//             // list.insert(it, a);
//             list.push_back(a);
//             if (!skip_token(Ptiger::COLON)) {
//               skip_after_end();
//               error_at(type.get_locus(), "missing ':' token");
//               return std::list<Ptiger::Func::arg>();
//             }
//             const_TokenPtr paramid = expect_token(Ptiger::IDENTIFIER);
//             if(!(this->*done)())
//               if (!skip_token(Ptiger::COMMA)) {
//                   skip_after_end();
//                   error_at(type.get_locus(), "missing ',' token");
//                   return std::list<Ptiger::Func::arg>();
//               }
//           }
//           return list;
//     }
//
//     std::list<Ptiger::Func::arg> Parser::parse_param_list_call(bool (Parser::*done)()){
//         // printf("parsing paramlist\n");
//         Ptiger::Func::arg a;
//         std::list<Ptiger::Func::arg> list;
//         // std::list<Ptiger::Func::arg>iterator it;
//         int i = 0;
//         while (!(this->*done)()) {
//             // printf("parsing argument %d\n", ++i);
//             Tree expr = parse_exp();
//             a.expr = expr.get_tree();
//             Tree type = expr.get_type();
//             a.arg_type = type.get_tree();
//             // it = list.end();
//             // list.insert(it, a);
//             list.push_back(a);
//             // tree t = expr.get_type();
//             if(!(this->*done)())
//               if (!skip_token(Ptiger::COMMA)) {
//                   skip_after_end();
//                   error_at(expr.get_locus(), "missing ',' token");
//                   return std::list<Ptiger::Func::arg>();
//               }
//             }
//             return list;
//     }
//
//     Tree Parser::parse_function_call(FuncPtr func){
//
//         const_TokenPtr funcname = expect_token(Ptiger::IDENTIFIER);
//         printf("vou chamar a função ~%s~\n", func->get_name().c_str());
//         if (funcname == NULL) {
//             skip_after_end();
//             return Tree::error();
//         }
//
//         if (!skip_token(Ptiger::LPAREN)) {
//             skip_after_end();
//             return Tree::error();
//         }
//
//         const_TokenPtr first_of_expr = lexer.peek_token();
//
//         std::list<Ptiger::Func::arg> funcall_argslist = parse_param_list_call(&Parser::done_parenthesis);
//         // Tree arg = parse_exp();
//
//         // printf("funcall_argslist.size() -> %d\n", funcall_argslist.size());
//         int foo1 = funcall_argslist.size();
//         int foo2 = func->get_argslist_size();
//
//         if(foo1 < foo2){
//             // printf(" foo1 -> %d | foo2 -> %d\n", foo1, foo2);
//             error_at(funcname->get_locus(), "few arguments to this function");
//             return Tree::error();
//         } else if (foo1 > foo2){
//             // printf(" foo1 -> %d | foo2 -> %d\n", foo1, foo2);
//             error_at(funcname->get_locus(), "to many arguments to this function");
//             return Tree::error();
//         } else if(foo1 != 0){
//             // printf(" foo1 -> %d | foo2 -> %d\n", foo1, foo2);
//             for (std::list<Ptiger::Func::arg>::iterator it1 = funcall_argslist.begin(), it2 = func->get_argslist().begin();
//                     it1 != funcall_argslist.end(), it2 != func->get_argslist().end();
//                         ++it1, ++it2){
//                 if(it1->arg_type != it2->arg_type){
//                     error_at(funcname->get_locus(), "expecting another type of argument");
//                     return Tree::error();
//                 }
//             }
//         }
//         if (!skip_token(Ptiger::RPAREN)) {
//             skip_after_end();
//             return Tree::error();
//         }
//
//         tree fncall_type;
//
//         if(foo2 == 0){
//             tree args[] = {NULL_TREE};
//             fncall_type = build_varargs_function_type_array(func->get_ret_type(), 0, args);
//         }
//         else{
//             tree args[foo2];
//             // tree args[1] = {integer_type_node};
//             int i = 0;
//             for (std::list<Ptiger::Func::arg>::iterator it = func->get_argslist().begin(); it != func->get_argslist().end(); ++it){
//                 args[i] = it->arg_type;
//                 i++;
//             }
//             fncall_type = build_varargs_function_type_array(func->get_ret_type(), foo2, args);
//             // fncall_type = build_varargs_function_type_array(func->get_ret_type(), 1, args);
//         }
//
//         tree fn_call = build_fn_decl(func->get_name().c_str(), fncall_type);
//         DECL_EXTERNAL(fn_call) = 1;
//
//         Tree fn = build1(ADDR_EXPR, build_pointer_type(fncall_type), fn_call);
//
// ///////
//         tree expr;
//         if(foo2 == 0){
//             tree args[] = {NULL_TREE};
//             expr = build_call_array_loc(funcname->get_locus(), func->get_ret_type(), fn.get_tree(), 0, args);
//         }
//         else{
//             tree args[foo2];
//             // args[0] = arg.get_tree();
//             int i = 0;
//             for (std::list<Ptiger::Func::arg>::iterator it = funcall_argslist.begin(); it != funcall_argslist.end(); ++it){
//                 // args[i] = it->expr;
//                 args[i] = it->expr;
//                 i++;
//             }
//
//             expr = build_call_array_loc(funcname->get_locus(), func->get_ret_type(), fn.get_tree(), foo2, args);
//             // expr = build_call_array_loc(first_of_expr->get_locus(), func->get_ret_type(), fn.get_tree(), 1, args);
//         }
//
//         return expr;
//     }
//
//
//     Tree Parser::parse_function_declaration(){
//         if (!skip_token(Ptiger::FUNCTION)) {
//             skip_after_end();
//             return Tree::error();
//         }
//
//         const_TokenPtr funcname = expect_token(Ptiger::IDENTIFIER);
//         if (funcname == NULL) {
//             skip_after_end();
//             return Tree::error();
//         }
//         printf("to parsando a função ~%s~\n", funcname->get_str().c_str());
//
//         if (scope.get_current_mapping_fn().get(funcname->get_str())) {
//             error_at(funcname->get_locus(),
//                      "function '%s' already declared in this scope",
//                      funcname->get_str().c_str());
//         }
//
//         if (!skip_token(Ptiger::LPAREN)) {
//             skip_after_end();
//             return Tree::error();
//         }
//         printf("vou pegar os argumentos !!\n");
//
//         const_TokenPtr first_of_expr = lexer.peek_token();
//
//         std::list<Ptiger::Func::arg> argslist = parse_param_list(&Parser::done_parenthesis);
//
//         if (!skip_token(Ptiger::RPAREN)) {
//             skip_after_end();
//             return Tree::error();
//         }
//         printf("terminei com os argumentos!!\n");
//
//         tree return_type = void_type_node;
//         // const_TokenPtr t = lexer.peek_token();
//         // if(t->get_id() == Ptiger::COLON) {
//         //     lexer.skip_token();
//         //     return_type = parse_type();
//         // }
//         printf("verifiquei o retorno !!\n");
//         if (return_type == void_type_node)
//             printf("a função não tem retorno\n");
//
//         if (!skip_token(Ptiger::EQ)) {
//             skip_after_end();
//             return Tree::error();
//         }
//         printf("vou começar a parsar o corpo da função !!\n");
//
//         enter_scope();
//
//         Tree last_expr = parse_func_expression_seq(&Parser::done_parenthesis);
//         // parse_expression_parenthesis_seq(&Parser::done_parenthesis);
//         printf("parsei o corpo!!\n");
//
//         TreeSymbolMapping function_body_scope = leave_scope();
//         Tree function_body_expr = function_body_scope.bind_expr;
//
//         FuncPtr func(new Func(Ptiger::INTERNAL, funcname->get_str(), return_type, argslist));
//         scope.get_current_mapping_fn().insert(func);
//         printf("adicionei na lista de funções!!\n");
//
//         /////////////////BUILD FUNC
//
//         int foosize = argslist.size();
//         tree fn_type;
//         if(foosize == 0){
//             tree args[] = {NULL_TREE};
//             fn_type = build_function_type_array(return_type, 0, args);
//         }
//         else{
//             tree args[foosize];
//             // tree args[1] = {integer_type_node};
//             int i = 0;
//             for (std::list<Ptiger::Func::arg>::iterator it = argslist.begin(); it != argslist.end(); ++it){
//                 args[i] = it->arg_type;
//                 i++;
//             }
//             fn_type = build_function_type_array(return_type, foosize, args);
//             // fn_type = build_varargs_function_type_array(return_type, 1, args);
//         }
//         printf("to buildando 1 !!\n");
//
//         tree fn_build_decl = build_fn_decl(funcname->get_str().c_str(), fn_type);
//
//         printf("to buildando 2 !!\n");
//
//         // tree ret_decl = build_decl(UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, return_type);
//         // DECL_CONTEXT(ret_decl) = fn_build_decl;
//         // DECL_RESULT(fn_build_decl) = ret_decl;
//         // printf("to buildando 3 !!\n");
//
//         // tree set_result;
//         // if (return_type == void_type_node) {
//         //     set_result = build2(INIT_EXPR, void_type_node, DECL_RESULT(fn_build_decl),
//         //                      return_type);
//         //                      printf("to buildando 4.1 !!\n");
//         //     // set_result = NULL_TREE;
//         // }
//         // else{
//         //     set_result = build2(INIT_EXPR, void_type_node, DECL_RESULT(fn_build_decl),
//         //                      convert(return_type, last_expr.get_tree()));
//         //                      printf("to buildando 4.2 !!\n");
//         // }
//         //
//         //
//         // tree return_expr = build1(RETURN_EXPR, void_type_node, set_result);
//         // printf("to buildando 5 !!\n");
//         //
//         // get_current_expr_list().append(return_expr);
//
//         printf("to buildando 6 !!\n");
//
//         BLOCK_SUPERCONTEXT(function_body_expr.get_tree()) = main_fndecl;
//         DECL_INITIAL(fn_build_decl) = function_body_expr.get_tree();
//         DECL_SAVED_TREE(fn_build_decl) = function_body_scope.bind_expr.get_tree();
//         printf("to buildando 7 !!\n");
//
//         DECL_EXTERNAL(fn_build_decl) = 0;
//         DECL_PRESERVE_P(fn_build_decl) = 1;
//         printf("to buildando 8 !!\n");
//
//         // Convert from GENERIC to GIMPLE
//         gimplify_function_tree(fn_build_decl);
//         // printf("to buildando 9 !!\n");
//         //
//         // // Insert it into the graph
//         cgraph_node::finalize_function(fn_build_decl, true);
//         // printf("to buildando 10 !!\n");
//
//         return fn_build_decl;
//         // fn_build_decl = NULL_TREE;
//         // return NULL_TREE;
//
//     }

std::list<Ptiger::Func::arg> Parser::parse_param_list(bool (Parser::*done)()){
        Ptiger::Func::arg a;
        std::list<Ptiger::Func::arg> list;
        // std::list<Ptiger::Func::arg>iterator it;
        int i = 0;
        while (!(this->*done)()) {
                const_TokenPtr paramid = expect_token(Ptiger::IDENTIFIER);
                if (!skip_token(Ptiger::COLON)) {
                        skip_after_end();
                        error_at(paramid->get_locus(), "missing ':' token");
                        return std::list<Ptiger::Func::arg>();
                }

                Tree type = parse_type();
                a.arg_type = type.get_tree();
                a.expr = NULL_TREE;
                a.argname = paramid->get_str();

                list.push_back(a);
                if(!(this->*done)())
                        if (!skip_token(Ptiger::COMMA)) {
                                skip_after_end();
                                error_at(type.get_locus(), "missing ',' token");
                                return std::list<Ptiger::Func::arg>();
                        }
        }
        return list;
}

std::list<Ptiger::Func::arg> Parser::parse_param_list_call(bool (Parser::*done)()){
        // printf("parsing paramlist\n");
        Ptiger::Func::arg a;
        std::list<Ptiger::Func::arg> list;
        // std::list<Ptiger::Func::arg>iterator it;
        int i = 0;
        while (!(this->*done)()) {
                // printf("parsing argument %d\n", ++i);
                Tree expr = parse_exp();
                a.expr = expr.get_tree();
                Tree type = expr.get_type();
                a.arg_type = type.get_tree();
                // a.argname = NULL;
                // it = list.end();
                // list.insert(it, a);
                list.push_back(a);
                // tree t = expr.get_type();
                if(!(this->*done)())
                        if (!skip_token(Ptiger::COMMA)) {
                                skip_after_end();
                                error_at(expr.get_locus(), "missing ',' token");
                                return std::list<Ptiger::Func::arg>();
                        }
        }
        return list;
}

Tree Parser::parse_function_call(FuncPtr func){
        const_TokenPtr funcname = expect_token(Ptiger::IDENTIFIER);
        if (funcname == NULL) {
                skip_after_end();
                return Tree::error();
        }

        if (!skip_token(Ptiger::LPAREN)) {
                skip_after_end();
                return Tree::error();
        }

        const_TokenPtr first_of_expr = lexer.peek_token();

        std::list<Ptiger::Func::arg> funcall_argslist = parse_param_list_call(&Parser::done_parenthesis);
        // Tree arg = parse_exp();

        // printf("funcall_argslist.size() -> %d\n", funcall_argslist.size());
        int foo1 = funcall_argslist.size();
        int foo2 = func->get_argslist_size();

        if(foo1 < foo2) {
                // printf(" foo1 -> %d | foo2 -> %d\n", foo1, foo2);
                error_at(funcname->get_locus(), "few arguments to this function");
                return Tree::error();
        } else if (foo1 > foo2) {
                // printf(" foo1 -> %d | foo2 -> %d\n", foo1, foo2);
                error_at(funcname->get_locus(), "to many arguments to this function");
                return Tree::error();
        } else if(foo1 != 0) {
                // printf(" foo1 -> %d | foo2 -> %d\n", foo1, foo2);
                for (std::list<Ptiger::Func::arg>::iterator it1 = funcall_argslist.begin(), it2 = func->get_argslist().begin();
                     it1 != funcall_argslist.end(), it2 != func->get_argslist().end();
                     ++it1, ++it2) {
                        if(it1->arg_type != it2->arg_type) {
                                error_at(funcname->get_locus(), "expecting another type of argument");
                                return Tree::error();
                        }
                }
        }
        if (!skip_token(Ptiger::RPAREN)) {
                skip_after_end();
                return Tree::error();
        }

        tree fncall_type;

        if(foo2 == 0) {
                tree args[] = {NULL_TREE};
                fncall_type = build_varargs_function_type_array(func->get_ret_type(), 0, args);
        }
        else{
                tree args[foo2];
                // tree args[1] = {integer_type_node};
                int i = 0;
                for (std::list<Ptiger::Func::arg>::iterator it = func->get_argslist().begin(); it != func->get_argslist().end(); ++it) {
                        args[i] = it->arg_type;
                        i++;
                }
                fncall_type = build_varargs_function_type_array(func->get_ret_type(), foo2, args);
                // fncall_type = build_varargs_function_type_array(func->get_ret_type(), 1, args);
        }

        tree fn_call = build_fn_decl(func->get_name().c_str(), fncall_type);
        DECL_EXTERNAL(fn_call) = 1;

        Tree fn = build1(ADDR_EXPR, build_pointer_type(fncall_type), fn_call);

        ///////
        tree expr;
        if(foo2 == 0) {
                tree args[] = {NULL_TREE};
                expr = build_call_array_loc(funcname->get_locus(), func->get_ret_type(), fn.get_tree(), 0, args);
        }
        else{
                tree args[foo2];
                // args[0] = arg.get_tree();
                int i = 0;
                for (std::list<Ptiger::Func::arg>::iterator it = funcall_argslist.begin(); it != funcall_argslist.end(); ++it) {
                        // args[i] = it->expr;
                        args[i] = it->expr;
                        i++;
                }

                expr = build_call_array_loc(funcname->get_locus(), func->get_ret_type(), fn.get_tree(), foo2, args);
                // expr = build_call_array_loc(first_of_expr->get_locus(), func->get_ret_type(), fn.get_tree(), 1, args);
        }

        return expr;
}


Tree Parser::parse_function_declaration(){
        if (!skip_token(Ptiger::FUNCTION)) {
                skip_after_end();
                return Tree::error();
        }

        const_TokenPtr funcname = expect_token(Ptiger::IDENTIFIER);
        if (funcname == NULL) {
                skip_after_end();
                return Tree::error();
        }

        if (scope.get_current_mapping_fn().get(funcname->get_str())) {
                error_at(funcname->get_locus(),
                         "function '%s' already declared in this scope",
                         funcname->get_str().c_str());
        }

        if (!skip_token(Ptiger::LPAREN)) {
                skip_after_end();
                return Tree::error();
        }

        // const_TokenPtr first_of_expr = lexer.peek_token();

        std::list<Ptiger::Func::arg> argslist = parse_param_list(&Parser::done_parenthesis);

        if (!skip_token(Ptiger::RPAREN)) {
                skip_after_end();
                return Tree::error();
        }

        tree return_type = void_type_node;
        // const_TokenPtr t = lexer.peek_token();
        // if(t->get_id() == Ptiger::COLON) {
        //         lexer.skip_token();
        //         Tree r = parse_type();
        //         return_type = r.get_tree();
        // }

        if (!skip_token(Ptiger::EQ)) {
                skip_after_end();
                return Tree::error();
        }

        // INSERT FUNC ON FUNCTABLE
        FuncPtr func(new Func(Ptiger::INTERNAL, funcname->get_str(), return_type, argslist));
        scope.get_current_mapping_fn().insert(func);

        /////////////////BUILD FUNC

        // BUILD FUNCTION  TYPE CONSIDERING RET_TYPE AND ARGUMENTS

        enter_scope();
        // printf("%s - 1\n", funcname->get_str().c_str());
        const_TokenPtr first_of_expr = lexer.peek_token();


        Tree last_expr = parse_func_expression_seq(&Parser::done_parenthesis);
        // parse_expression_parenthesis_seq(&Parser::done_parenthesis);

        TreeSymbolMapping function_body_scope = leave_scope();
        Tree function_block = function_body_scope.block;
        // printf("%s - 2\n", funcname->get_str().c_str());
        int foosize = argslist.size();
        tree fn_type;
        tree thereareargs[foosize];
        if(foosize == 0) {
                tree args[] = {NULL_TREE};
                fn_type = build_function_type_array(void_type_node, 0, args);
        }
        else{
                // thereareargs[foosize];
                // tree args[1] = {integer_type_node};
                int i = 0;
                for (std::list<Ptiger::Func::arg>::iterator it = argslist.begin(); it != argslist.end(); ++it) {
                        thereareargs[i] = it->arg_type;
                        i++;
                }
                fn_type = build_function_type_array(return_type, foosize, thereareargs);
                // fn_type = build_varargs_function_type_array(return_type, 1, args);
        }
        // printf("%s - 3\n", funcname->get_str().c_str());
        // BUILD FUNCTION DECLARATION
        // tree fndecl = build_fn_decl(funcname->get_str().c_str(), fn_type);
        tree ident = get_identifier (funcname->get_str().c_str());
        tree fndecl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL, ident, fn_type);
        // printf("%s - 4\n", funcname->get_str().c_str());
        DECL_EXTERNAL (fndecl) = 0;
        TREE_PUBLIC (fndecl) = 1;
        TREE_STATIC (fndecl) = 1;
        // printf("%s - 5\n", funcname->get_str().c_str());
        tree arglist = NULL_TREE;
        // printf("%s - 6\n", funcname->get_str().c_str());
        if(return_type != void_type_node) {
                tree resdecl = build_decl(UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, return_type);
                DECL_CONTEXT(resdecl) = fndecl;
                DECL_RESULT(fndecl) = resdecl;
                tree set_result = build2(INIT_EXPR, void_type_node, DECL_RESULT(fndecl),
                                         last_expr.get_tree());
                tree return_expr = build1(RETURN_EXPR, void_type_node, set_result);

                get_current_expr_list().append(return_expr);
        }
        else {
                tree result_decl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE, return_type);
                DECL_RESULT (fndecl) = result_decl;
        }
        // printf("%s - 7\n", funcname->get_str().c_str());

        SET_DECL_ASSEMBLER_NAME (fndecl, ident);
        // printf("%s - 8\n", funcname->get_str().c_str());
        if(foosize != 0) {
                int i = 0;
                for (std::list<Ptiger::Func::arg>::iterator it = argslist.begin(); it != argslist.end(); ++it) {
                        // args[i] = it->arg_type;
                        // printf("HALLO WIE GEHTS?\n");
                        // printf("%s - 1 - PARAM <%s>\n", funcname->get_str().c_str(), it->argname.c_str());

                        tree self_parm_decl = build_decl (BUILTINS_LOCATION, PARM_DECL,
                                                          get_identifier (it->argname.c_str()),
                                                          integer_type_node);
                        // printf("%s - 2 - PARAM <%s>\n", funcname->get_str().c_str(), it->argname.c_str());
                        DECL_CONTEXT (self_parm_decl) = fndecl;
                        // printf("%s - 3 - PARAM <%s>\n", funcname->get_str().c_str(), it->argname.c_str());
                        DECL_ARG_TYPE (self_parm_decl) = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
                        // printf("%s - 4 - PARAM <%s>\n", funcname->get_str().c_str(), it->argname.c_str());
                        TREE_READONLY (self_parm_decl) = 1;
                        // printf("%s - 5 - PARAM <%s>\n", funcname->get_str().c_str(), it->argname.c_str());
                        thereareargs[i] = chainon (thereareargs[i], self_parm_decl);
                        // printf("%s - 6 - PARAM <%s>\n", funcname->get_str().c_str(), it->argname.c_str());
                        TREE_USED (self_parm_decl) = 1;
                        // printf("%s - 7 - PARAM <%s>\n", funcname->get_str().c_str(), it->argname.c_str());
                        // DECL_ARGUMENTS (fndecl) = thereareargs[i];
                        // printf("%s - 8 - PARAM <%s>\n", funcname->get_str().c_str(), it->argname.c_str());
                        i++;
                }
        }

        tree block = function_body_scope.bind_expr.get_tree();



        // get_current_expr_list().append(block);

        // DECL_INITIAL(fndecl) = block;

        tree bl = function_block.get_tree();
        BLOCK_SUPERCONTEXT(bl) = fndecl;
        DECL_INITIAL(fndecl) = bl;
        BLOCK_VARS(bl) = NULL_TREE;
        TREE_USED(bl) = 1;
        tree bind = build3(BIND_EXPR, void_type_node, BLOCK_VARS(bl),
                           NULL_TREE, bl);
        TREE_SIDE_EFFECTS(bind) = 1;

        BIND_EXPR_BODY(bind) = block;
        // block = bind;
        DECL_SAVED_TREE(fndecl) = block;
        // bind;
        DECL_PRESERVE_P(fndecl) = 1;
        // printf("%s - 10\n", funcname->get_str().c_str());
        gimplify_function_tree(fndecl);
        // Insert it into the graph
        // cgraph_node::add_new_function(fndecl, false);
        cgraph_node::finalize_function(fndecl, true);
        // printf("%s - FINISH\n", funcname->get_str().c_str());

        // tree stmt
        //         = build_call_array_loc(first_of_expr->get_locus(), integer_type_node,
        //                                block, foosize, thereareargs);
        // Tree function
        //  = build1 (ADDR_EXPR, build_pointer_type (fn_type), fndecl);
        return NULL_TREE;
}


Tree Parser::parse_declaration(){
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
//            case Ptiger::TYPE:
//                return parse_type_declaration();
        case Ptiger::VAR:
                return parse_variable_declaration();
        case Ptiger::FUNCTION:
                // printf("achei uma declaração de função\n");
                return parse_function_declaration();
        default:
                unexpected_token(t);
                skip_after_semicolon();
                return Tree::error();
                break;
        }
}

namespace {

bool is_string_type(Tree type) {
        gcc_assert(TYPE_P(type.get_tree()));
        return type.get_tree_code() == POINTER_TYPE
               && TYPE_MAIN_VARIANT(TREE_TYPE(type.get_tree())) == char_type_node;
}

bool is_array_type(Tree type) {
        gcc_assert(TYPE_P(type.get_tree()));
        return type.get_tree_code() == ARRAY_TYPE;
}

bool is_record_type(Tree type) {
        gcc_assert(TYPE_P(type.get_tree()));
        return type.get_tree_code() == RECORD_TYPE;
}

}

const char * Parser::print_type(Tree type) {
        gcc_assert(TYPE_P(type.get_tree()));

        if (type == void_type_node) {
                return "void";
        } else if (type == integer_type_node) {
                return "int";
        } else if (type == float_type_node) {
                return "real";
        }
        else if (is_string_type(type)) {
                return "string";
        } else if (is_array_type(type)) {
                return "array";
        } else if (type == boolean_type_node) {
                return "boolean";
        }
        else {
                return "<<unknown-type>>";
        }
}

// Tree
// Parser::parse_field_declaration(std::vector <std::string> &field_names) {
//     // identifier ':' type ';'
//     const_TokenPtr identifier = expect_token(Ptiger::IDENTIFIER);
//     if (identifier == NULL) {
//         skip_after_semicolon();
//         return Tree::error();
//     }
//
//     skip_token(Ptiger::COLON);
//
//     Tree type = parse_type();
//
//     skip_token(Ptiger::SEMICOLON);
//
//     if (type.is_error())
//         return Tree::error();
//
//     if (std::find(field_names.begin(), field_names.end(),
//                   identifier->get_str())
//         != field_names.end()) {
//         error_at(identifier->get_locus(), "repeated field name");
//         return Tree::error();
//     }
//     field_names.push_back(identifier->get_str());
//
//     Tree field_decl
//             = build_decl(identifier->get_locus(), FIELD_DECL,
//                          get_identifier(identifier->get_str().c_str()),
//                          type.get_tree());
//     TREE_ADDRESSABLE(field_decl.get_tree()) = 1;
//
//     return field_decl;
// }

// Tree
// Parser::parse_record() {
//     // "record" field-decl* "end"
//     const_TokenPtr record_tok = expect_token(Ptiger::RECORD);
//     if (record_tok == NULL) {
//         skip_after_semicolon();
//         return Tree::error();
//     }
//
//     Tree record_type = make_node(RECORD_TYPE);
//     Tree field_list, field_last;
//     std::vector <std::string> field_names;
//
//     const_TokenPtr next = lexer.peek_token();
//     while (next->get_id() != Ptiger::END) {
//         Tree field_decl = parse_field_declaration(field_names);
//
//         if (!field_decl.is_error()) {
//             DECL_CONTEXT(field_decl.get_tree()) = record_type.get_tree();
//             if (field_list.is_null())
//                 field_list = field_decl;
//             if (!field_last.is_null())
//                 TREE_CHAIN(field_last.get_tree()) = field_decl.get_tree();
//             field_last = field_decl;
//         }
//         next = lexer.peek_token();
//     }
//
//     skip_token(Ptiger::END);
//
//     TYPE_FIELDS(record_type.get_tree()) = field_list.get_tree();
//     layout_type(record_type.get_tree());
//
//     return record_type;
// }

Tree Parser::parse_type() {
        // type -> "int"
        //      | "real"
        //      | IDENTIFIER
        //      | type '[' expr ']'
        //      | type '(' expr : expr ')'
        //      | "record" field-decl* "end"

        const_TokenPtr t = lexer.peek_token();

        Tree type;

        switch (t->get_id()) {
        case Ptiger::INT:
                lexer.skip_token();
                type = integer_type_node;
                break;
        case Ptiger::REAL:
                lexer.skip_token();
                type = float_type_node;
                break;
        // case Ptiger::BOOL:
        //     lexer.skip_token();
        //     type = boolean_type_node;
        //     break;
        case Ptiger::IDENTIFIER: {
                SymbolPtr s = query_type(t->get_str(), t->get_locus());
                lexer.skip_token();
                if (s == NULL)
                        type = Tree::error();
                else
                        type = TREE_TYPE(s->get_tree_decl().get_tree());
        }
        break;
        // case Ptiger::RECORD:
        //     type = parse_record();
        //     break;
        default:
                unexpected_token(t);
                return Tree::error();
                break;
        }

        typedef std::vector <std::pair<Tree, Tree> > Dimensions;
        Dimensions dimensions;

        t = lexer.peek_token();
        while (t->get_id() == Ptiger::LPAREN || t->get_id() == Ptiger::LBRACK) {
                lexer.skip_token();

                Tree lower_bound, upper_bound;
                if (t->get_id() == Ptiger::LBRACK) {
                        Tree size = parse_integer_expression();
                        skip_token(Ptiger::RBRACK);

                        lower_bound = Tree(build_int_cst_type(integer_type_node, 0),
                                           size.get_locus());
                        upper_bound
                                = build_tree(MINUS_EXPR, size.get_locus(), integer_type_node,
                                             size, build_int_cst(integer_type_node, 1));

                } else if (t->get_id() == Ptiger::LPAREN) {
                        lower_bound = parse_integer_expression();
                        skip_token(Ptiger::COLON);

                        upper_bound = parse_integer_expression();
                        skip_token(Ptiger::RPAREN);
                } else {
                        gcc_unreachable();
                }

                dimensions.push_back(std::make_pair(lower_bound, upper_bound));
                t = lexer.peek_token();
        }

        for (Dimensions::reverse_iterator it = dimensions.rbegin();
             it != dimensions.rend(); it++) {
                it->first = Tree(fold(it->first.get_tree()), it->first.get_locus());
                //       if (it->first.get_tree_code () != INTEGER_CST)
                //  {
                //    error_at (it->first.get_locus (), "is not an integer constant");
                //    break;
                //  }
                it->second
                        = Tree(fold(it->second.get_tree()), it->second.get_locus());
                //       if (it->second.get_tree_code () != INTEGER_CST)
                //  {
                //    error_at (it->second.get_locus (), "is not an integer constant");
                //    break;
                //  }

                if (!type.is_error()) {
                        Tree range_type
                                = build_range_type(integer_type_node, it->first.get_tree(),
                                                   it->second.get_tree());
                        type = build_array_type(type.get_tree(), range_type.get_tree());
                }
        }

        return type;
}

SymbolPtr Parser::query_type(const std::string &name, location_t loc) {
        SymbolPtr sym = scope.lookup(name);
        if (sym == NULL) {
                error_at(loc, "type '%s' not declared in the current scope",
                         name.c_str());
        } else if (sym->get_kind() != Ptiger::TYPENAME) {
                error_at(loc, "name '%s' is not a type", name.c_str());
                sym = SymbolPtr();
        }
        return sym;
}

SymbolPtr Parser::query_variable(const std::string &name, location_t loc) {
        SymbolPtr sym = scope.lookup(name);
        if (sym == NULL) {
                error_at(loc, "variable '%s' not declared in the current scope",
                         name.c_str());
        } else if (sym->get_kind() != Ptiger::VARIABLE) {
                error_at(loc, "name '%s' is not a variable", name.c_str());
                sym = SymbolPtr();
        }
        return sym;
}

SymbolPtr Parser::query_integer_variable(const std::string &name, location_t loc) {
        SymbolPtr sym = query_variable(name, loc);
        if (sym != NULL) {
                Tree var_decl = sym->get_tree_decl();
                gcc_assert(!var_decl.is_null());

                if (var_decl.get_type() != integer_type_node) {
                        error_at(loc, "variable '%s' does not have integer type",
                                 name.c_str());
                        sym = SymbolPtr();
                }
        }

        return sym;
}


Tree Parser::build_label_decl(const char *name, location_t loc) {
        tree t = build_decl(loc, LABEL_DECL, get_identifier(name), void_type_node);

        gcc_assert(main_fndecl != NULL_TREE);
        DECL_CONTEXT(t) = main_fndecl;

        return t;
}

Tree Parser::build_if_expression(Tree bool_expr, Tree then_part, Tree else_part) {
        if (bool_expr.is_error())
                return Tree::error();

        Tree then_label_decl = build_label_decl("then", then_part.get_locus());

        Tree else_label_decl;
        if (!else_part.is_null())
                else_label_decl = build_label_decl("else", else_part.get_locus());

        Tree endif_label_decl = build_label_decl("end_if", then_part.get_locus());

        Tree goto_then = build_tree(GOTO_EXPR, bool_expr.get_locus(),
                                    void_type_node, then_label_decl);
        Tree goto_endif = build_tree(GOTO_EXPR, bool_expr.get_locus(),
                                     void_type_node, endif_label_decl);

        Tree goto_else_or_endif;
        if (!else_part.is_null())
                goto_else_or_endif = build_tree(GOTO_EXPR, bool_expr.get_locus(),
                                                void_type_node, else_label_decl);
        else
                goto_else_or_endif = goto_endif;

        TreeExprList expr_list;

        Tree cond_expr
                = build_tree(COND_EXPR, bool_expr.get_locus(), void_type_node, bool_expr,
                             goto_then, goto_else_or_endif);
        expr_list.append(cond_expr);

        Tree then_label_expr = build_tree(LABEL_EXPR, then_part.get_locus(),
                                          void_type_node, then_label_decl);
        expr_list.append(then_label_expr);

        expr_list.append(then_part);

        if (!else_part.is_null()) {
                // Make sure after then part has been executed we go to the end if
                expr_list.append(goto_endif);

                Tree else_label_expr = build_tree(LABEL_EXPR, else_part.get_locus(),
                                                  void_type_node, else_label_decl);
                expr_list.append(else_label_expr);

                expr_list.append(else_part);
        }

        // FIXME - location
        Tree endif_label_expr = build_tree(LABEL_EXPR, UNKNOWN_LOCATION,
                                           void_type_node, endif_label_decl);
        expr_list.append(endif_label_expr);

        return expr_list.get_tree();
}

Tree Parser::parse_if_expression() {
        if (!skip_token(Ptiger::IF)) {
                skip_after_end();
                return Tree::error();
        }

        Tree expr = parse_boolean_expression();

        skip_token(Ptiger::THEN);

        enter_scope();

        parse_expression_parenthesis_seq(&Parser::done_parenthesis);

        TreeSymbolMapping then_tree_scope = leave_scope();
        Tree then_expr = then_tree_scope.bind_expr;

        Tree else_expr;
        const_TokenPtr tok = lexer.peek_token();
        if (tok->get_id() == Ptiger::ELSE) {
                // Consume 'else'
                skip_token(Ptiger::ELSE);

                enter_scope();
                parse_expression_parenthesis_seq(&Parser::done_parenthesis);
                TreeSymbolMapping else_tree_scope = leave_scope();
                else_expr = else_tree_scope.bind_expr;

                // Consume 'end'
                // skip_token(Ptiger::END);
        }
        // else if (tok->get_id() == Ptiger::END) {
        //     // Consume 'end'
        //     skip_token(Ptiger::END);
        // } else {
        //     unexpected_token(tok);
        //     return Tree::error();
        // }

        return build_if_expression(expr, then_expr, else_expr);
}


Tree Parser::parse_let_expression() {
        if (!skip_token (Ptiger::LET)) {
                skip_after_end ();
                return Tree::error ();
        }

        enter_scope();
        parse_declaration_seq(&Parser::done_in);

        skip_token (Ptiger::IN);

        parse_expression_seq(&Parser::done_let);

        skip_token (Ptiger::END);

        TreeSymbolMapping let_tree_scope = leave_scope ();
        Tree let_exp = let_tree_scope.bind_expr;

        //return build_if_expression (expr, then_stmt, else_stmt);
        return let_exp;
}

Tree Parser::build_while_expression(Tree bool_expr, Tree while_body) {
        if (bool_expr.is_error())
                return Tree::error();

        TreeExprList expr_list;

        Tree while_check_label_decl
                = build_label_decl("while_check", bool_expr.get_locus());

        Tree while_check_label_expr
                = build_tree(LABEL_EXPR, bool_expr.get_locus(), void_type_node,
                             while_check_label_decl);
        expr_list.append(while_check_label_expr);

        Tree while_body_label_decl
                = build_label_decl("while_body", while_body.get_locus());
        Tree end_of_while_label_decl
        // FIXME - location
                = build_label_decl("end_of_while", UNKNOWN_LOCATION);

        Tree cond_expr
                = build_tree(COND_EXPR, bool_expr.get_locus(), void_type_node, bool_expr,
                             build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node,
                                        while_body_label_decl),
                             build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node,
                                        end_of_while_label_decl));
        expr_list.append(cond_expr);

        Tree while_body_label_expr
                = build_tree(LABEL_EXPR, while_body.get_locus(), void_type_node,
                             while_body_label_decl);
        expr_list.append(while_body_label_expr);

        expr_list.append(while_body);

        // FIXME - location
        Tree goto_check = build_tree(GOTO_EXPR, UNKNOWN_LOCATION, void_type_node,
                                     while_check_label_decl);
        expr_list.append(goto_check);

        // FIXME - location
        Tree end_of_while_label_expr
                = build_tree(LABEL_EXPR, UNKNOWN_LOCATION, void_type_node,
                             end_of_while_label_decl);
        expr_list.append(end_of_while_label_expr);

        return expr_list.get_tree();
}

Tree Parser::parse_while_expression() {
        if (!skip_token(Ptiger::WHILE)) {
                skip_after_end();
                return Tree::error();
        }

        Tree expr = parse_boolean_expression();
        if (!skip_token(Ptiger::DO)) {
                skip_after_end();
                return Tree::error();
        }

        enter_scope();
        parse_expression_parenthesis_seq(&Parser::done_parenthesis);
        TreeSymbolMapping while_body_tree_scope = leave_scope();

        Tree while_body_expr = while_body_tree_scope.bind_expr;

        // skip_token(Ptiger::END);

        return build_while_expression(expr, while_body_expr);
}

Tree Parser::build_for_expression(SymbolPtr ind_var, Tree lower_bound, Tree upper_bound, Tree for_body_expr_list) {
        if (ind_var == NULL)
                return Tree::error();
        Tree ind_var_decl = ind_var->get_tree_decl();

        // Lower
        if (lower_bound.is_error())
                return Tree::error();

        // Upper
        if (upper_bound.is_error())
                return Tree::error();

        // ind_var := lower;
        TreeExprList expr_list;

        Tree init_ind_var = build_tree(MODIFY_EXPR, /* FIXME */ UNKNOWN_LOCATION,
                                       void_type_node, ind_var_decl, lower_bound);
        expr_list.append(init_ind_var);

        // ind_var <= upper
        Tree while_condition
                = build_tree(LE_EXPR, upper_bound.get_locus(), boolean_type_node,
                             ind_var_decl, upper_bound);

        // for-body
        // ind_var := ind_var + 1
        Tree incr_ind_var
                = build_tree(MODIFY_EXPR, /* FIXME */ UNKNOWN_LOCATION, void_type_node,
                             ind_var_decl,
                             build_tree(PLUS_EXPR, UNKNOWN_LOCATION, integer_type_node,
                                        ind_var_decl,
                                        build_int_cst_type(::integer_type_node, 1)));

        // Wrap as a stmt list
        TreeExprList for_expr_list = for_body_expr_list;
        for_expr_list.append(incr_ind_var);

        // construct the associated while statement
        Tree while_expr
                = build_while_expression(while_condition, for_expr_list.get_tree());
        expr_list.append(while_expr);

        return expr_list.get_tree();
}

Tree Parser::parse_for_declaration_expression(const_TokenPtr identifier) {

        // const_TokenPtr identifier = expect_token(Ptiger::IDENTIFIER);
        // if (identifier == NULL) {
        //     skip_after_end();
        //     return Tree::error();
        // }


        const_TokenPtr assig_tok = expect_token(Ptiger::ASSIGN);
        if (assig_tok == NULL) {
                skip_after_end();
                return Tree::error();
        }

        Tree lower_bound = parse_integer_expression();


        Tree type_tree = integer_type_node;

        // if (type_tree.is_error()) {
        //       skip_after_end();
        //       return Tree::error();
        // }

        if (scope.get_current_mapping().get(identifier->get_str())) {
                error_at(identifier->get_locus(),
                         "name '%s' already declared in this scope",
                         identifier->get_str().c_str());
        }

        SymbolPtr sym(new Symbol(Ptiger::VARIABLE, identifier->get_str()));
        scope.get_current_mapping().insert(sym);

        Tree decl = build_decl(identifier->get_locus(), VAR_DECL,
                               get_identifier(sym->get_name().c_str()),
                               type_tree.get_tree());
        DECL_CONTEXT(decl.get_tree()) = main_fndecl;

        gcc_assert(!stack_var_decl_chain.empty());
        stack_var_decl_chain.back().append(decl);

        sym->set_tree_decl(decl);


        Tree expr = build_tree(DECL_EXPR, identifier->get_locus(), void_type_node, decl);
        get_current_expr_list().append(expr);


        return lower_bound;
}


Tree Parser::parse_for_expression() {
        if (!skip_token(Ptiger::FOR)) {
                skip_after_end();
                return Tree::error();
        }

        const_TokenPtr identifier = lexer.peek_token();
        if (identifier->get_id() != Ptiger::IDENTIFIER) {
                skip_after_end();
                return Tree::error();
        }
        lexer.skip_token();

        // const_TokenPtr identifier = expect_token(Ptiger::IDENTIFIER);
        // if (identifier == NULL) {
        //     skip_after_end();
        //     return Tree::error();
        // }
        //
        //
        //
        // if (!skip_token(Ptiger::ASSIGN)) {
        //     skip_after_end();
        //     return Tree::error();
        // }
        //
        // Tree lower_bound = parse_integer_expression();

        Tree lower_bound = parse_for_declaration_expression(identifier);

        if (!skip_token(Ptiger::TO)) {
                skip_after_end();
                return Tree::error();
        }

        Tree upper_bound = parse_integer_expression();

        if (!skip_token(Ptiger::DO)) {
                skip_after_end();
                return Tree::error();
        }

        enter_scope();
        parse_expression_parenthesis_seq(&Parser::done_parenthesis);

        TreeSymbolMapping for_body_tree_scope = leave_scope();
        Tree for_body_expr = for_body_tree_scope.bind_expr;

        // skip_token(Ptiger::END);

        // Induction var
        SymbolPtr ind_var = query_integer_variable(identifier->get_str(), identifier->get_locus());

        return build_for_expression(ind_var, lower_bound, upper_bound, for_body_expr);

        //REMOVER VARIAVER I DA TABELA DE SIMBOLOS
}

// Tree
// Parser::get_scanf_addr() {
//     if (scanf_fn.is_null()) {
//         tree fndecl_type_param[] = {
//                 build_pointer_type(
//                         build_qualified_type(char_type_node,
//                                              TYPE_QUAL_CONST)) /* const char* */
//         };
//         tree fndecl_type
//                 = build_varargs_function_type_array(integer_type_node, 1,
//                                                     fndecl_type_param);
//
//         tree scanf_fn_decl = build_fn_decl("scanf", fndecl_type);
//         DECL_EXTERNAL(scanf_fn_decl) = 1;
//
//         scanf_fn
//                 = build1(ADDR_EXPR, build_pointer_type(fndecl_type), scanf_fn_decl);
//     }
//
//     return scanf_fn;
// }

// Tree
// Parser::parse_read_statement() {
//     if (!skip_token(Ptiger::READ)) {
//         skip_after_semicolon();
//         return Tree::error();
//     }
//
//     const_TokenPtr first_of_expr = lexer.peek_token();
//     Tree expr = parse_expression_naming_variable();
//
//     skip_token(Ptiger::SEMICOLON);
//
//     if (expr.is_error())
//         return Tree::error();
//
//     // Now this variable must be addressable
//     TREE_ADDRESSABLE(expr.get_tree()) = 1;
//
//     const char *format = NULL;
//     if (expr.get_type() == integer_type_node) {
//         format = "%d";
//     } else if (expr.get_type() == float_type_node) {
//         format = "%f";
//     } else {
//         error_at(first_of_expr->get_locus(),
//                  "variable of type %s is not a valid read operand",
//                  print_type(expr.get_type()));
//         return Tree::error();
//     }
//
//     tree args[]
//             = {build_string_literal(strlen(format) + 1, format),
//                     // FIXME
//                build_tree(ADDR_EXPR, first_of_expr->get_locus(),
//                           build_pointer_type(expr.get_type().get_tree()), expr)
//                        .get_tree()};
//
//     Tree scanf_fn = get_scanf_addr();
//
//     tree stmt
//             = build_call_array_loc(first_of_expr->get_locus(), integer_type_node,
//                                    scanf_fn.get_tree(), 2, args);
//
//     return stmt;
// }
//

// ---------------- BEGINWRITE-------------------------
Tree Parser::get_puts_addr() {
        if (puts_fn.is_null()) {
                tree fndecl_type_param[] = {
                        build_pointer_type(
                                build_qualified_type(char_type_node,
                                                     TYPE_QUAL_CONST)) /* const char* */
                };
                tree fndecl_type
                        = build_function_type_array(integer_type_node, 1, fndecl_type_param);

                tree puts_fn_decl = build_fn_decl("puts", fndecl_type);
                DECL_EXTERNAL(puts_fn_decl) = 1;

                puts_fn
                        = build1(ADDR_EXPR, build_pointer_type(fndecl_type), puts_fn_decl);
        }

        return puts_fn;
}

Tree Parser::get_printf_addr() {
        if (printf_fn.is_null()) {
                tree fndecl_type_param[] = {
                        build_pointer_type(
                                build_qualified_type(char_type_node,
                                                     TYPE_QUAL_CONST)) /* const char* */
                };
                tree fndecl_type
                        = build_varargs_function_type_array(integer_type_node, 1,
                                                            fndecl_type_param);

                tree printf_fn_decl = build_fn_decl("printf", fndecl_type);
                DECL_EXTERNAL(printf_fn_decl) = 1;

                printf_fn
                        = build1(ADDR_EXPR, build_pointer_type(fndecl_type), printf_fn_decl);
        }

        return printf_fn;
}

Tree Parser::parse_write_statement() {
        // write_statement -> "write" expression ";"
        if (!skip_token(Ptiger::WRITE)) {
                skip_after_semicolon();
                return Tree::error();
        }

        const_TokenPtr first_of_expr = lexer.peek_token();
        Tree expr = parse_exp();


        // skip_token(Ptiger::SEMICOLON);

        if (expr.is_error())
                return Tree::error();

        if (expr.get_type() == integer_type_node) {
                // printf("%d\n", expr)
                const char *format_integer = "%d\n";
                tree args[]
                        = {build_string_literal(strlen(format_integer) + 1, format_integer),
                           expr.get_tree()};

                Tree printf_fn = get_printf_addr();

                tree stmt
                        = build_call_array_loc(first_of_expr->get_locus(), integer_type_node,
                                               printf_fn.get_tree(), 2, args);

                return stmt;
        } else if (expr.get_type() == float_type_node) {
                // printf("%f\n", (double)expr)
                const char *format_float = "%f\n";
                tree args[]
                        = {build_string_literal(strlen(format_float) + 1, format_float),
                           convert(double_type_node, expr.get_tree())};

                Tree printf_fn = get_printf_addr();

                tree stmt
                        = build_call_array_loc(first_of_expr->get_locus(), integer_type_node,
                                               printf_fn.get_tree(), 2, args);
                return stmt;
        } else if (is_string_type(expr.get_type())) {
                // Alternatively we could use printf('%s\n', expr) instead of puts(expr)
                tree args[] = {expr.get_tree()};

                Tree puts_fn = get_puts_addr();

                tree stmt
                        = build_call_array_loc(first_of_expr->get_locus(), integer_type_node,
                                               puts_fn.get_tree(), 1, args);
                return stmt;
        } else {
                error_at(first_of_expr->get_locus(),
                         "value of type %s is not a valid write operand",
                         print_type(expr.get_type()));
                return Tree::error();
        }
        gcc_unreachable();
}
// ---------------- ENDWRITE-------------------------
// This is a Pratt parser
Tree Parser::parse_exp(int right_binding_power) {
        const_TokenPtr current_token = lexer.peek_token();
        lexer.skip_token();

        Tree expr = null_denotation(current_token);

        if (expr.is_error())
                return Tree::error();

        while (right_binding_power < left_binding_power(lexer.peek_token())) {
                current_token = lexer.peek_token();
                lexer.skip_token();

                expr = left_denotation(current_token, expr);
                if (expr.is_error())
                        return Tree::error();
        }

        return expr;
}

namespace {
enum binding_powers {
        // Highest priority
        LBP_HIGHEST = 100,

        LBP_DOT = 90,

        LBP_ARRAY_REF = 80,

        LBP_UNARY_PLUS = 50,      // Used only when the null denotation is +
        LBP_UNARY_MINUS = LBP_UNARY_PLUS,     // Used only when the null denotation is -

        LBP_MUL = 40,
        LBP_DIV = LBP_MUL,
        LBP_MOD = LBP_MUL,

        LBP_PLUS = 30,
        LBP_MINUS = LBP_PLUS,

        LBP_EQUAL = 20,
        LBP_DIFFERENT = LBP_EQUAL,
        LBP_LOWER_THAN = LBP_EQUAL,
        LBP_LOWER_EQUAL = LBP_EQUAL,
        LBP_GREATER_THAN = LBP_EQUAL,
        LBP_GREATER_EQUAL = LBP_EQUAL,

        LBP_LOGICAL_AND = 10,
        LBP_LOGICAL_OR = LBP_LOGICAL_AND,
        LBP_LOGICAL_NOT = LBP_LOGICAL_AND,

        // Lowest priority
        LBP_LOWEST = 0,
};
}

// This implements priorities
int Parser::left_binding_power(const_TokenPtr token) {
        switch (token->get_id()) {
        case Ptiger::DOT:
                return LBP_DOT;

        case Ptiger::LBRACK:
                return LBP_ARRAY_REF;

        case Ptiger::TIMES:
                return LBP_MUL;
        case Ptiger::DIVIDE:
                return LBP_DIV;

        case Ptiger::PLUS:
                return LBP_PLUS;
        case Ptiger::MINUS:
                return LBP_MINUS;

        case Ptiger::EQ:
                return LBP_EQUAL;
        case Ptiger::NEQ:
                return LBP_DIFFERENT;
        case Ptiger::GT:
                return LBP_GREATER_THAN;
        case Ptiger::GE:
                return LBP_GREATER_EQUAL;
        case Ptiger::LT:
                return LBP_LOWER_THAN;
        case Ptiger::LE:
                return LBP_LOWER_EQUAL;
        //
        case Ptiger::OR:
                return LBP_LOGICAL_OR;
        case Ptiger::AND:
                return LBP_LOGICAL_AND;
        // Anything that cannot appear after a left operand
        // is considered a terminator
        default:
                return LBP_LOWEST;
        }
}

// This is invoked when a token (including prefix operands) is found at a
// "prefix" position
Tree Parser::null_denotation(const_TokenPtr tok) {
        switch (tok->get_id()) {
        case Ptiger::IDENTIFIER: {
                SymbolPtr s = query_variable(tok->get_str(), tok->get_locus());
                if (s == NULL)
                        return Tree::error();
                return Tree(s->get_tree_decl(), tok->get_locus());
        }
        case Ptiger::INTEGER_LITERAL:
                // FIXME : check ranges
                return Tree(build_int_cst_type(integer_type_node,
                                               atoi(tok->get_str().c_str())),
                            tok->get_locus());
                break;
        case Ptiger::REAL_LITERAL: {
                REAL_VALUE_TYPE real_value;
                real_from_string3(&real_value, tok->get_str().c_str(),
                                  TYPE_MODE(float_type_node));

                return Tree(build_real(float_type_node, real_value),
                            tok->get_locus());
        }
        break;
        case Ptiger::STRING_LITERAL: {
                std::string str = tok->get_str();
                const char *c_str = str.c_str();
                return Tree(build_string_literal(::strlen(c_str) + 1, c_str),
                            tok->get_locus());
        }
        break;
                // case Ptiger::TRUE_LITERAL : {
                //     return Tree(build_int_cst_type(boolean_type_node, 1),
                //                 tok->get_locus());
                // }
                //     break;
                // case Ptiger::FALSE_LITERAL : {
                //     return Tree(build_int_cst_type(boolean_type_node, 0),
                //                 tok->get_locus());
                // }
                break;
        case Ptiger::LPAREN: {
                Tree expr = parse_exp();
                tok = lexer.peek_token();
                if (tok->get_id() != Ptiger::RPAREN)
                        error_at(tok->get_locus(), "expecting ) but %s found\n",
                                 tok->get_token_description());
                else
                        lexer.skip_token();
                return Tree(expr, tok->get_locus());
        }
        case Ptiger::PLUS: {
                Tree expr = parse_exp(LBP_UNARY_PLUS);
                if (expr.is_error())
                        return Tree::error();
                if (expr.get_type() != integer_type_node
                    || expr.get_type() != float_type_node) {
                        error_at(tok->get_locus(),
                                 "operand of unary plus must be int or float but it is %s",
                                 print_type(expr.get_type()));
                        return Tree::error();
                }
                return Tree(expr, tok->get_locus());
        }
        case Ptiger::MINUS: {
                Tree expr = parse_exp(LBP_UNARY_MINUS);
                if (expr.is_error())
                        return Tree::error();

                if (expr.get_type() != integer_type_node
                    || expr.get_type() != float_type_node) {
                        error_at(
                                tok->get_locus(),
                                "operand of unary minus must be int or float but it is %s",
                                print_type(expr.get_type()));
                        return Tree::error();
                }

                expr
                        = build_tree(NEGATE_EXPR, tok->get_locus(), expr.get_type(), expr);
                return expr;
        }
        // case Ptiger::NOT: {
        //     Tree expr = parse_expression(LBP_LOGICAL_NOT);
        //     if (expr.is_error())
        //         return Tree::error();
        //
        //     if (expr.get_type() != boolean_type_node) {
        //         error_at(tok->get_locus(),
        //                  "operand of logical not must be a boolean but it is %s",
        //                  print_type(expr.get_type()));
        //         return Tree::error();
        //     }
        //
        //     expr = build_tree(TRUTH_NOT_EXPR, tok->get_locus(), boolean_type_node,
        //                       expr);
        //     return expr;
        // }
        default:
                unexpected_token(tok);
                return Tree::error();
        }
}

Tree Parser::coerce_binary_arithmetic(const_TokenPtr tok, Tree *left, Tree *right) {
        Tree left_type = left->get_type();
        Tree right_type = right->get_type();

        if (left_type.is_error() || right_type.is_error())
                return Tree::error();

        if (left_type == right_type) {
                if (left_type == integer_type_node || left_type == float_type_node) {
                        return left_type;
                }
        } else if ((left_type == integer_type_node && right_type == float_type_node)
                   || (left_type == float_type_node && right_type == integer_type_node)) {
                // We will coerce the integer into a float
                if (left_type == integer_type_node) {
                        *left = build_tree(FLOAT_EXPR, left->get_locus(), float_type_node,
                                           left->get_tree());
                } else {
                        *right = build_tree(FLOAT_EXPR, right->get_locus(),
                                            float_type_node, right->get_tree());
                }
                return float_type_node;
        }

        // i.e. int + boolean
        error_at(tok->get_locus(),
                 "invalid operands of type %s and %s for operator %s",
                 print_type(left_type), print_type(right_type),
                 tok->get_token_description());
        return Tree::error();
}

Parser::BinaryHandler
Parser::get_binary_handler(TokenId id) {
        switch (id) {
#define BINARY_HANDLER(name, token_id)                                         \
case Ptiger::token_id:                                                         \
        return &Parser::binary_ ## name;
                BINARY_HANDLER_LIST
#undef BINARY_HANDLER
        default:
                return NULL;
        }
}

Tree
Parser::binary_plus(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_PLUS);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(PLUS_EXPR, tok->get_locus(), tree_type, left, right);
}

Tree
Parser::binary_minus(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_MINUS);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(MINUS_EXPR, tok->get_locus(), tree_type, left, right);
}

Tree
Parser::binary_mult(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_MUL);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(MULT_EXPR, tok->get_locus(), tree_type, left, right);
}

Tree
Parser::binary_div(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_DIV);
        if (right.is_error())
                return Tree::error();

        if (left.get_type() == integer_type_node
            && right.get_type() == integer_type_node) {
                // Integer division (truncating, like in C)
                return build_tree(TRUNC_DIV_EXPR, tok->get_locus(), integer_type_node,
                                  left, right);
        } else {
                // Real division
                Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
                if (tree_type.is_error())
                        return Tree::error();

                gcc_assert(tree_type == float_type_node);

                return build_tree(RDIV_EXPR, tok->get_locus(), tree_type, left, right);
        }
}

// Tree
// Parser::binary_mod(const_TokenPtr tok, Tree left) {
//     Tree right = parse_exp(LBP_MOD);
//     if (right.is_error())
//         return Tree::error();
//
//     if (left.get_type() == integer_type_node
//         && right.get_type() == integer_type_node) {
//         // Integer division (truncating, like in C)
//         return build_tree(TRUNC_MOD_EXPR, tok->get_locus(), integer_type_node,
//                           left, right);
//     } else {
//         error_at(tok->get_locus(),
//                  "operands of modulus must be of integer type");
//         return Tree::error();
//     }
// }

Tree Parser::binary_equal(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_EQUAL);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(EQ_EXPR, tok->get_locus(), boolean_type_node, left,
                          right);
}

Tree Parser::binary_different(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_DIFFERENT);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(NE_EXPR, tok->get_locus(), boolean_type_node, left,
                          right);
}

Tree Parser::binary_lower_than(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_LOWER_THAN);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(LT_EXPR, tok->get_locus(), boolean_type_node, left,
                          right);
}

Tree Parser::binary_lower_equal(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_LOWER_EQUAL);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(LE_EXPR, tok->get_locus(), boolean_type_node, left,
                          right);
}

Tree Parser::binary_greater_than(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_GREATER_THAN);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(GT_EXPR, tok->get_locus(), boolean_type_node, left,
                          right);
}

Tree Parser::binary_greater_equal(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_GREATER_EQUAL);
        if (right.is_error())
                return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
                return Tree::error();

        return build_tree(GE_EXPR, tok->get_locus(), boolean_type_node, left,
                          right);
}

bool Parser::check_logical_operands(const_TokenPtr tok, Tree left, Tree right) {
        if (left.get_type() != boolean_type_node
            || right.get_type() != boolean_type_node) {
                error_at(
                        tok->get_locus(),
                        "operands of operator %s must be boolean but they are %s and %s\n",
                        tok->get_token_description(), print_type(left.get_type()),
                        print_type(right.get_type()));
                return false;
        }

        return true;
}

Tree Parser::binary_logical_and(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_LOGICAL_AND);
        if (right.is_error())
                return Tree::error();

        if (!check_logical_operands(tok, left, right))
                return Tree::error();

        return build_tree(TRUTH_ANDIF_EXPR, tok->get_locus(), boolean_type_node,
                          left, right);
}

Tree Parser::binary_logical_or(const_TokenPtr tok, Tree left) {
        Tree right = parse_exp(LBP_LOGICAL_OR);
        if (right.is_error())
                return Tree::error();

        if (!check_logical_operands(tok, left, right))
                return Tree::error();

        return build_tree(TRUTH_ORIF_EXPR, tok->get_locus(), boolean_type_node,
                          left, right);
}

Tree Parser::binary_array_ref(const const_TokenPtr tok, Tree left) {
        Tree right = parse_integer_expression();
        if (right.is_error())
                return Tree::error();

        if (!skip_token(Ptiger::RBRACK))
                return Tree::error();

        if (!is_array_type(left.get_type())) {
                error_at(left.get_locus(), "does not have array type");
                return Tree::error();
        }

        Tree element_type = TREE_TYPE(left.get_type().get_tree());

        return build_tree(ARRAY_REF, tok->get_locus(), element_type, left, right, Tree(), Tree());
}

Tree Parser::binary_field_ref(const const_TokenPtr tok, Tree left) {
        const_TokenPtr identifier = expect_token(Ptiger::IDENTIFIER);
        if (identifier == NULL) {
                return Tree::error();
        }

        if (!is_record_type(left.get_type())) {
                error_at(left.get_locus(), "does not have record type");
                return Tree::error();
        }

        Tree field_decl = TYPE_FIELDS(left.get_type().get_tree());
        while (!field_decl.is_null()) {
                Tree decl_name = DECL_NAME(field_decl.get_tree());
                const char *field_name = IDENTIFIER_POINTER(decl_name.get_tree());

                if (field_name == identifier->get_str())
                        break;

                field_decl = TREE_CHAIN(field_decl.get_tree());
        }

        if (field_decl.is_null()) {
                error_at(left.get_locus(),
                         "record type does not have a field named '%s'",
                         identifier->get_str().c_str());
                return Tree::error();
        }

        return build_tree(COMPONENT_REF, tok->get_locus(),
                          TREE_TYPE(field_decl.get_tree()), left, field_decl,
                          Tree());
}

// This is invoked when a token (likely an operand) is found at a (likely
// infix) non-prefix position
Tree Parser::left_denotation(const_TokenPtr tok, Tree left) {
        BinaryHandler binary_handler = get_binary_handler(tok->get_id());
        if (binary_handler == NULL) {
                unexpected_token(tok);
                return Tree::error();
        }

        return (this->*binary_handler)(tok, left);
}

Tree Parser::parse_exp() {
        return parse_exp(/* right_binding_power */ 0);
}

Tree Parser::parse_boolean_expression() {
        Tree expr = parse_exp();
        if (expr.is_error())
                return expr;

        if (expr.get_type() != boolean_type_node) {
                error_at(expr.get_locus(),
                         "expected expression of boolean type but its type is %s",
                         print_type(expr.get_type()));
                return Tree::error();
        }
        return expr;
}
//
Tree Parser::parse_integer_expression() {
        Tree expr = parse_exp();
        if (expr.is_error())
                return expr;

        if (expr.get_type() != integer_type_node) {
                error_at(expr.get_locus(),
                         "expected expression of integer type but its type is %s",
                         print_type(expr.get_type()));
                return Tree::error();
        }
        return expr;
}

Tree Parser::parse_expression_naming_variable_declaration(Tree expr) {
        if (expr.is_error())
                return expr;

        if (expr.get_tree_code() != VAR_DECL && expr.get_tree_code() != ARRAY_REF
            && expr.get_tree_code() != COMPONENT_REF) {
                error_at(expr.get_locus(),
                         "does not designate a variable, array element or field");
                return Tree::error();
        }
        return expr;
}

Tree Parser::parse_expression_naming_variable() {
        Tree expr = parse_exp();
        if (expr.is_error())
                return expr;

        if (expr.get_tree_code() != VAR_DECL && expr.get_tree_code() != ARRAY_REF
            && expr.get_tree_code() != COMPONENT_REF) {
                error_at(expr.get_locus(),
                         "does not designate a variable, array element or field");
                return Tree::error();
        }
        return expr;
}

Tree Parser::parse_lhs_assignment_expression() {
        return parse_expression_naming_variable();
}
} // END OF PTIGER NAMESPACE

// ------------ Parse Functions Used By ptiger1.cc ------------------

static void ptiger_parse_file(const char *filename);

void
ptiger_parse_files(int num_files, const char **files) {
        for (int i = 0; i < num_files; i++) {
                ptiger_parse_file(files[i]);
        }
}

static void
ptiger_parse_file(const char *filename) {
        // FIXME: handle stdin "-"
        FILE *file = fopen(filename, "r");
        if (file == NULL) {
                fatal_error(UNKNOWN_LOCATION, "cannot open filename %s: %m", filename);
        }

        Ptiger::Lexer lexer(filename, file);
        Ptiger::Parser parser(lexer);

        parser.parse_program();

        fclose(file);
}
