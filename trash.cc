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
    printf("to parsando a função ~%s~\n", funcname->get_str().c_str());

    if (scope.get_current_mapping_fn().get(funcname->get_str())) {
        error_at(funcname->get_locus(),
                 "function '%s' already declared in this scope",
                 funcname->get_str().c_str());
    }

    if (!skip_token(Ptiger::LPAREN)) {
        skip_after_end();
        return Tree::error();
    }
    printf("vou pegar os argumentos !!\n");

    const_TokenPtr first_of_expr = lexer.peek_token();

    std::list<Ptiger::Func::arg> argslist = parse_param_list(&Parser::done_parenthesis);

    if (!skip_token(Ptiger::RPAREN)) {
        skip_after_end();
        return Tree::error();
    }
    printf("terminei com os argumentos!!\n");

    tree return_type = void_type_node;
    // const_TokenPtr t = lexer.peek_token();
    // if(t->get_id() == Ptiger::COLON) {
    //     lexer.skip_token();
    //     return_type = parse_type();
    // }
    printf("verifiquei o retorno !!\n");
    if (return_type == void_type_node)
        printf("a função não tem retorno\n");

    if (!skip_token(Ptiger::EQ)) {
        skip_after_end();
        return Tree::error();
    }
    printf("vou começar a parsar o corpo da função !!\n");

    enter_scope();

    // Tree last_expr = parse_func_expression_seq(&Parser::done_parenthesis);
    parse_expression_parenthesis_seq(&Parser::done_parenthesis);
    printf("parsei o corpo!!\n");

    TreeSymbolMapping function_body_scope = leave_scope();
    Tree function_body_expr = function_body_scope.bind_expr;

    FuncPtr func(new Func(Ptiger::INTERNAL, funcname->get_str(), return_type, argslist));
    scope.get_current_mapping_fn().insert(func);
    printf("adicionei na lista de funções!!\n");

    /////////////////BUILD FUNC

    int foosize = argslist.size();
    tree fn_type;
    if(foosize == 0){
        tree args[] = {NULL_TREE};
        fn_type = build_function_type_array(return_type, 0, args);
    }
    else{
        tree args[foosize];
        // tree args[1] = {integer_type_node};
        int i = 0;
        for (std::list<Ptiger::Func::arg>::iterator it = argslist.begin(); it != argslist.end(); ++it){
            args[i] = it->arg_type;
            i++;
        }
        fn_type = build_function_type_array(return_type, foosize, args);
        // fn_type = build_varargs_function_type_array(return_type, 1, args);
    }
    printf("DEBUGE 1\n");
    //build fundec
    tree ident = get_identifier (funcname->get_str().c_str());
    tree fndecl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL, ident, fn_type);

    DECL_EXTERNAL (fndecl) = 0;
    TREE_PUBLIC (fndecl) = 1;
    // TREE_STATIC (fndecl) = 1;
    tree arglist = NULL_TREE;

    //define return
    tree result_decl = build_decl (BUILTINS_LOCATION, RESULT_DECL, NULL_TREE,
                                     return_type);
    DECL_RESULT (fndecl) = result_decl;

    SET_DECL_ASSEMBLER_NAME (fndecl, ident);
    printf("DEBUGE 2\n");
    // tree self_parm_decl = build_decl (BUILTINS_LOCATION, PARM_DECL,
    //                                     get_identifier ("a"),
    //                                     integer_type_node);
    //
    // DECL_CONTEXT (self_parm_decl) = fndecl;
    // DECL_ARG_TYPE (self_parm_decl) = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
    // TREE_READONLY (self_parm_decl) = 1;
    // arglist = chainon (arglist, self_parm_decl);
    //
    // TREE_USED (self_parm_decl) = 1;
    // DECL_ARGUMENTS (fndecl) = arglist;

    get_current_expr_list().append(function_body_scope.bind_expr.get_tree());


    DECL_INITIAL(fndecl) = function_body_scope.bind_expr.get_tree();

    tree bl = function_body_expr.get_tree();
    BLOCK_SUPERCONTEXT(bl) = fndecl;
    DECL_INITIAL(fndecl) = bl;
    BLOCK_VARS(bl) = NULL_TREE;
    TREE_USED(bl) = 1;
    tree bind = build3(BIND_EXPR, void_type_node, BLOCK_VARS(bl),
                       NULL_TREE, bl);
    TREE_SIDE_EFFECTS(bind) = 1;
    printf("DEBUGE 3\n");
    BIND_EXPR_BODY(bind) =  function_body_expr.get_tree();
;
    // block = bind;
    DECL_SAVED_TREE(fndecl) = function_body_expr.get_tree();
    printf("DEBUGE 4\n");
    gimplify_function_tree (fndecl);
    printf("DEBUGE 5\n");
    cgraph_node::add_new_function(fndecl, false);
    printf("DEBUGE 6\n");
    cgraph_node::finalize_function(fndecl, true);
    printf("cheguei no final\n");

    return NULL_TREE;

}
