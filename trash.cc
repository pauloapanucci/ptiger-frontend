Tree Parser::parse_type_declaration (){
        // type_declaration -> "type" identifier ":" type ";"
        if (!skip_token (Ptiger::TYPE)) {
                skip_after_end ();
                return Tree::error ();
        }

        const_TokenPtr identifier = expect_token (Ptiger::IDENTIFIER);
        if (identifier == NULL) {
                skip_after_end ();
                return Tree::error ();
        }

        std::string name;
        Tree expr;

        const_TokenPtr t = lexer.peek_token();

        if(t->get_id() == Ptiger::COLON) {
                // if (!skip_token (Ptiger::COLON)) {
                //         skip_after_end ();
                //         return Tree::error ();
                // }
                lexer.skip_token();

                Tree type_tree = parse_type();

                if (type_tree.is_error ()) {
                        skip_after_end();
                        return Tree::error();
                }

                //skip_token (Ptiger::SEMICOLON);

                if (scope.get_current_mapping ().get (identifier->get_str ())) {
                        error_at (identifier->get_locus (),
                                  "name '%s' already declared in this scope",
                                  identifier->get_str ().c_str ());
                }

                SymbolPtr sym (new Symbol (Ptiger::TYPENAME, identifier->get_str ()));
                scope.get_current_mapping ().insert (sym);

                name = sym->get_name ().c_str ();


        } else {
                if (!skip_token (Ptiger::EQ)) {
                        skip_after_end ();
                        return Tree::error ();
                }

                const_TokenPtr t = lexer.peek_token();
                if(t->get_id() == Ptiger::LBRACK) {

                        Tree type_tree = parse_type_record ();
                        if (type_tree.is_error ()) {
                                skip_after_end();
                                return Tree::error ();
                        }
                        //skip_token (Ptiger::SEMICOLON);
                        if (scope.get_current_mapping ().get (identifier->get_str ())) {
                                error_at (identifier->get_locus (),
                                          "name '%s' already declared in this scope",
                                          identifier->get_str ().c_str ());
                        }
                        SymbolPtr sym (new Symbol (Ptiger::TYPENAME, identifier->get_str ()));
                        scope.get_current_mapping ().insert (sym);
                        name = sym->get_name ().c_str ();

                }else if(t->get_id() == Ptiger::ARRAY) {
                        skip_token(Ptiger::ARRAY);
                        skip_token(Ptiger::OF);

                        if (scope.get_current_mapping ().get (identifier->get_str ())) {
                                error_at (identifier->get_locus (),
                                          "type '%s' already declared in this scope",
                                          identifier->get_str ().c_str ());
                        }
                        t = lexer.peek_token();
                        if(t->get_id() == Ptiger::INT) {
                                SymbolPtr sym (new Symbol (Ptiger::TYPEARRAYI, identifier->get_str ()));
                                Tree type_tree = parse_type ();
                                if (type_tree.is_error ()) {
                                        skip_after_end();
                                        return Tree::error ();
                                }
                                scope.get_current_mapping ().insert (sym);

                                name = sym->get_name ().c_str ();
                        }
                        else if(t->get_id() == Ptiger::REAL) {
                                SymbolPtr sym (new Symbol (Ptiger::TYPEARRAYR, identifier->get_str ()));
                                Tree type_tree = parse_type ();
                                if (type_tree.is_error ()) {
                                        skip_after_end();
                                        return Tree::error ();
                                }
                                scope.get_current_mapping ().insert (sym);

                                name = sym->get_name ().c_str ();
                        }
                        else if(t->get_id() == Ptiger::STRING) {
                                SymbolPtr sym (new Symbol (Ptiger::TYPEARRAYS, identifier->get_str ()));
                                Tree type_tree = parse_type ();
                                if (type_tree.is_error ()) {
                                        skip_after_end();
                                        return Tree::error ();
                                }
                                scope.get_current_mapping ().insert (sym);

                                name = sym->get_name ().c_str ();
                        }
                }
                return Tree::error();
        }

        Tree decl = build_decl (identifier->get_locus (), TYPE_DECL,
                                get_identifier (name),
                                type_tree.get_tree ());
        DECL_CONTEXT (decl.get_tree()) = main_fndecl;
        gcc_assert (!stack_var_decl_chain.empty ());
        stack_var_decl_chain.back ().append (decl);
        sym->set_tree_decl (decl);

        expr = build_tree (DECL_EXPR, identifier->get_locus (), void_type_node, decl);
        return expr;
}


/*for (i = 0; i < size; i++){
    aux[i] = (double*)malloc(size * sizeof(double));
    for (j = 0; j <= i; j++){
      m[i][j] = (double)(-j % size) / size + 1;
    }
    for (j = i+1; j < size; j++) {
      m[i][j] = 0;
    }
    m[i][i] = 1;
  }

  /* Make the matrix positive semi-definite. */
  int r,s,t;

  for (r = 0; r < size; ++r)
    for (s = 0; s < size; ++s)
      aux[r][s] = 0;
  for (t = 0; t < size; ++t)
    for (r = 0; r < size; ++r)
      for (s = 0; s < size; ++s)
        aux[r][s] += m[r][t] * m[s][t];
  for (r = 0; r < size; ++r)
    for (s = 0; s < size; ++s){
      m[r][s] = aux[r][s];
    }

    function modulus(x,y){
    var m = Math.floor(x / y);
    var r = m * y;
    return x - r;

    for (i = 0; i < n; i++)
        for (j = 0; j <= i; j++) {
            if ((i * n + j) % 20 == 0) printf("\n");
            printf("%.2f ",m[i][j]);
        }

        for i := 0 to size-1 do (
            for j := 0 to size-1 do (
                x := i * n + j;
                y := size;
                m := x / y;
                r := m * y;
                mod := x - r;
                if mod = 0 then println();
                print_real(I[i][j]);
            );
            println();
        );
}

*/
